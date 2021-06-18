module Client
  ( cache
  , repetitiveCache
  )
  where

import           RIO
import           RIO.ByteString.Lazy (toStrict)

import           Data.Aeson          (Value, encode)
import           Servant
import           Servant.Client      (ClientM, client, hoistClient, runClientM)
import           System.IO           (print)

-- local imports
import           Foundation
import           Persist             (Location (..), runQuery)
import           Query               (insertLocation_)
import           Types               (Appid, City)

type RequiredQueryParam = QueryParam' '[Required]

type WeatherApi =
  "data" :> "2.5" :> "weather"    :>
  RequiredQueryParam "q"     City :>
  RequiredQueryParam "appid" Text :>
  Get '[JSON] Value

-- | Grabs the weather data from a third party
-- api and stores it in a cache
cache :: (MonadReader Config m, MonadThrow m, MonadIO m)
  => City -> m Value
cache city = do
  key      <- asks _appid
  response <- pull city key
  let location =
        Location city (toStrict $ encode response)
  runQuery $ insertLocation_ location
  pure response

repetitiveCache :: (MonadReader Config m, MonadThrow m, MonadIO m)
  => m ()
repetitiveCache = forever $ do
  cities <- asks $ locations . _appConf
  period <- asks $ cooldown  . _appConf
  forM_ cities cache
  liftIO
    $ print
    $ "the weather for "
    <> (show cities)
    <> " is written to the cache"
  threadDelay $ fromMaybe 300000 period

-- | Pulls the weather data from a third party api
pull :: (MonadReader Config m, MonadThrow m, MonadIO m)
  => City -> Appid -> m Value
pull = hoistClient proxy run (client proxy)

run :: (MonadReader Config m, MonadThrow m, MonadIO m)
  => ClientM a -> m a
run client = do
  cenv   <- asks _clientEnv
  result <- liftIO $ ($ cenv) . runClientM $ client
  case result of
    Left  _ -> throwM err404
    Right v -> pure v

proxy :: Proxy WeatherApi
proxy = Proxy

