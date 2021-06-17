module Client
  ( pull
  )
  where

import           RIO

import           Data.Aeson     (Value)
import           Servant
import           Servant.Client

-- local imports
import           Foundation

type WeatherApi =
  QueryParam "lat"   Double :>
  QueryParam "lon"   Double :>
  QueryParam "appid" Text   :>
  Get '[JSON] Value

proxy :: Proxy WeatherApi
proxy = Proxy

run :: (MonadReader Config m, MonadIO m)
  => ClientM a -> m a
run client = do
  cenv   <- asks _clientEnv
  result <- liftIO $ ($ cenv) . runClientM $ client
  pure $ either (error . show) id $ result

pull :: (MonadReader Config m, MonadIO m)
  => Maybe Double -> Maybe Double -> Maybe Text -> m Value
pull = hoistClient proxy run (client proxy)

