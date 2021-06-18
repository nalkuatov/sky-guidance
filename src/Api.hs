{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}

module Api where

import           RIO

import           Data.Aeson (Value, decodeStrict)
import           Servant

-- local imports
import           Client     (cache)
import           Foundation (AppT, Config, appToHandler)
import           Persist    (Location (..), runQuery)
import           Query      (selectLocation)
import           Types      (City)

type Api =
  "api" :>
  "forecast" :>
  QueryParam' '[Required] "city" City :>
  Get '[JSON] (Maybe Value)

handler :: (MonadThrow m, MonadIO m) => ServerT Api (AppT m)
handler city = do
  location <- runQuery $ selectLocation city
  case location of
    Nothing -> Just <$> cache city
    Just v  -> pure $ decodeStrict $ locationWeather v

init :: Config -> Application
init config =
  serve api $
  hoistServer api (appToHandler config) handler

api :: Proxy Api
api = Proxy

