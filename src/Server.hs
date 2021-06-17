module Server where

import           RIO

import           Database.Persist.Redis
import           Network.HTTP.Client
import           Servant.Client
import           Servant.Client.Core
import           System.IO              (print)

-- local imports
import           Client
import           Foundation

start :: IO ()
start = do
  manager  <- newManager defaultManagerSettings
  let cenv  = mkClientEnv manager (BaseUrl Http "api.openweathermap.org" 80 "/data/2.5/weather")
  let rconf = RedisConf "localhost" (PortNumber 6379) Nothing 10
  let conf  = Config rconf cenv
  let appid = Just "505e41dc43d2ea7c3abffa42042d82aa"
  res      <- runAppT (pull (Just 30) (Just 45) appid) conf
  print res

