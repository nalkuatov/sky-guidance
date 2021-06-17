module Server where

import           RIO

import           Data.Yaml
import           Database.Persist.Redis
import           Network.HTTP.Client
import           RIO.Text               (pack)
import           Servant.Client
import           Servant.Client.Core
import           System.Environment     (lookupEnv)
import           System.IO              (print)
import           System.IO.Error        (userError)

-- local imports
import           Client
import           Foundation

start :: IO ()
start = do

  manager  <- newManager defaultManagerSettings
  apikey   <- lookupEnv "API_KEY"

  case apikey of
    Nothing -> throwUserErr "please set your API_KEY"
    Just v  -> pure v

  apiroot  <- fromMaybe "api.openweathermap.org" <$> lookupEnv "API_ROOT"
  aconf    <- decodeFileThrow "config.yaml"

  let cenv  = mkClientEnv manager $ BaseUrl Http apiroot 80 "/data/2.5/weather"

  let (rhost, rport) = (redishost aconf, fromInteger $ redisport aconf)
  let rconf = RedisConf rhost (PortNumber rport) Nothing 10

  let conf  = Config rconf cenv aconf

  res      <- runAppT (pull (Just 30) (Just 45) (pack <$> apikey)) conf
  print res

throwUserErr :: String -> IO a
throwUserErr = throwIO . userError

