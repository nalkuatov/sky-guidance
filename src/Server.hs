module Server where

import           RIO
import           RIO.Text                 (pack)

import           Api                      (Api, handler)
import           Control.Concurrent       (forkIO)
import           Data.Yaml
import           Database.Persist.Redis
import           Network.HTTP.Client
import           Network.Wai.Handler.Warp
import           Servant.Client
import           System.Environment       (lookupEnv)
import           System.IO.Error          (userError)

-- local imports
import           Api                      (init)
import           Client
import           Foundation
import           Persist
import           Query

start :: IO ()
start = do

  manager  <- newManager defaultManagerSettings
  apikey   <- lookupEnv "API_KEY"

  key      <- case apikey of
    Nothing -> throwUserErr "please set your API_KEY"
    Just v  -> pure v

  apiroot  <- fromMaybe "api.openweathermap.org" <$> lookupEnv "API_ROOT"
  aconf    <- decodeFileThrow "config.yaml"

  let cenv  = mkClientEnv manager $ BaseUrl Http apiroot 80 ""

  let (rhost, rport) = (redishost aconf, fromInteger $ redisport aconf)
  let rconf = RedisConf rhost (PortNumber rport) Nothing 10

  let conf  = Config rconf cenv aconf (pack key)

  _        <- forkIO $ runAppT repetitiveCache conf

  run (appport aconf) $ init conf



throwUserErr :: String -> IO a
throwUserErr = throwIO . userError

