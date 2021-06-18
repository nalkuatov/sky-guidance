{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Foundation where

import           RIO                    hiding (Handler)

import           Control.Monad.Except
import           Data.Yaml
import           Database.Persist.Redis (RedisConf)
import           Servant                (Handler (..))
import           Servant.Client         (ClientEnv)

-- local imports
import           Types                  (City)

type App = AppT IO

newtype AppT m a =
  AppT { unAppT :: ReaderT Config m a
       } deriving ( Functor
                  , Applicative
                  , Monad
                  , MonadIO
                  , MonadThrow
                  , MonadReader Config
                  )

runAppT :: AppT m a -> Config -> m a
runAppT = runReaderT . unAppT

appToHandler :: Config -> App a -> Handler a
appToHandler config =
  Handler . ExceptT . try . ($ config) . runAppT

data Config =
  Config { _redisConf :: RedisConf
         , _clientEnv :: ClientEnv
         , _appConf   :: AppConf
         , _appid     :: Text -- ^ an api_key
         }

data AppConf =
  AppConf { locations  :: [City]
          , appport    :: Int
          , redishost  :: Text
          , redisport  :: Integer
          , cooldown   :: Maybe Int
          , errorrange :: Maybe Integer
          } deriving (Eq, Show, Generic)

instance FromJSON AppConf

