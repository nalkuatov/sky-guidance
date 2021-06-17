{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Foundation where

import           RIO                    hiding (Handler)

import           Control.Monad.Except
import           Data.Yaml
import           Database.Persist.Redis (RedisConf, RedisT)
import           GHC.Generics
import           Servant
import           Servant.Client         (ClientEnv)

type App = AppT IO

newtype AppT m a =
  AppT { unAppT :: ReaderT Config m a
       } deriving ( Functor
                  , Applicative
                  , Monad
                  , MonadIO
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
         }

data AppConf =
  AppConf { locations  :: [Text]
          , port       :: Integer
          , redishost  :: Text
          , redisport  :: Integer
          , cooldown   :: Maybe Integer
          , errorrange :: Maybe Integer
          } deriving (Eq, Show, Generic)

instance FromJSON AppConf

