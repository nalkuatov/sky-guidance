{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Foundation where

import           RIO                    hiding (Handler)

import           Control.Monad.Except
import           Database.Persist.Redis (RedisConf, RedisT)
import           Servant

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
         }

