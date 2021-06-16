{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Persist where

import RIO

import Language.Haskell.TH.Syntax
import Database.Persist
import Database.Persist.Redis
import Database.Persist.TH

-- local imports
import Foundation

let redisSettings = mkPersistSettings (ConT ''RedisBackend)
 in share [mkPersist redisSettings] [persistLowerCase|
    Location
      lat Double
      lon Double
      deriving Show
    Weather
      description Text
      deriving Show
|]

runQuery :: (MonadReader Config m, MonadIO m) => RedisT m a -> m a
runQuery query = do
  conf <- asks _redisConf
  withRedisConn conf $ runRedisPool $ query
