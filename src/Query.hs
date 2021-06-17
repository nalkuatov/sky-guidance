module Query
  ( insertLocation
  )
  where

import           RIO

import           Database.Persist
import           Database.Persist.Redis

-- local imports
import           Persist


insertLocation :: MonadIO m => Location -> RedisT m (Key Location)
insertLocation = insert

