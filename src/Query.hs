module Query
  ( insertLocation_
  , selectLocation
  )
  where

import           RIO
import           RIO.Text               (unpack)

import           Database.Persist.Redis

-- local imports
import           Persist
import           Types                  (City)


insertLocation_ :: MonadIO m
  => Location -> RedisT m ()
insertLocation_ location = do
  key <- mkKey $ locationCity location
  repsert key location

selectLocation :: MonadIO m
  => City -> RedisT m (Maybe Location)
selectLocation = get <=< mkKey

mkKey :: (MonadIO m, PersistEntity val) => Text -> m (Key val)
mkKey s = case keyFromValues [PersistText s] of
    Right z -> return z
    Left  a -> liftIO $ fail (unpack a)

