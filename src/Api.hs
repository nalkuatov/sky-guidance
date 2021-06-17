module Api where

import           RIO

import           Servant

-- local imports
import           Foundation
import           Persist
import           Query

type Api =
  "api" :>
  "forecast" :>
  Capture "lat" Double :>
  Capture "lon" Double :>
  Get '[JSON] Text

handler :: MonadIO m => ServerT Api (AppT m)
handler lat lon = do
  _ <- runQuery $ insertLocation $ Location lat lon
  pure "ok"

