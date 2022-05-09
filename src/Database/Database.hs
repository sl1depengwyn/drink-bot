module Database.Database where

import qualified Data.Aeson.Extended as A
import qualified Data.Text as T
import qualified Data.Text.Encoding         as T
import Data.Time (UTCTime)
import Database.Beam
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as PGS
import qualified Data.Pool                  as Pool
import Database.Migration (migrateDB)

newtype Config = Config
  { cConnectionString :: T.Text
  }
  deriving (Show, Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

data Handle = Handle
  { hConfig :: Config
  , hPool   :: Pool.Pool PGS.Connection
  }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle conf f = do
  let connString = cConnectionString conf
  pool <- Pool.createPool (PGS.connectPostgreSQL $ T.encodeUtf8 connString) PGS.close 1 10 4
  let h = Handle conf pool
  Pool.withResource pool migrateDB
  res <- f h
  Pool.destroyAllResources pool
  return res


