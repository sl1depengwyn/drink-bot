module Database.Database where

import qualified Data.Aeson.Extended as A
import Data.Int
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Database.Beam
import Database.Beam.Postgres
import Database.Migration (migrateDB)
import qualified Database.PostgreSQL.Simple as PGS
import Database.Types
import Lens.Micro

newtype Config = Config
  { cConnectionString :: T.Text
  }
  deriving (Show, Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

data Handle = Handle
  { hConfig :: Config,
    hPool :: Pool.Pool PGS.Connection
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

runQuery :: Handle -> Pg b -> IO b
runQuery h q = Pool.withResource (hPool h) $ \conn -> runBeamPostgresDebug putStrLn conn q

addRecord' :: MonadBeam Postgres m => Int32 -> Int32 -> Int32 -> m ()
addRecord' userId messageId amount =
  runInsert $
    insert (drinkDb ^. drinkRecords) $
      insertExpressions
        [Record (val_ (UserId userId)) (val_ messageId) (val_ amount) default_]

addRecord :: Handle -> Int -> Int -> Int -> IO ()
addRecord h uId mId amount = runQuery h (addRecord' (fromIntegral uId) (fromIntegral mId) (fromIntegral amount))

getAmount' :: MonadBeam Postgres m => Int32 -> Day -> m (Maybe (Maybe Int32))
getAmount' uId stamp = runSelectReturningOne $
  select $
    aggregate_ sum_ $
      do
        record <- all_ (drinkDb ^. drinkRecords)
        guard_ (record ^. recordUId ==. val_ uId)
        guard_ ((record ^. recordTStamp) ==. val_ stamp)
        pure $ record ^. recordAmount

getAmonut :: Handle -> Int -> Day -> IO (Maybe (Maybe Int32))
getAmonut h user day = runQuery h (getAmount' (fromIntegral user) day)

getTodaysAmount :: Handle -> Int -> IO (Maybe (Maybe Int32))
getTodaysAmount h user = getCurrentTime >>= getAmonut h user . utctDay