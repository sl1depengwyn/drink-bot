module Database.Database where

import qualified Data.Aeson.Extended as A
import Data.Int
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (BeamHasInsertOnConflict (anyConflict, insertOnConflict, onConflictDoNothing))
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Migration
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

drinkDb :: DatabaseSettings Postgres DrinkDb
drinkDb = unCheckDatabase $ evaluateDatabase initialSetupStep

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

addUser' :: MonadBeam Postgres m => Int32 -> m ()
addUser' userId =
  runInsert $
    insertOnConflict
      (drinkDb ^. drinkUsers)
      (insertValues [User userId Nothing])
      anyConflict
      onConflictDoNothing

addUser :: Integral a => Handle -> a -> IO ()
addUser h userId = runQuery h (addUser' (fromIntegral userId))

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

updateLastMessageId' :: MonadBeam Postgres m => Int32 -> Int32 -> m ()
updateLastMessageId' userId mId = do
  user' <- runSelectReturningOne $ lookup_ (drinkDb ^. drinkUsers) (UserId userId)
  case user' of
    (Just user) -> runUpdate $ save (drinkDb ^. drinkUsers) (user & lastMessageId ?~ mId)
    Nothing -> pure () -- TODO !!write error to log!!

updateLastMessageId :: Handle -> Int -> Int -> IO ()
updateLastMessageId h userId mId = runQuery h (updateLastMessageId' (fromIntegral userId) (fromIntegral mId))

getLastMessageId' :: MonadBeam Postgres m => Int32 -> m (Maybe Int)
getLastMessageId' userId = do
  user' <- runSelectReturningOne $ lookup_ (drinkDb ^. drinkUsers) (UserId userId)
  pure $ fromIntegral <$> (user' >>= (^. lastMessageId))

getLastMessageId :: Handle -> Int -> IO (Maybe Int)
getLastMessageId h userId = runQuery h (getLastMessageId' (fromIntegral userId))

updateRecord' :: MonadBeam Postgres m => Int32 -> Int32 -> Int32 -> m ()
updateRecord' userId mId amount = do
  record' <- runSelectReturningOne $ lookup_ (drinkDb ^. drinkRecords) (RecordId (UserId userId) mId)
  case record' of
    (Just record) -> runUpdate $ save (drinkDb ^. drinkRecords) (record & recordAmount .~ amount)
    Nothing -> pure () -- TODO !!write error to log!!

updateRecord :: Handle -> Int -> Int -> Int -> IO ()
updateRecord h userId mId amount = runQuery h (updateRecord' userId' mId' amount')
  where
    [userId', mId', amount'] = map fromIntegral [userId, mId, amount]