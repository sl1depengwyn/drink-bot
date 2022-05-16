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
import Database.Beam.Migrate.Simple (bringUpToDateWithHooks)
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate as PG
import Database.Migration
import qualified Database.PostgreSQL.Simple as PGS
import Database.Types
import Lens.Micro
import qualified Logger

newtype Config = Config
  { cConnectionString :: T.Text
  }
  deriving (Show, Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

data Handle = Handle
  { hConfig :: Config,
    hLogger :: Logger.Handle,
    hPool :: Pool.Pool PGS.Connection
  }

withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle conf lh f = do
  let connString = cConnectionString conf
  pool <- Pool.createPool (PGS.connectPostgreSQL $ T.encodeUtf8 connString) PGS.close 1 10 4
  let h = Handle {hConfig = conf, hLogger = lh, hPool = pool}
  Pool.withResource pool (migrateDB lh)
  res <- f h
  Pool.destroyAllResources pool
  return res

migrateDB :: Logger.Handle -> Connection -> IO (Maybe (CheckedDatabaseSettings Postgres DrinkDb))
migrateDB h conn =
  runBeamPostgresDebug (Logger.debug h) conn $
    bringUpToDateWithHooks
      allowDestructive
      PG.migrationBackend
      initialSetupStep

drinkDb :: DatabaseSettings Postgres DrinkDb
drinkDb = unCheckDatabase $ evaluateDatabase initialSetupStep

runQuery :: Handle -> Pg b -> IO b
runQuery h q = Pool.withResource (hPool h) $ \conn -> runBeamPostgresDebug (Logger.debug (hLogger h)) conn q

addRecord' :: MonadBeam Postgres m => Int32 -> Int32 -> Int32 -> UTCTime -> m ()
addRecord' userId messageId amount t =
  runInsert $
    insert (drinkDb ^. drinkRecords) $
      insertValues
        [Record (UserId userId) messageId amount t]

addRecord :: Handle -> Int -> Int -> Int -> UTCTime -> IO ()
addRecord h uId mId amount t = runQuery h (addRecord' (fromIntegral uId) (fromIntegral mId) (fromIntegral amount) t)

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

getAmount' :: MonadBeam Postgres m => Int32 -> UTCTime -> m (Maybe (Maybe Int32))
getAmount' uId t = runSelectReturningOne $
  select $
    aggregate_ sum_ $
      do
        let (_, _, day) = toGregorian $ utctDay t
        record <-
          filter_
            ( \record ->
                (record ^. recordUId ==. val_ uId)
                  &&. (extract_ day_ (record ^. recordTStamp) ==. val_ (fromIntegral day))
            )
            $ all_ (drinkDb ^. drinkRecords)
        pure $ record ^. recordAmount

getAmonut :: Handle -> Int -> UTCTime -> IO (Maybe (Maybe Int32))
getAmonut h user t = runQuery h (getAmount' (fromIntegral user) t)

getTodaysAmount :: Handle -> Int -> IO (Maybe (Maybe Int32))
getTodaysAmount h user = getCurrentTime >>= getAmonut h user

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