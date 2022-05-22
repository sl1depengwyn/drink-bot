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
import Database.Beam.Query.Internal
import Database.Migration
import qualified Database.PostgreSQL.Simple as PGS
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
      migration

drinkDb :: DatabaseSettings Postgres DrinkDb
drinkDb = unCheckDatabase $ evaluateDatabase migration

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

getSumAmountOf' ::
  MonadBeam Postgres m =>
  (RecordT (QExpr Postgres (QNested QBaseScope)) -> QExpr Postgres (QNested QBaseScope) Bool) ->
  m (Maybe (Maybe Int32))
getSumAmountOf' cmp = runSelectReturningOne $
  select $
    aggregate_ sum_ $
      do
        record <- filter_ cmp $ all_ (drinkDb ^. drinkRecords)
        pure $ record ^. recordAmount

getSumTodaysAmount' :: MonadBeam Postgres m => Int32 -> UTCTime -> m (Maybe (Maybe Int32))
getSumTodaysAmount' uId t =
  let (year, month, day) = toGregorian $ utctDay t
   in getSumAmountOf'
        ( \record ->
            (record ^. recordUId ==. val_ uId)
              &&. (extract_ year_ (record ^. recordTStamp) ==. val_ (fromIntegral year))
              &&. (extract_ month_ (record ^. recordTStamp) ==. val_ (fromIntegral month))
              &&. (extract_ day_ (record ^. recordTStamp) ==. val_ (fromIntegral day))
        )

getRecordsOf' uId cmp = undefined

getTodaysRecords' = undefined

getMonthRecords' = undefined


getSumTodaysAmount :: Handle -> Int -> IO (Maybe (Maybe Int32))
getSumTodaysAmount h user = getCurrentTime >>= runQuery h . getSumTodaysAmount' (fromIntegral user)

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

addButton' :: MonadBeam Postgres m => Int32 -> Int32 -> m ()
addButton' userId amount =
  runInsert $
    insertOnConflict
      (drinkDb ^. drinkButtons)
      (insertValues [Button (UserId userId) amount])
      anyConflict
      onConflictDoNothing

addButton :: Handle -> Int -> Int -> IO ()
addButton h userId amount = runQuery h (addButton' (fromIntegral userId) (fromIntegral amount))

removeButton' :: MonadBeam Postgres m => Int32 -> Int32 -> m ()
removeButton' userId amount =
  runDelete $
    delete
      (drinkDb ^. drinkButtons)
      (\b -> val_ (Button (UserId userId) amount) ==. b)

removeButton :: Handle -> Int -> Int -> IO ()
removeButton h userId amount = runQuery h (removeButton' (fromIntegral userId) (fromIntegral amount))

getButtons' :: MonadBeam Postgres m => Int32 -> m [Int32]
getButtons' userId =
  runSelectReturningList $
    select $ do
      btn <- filter_ (\b -> b ^. buttonUId ==. val_ userId) $ all_ (drinkDb ^. drinkButtons)
      pure $ btn ^. buttonAmount

getButtons :: Handle -> Int -> IO [Int]
getButtons h userId = map fromIntegral <$> runQuery h (getButtons' (fromIntegral userId))
