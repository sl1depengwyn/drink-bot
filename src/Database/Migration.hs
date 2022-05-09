module Database.Migration where

import Database.Types
import Data.Time (UTCTime)
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate as PG

utctime :: BeamSqlBackend be => DataType be UTCTime
utctime = DataType (timestampType Nothing False)

initialSetup :: Migration Postgres (CheckedDatabaseSettings Postgres DrinkDb)
initialSetup =
  DrinkDb
    <$> (createTable "users" $ User {_userId = field "id" int notNull unique})
    <*> ( createTable "records" $
            Record
              { _ruserId = field "id" int notNull,
                _rmessageId = field "id" int notNull,
                _ramount = field "amount" int notNull,
                _rtimeStamp =
                  field
                    "drink_time"
                    utctime
                    notNull
                    (defaultTo_ (cast_ currentTimestamp_ utctime))
              }
        )

initialSetupStep ::
  MigrationSteps Postgres () (CheckedDatabaseSettings Postgres DrinkDb)
initialSetupStep = migrationStep "initial_setup" (const initialSetup)

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive =
  defaultUpToDateHooks
    { runIrreversibleHook = pure True
    }

migrateDB :: Connection -> IO (Maybe (CheckedDatabaseSettings Postgres DrinkDb))
migrateDB conn =
  runBeamPostgresDebug putStrLn conn $
    bringUpToDateWithHooks
      allowDestructive
      PG.migrationBackend
      initialSetupStep