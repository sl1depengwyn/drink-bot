module Database.Migration where

import Data.Time (UTCTime)
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Types

utctime :: BeamSqlBackend be => DataType be UTCTime
utctime = DataType (timestampType Nothing True)

initialSetup :: Migration Postgres (CheckedDatabaseSettings Postgres DrinkDb)
initialSetup =
  DrinkDb
    <$> ( createTable "users" $
            User
              { _userId = field "id" int notNull unique,
                _lastMessageId = field "lastmessageid" (maybeType int)
              }
        )
    <*> ( createTable "records" $
            Record
              { _ruserId = UserId $ field "userid" int notNull,
                _rmessageId = field "id" int notNull,
                _ramount = field "amount" int notNull,
                _rtimeStamp = field "date" utctime notNull
              }
        )
    <*> ( createTable "buttons" $
            Button
              { _buserId = UserId $ field "userid" int notNull,
                _bamount = field "amount" int notNull
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
