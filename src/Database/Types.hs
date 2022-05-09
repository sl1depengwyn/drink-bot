module Database.Types where

import Data.Time
import Database.Beam
import Database.Beam.Postgres

newtype UserT f = User
  { _userId :: Columnar f Int
  }
  deriving (Generic, Beamable)

type User = UserT Identity

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = UserId . _userId

data RecordT f = Record
  { _ruserId :: Columnar f Int,
    _rmessageId :: Columnar f Int,
    _ramount :: Columnar f Int,
    _rtimeStamp :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

type Record = RecordT Identity

deriving instance Show Record

deriving instance Eq Record

instance Table RecordT where
  data PrimaryKey RecordT f
    = RecordId
        (Columnar f Int)
        (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = RecordId <$> _ruserId <*> _rmessageId

data DrinkDb f = DrinkDb
  { _dUsers :: f (TableEntity UserT),
    _dRecords :: f (TableEntity RecordT)
  }
  deriving (Generic, Database Postgres)

drinkDb :: DatabaseSettings Postgres DrinkDb
drinkDb = defaultDbSettings
