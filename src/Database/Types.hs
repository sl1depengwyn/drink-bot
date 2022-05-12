{-# LANGUAGE ImpredicativeTypes #-}

module Database.Types where

import Data.Int
import Data.Time
import Database.Beam
import Database.Beam.Postgres
import Lens.Micro hiding (to)

data UserT f = User
  { _userId :: Columnar f Int32,
    _lastMessageId :: Columnar f (Maybe Int32)
  }
  deriving (Generic, Beamable)

type User = UserT Identity

deriving instance Show User

deriving instance Eq User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = UserId . _userId

User (LensFor userId) (LensFor lastMessageId) = tableLenses

data RecordT f = Record
  { _ruserId :: PrimaryKey UserT f,
    _rmessageId :: Columnar f Int32,
    _ramount :: Columnar f Int32,
    _rtimeStamp :: Columnar f Day
  }
  deriving (Generic, Beamable)

type Record = RecordT Identity

-- deriving instance Show Record

-- deriving instance Eq Record

instance Table RecordT where
  data PrimaryKey RecordT f
    = RecordId
        (PrimaryKey UserT f)
        (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = RecordId <$> _ruserId <*> _rmessageId

Record
  (UserId (LensFor recordUId))
  (LensFor recordMId)
  (LensFor recordAmount)
  (LensFor recordTStamp) = tableLenses

data DrinkDb f = DrinkDb
  { _dUsers :: f (TableEntity UserT),
    _dRecords :: f (TableEntity RecordT)
  }
  deriving (Generic, Database Postgres)

DrinkDb
  (TableLens drinkUsers)
  (TableLens drinkRecords) = dbLenses