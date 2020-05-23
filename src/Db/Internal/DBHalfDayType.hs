{-# LANGUAGE TemplateHaskell #-}

-- | A type for a non-working day, version used in the db schema
module Db.Internal.DBHalfDayType
    ( DBHalfDayType (..),
    )
where

import Database.Persist.TH (derivePersistField)
import RIO

-- | Simple sum type for setting the kind of halfday
data DBHalfDayType
    = DBPaidLeave
    | DBFamilyEvent
    | DBRTTE
    | DBRTTS
    | DBUnpaidLeave
    | DBPublicHoliday
    | DBPartTime
    | DBWorked
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

derivePersistField "DBHalfDayType"
