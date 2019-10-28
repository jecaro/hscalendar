-- | A type for a non-working day, version used in the db schema
{-# LANGUAGE TemplateHaskell #-}
module Db.Internal.DBHalfDayType
    ( DBHalfDayType(..)
    ) where

import           RIO

import           Database.Persist.TH (derivePersistField)

-- | Simple sum type for setting the kind of halfday
data DBHalfDayType = DBPaidLeave
                   | DBFamilyEvent
                   | DBRTTE
                   | DBRTTS
                   | DBUnpaidLeave
                   | DBPublicHoliday
                   | DBPartTime
                   | DBWorked
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)
derivePersistField "DBHalfDayType"

