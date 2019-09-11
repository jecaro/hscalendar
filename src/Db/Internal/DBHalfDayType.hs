{-# LANGUAGE TemplateHaskell #-}
module Db.Internal.DBHalfDayType 
    ( DBHalfDayType(..)
    ) where

import           RIO

import           Database.Persist.TH (derivePersistField)

-- | Simple sum type for setting the kind of halfday
data DBHalfDayType = DBPayedLeave
                   | DBFamilyEvent
                   | DBRTTE
                   | DBRTTS
                   | DBUnpayedLeave
                   | DBPublicHoliday 
                   | DBPartTime
                   | DBWorked
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)
derivePersistField "DBHalfDayType"

