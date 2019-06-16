{-# LANGUAGE TemplateHaskell #-}
module HalfDayType where

import           RIO

import           Database.Persist.TH (derivePersistField)

import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

-- | Simple sum type for setting the kind of halfday
data HalfDayType = PayedLeave
                 | FamilyEvent
                 | RTTE
                 | RTTS
                 | UnpayedLeave
                 | PublicHoliday 
                 | PartTime
                 | Worked
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)
derivePersistField "HalfDayType"

-- | Arbitrary instance for QuickCheck
instance Arbitrary HalfDayType where
    arbitrary = arbitraryBoundedEnum
