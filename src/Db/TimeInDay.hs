{-# LANGUAGE TemplateHaskell #-}
module Db.TimeInDay where

import           RIO

import           Database.Persist.TH (derivePersistField)

import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

-- | Simple sum type of defining the time in the day
data TimeInDay = Morning | Afternoon
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
derivePersistField "TimeInDay"

-- | Arbitrary instance for QuickCheck
instance Arbitrary TimeInDay where
    arbitrary = arbitraryBoundedEnum

-- | Switch to the other TimeInDay
other :: TimeInDay -> TimeInDay
other Morning   = Afternoon
other Afternoon = Morning
