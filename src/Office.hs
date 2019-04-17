{-# LANGUAGE TemplateHaskell #-}
module Office where

import           RIO

import           Database.Persist.TH (derivePersistField)

import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

-- | Simple sum type for defining a work location
data Office = Rennes | Home | Poool
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
derivePersistField "Office"

-- | Arbitrary instance for QuickCheck
instance Arbitrary Office where
    arbitrary = arbitraryBoundedEnum
