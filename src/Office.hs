{-# LANGUAGE TemplateHaskell #-}
module Office where

import           RIO
import           Data.Yaml 
    ( FromJSON
    , ToJSON)

import           Database.Persist.TH (derivePersistField)

import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

-- | Simple sum type for defining a work location
data Office = Rennes | Home | Poool | OutOfOffice
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)
derivePersistField "Office"

instance FromJSON Office
instance ToJSON Office

-- | Arbitrary instance for QuickCheck
instance Arbitrary Office where
    arbitrary = arbitraryBoundedEnum
