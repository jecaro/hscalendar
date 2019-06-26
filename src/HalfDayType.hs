{-# LANGUAGE TemplateHaskell #-}
module HalfDayType 
    ( HalfDayType(..)
    , parser
    ) where

import           RIO

import           Data.Attoparsec.Text
    ( Parser
    , asciiCI 
    )
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

parser :: Parser HalfDayType
parser =   asciiCI "pl"   $> PayedLeave
       <|> asciiCI "fe"   $> FamilyEvent
       <|> asciiCI "rtte" $> RTTE
       <|> asciiCI "rtts" $> RTTS
       <|> asciiCI "ul"   $> UnpayedLeave
       <|> asciiCI "ph"   $> PublicHoliday
       <|> asciiCI "pt"   $> PartTime