module Db.IdleDayType
where

import RIO

import           Data.Attoparsec.Text
    ( Parser
    , asciiCI 
    )
import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

data IdleDayType = PayedLeave
                 | FamilyEvent
                 | RTTE
                 | RTTS
                 | UnpayedLeave
                 | PublicHoliday 
                 | PartTime
    deriving (Bounded, Enum, Eq, Show)

parser :: Parser IdleDayType
parser =   asciiCI "pl"   $> PayedLeave
       <|> asciiCI "fe"   $> FamilyEvent
       <|> asciiCI "rtte" $> RTTE
       <|> asciiCI "rtts" $> RTTS
       <|> asciiCI "ul"   $> UnpayedLeave
       <|> asciiCI "ph"   $> PublicHoliday
       <|> asciiCI "pt"   $> PartTime

-- | Arbitrary instance for QuickCheck
instance Arbitrary IdleDayType where
    arbitrary = arbitraryBoundedEnum
