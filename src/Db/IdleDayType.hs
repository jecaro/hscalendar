-- | A type for a non-working day
module Db.IdleDayType
where

import RIO

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Attoparsec.Text
    ( Parser
    , asciiCI 
    )
import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

-- | All the different kinds of non-working half-day
data IdleDayType = PayedLeave
                 | FamilyEvent
                 | RTTE
                 | RTTS
                 | UnpayedLeave
                 | PublicHoliday 
                 | PartTime
    deriving (Bounded, Enum, Eq, Generic, Show)

-- | Arbitrary instance for QuickCheck
instance Arbitrary IdleDayType where
    arbitrary = arbitraryBoundedEnum

instance ToJSON IdleDayType
instance FromJSON IdleDayType

-- | Parser for this type
parser :: Parser IdleDayType
parser =   asciiCI "pl"   $> PayedLeave
       <|> asciiCI "fe"   $> FamilyEvent
       <|> asciiCI "rtte" $> RTTE
       <|> asciiCI "rtts" $> RTTS
       <|> asciiCI "ul"   $> UnpayedLeave
       <|> asciiCI "ph"   $> PublicHoliday
       <|> asciiCI "pt"   $> PartTime

