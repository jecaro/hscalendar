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
data IdleDayType = PaidLeave
                 | FamilyEvent
                 | RTTE
                 | RTTS
                 | UnpaidLeave
                 | PublicHoliday
                 | PartTime
    deriving (Bounded, Enum, Eq, Generic, Show)

-- | Arbitrary instance for QuickCheck
instance Arbitrary IdleDayType where
    arbitrary = arbitraryBoundedEnum

instance ToJSON IdleDayType
instance FromJSON IdleDayType

instance Display IdleDayType where
    textDisplay PaidLeave = "Paid leave"
    textDisplay FamilyEvent = "Family event"
    textDisplay RTTE = "RTTE"
    textDisplay RTTS = "RTTS"
    textDisplay UnpaidLeave = "Unpaid leave"
    textDisplay PublicHoliday = "Public holiday"
    textDisplay PartTime = "Part time"

-- | Parser for this type
parser :: Parser IdleDayType
parser =   asciiCI "pl"   $> PaidLeave
       <|> asciiCI "fe"   $> FamilyEvent
       <|> asciiCI "rtte" $> RTTE
       <|> asciiCI "rtts" $> RTTS
       <|> asciiCI "ul"   $> UnpaidLeave
       <|> asciiCI "ph"   $> PublicHoliday
       <|> asciiCI "pt"   $> PartTime

