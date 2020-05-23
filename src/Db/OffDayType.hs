-- | A type for a non-working day
module Db.OffDayType where

import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.Text
    ( Parser,
      asciiCI,
    )
import RIO
import Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

-- | All the different kinds of non-working half-day
data OffDayType
    = PaidLeave
    | FamilyEvent
    | RTTE
    | RTTS
    | UnpaidLeave
    | PublicHoliday
    | PartTime
    deriving (Bounded, Enum, Eq, Generic, Show)

-- | Arbitrary instance for QuickCheck
instance Arbitrary OffDayType where
    arbitrary = arbitraryBoundedEnum

instance ToJSON OffDayType

instance FromJSON OffDayType

instance Hashable OffDayType

instance Display OffDayType where
    display PaidLeave = "Paid leave"
    display FamilyEvent = "Family event"
    display RTTE = "RTTE"
    display RTTS = "RTTS"
    display UnpaidLeave = "Unpaid leave"
    display PublicHoliday = "Public holiday"
    display PartTime = "Part time"

-- | Parser for this type
parser :: Parser OffDayType
parser =
    asciiCI "pl" $> PaidLeave
        <|> asciiCI "fe" $> FamilyEvent
        <|> asciiCI "rtte" $> RTTE
        <|> asciiCI "rtts" $> RTTS
        <|> asciiCI "ul" $> UnpaidLeave
        <|> asciiCI "ph" $> PublicHoliday
        <|> asciiCI "pt" $> PartTime
