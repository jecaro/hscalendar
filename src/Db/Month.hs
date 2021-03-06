-- | Simple data type to represent a Month
module Db.Month (Month (..), day, nbDays, firstDay, fromDay, lastDay) where

import Data.Aeson (FromJSON, ToJSON)
import RIO
import qualified RIO.Time as Time
    ( Day,
      fromGregorian,
      gregorianMonthLength,
      toGregorian,
    )

data Month = MkMonth {_year :: !Integer, _month :: !Int}
    deriving (Eq, Generic, Show)

instance FromJSON Month

instance ToJSON Month

firstDay :: Month -> Time.Day
firstDay month = day month 1

lastDay :: Month -> Time.Day
lastDay month@(MkMonth y m) = day month last
    where
        last = Time.gregorianMonthLength y m

day :: Month -> Int -> Time.Day
day (MkMonth year month) = Time.fromGregorian year month

nbDays :: Month -> Int
nbDays (MkMonth year month) = Time.gregorianMonthLength year month

fromDay :: Time.Day -> Month
fromDay d = MkMonth y m
    where
        (y, m, _) = Time.toGregorian d
