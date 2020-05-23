-- | Simple data type to represent a Week
module Db.Week
    ( Week (..),
      friday,
      fromDay,
      monday,
      saturday,
      sunday,
      thursday,
      tuesday,
      wednesday,
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import RIO
import qualified RIO.Time as Time (Day)

data Week = MkWeek {_year :: !Integer, _week :: !Int}
    deriving (Eq, Generic, Show)

instance FromJSON Week

instance ToJSON Week

monday :: Week -> Time.Day
monday = flip toDay 1

tuesday :: Week -> Time.Day
tuesday = flip toDay 2

wednesday :: Week -> Time.Day
wednesday = flip toDay 3

thursday :: Week -> Time.Day
thursday = flip toDay 4

friday :: Week -> Time.Day
friday = flip toDay 5

saturday :: Week -> Time.Day
saturday = flip toDay 6

sunday :: Week -> Time.Day
sunday = flip toDay 7

toDay :: Week -> Int -> Time.Day
toDay MkWeek {_year = year, _week = week} = fromWeekDate year week

fromDay :: Time.Day -> (Week, Int)
fromDay day =
    let (y, w, d) = toWeekDate day
     in (MkWeek y w, d)
