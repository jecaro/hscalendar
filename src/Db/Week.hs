-- | Simple data type to represent a Week
module Db.Week
    ( Week(..)
    , fromDay
    , monday
    , sunday)
where

import           RIO
import qualified RIO.Time as Time (Day)

import           Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)

data Week = MkWeek { _year :: !Integer, _week :: !Int}

monday :: Week -> Time.Day
monday MkWeek { _year = year, _week = week } = fromWeekDate year week 1

sunday :: Week -> Time.Day
sunday MkWeek { _year = year, _week = week } = fromWeekDate year week 7

fromDay :: Time.Day -> Week
fromDay day = let (y, w, _) = toWeekDate day
              in MkWeek y w

