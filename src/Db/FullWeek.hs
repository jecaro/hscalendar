{-# LANGUAGE TemplateHaskell #-}
module Db.FullWeek
    ( FullWeek(..)
    , add
    , empty
    , monday
    , tuesday
    , wednesday
    , thursday
    , friday
    , saturday
    , sunday
    )
where

import RIO

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Lens.Micro.Platform (makeFields, (?~))

import qualified Db.FullDay as FullDay (FullDay(..), afternoon, empty, morning)
import           Db.HalfDay (HalfDay, day, timeInDay)
import           Db.TimeInDay (TimeInDay(..))

data FullWeek = MkFullWeek
    { _fullWeekMonday :: !FullDay.FullDay
    , _fullWeekTuesday :: !FullDay.FullDay
    , _fullWeekWednesday :: !FullDay.FullDay
    , _fullWeekThursday :: !FullDay.FullDay
    , _fullWeekFriday :: !FullDay.FullDay
    , _fullWeekSaturday :: !FullDay.FullDay
    , _fullWeekSunday :: !FullDay.FullDay
    }
    deriving (Eq, Generic, Show)
makeFields ''FullWeek

instance FromJSON FullWeek
instance ToJSON FullWeek

instance Display FullWeek where
    display fullWeek
        =  "Monday:\n" <> display (fullWeek ^. monday) <> "\n--\n"
        <> "Tuesday:\n" <> display (fullWeek ^. tuesday) <> "\n--\n"
        <> "Wednesday:\n" <> display (fullWeek ^. wednesday) <> "\n--\n"
        <> "Thursday:\n" <> display (fullWeek ^. thursday) <> "\n--\n"
        <> "Friday:\n" <> display (fullWeek ^. friday) <> "\n--\n"
        <> "Saturday:\n" <> display (fullWeek ^. saturday) <> "\n--\n"
        <> "Sunday:\n" <> display (fullWeek ^. sunday) <> "\n--"

empty :: FullWeek
empty = MkFullWeek
    { _fullWeekMonday = FullDay.MkFullDay Nothing Nothing
    , _fullWeekTuesday = FullDay.MkFullDay Nothing Nothing
    , _fullWeekWednesday = FullDay.MkFullDay Nothing Nothing
    , _fullWeekThursday = FullDay.MkFullDay Nothing Nothing
    , _fullWeekFriday = FullDay.MkFullDay Nothing Nothing
    , _fullWeekSaturday = FullDay.MkFullDay Nothing Nothing
    , _fullWeekSunday = FullDay.MkFullDay Nothing Nothing
    }

add :: HalfDay -> FullWeek -> FullWeek
add hd fullWeek =
    let (_, _, weekDay) = toWeekDate (view day hd)
        dayLens = case weekDay of
                      1 -> monday
                      2 -> tuesday
                      3 -> wednesday
                      4 -> thursday
                      5 -> friday
                      6 -> saturday
                      7 -> sunday
                      _ -> lensDoingNothing
        tidLens = case hd ^. timeInDay of
                      Morning -> FullDay.morning
                      Afternoon -> FullDay.afternoon
        lensDoingNothing = lens (const FullDay.empty) const
    in fullWeek & dayLens . tidLens ?~ hd

