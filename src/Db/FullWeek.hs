{-# LANGUAGE TemplateHaskell #-}
module Db.FullWeek
    ( FullWeek(..)
    , emptyFullWeek
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

import           Lens.Micro.Platform (makeFields)

import           Db.FullDay (FullDay(..))

data FullWeek = MkFullWeek
    { _fullWeekMonday :: !FullDay
    , _fullWeekTuesday :: !FullDay
    , _fullWeekWednesday :: !FullDay
    , _fullWeekThursday :: !FullDay
    , _fullWeekFriday :: !FullDay
    , _fullWeekSaturday :: !FullDay
    , _fullWeekSunday :: !FullDay
    }
makeFields ''FullWeek

instance Display FullWeek where
    display fullWeek
        =  "Monday:\n" <> display (fullWeek ^. monday) <> "\n--\n"
        <> "Tuesday:\n" <> display (fullWeek ^. tuesday) <> "\n--\n"
        <> "Wednesday:\n" <> display (fullWeek ^. wednesday) <> "\n--\n"
        <> "Thursday:\n" <> display (fullWeek ^. thursday) <> "\n--\n"
        <> "Friday:\n" <> display (fullWeek ^. friday) <> "\n--\n"
        <> "Saturday:\n" <> display (fullWeek ^. saturday) <> "\n--\n"
        <> "Sunday:\n" <> display (fullWeek ^. sunday) <> "\n--"

emptyFullWeek :: FullWeek
emptyFullWeek = MkFullWeek
    { _fullWeekMonday = MkFullDay Nothing Nothing
    , _fullWeekTuesday = MkFullDay Nothing Nothing
    , _fullWeekWednesday = MkFullDay Nothing Nothing
    , _fullWeekThursday = MkFullDay Nothing Nothing
    , _fullWeekFriday = MkFullDay Nothing Nothing
    , _fullWeekSaturday = MkFullDay Nothing Nothing
    , _fullWeekSunday = MkFullDay Nothing Nothing
    }


