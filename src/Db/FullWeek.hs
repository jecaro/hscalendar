-- | Contains the 'FullWeek' data type
{-# LANGUAGE TemplateHaskell #-}
module Db.FullWeek
    ( FullWeek(..)
    , add
    , empty
    , friday
    , monday
    , saturday
    , sunday
    , thursday
    , tuesday
    , wednesday
    )
where

import RIO

import           Data.Aeson (FromJSON, ToJSON)
import           Lens.Micro.Platform (makeFields, (?~))

import qualified Db.FullDay as FullDay (FullDay(..), afternoon, empty, morning)
import           Db.HalfDay (HalfDay, day, timeInDay)
import           Db.TimeInDay (TimeInDay(..))
import qualified Db.Week as Week
    ( Week
    , friday
    , fromDay
    , monday
    , saturday
    , sunday
    , thursday
    , tuesday
    , wednesday
    )

-- | A 'FullWeek' contains every day of the week
data FullWeek a = MkFullWeek
    { _fullWeekWeek :: !Week.Week
    , _fullWeekMonday :: FullDay.FullDay a
    , _fullWeekTuesday :: FullDay.FullDay a
    , _fullWeekWednesday :: FullDay.FullDay a
    , _fullWeekThursday :: FullDay.FullDay a
    , _fullWeekFriday :: FullDay.FullDay a
    , _fullWeekSaturday :: FullDay.FullDay a
    , _fullWeekSunday :: FullDay.FullDay a
    }
    deriving (Eq, Functor, Generic, Show)
makeFields ''FullWeek

instance FromJSON (FullWeek (Maybe HalfDay))
instance ToJSON (FullWeek (Maybe HalfDay))

instance Display (FullWeek (Maybe HalfDay)) where
    display fullWeek
        =  "Monday:\n" <> display (fullWeek ^. monday) <> "\n--\n"
        <> "Tuesday:\n" <> display (fullWeek ^. tuesday) <> "\n--\n"
        <> "Wednesday:\n" <> display (fullWeek ^. wednesday) <> "\n--\n"
        <> "Thursday:\n" <> display (fullWeek ^. thursday) <> "\n--\n"
        <> "Friday:\n" <> display (fullWeek ^. friday) <> "\n--\n"
        <> "Saturday:\n" <> display (fullWeek ^. saturday) <> "\n--\n"
        <> "Sunday:\n" <> display (fullWeek ^. sunday) <> "\n--"

-- | Create an empty 'FullWeek'
empty :: a -> Week.Week -> FullWeek a
empty a week' =
    MkFullWeek
    { _fullWeekWeek = week'
    , _fullWeekMonday = FullDay.empty a $ Week.monday week'
    , _fullWeekTuesday = FullDay.empty a $ Week.tuesday week'
    , _fullWeekWednesday = FullDay.empty a $ Week.wednesday week'
    , _fullWeekThursday = FullDay.empty a $ Week.thursday week'
    , _fullWeekFriday = FullDay.empty a $ Week.friday week'
    , _fullWeekSaturday = FullDay.empty a $ Week.saturday week'
    , _fullWeekSunday = FullDay.empty a $ Week.sunday week'
    }

-- | Add a 'HalfDay' into a 'FullWeek'
add :: HalfDay -> FullWeek (Maybe HalfDay) -> Maybe (FullWeek (Maybe HalfDay))
add hd fullWeek =
    let day' = view day hd
        (week', dayNb) = Week.fromDay (view day hd)
        dayLens = case dayNb of
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
        lensDoingNothing = lens (const $ FullDay.empty Nothing day') const
     in if week' == (fullWeek ^. week)
          then Just (fullWeek & dayLens . tidLens ?~ hd)
          else Nothing

