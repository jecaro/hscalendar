-- | Contains the 'FullDay' data type
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Db.FullDay
    ( FullDay(..)
    , afternoon
    , day
    , empty
    , morning
    , ok
    , overWork
    )
where

import           RIO
import qualified RIO.Time as Time (Day)

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Lens.Micro.Platform (makeFields)

import           Db.HalfDay (HalfDay)

-- | A 'FullDay' contains a 'HalfDay' for the morning and a 'HalfDay' for the
-- afternoon
data FullDay a = MkFullDay
    { _fullDayDay :: !Time.Day
    , _fullDayMorning :: !a
    , _fullDayAfternoon :: !a
    }
    deriving (Eq, Foldable, Functor, Generic, Show, Traversable)
makeFields ''FullDay

instance FromJSON (FullDay (Maybe HalfDay))
instance ToJSON (FullDay (Maybe HalfDay))

instance Display (Maybe HalfDay) where
    display (Just hd) = display hd
    display Nothing = "\t\tNothing"

instance Display (FullDay (Maybe HalfDay)) where
    display fullDay
        =  weekDay fullDay <> " "
        <> display (fullDay ^. day) <> "\n"
        <> "\tMorning\n" <> display (fullDay ^. morning) <> "\n"
        <> "\tAfternoon\n" <> display (fullDay ^. afternoon)

-- | Create an empty 'FullDay'
empty :: a -> Time.Day -> FullDay a
empty a day' = MkFullDay
    { _fullDayDay = day'
    , _fullDayMorning = a
    , _fullDayAfternoon = a
    }

-- | Indicate if a day is an open day or a weekend day
openDay :: Time.Day -> Bool
openDay d = weekNb >= 1 && weekNb <= 5
    where (_, _, weekNb) = toWeekDate d

-- | Check if a day is ok. A day is ok if
-- - It's an open day and it's got an entry for the morning and the afternoon
-- - It's saturday or sunday
ok :: FullDay (Maybe HalfDay) -> Bool
ok (MkFullDay d m a) = openDay d && workAllDay || not (openDay d)
    where workAllDay = isJust m && isJust a


-- | Check if a day is over worked, ie work during the week end
overWork :: FullDay (Maybe HalfDay) -> Bool
overWork (MkFullDay d m a) = not (openDay d) && (isJust m || isJust a)

-- | Return the day of the week in a string
weekDay :: FullDay (Maybe HalfDay) -> Utf8Builder
weekDay fd = case dayNb of
                 1 -> "Monday"
                 2 -> "Tuesday"
                 3 -> "Wednesday"
                 4 -> "Thursday"
                 5 -> "Friday"
                 6 -> "Saturday"
                 7 -> "Sunday"
                 _ -> error "The day number must be between one and seven"
    where (_, _, dayNb) = toWeekDate (fd ^. day)

