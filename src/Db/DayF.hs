-- | Contains the 'DayF' data type
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Db.DayF
    ( DayF(..)
    , afternoon
    , day
    , empty
    , morning
    , stats
    , ok
    , overWork
    )
where

import           RIO
import qualified RIO.Time as Time (Day)
import qualified RIO.HashMap as HM (insertWith)

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Lens.Micro.Platform (makeFields)

import           Db.HalfDay (HalfDay(..))
import           Db.Stats (Stats)
import           Db.Worked (project)

-- | A 'DayF' contains a 'HalfDay' for the morning and a 'HalfDay' for the
-- afternoon
data DayF a = MkDayF
    { _dayFDay :: !Time.Day
    , _dayFMorning :: !a
    , _dayFAfternoon :: !a
    }
    deriving (Eq, Foldable, Functor, Generic, Show, Traversable)
makeFields ''DayF

instance FromJSON (DayF (Maybe HalfDay))
instance ToJSON (DayF (Maybe HalfDay))

instance Display (Maybe HalfDay) where
    display (Just hd) = display hd
    display Nothing = "\t\tNothing"

instance Display (DayF (Maybe HalfDay)) where
    display dayF
        =  display (dayF ^. day) <> "\n"
        <> "\tMorning\n" <> display (dayF ^. morning) <> "\n"
        <> "\tAfternoon\n" <> display (dayF ^. afternoon)

-- | Create an empty 'DayF'
empty :: a -> Time.Day -> DayF a
empty a day' = MkDayF day' a a

-- | Indicate if a day is an open day or a weekend day
openDay :: Time.Day -> Bool
openDay d = weekNb >= 1 && weekNb <= 5
    where (_, _, weekNb) = toWeekDate d

-- | Check if a day is ok. A day is ok if
-- - It's an open day and it's got an entry for the morning and the afternoon
-- - It's saturday or sunday
ok :: DayF (Maybe HalfDay) -> Bool
ok (MkDayF d m a) = openDay d && workAllDay || not (openDay d)
    where workAllDay = isJust m && isJust a


-- | Check if a day is over worked, ie work during the week end
overWork :: DayF (Maybe HalfDay) -> Bool
overWork (MkDayF d m a) = not (openDay d) && (isJust m || isJust a)

stats :: DayF (Maybe HalfDay) -> Stats -> Stats
stats d s = foldr statsOnMaybeHalfDay s d

statsOnMaybeHalfDay :: Maybe HalfDay -> Stats -> Stats
statsOnMaybeHalfDay Nothing s = s
statsOnMaybeHalfDay (Just (MkHalfDayIdle _)) s = s
statsOnMaybeHalfDay (Just (MkHalfDayWorked worked)) s =
    HM.insertWith (+) (worked ^. project) 1 s
