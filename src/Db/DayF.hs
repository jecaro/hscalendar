-- | Contains the 'DayF' data type
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Db.DayF
    ( DayF(..)
    , DayWithHalfDays
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
import qualified RIO.HashMap as HM (HashMap, insertWith)

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Lens.Micro.Platform (makeFields, (+~), (%~))

import           Db.HalfDay (HalfDay(..))
import qualified Db.Stats as Stats (Stats, idle, week, worked)
import qualified Db.Worked as Worked (project)
import           Db.Idle (dayType)

-- | A 'DayF' contains a 'HalfDay' for the morning and a 'HalfDay' for the
-- afternoon
data DayF a = MkDayF
    { _dayFDay :: !Time.Day
    , _dayFMorning :: !a
    , _dayFAfternoon :: !a
    }
    deriving (Eq, Foldable, Functor, Generic, Show, Traversable)
makeFields ''DayF

type DayWithHalfDays = DayF (Maybe HalfDay)

instance FromJSON DayWithHalfDays
instance ToJSON DayWithHalfDays

instance Display (Maybe HalfDay) where
    display (Just hd) = display hd
    display Nothing = "\t\tNothing"

instance Display DayWithHalfDays where
    display dayF
        =  display (dayF ^. day) <> "\n"
        <> "\tMorning\n" <> display (dayF ^. morning) <> "\n"
        <> "\tAfternoon\n" <> display (dayF ^. afternoon)

-- | Create an empty 'DayF'
empty :: a -> Time.Day -> DayF a
empty a day' = MkDayF day' a a

-- | Indicate if a day is an open day or a weekend day
weekDay :: Time.Day -> Bool
weekDay d = weekNb >= 1 && weekNb <= 5
    where (_, _, weekNb) = toWeekDate d

-- | Check if a day is ok. A day is ok if
-- - It's an open day and it's got an entry for the morning and the afternoon
-- - It's saturday or sunday
ok :: DayWithHalfDays -> Bool
ok (MkDayF d m a) = weekDay d && workAllDay || not (weekDay d)
    where workAllDay = isJust m && isJust a


-- | Check if a day is over worked, ie work during the week end
overWork :: DayWithHalfDays -> Bool
overWork (MkDayF d m a) = not (weekDay d) && (isJust m || isJust a)

stats :: DayWithHalfDays -> Stats.Stats -> Stats.Stats
stats d s =
    let s' = foldr statsOnMaybeHalfDay s d
        updateWeek = if weekDay (d ^. day)
                         then Stats.week +~ 2
                         else Stats.week %~ id
    in s' & updateWeek

addOneFor :: (Eq k, Hashable k, Num v) => k -> HM.HashMap k v -> HM.HashMap k v
addOneFor k = HM.insertWith (+) k 1

statsOnMaybeHalfDay :: Maybe HalfDay -> Stats.Stats -> Stats.Stats
statsOnMaybeHalfDay Nothing s = s
statsOnMaybeHalfDay (Just (MkHalfDayIdle idle)) s =
    s & Stats.idle %~ addOneFor (idle ^. dayType)
statsOnMaybeHalfDay (Just (MkHalfDayWorked worked)) s =
    s & Stats.worked %~ addOneFor (worked ^. Worked.project)
