-- | Contains the 'WeekF' data type
{-# LANGUAGE TemplateHaskell #-}
module Db.WeekF
    ( WeekWithDays
    , add
    , empty
    , full
    , friday
    , monday
    , overWork
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

import qualified Db.DayF as DayF
    ( DayF(..)
    , afternoon
    , empty
    , morning
    , ok
    , overWork
    )
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

-- | A 'WeekF' contains every day of the week
data WeekF a = MkWeekF
    { _weekFWeek :: !Week.Week
    , _weekFMonday :: a
    , _weekFTuesday :: a
    , _weekFWednesday :: a
    , _weekFThursday :: a
    , _weekFFriday :: a
    , _weekFSaturday :: a
    , _weekFSunday :: a
    }
    deriving (Eq, Foldable, Functor, Generic, Show, Traversable)
makeFields ''WeekF

-- | Specialization for the type actually used
type WeekWithDays = WeekF (DayF.DayF (Maybe HalfDay))

instance FromJSON WeekWithDays
instance ToJSON WeekWithDays

instance Display WeekWithDays where
    display w =  line <> foldMap (\d -> "\n" <> display d <> "\n" <> line) w
               <> full'
               <> overWork'
        where line = "----------------------------------------"
              full'
                  | full w = "\nThe week is complete"
                  | otherwise = "\nThe week is missing entries"
              overWork'
                  | overWork w = "\nWork has been done during the week-end"
                  | otherwise = ""

-- | Create an empty 'WeekF'
empty :: a -> Week.Week -> WeekF (DayF.DayF a)
empty a week' =
    MkWeekF
    { _weekFWeek = week'
    , _weekFMonday = DayF.empty a $ Week.monday week'
    , _weekFTuesday = DayF.empty a $ Week.tuesday week'
    , _weekFWednesday = DayF.empty a $ Week.wednesday week'
    , _weekFThursday = DayF.empty a $ Week.thursday week'
    , _weekFFriday = DayF.empty a $ Week.friday week'
    , _weekFSaturday = DayF.empty a $ Week.saturday week'
    , _weekFSunday = DayF.empty a $ Week.sunday week'
    }

-- | Add a 'HalfDay' into a 'WeekF'
add
    :: HalfDay
    -> WeekF (DayF.DayF (Maybe HalfDay))
    -> Maybe (WeekF (DayF.DayF (Maybe HalfDay)))
add hd w =
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
                      Morning -> DayF.morning
                      Afternoon -> DayF.afternoon
        lensDoingNothing = lens (const $ DayF.empty Nothing day') const
     in if week' == (w ^. week)
          then Just (w & dayLens . tidLens ?~ hd)
          else Nothing

-- | Check if the week is complete: every open day has two entries
full :: WeekF (DayF.DayF (Maybe HalfDay)) -> Bool
full = all DayF.ok

-- | Check if some work has been done during the week-end
overWork :: WeekF (DayF.DayF (Maybe HalfDay)) -> Bool
overWork = any DayF.overWork
