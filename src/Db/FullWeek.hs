-- | Contains the 'FullWeek' data type
{-# LANGUAGE TemplateHaskell #-}
module Db.FullWeek
    ( FullWeek(..)
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

import qualified Db.FullDay as FullDay
    ( FullDay(..)
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

-- | A 'FullWeek' contains every day of the week
data FullWeek a = MkFullWeek
    { _fullWeekWeek :: !Week.Week
    , _fullWeekMonday :: a
    , _fullWeekTuesday :: a
    , _fullWeekWednesday :: a
    , _fullWeekThursday :: a
    , _fullWeekFriday :: a
    , _fullWeekSaturday :: a
    , _fullWeekSunday :: a
    }
    deriving (Eq, Foldable, Functor, Generic, Show, Traversable)
makeFields ''FullWeek

instance FromJSON (FullWeek (Maybe HalfDay))
instance ToJSON (FullWeek (Maybe HalfDay))

instance Display (FullWeek (FullDay.FullDay (Maybe HalfDay))) where
    display fw = line <> foldMap (\d -> "\n" <> display d <> "\n" <> line) fw
        where line = "----------------------------------------"

-- | Create an empty 'FullWeek'
empty :: a -> Week.Week -> FullWeek (FullDay.FullDay a)
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
add
    :: HalfDay
    -> FullWeek (FullDay.FullDay (Maybe HalfDay))
    -> Maybe (FullWeek (FullDay.FullDay (Maybe HalfDay)))
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

-- | Check if the week is complete: every open day has two entries
full :: FullWeek (FullDay.FullDay (Maybe HalfDay)) -> Bool
full = all FullDay.ok

-- | Check if some work has been done during the week-end
overWork :: FullWeek (FullDay.FullDay (Maybe HalfDay)) -> Bool
overWork = any FullDay.overWork
