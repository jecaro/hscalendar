{-# LANGUAGE TemplateHaskell #-}

module Db.MonthF (MonthWithDays, add, empty, stats) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Db.DayF as DayF (DayF, afternoon, empty, morning, stats)
import qualified Db.HalfDay as HalfDay (HalfDay, day, timeInDay)
import Db.Month (Month, day, fromDay, nbDays)
import qualified Db.Stats as Stats (Stats (..), empty)
import Db.TimeInDay (TimeInDay (..))
import Lens.Micro.Platform ((.~), (?~), _3, ix, makeFields)
import RIO
import qualified RIO.Time as Time
import qualified RIO.Vector.Boxed as VB ((!?), Vector, generate)

data MonthF a = MkMonthF
    { _monthFMonth :: !Month,
      _monthFDays :: !(VB.Vector a)
    }
    deriving (Eq, Foldable, Functor, Generic, Show, Traversable)

makeFields ''MonthF

type MonthWithDays = MonthF (DayF.DayF (Maybe HalfDay.HalfDay))

instance FromJSON MonthWithDays

instance ToJSON MonthWithDays

instance Display MonthWithDays where
    display m = line <> foldMap (\d -> "\n" <> display d <> "\n" <> line) m
        where
            line = "----------------------------------------"

empty :: a -> Month -> MonthF (DayF.DayF a)
empty a month' =
    MkMonthF month' $
        -- We need to add one to the index to have the day number in the month
        VB.generate (nbDays month') (DayF.empty a . day month' . (+) 1)

add :: HalfDay.HalfDay -> MonthWithDays -> Maybe MonthWithDays
add hd m
    -- Different month nothing we can do
    | fromDay (hd ^. HalfDay.day) /= m ^. month = Nothing
    | otherwise =
        -- Get the index of the day in the month
        let hdDay = hd ^. HalfDay.day
            -- Dont forget to remove 1 for the day to use it as an index
            d = Time.toGregorian hdDay ^. _3 - 1
            -- Get the right lens to update the DayF in the vector
            tidLens =
                case hd ^. HalfDay.timeInDay of
                    Morning -> DayF.morning
                    Afternoon -> DayF.afternoon
            -- Get the vector of days
            daysVect = m ^. days
            -- Safely extract the day corresponding to hd or create an
            -- empty day
            emptyDay = DayF.empty Nothing hdDay
            dayInVectOrEmpty = fromMaybe emptyDay (daysVect VB.!? d)
            -- Update the day then the vector
            newDay = dayInVectOrEmpty & tidLens ?~ hd
            newDaysVect = daysVect & ix d .~ newDay
         in Just (m & days .~ newDaysVect)

stats :: MonthWithDays -> Stats.Stats
stats = foldr DayF.stats Stats.empty
