{-# LANGUAGE TemplateHaskell #-}
module Db.MonthF (MonthWithDays, add, empty)
where

import           RIO
import qualified RIO.Time as Time
import qualified RIO.Vector.Boxed as VB (Vector, generate, (!?))
import qualified RIO.Vector.Boxed.Partial as VB ((//))

import           Data.Aeson (FromJSON, ToJSON)

import           Lens.Micro.Platform (makeFields, (.~), (?~))

import qualified Db.DayF as DayF (DayF, afternoon, empty, morning)
import qualified Db.HalfDay as HalfDay (HalfDay, day, timeInDay)
import           Db.Month (Month, day, fromDay, nbDays)
import           Db.TimeInDay (TimeInDay(..))


data MonthF a = MkMonthF
    { _monthFMonth :: !Month
    , _monthFDays :: !(VB.Vector a)
    }
    deriving (Eq, Foldable, Functor, Generic, Show, Traversable)
makeFields ''MonthF

type MonthWithDays = MonthF (DayF.DayF (Maybe HalfDay.HalfDay))

instance FromJSON MonthWithDays
instance ToJSON MonthWithDays

instance Display MonthWithDays where
    display m =  line <> foldMap (\d -> "\n" <> display d <> "\n" <> line) m
        where line = "----------------------------------------"


empty :: a -> Month -> MonthF (DayF.DayF a)
empty a month' = MkMonthF month' $
    VB.generate (nbDays month') (DayF.empty a . day month')

add :: HalfDay.HalfDay -> MonthWithDays -> Maybe MonthWithDays
add hd m
    -- Different month nothing we can do
    | fromDay (hd ^. HalfDay.day) /= m ^. month = Nothing
    | otherwise =
            -- Get the index of the day in the month
        let hdDay = hd ^. HalfDay.day
            (_, _, d) = Time.toGregorian hdDay
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
            newDaysVect = daysVect VB.// [(d, newDay)]
        in Just (m & days .~ newDaysVect)
