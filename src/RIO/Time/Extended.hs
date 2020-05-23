{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Additional functions related to "RIO.Time"
module RIO.Time.Extended (parser, weekDay) where

import Data.Attoparsec.Text
    ( Parser,
      char,
      decimal,
    )
import Data.Time.Calendar.WeekDate (toWeekDate)
import Formatting.Extended (formatTwoDigitsPadZero)
import RIO
import qualified RIO.Text as Text (intercalate)
import qualified RIO.Time as Time
    ( Day,
      TimeOfDay (..),
      toGregorian,
    )

instance Display Time.Day where
    textDisplay day =
        weekDay day <> " "
            <> Text.intercalate "-" (fmap formatTwoDigitsPadZero [d, m, intY])
        where
            (y, m, d) = Time.toGregorian day
            intY = fromIntegral y

instance Display Time.TimeOfDay where
    textDisplay (Time.TimeOfDay h m _) =
        Text.intercalate ":" $ fmap formatTwoDigitsPadZero [h, m]

-- | Parse a 'Time.TimeOfDay' without handling the seconds
parser :: Parser Time.TimeOfDay
parser = (\h m -> Time.TimeOfDay h m 0) <$> decimal <*> (char ':' *> decimal)

-- | Return the day of the week in a string
weekDay :: IsString s => Time.Day -> s
weekDay d = case dayNb of
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"
    7 -> "Sunday"
    _ -> error "The day number must be between one and seven"
    where
        (_, _, dayNb) = toWeekDate d
