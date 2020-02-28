{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Additional functions related to "RIO.Time"
module RIO.Time.Extended (parser)
where

import           RIO
import qualified RIO.Time as Time
    ( Day
    , TimeOfDay(..)
    , toGregorian
    )
import qualified RIO.Text as Text (intercalate)

import           Data.Attoparsec.Text
    ( Parser
    , decimal
    , char
    )
import           Formatting.Extended (formatTwoDigitsPadZero)

instance Display Time.Day where
    textDisplay day = Text.intercalate "-" (fmap formatTwoDigitsPadZero [d, m, intY])
        where (y, m, d) = Time.toGregorian day
              intY = fromIntegral y

instance Display Time.TimeOfDay where
    textDisplay (Time.TimeOfDay h m _) =
            Text.intercalate ":" $ fmap formatTwoDigitsPadZero [h, m]

-- | Parse a 'Time.TimeOfDay' without handling the seconds
parser :: Parser Time.TimeOfDay
parser = (\h m -> Time.TimeOfDay h m 0) <$> decimal <*> (char ':' *> decimal)
