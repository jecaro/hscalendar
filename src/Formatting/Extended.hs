-- | Fonctions related to the "Formatting" module
module Formatting.Extended (formatTwoDigitsPadZero) where

import Formatting ((%.), int, left, sformat)
import RIO

-- | Print a 'Int', suitable to output day or month numbers
formatTwoDigitsPadZero :: Int -> Text
formatTwoDigitsPadZero = sformat (left 2 '0' %. int)
