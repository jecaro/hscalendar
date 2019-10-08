module Formatting.Extended (formatTwoDigitsPadZero)
where

import RIO

import Formatting (int, left, sformat, (%.))

formatTwoDigitsPadZero :: Int -> Text
formatTwoDigitsPadZero = sformat (left 2 '0' %. int) 
