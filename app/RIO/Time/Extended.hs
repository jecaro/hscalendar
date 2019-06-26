module RIO.Time.Extended (parser)
where

import           RIO
import qualified RIO.Time as Time (TimeOfDay(..))

import           Data.Attoparsec.Text
    ( Parser
    , decimal
    , char
    )

parser :: Parser Time.TimeOfDay
parser = (\h m -> Time.TimeOfDay h m 0) <$> decimal <*> (char ':' *> decimal)