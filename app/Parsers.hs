module Parsers
    ( halfDayTypeParser
    , timeOfDayParser
    )
where

import           RIO
import qualified RIO.Time as Time (TimeOfDay(..))

import           Data.Attoparsec.Text
    ( Parser
    , asciiCI 
    , decimal
    , char
    )

import           HalfDayType (HalfDayType(..))

halfDayTypeParser :: Parser HalfDayType
halfDayTypeParser = asciiCI "pl"   $> PayedLeave
                <|> asciiCI "fe"   $> FamilyEvent
                <|> asciiCI "rtte" $> RTTE
                <|> asciiCI "rtts" $> RTTS
                <|> asciiCI "ul"   $> UnpayedLeave
                <|> asciiCI "ph"   $> PublicHoliday
                <|> asciiCI "pt"   $> PartTime

timeOfDayParser :: Parser Time.TimeOfDay
timeOfDayParser = (\h m -> Time.TimeOfDay h m 0) <$> decimal <*> (char ':' *> decimal)