module Parsers
    ( customDayParser
    , officeParser
    , halfDayTypeParser
    , timeOfDayParser
    )
where

import           RIO
import qualified RIO.Time as Time (TimeOfDay(..), fromGregorian)

import           Data.Attoparsec.Text
    ( Parser
    , asciiCI 
    , decimal
    , char
    )

import           CustomDay (CustomDay(..))
import           HalfDayType (HalfDayType(..))
import           Office (Office(..))

customDayParser :: Parser CustomDay
customDayParser = asciiCI "today"     $> Today
              <|> asciiCI "yesterday" $> Yesterday
              <|> asciiCI "tomorrow"  $> Tomorrow
              <|> mkDayFromGregorian <$> decimal
                                     <*> (char '-' *> decimal)
                                     <*> (char '-' *> decimal)
              <|> MkDayMonthNum <$> decimal <*> (char '-' *> decimal)
              <|> MkDayNum <$> decimal
  where mkDayFromGregorian d m y = MkDay $ Time.fromGregorian y m d

officeParser :: Parser Office
officeParser = asciiCI "home"   $> Home
           <|> asciiCI "out"    $> OutOfOffice
           <|> asciiCI "poool"  $> Poool
           <|> asciiCI "rennes" $> Rennes

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