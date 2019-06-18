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
    , string
    , decimal
    , char
    )

import           CustomDay (CustomDay(..))
import           HalfDayType (HalfDayType(..))
import           Office (Office(..))

customDayParser :: Parser CustomDay
customDayParser = string "today"     $> Today
              <|> string "yesterday" $> Yesterday
              <|> string "tomorrow"  $> Tomorrow
              <|> mkDayFromGregorian <$> decimal
                                     <*> (char '-' *> decimal)
                                     <*> (char '-' *> decimal)
              <|> MkDayMonthNum <$> decimal <*> (char '-' *> decimal)
              <|> MkDayNum <$> decimal
  where mkDayFromGregorian d m y = MkDay $ Time.fromGregorian y m d

officeParser :: Parser Office
officeParser = string "home"   $> Home
           <|> string "out"    $> OutOfOffice
           <|> string "poool"  $> Poool
           <|> string "rennes" $> Rennes

halfDayTypeParser :: Parser HalfDayType
halfDayTypeParser = string "pl"   $> PayedLeave
                <|> string "fe"   $> FamilyEvent
                <|> string "rtte" $> RTTE
                <|> string "rtts" $> RTTS
                <|> string "ul"   $> UnpayedLeave
                <|> string "ph"   $> PublicHoliday
                <|> string "pt"   $> PartTime

timeOfDayParser :: Parser Time.TimeOfDay
timeOfDayParser = (\h m -> Time.TimeOfDay h m 0) <$> decimal <*> (char ':' *> decimal)