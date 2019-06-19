module Editor
where

import           RIO
import           RIO.Text as Text(pack)
import           RIO.Time as Time(TimeOfDay)

import           Data.Attoparsec.Text

import           CustomDay(CustomDay(..))
import           HalfDayType(HalfDayType(..))
import           Model (NotesText, Project, mkNotes, mkProject) 
import           Parsers 
    ( customDayParser
    , halfDayTypeParser
    , officeParser
    , timeOfDayParser
    )
import           Office (Office(..)) 
import           TimeInDay(TimeInDay(..))

-- Example of data
-- 
-- # Worked day -> create half-day half-day worked
-- 22/03/1979 morning COMAREM
-- rennes 9:00 12:00
-- these are the notes
-- 
-- # Holiday -> create half-day
-- 22/03/1979 morning ph

data FirstLine = FirstLine
    { day                  :: !CustomDay
    , tid                  :: !TimeInDay
    , halfDayTypeOrProject :: !(Either HalfDayType Project)}
  deriving Show

data OfficeAndTime = OfficeAndTime
    { office  :: !Office
    , arrived :: !Time.TimeOfDay
    , left    :: !Time.TimeOfDay
    }
  deriving Show

skipHorizontalSpaces :: Parser ()
skipHorizontalSpaces = skipWhile isHorizontalSpace

timeInDayParser :: Parser TimeInDay
timeInDayParser = string "morning"   $> Morning
              <|> string "afternoon" $> Afternoon

projectParser :: Parser Project
projectParser = do
    str <- many1 letter
    case mkProject (Text.pack str) of
        Nothing -> fail "Unable to parse project"
        Just p  -> return p

notesTextParser :: Parser NotesText
notesTextParser = do
  str <- takeText
  case mkNotes str of
      Nothing -> fail "Unable to parse notes"
      Just p  -> return p

firstLine :: Parser FirstLine
firstLine = FirstLine 
    <$> (customDayParser <* skipHorizontalSpaces)
    <*> (timeInDayParser <* skipHorizontalSpaces)
    <*> (eitherP halfDayTypeParser projectParser) 

officeAndTime :: Parser OfficeAndTime
officeAndTime = OfficeAndTime 
    <$> (officeParser <* skipHorizontalSpaces)
    <*> (timeOfDayParser <* skipHorizontalSpaces)
    <*> (timeOfDayParser <* skipHorizontalSpaces)

