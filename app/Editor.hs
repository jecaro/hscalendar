module Editor

where

import           RIO
import           RIO.Text as Text(pack)
import           RIO.Time as Time(TimeOfDay)

import           Data.Attoparsec.Text 
    ( Parser
    , endOfLine
    , isHorizontalSpace
    , letter
    , many1
    , skipWhile
    , string
    , takeText
    )

import           CustomDay(CustomDay(..))
import qualified HalfDayType as HDT(HalfDayType(..))
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
-- worked 22/03/1979 morning COMAREM
-- rennes 9:00 12:00
-- these are the notes
-- 
-- # Holiday -> create half-day
-- holiday 22/03/1979 morning ph

data FileWorked = FileWorked
    { _fileWorkedDay     :: !CustomDay
    , _fileWorkedTid     :: !TimeInDay
    , _fileWorkedProject :: !Project
    , _fileWorkedOffice  :: !Office
    , _fileWorkedArrived :: !Time.TimeOfDay
    , _fileWorkedLeft    :: !Time.TimeOfDay
    , _fileWorkedNotes   :: !NotesText
    }
  deriving Show

data FileHoliday = FileHoliday
    { _fileHolidayDay         :: !CustomDay
    , _fileHolidayTid         :: !TimeInDay
    , _fileHolidayHalfDayType :: !HDT.HalfDayType
    }
  deriving Show

data FileContent = FileContentWorked FileWorked | FileContentHoliday FileHoliday
  deriving Show

data Occupation = Worked | Holiday

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

fileWorkedParser :: Parser FileWorked
fileWorkedParser = FileWorked
    <$> customDayParser <* skipHorizontalSpaces
    <*> timeInDayParser <* skipHorizontalSpaces
    <*> projectParser   <* skipHorizontalSpaces <* endOfLine
    <*> officeParser    <* skipHorizontalSpaces 
    <*> timeOfDayParser <* skipHorizontalSpaces 
    <*> timeOfDayParser <* skipHorizontalSpaces <* endOfLine
    <*> notesTextParser

fileHolidayParser :: Parser FileHoliday
fileHolidayParser = FileHoliday
    <$> customDayParser <* skipHorizontalSpaces
    <*> timeInDayParser <* skipHorizontalSpaces
    <*> halfDayTypeParser

occupationParser :: Parser Occupation
occupationParser = "worked" $> Worked <|> "holiday" $> Holiday

fileParser :: Parser FileContent
fileParser = do
    occupation <- occupationParser
    skipHorizontalSpaces
    case occupation of
        Worked  -> FileContentWorked  <$> fileWorkedParser
        Holiday -> FileContentHoliday <$> fileHolidayParser

