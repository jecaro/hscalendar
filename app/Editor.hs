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
    , takeText
    )

import           Model (NotesText, Project, mkNotes, mkProject) 
import           Parsers 
    ( officeParser
    , timeOfDayParser
    )
import           Office (Office(..)) 

-- Example of data
-- 
-- # Worked day -> create half-day half-day worked
-- # 22/03/1979 morning 
-- COMAREM
-- rennes 9:00 12:00
-- these are the notes

data FileWorked = FileWorked
    { _fileWorkedProject :: !Project
    , _fileWorkedOffice  :: !Office
    , _fileWorkedArrived :: !Time.TimeOfDay
    , _fileWorkedLeft    :: !Time.TimeOfDay
    , _fileWorkedNotes   :: !NotesText
    }
  deriving Show

skipHorizontalSpaces :: Parser ()
skipHorizontalSpaces = skipWhile isHorizontalSpace

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

fileParser :: Parser FileWorked
fileParser = FileWorked
    <$> projectParser   <* skipHorizontalSpaces <* endOfLine
    <*> officeParser    <* skipHorizontalSpaces 
    <*> timeOfDayParser <* skipHorizontalSpaces 
    <*> timeOfDayParser <* skipHorizontalSpaces <* endOfLine
    <*> notesTextParser


