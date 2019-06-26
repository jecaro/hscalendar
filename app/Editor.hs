{-# LANGUAGE TemplateHaskell #-}
module Editor
    ( ParseError(..)
    , parse
    )
where

import           RIO
import qualified RIO.Text as Text(isPrefixOf, lines, null, pack, unlines, unpack)
import qualified RIO.Time as Time(TimeOfDay)
import qualified RIO.Time.Extended as Time(parser)

import           Lens.Micro.Platform (makeFields)

import           Data.Attoparsec.Text 
    ( Parser
    , endOfLine
    , endOfInput
    , isHorizontalSpace
    , letter
    , many1
    , parseOnly
    , skipWhile
    , takeText
    )

import           CommandLine 
    ( SetArrived(..)
    , SetLeft(..)
    , SetNotes(..)
    , SetOffice(..)
    , SetProj(..)
    , WorkOption(..)
    )
import           Model (NotesText, Project, mkNotes, mkProject) 
import qualified Office as Office (Office(..), parser) 

-- Example of data
-- 
-- # Worked day -> create half-day half-day worked
-- # 22/03/1979 morning 
-- COMAREM
-- rennes 9:00 12:00
-- these are the notes

data FileWorked = FileWorked
    { _fileWorkedProject :: !Project
    , _fileWorkedOffice  :: !Office.Office
    , _fileWorkedArrived :: !Time.TimeOfDay
    , _fileWorkedLeft    :: !Time.TimeOfDay
    , _fileWorkedNotes   :: !NotesText
    }
  deriving Show
makeFields ''FileWorked

data ParseError = EmptyFileError | ParserError Text
    deriving Typeable

instance Exception ParseError

instance Show ParseError where
    show (EmptyFileError) = "The file is empty"
    show (ParserError msg) = "Parse error: " <> Text.unpack msg

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
    <$> projectParser <* skipHorizontalSpaces <* endOfLine
    <*> Office.parser <* skipHorizontalSpaces 
    <*> Time.parser   <* skipHorizontalSpaces 
    <*> Time.parser   <* skipHorizontalSpaces <* endOfLine
    <*> notesTextParser

parse :: Text -> Either ParseError [WorkOption]
parse fileContent = do
    -- Remove comments
    let prunedContent = Text.unlines $ filter notComment $ Text.lines fileContent
    -- Remaining file is empty
    if Text.null prunedContent
        then Left EmptyFileError
        else do
            -- Parse the lines
            case parseOnly (fileParser <* endOfInput) prunedContent of 
                Left msg -> Left $ ParserError $ Text.pack msg
                Right fileWorked -> Right $ toOptions fileWorked
  where notComment text = not $ Text.isPrefixOf "#" text

toOptions :: FileWorked -> [WorkOption]
toOptions file = [ MkSetProj    . SetProj    $ file ^. project 
                 , MkSetOffice  . SetOffice  $ file ^. office 
                 , MkSetArrived . SetArrived $ file ^. arrived
                 , MkSetLeft    . SetLeft    $ file ^. left
                 , MkSetNotes   . SetNotes   $ file ^. notes 
                 ]
