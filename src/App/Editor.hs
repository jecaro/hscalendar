{-# LANGUAGE TemplateHaskell #-}
module App.Editor
    ( ParseError(..)
    , editorToOptions
    , parse
    , hdAsText
    )
where

import           RIO
import           RIO.Process (HasProcessContext, proc, runProcess)
import qualified RIO.Text as Text
    ( isPrefixOf
    , lines
    , null
    , pack
    , unlines
    , unpack
    )
import qualified RIO.Time as Time (TimeOfDay(..), Day(..))
import qualified RIO.Time.Extended as Time(parser)

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
import           Lens.Micro.Platform (makeFields)
import           System.Directory (removeFile)
import           System.Environment (lookupEnv)
import           System.IO.Temp (emptySystemTempFile)

import           App.CustomDay (CustomDay(..))
import           App.WorkOption
    ( SetArrived(..)
    , SetLeft(..)
    , SetNotes(..)
    , SetOffice(..)
    , SetProj(..)
    , WorkOption(..)
    )

import           Db.HalfDay (HalfDay(..))
import           Db.Idle (Idle(..))
import           Db.Model (HdNotFound(..))
import           Db.Notes (Notes, mkNotes, unNotes)
import qualified Db.Office as Office (Office(..), parser)
import           Db.Project (Project, mkProject, unProject)
import           Db.TimeInDay
import           Db.Worked (Worked(..))

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
    , _fileWorkedNotes   :: !Notes
    }
  deriving Show
makeFields ''FileWorked

data ParseError = EmptyFileError | ParserError Text
    deriving Typeable

instance Exception ParseError

instance Show ParseError where
    show EmptyFileError = "The file is empty"
    show (ParserError msg) = "Parse error: " <> Text.unpack msg

-- | The editor returned an error
newtype ProcessReturnedError = ProcessReturnedError String

instance Exception ProcessReturnedError

instance Show ProcessReturnedError where
    show (ProcessReturnedError cmd) = "The process " <> cmd <> " returned an error"

skipHorizontalSpaces :: Parser ()
skipHorizontalSpaces = skipWhile isHorizontalSpace

projectParser :: Parser Project
projectParser = do
    str <- many1 letter
    case mkProject (Text.pack str) of
        Nothing -> fail "Unable to parse project"
        Just p  -> return p

notesTextParser :: Parser Notes
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
        -- Parse the lines
        else case parseOnly (fileParser <* endOfInput) prunedContent of
            Left msg         -> Left  $ ParserError $ Text.pack msg
            Right fileWorked -> Right $ toOptions fileWorked
  where notComment text = not $ Text.isPrefixOf "#" text

toOptions :: FileWorked -> [WorkOption]
toOptions file = [ MkSetProj    . SetProj    $ file ^. project
                 , MkSetOffice  . SetOffice  $ file ^. office
                 , MkSetArrived . SetArrived $ file ^. arrived
                 , MkSetLeft    . SetLeft    $ file ^. left
                 , MkSetNotes   . SetNotes   $ file ^. notes
                 ]

header :: Time.Day -> TimeInDay -> Text
header day tid = "# " <> textDisplay day <> " " <> packShow tid

packShow :: Show a => a -> Text
packShow = Text.pack . show

hdAsText :: Either HdNotFound HalfDay -> Text
hdAsText (Left (HdNotFound day tid)) = header day tid <> " Nothing\n"
hdAsText (Right (MkHalfDayIdle (MkIdle day tid hdt))) = header day tid <> " " <> packShow hdt <> "\n"
hdAsText (Right (MkHalfDayWorked (MkWorked wDay wTid wArrived wLeft wOffice wNotes wProject))) =
    header wDay wTid <> "\n"
                   <> unProject wProject <> "\n"
                   <> packShow wOffice <> " " <> textDisplay wArrived <> " " <> textDisplay wLeft <> "\n"
                   <> unNotes wNotes

-- | Launch an editor with the current occupation for the specified half-day.
-- On return, parse the file to extract `[WorkOption]` to be applied to the
-- current half-day.
editorToOptions
    :: (HasProcessContext env, HasLogFunc env)
    => (CustomDay -> TimeInDay -> RIO env HalfDay)
    -> CustomDay
    -> TimeInDay
    -> RIO env [App.WorkOption.WorkOption]
editorToOptions hdGet cd tid = do
    oldRecord <- hdAsText <$> try (hdGet cd tid)
    -- Bracket to make sure the temporary file will be deleted no matter what
    fileContent <- bracket (liftIO $ emptySystemTempFile "hscalendar")
        (liftIO . removeFile)
        (\filename -> do
            -- Write old record
            writeFileUtf8 filename oldRecord
            -- Find an editor
            editor <- liftIO $ fromMaybe "vim" <$> lookupEnv "EDITOR"
            -- Launch process
            exitCode <- proc editor [filename] runProcess
            -- Handle error code
            when (exitCode /= ExitSuccess) (throwIO $ ProcessReturnedError editor)
            -- Read file content
            readFileUtf8 filename
        )
    if fileContent == oldRecord
        then return []
        else case parse fileContent of
            Left e@(ParserError _) -> throwIO e
            Left e@EmptyFileError  -> throwIO e
            Right options          -> return options

