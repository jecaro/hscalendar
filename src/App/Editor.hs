{-# LANGUAGE TemplateHaskell #-}

module App.Editor
    ( ParseError (..),
      editorToOptions,
      fileParser,
      parse,
      hdAsText,
    )
where

import App.DayDesc (DayDesc (..))
import App.WorkOption
    ( SetArrived (..),
      SetLeft (..),
      SetNotes (..),
      SetOffice (..),
      SetProj (..),
      WorkOption (..),
    )
import Data.Attoparsec.Text
    ( Parser,
      endOfInput,
      endOfLine,
      isHorizontalSpace,
      parseOnly,
      skipWhile,
    )
import Db.HalfDay (HalfDay (..))
import Db.Model (HdNotFound (..))
import qualified Db.Notes as Notes (Notes, parser, unNotes)
import Db.Off (Off (..))
import qualified Db.Office as Office (Office (..), parser)
import qualified Db.Project as Project (Project, parser, unProject)
import Db.TimeInDay
import Db.Worked (Worked (..))
import Lens.Micro.Platform (makeFields)
import RIO
import RIO.Process (HasProcessContext, proc, runProcess)
import qualified RIO.Text as Text
    ( isPrefixOf,
      lines,
      null,
      pack,
      stripEnd,
      unlines,
      unpack,
    )
import qualified RIO.Time as Time (Day (..), TimeOfDay (..))
import qualified RIO.Time.Extended as Time (parser)
import System.Directory (removeFile)
import System.Environment (lookupEnv)
import System.IO.Temp (emptySystemTempFile)

-- Example of data
--
-- # Worked day -> create half-day half-day worked
-- # 22/03/1979 morning
-- COMAREM
-- rennes 9:00 12:00
-- these are the notes

data FileWorked = FileWorked
    { _fileWorkedProject :: !Project.Project,
      _fileWorkedOffice :: !Office.Office,
      _fileWorkedArrived :: !Time.TimeOfDay,
      _fileWorkedLeft :: !Time.TimeOfDay,
      _fileWorkedNotes :: !Notes.Notes
    }
    deriving (Show)

makeFields ''FileWorked

data ParseError = EmptyFileError | ParserError Text
    deriving (Typeable)

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

fileParser :: Parser FileWorked
fileParser =
    FileWorked
        <$> Project.parser <* skipHorizontalSpaces <* endOfLine
        <*> Office.parser <* skipHorizontalSpaces
        <*> Time.parser <* skipHorizontalSpaces
        <*> Time.parser <* optional (skipHorizontalSpaces <* endOfLine)
        <*> Notes.parser

parse :: Text -> Either ParseError [WorkOption]
parse fileContent = do
    -- Remove comments
    let prunedContent = Text.stripEnd $ Text.unlines $ filter notComment $ Text.lines fileContent
    -- Remaining file is empty
    if Text.null prunedContent
        then Left EmptyFileError
        else -- Parse the lines
        case parseOnly (fileParser <* endOfInput) prunedContent of
            Left msg -> Left $ ParserError $ Text.pack msg
            Right fileWorked -> Right $ toOptions fileWorked
    where
        notComment text = not $ Text.isPrefixOf "#" text

toOptions :: FileWorked -> [WorkOption]
toOptions file =
    [ MkSetProj . SetProj $ file ^. project,
      MkSetOffice . SetOffice $ file ^. office,
      MkSetArrived . SetArrived $ file ^. arrived,
      MkSetLeft . SetLeft $ file ^. left,
      MkSetNotes . SetNotes $ file ^. notes
    ]

header :: Time.Day -> TimeInDay -> Text
header day tid = "# " <> textDisplay day <> " " <> packShow tid

packShow :: Show a => a -> Text
packShow = Text.pack . show

hdAsText :: Either HdNotFound HalfDay -> Text
hdAsText (Left (HdNotFound day tid)) = header day tid <> " Nothing\n"
hdAsText (Right (MkHalfDayOff (MkOff day tid hdt))) = header day tid <> " " <> packShow hdt <> "\n"
hdAsText (Right (MkHalfDayWorked (MkWorked wDay wTid wArrived wLeft wOffice wNotes wProject))) =
    header wDay wTid <> "\n"
        <> Project.unProject wProject
        <> "\n"
        <> packShow wOffice
        <> " "
        <> textDisplay wArrived
        <> " "
        <> textDisplay wLeft
        <> "\n"
        <> Notes.unNotes wNotes

-- | Launch an editor with the current occupation for the specified half-day.
-- On return, parse the file to extract `[WorkOption]` to be applied to the
-- current half-day.
editorToOptions ::
    (HasProcessContext env, HasLogFunc env) =>
    (DayDesc -> TimeInDay -> RIO env HalfDay) ->
    DayDesc ->
    TimeInDay ->
    RIO env [App.WorkOption.WorkOption]
editorToOptions hdGet cd tid = do
    oldRecord <- hdAsText <$> try (hdGet cd tid)
    -- Bracket to make sure the temporary file will be deleted no matter what
    fileContent <-
        bracket
            (liftIO $ emptySystemTempFile "hscalendar")
            (liftIO . removeFile)
            ( \filename -> do
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
        then pure []
        else case parse fileContent of
            Left e@(ParserError _) -> throwIO e
            Left e@EmptyFileError -> throwIO e
            Right options -> pure options
