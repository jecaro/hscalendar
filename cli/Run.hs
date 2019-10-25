-- | Implement all the commands
module Run 
  ( App(..)
  , run
  )
where

import           RIO
import           RIO.Orphans ()
import           RIO.Process (HasProcessContext, proc, runProcess)
import qualified RIO.Text as Text (pack)

import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Database.Persist.Sql (runMigration)
import           System.Directory (removeFile)
import           System.Environment (lookupEnv)
import           System.IO.Temp (emptySystemTempFile)


import           App.App 
    ( App(..)
    , HasConfig(..)
    , HasConnPool(..)
    , runDB
    )
import           App.WorkOption (ProjCmdIsMandatory(..), runWorkOptions)
import           App.CustomDay(toDay)
import           Db.HalfDay (HalfDay(..))
import           Db.Idle (Idle(..))
import           Db.Model
    ( HdNotFound(..)
    , ProjExists(..)
    , ProjHasHd(..)
    , ProjNotFound(..)
    , TimesAreWrong(..)
    , hdGet
    , hdRm
    , hdSetHoliday
    , migrateAll
    , projAdd
    , projList
    , projRename
    , projRm
    , showDay
    , showTime
    )
import           Db.Notes (unNotes)
import           Db.Project (unProject)
import           Db.Worked (Worked(..))

import           CommandLine (Cmd(..))
import           Editor (ParseError(..), hdAsText, parse)

-- | The editor returned an error
newtype ProcessReturnedError = ProcessReturnedError String

instance Exception ProcessReturnedError

instance Show ProcessReturnedError where
    show (ProcessReturnedError cmd) = "The process " <> cmd <> " returned an error"

-- | Print an exception 
printException :: (MonadIO m, MonadReader env m, HasLogFunc env, Show a) => a -> m ()
printException =  logError . displayShow

-- | Execute the command
run :: (HasConnPool env, HasConfig env, HasLogFunc env, HasProcessContext env) 
    => Cmd 
    -> RIO env ()

-- Migrate the database
run Migrate = runDB $ runMigration migrateAll

-- List the projects
run ProjList = runDB projList >>= mapM_ (logInfo . display . unProject) 

-- Add a project
run (ProjAdd project) = catch (void $ runDB $ projAdd project) 
                           (\e@(ProjExists _) -> printException e)

-- Remove a project
run (ProjRm project) = catches (runDB $ projRm project) 
    [ Handler (\e@(ProjHasHd _)    -> printException e)
    , Handler (\e@(ProjNotFound _) -> printException e)
    ]

-- Rename a project
run (ProjRename p1 p2) = catches (runDB $ projRename p1 p2)
    [ Handler (\e@(ProjExists _)   -> printException e)
    , Handler (\e@(ProjNotFound _) -> printException e)
    ]

-- Display an entry
run (DiaryDisplay cd tid) = do
    -- Get actual day
    day <- toDay cd
    -- Display input date
    logInfo . display $ showDay day <> " " <> (Text.pack . show) tid
    -- Get half-day
    eiHd <- try $ runDB $ hdGet day tid
    -- Analyse output to produce lines of text
    let hdStr = case eiHd of
           Left e@(HdNotFound _ _) -> [ (Text.pack . show) e ]
           Right (MkHalfDayIdle (MkIdle _ _ hdt)) -> [ (Text.pack . show) hdt ]
           Right (MkHalfDayWorked (MkWorked _ _ tArrived tLeft office notes project)) ->
               [ (Text.pack . show) office <> ":  " <> showTime tArrived <> " - " <> showTime tLeft
               , "Project: " <> unProject project
               , "Notes:"
               , unNotes notes
               ]
    -- Print it
    mapM_ (logInfo . display) hdStr

-- Edit an entry
run (DiaryEdit cd tid) = do
    -- Get the old record to put as default in the file
    day <- toDay cd
    oldRecord <- hdAsText <$> try (runDB $ hdGet day tid)
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
        then nothingToDo
        else case parse fileContent of
            Left e@(ParserError _) -> throwIO e
            Left EmptyFileError    -> nothingToDo
            Right options          -> run $ DiaryWork cd tid options
  where nothingToDo = logWarn "Nothing to do"

-- Set a work entry 
run (DiaryWork cd tid wopts) = do
    -- Get actual day
    day <- toDay cd
    -- Create the record in DB
    catches (runWorkOptions day tid wopts)
        [ Handler (\e@TimesAreWrong      -> printException e)
        , Handler (\e@ProjCmdIsMandatory -> printException e)
        , Handler (\e@(ProjNotFound _)   -> printException e)
        ]
    -- Display new Half-Day
    run $ DiaryDisplay cd tid
 
-- Set a holiday entry
run (DiaryHoliday cd tid hdt) = do
    day <- toDay cd
    runDB $ hdSetHoliday day tid hdt
    -- Display new Half-Day
    run $ DiaryDisplay cd tid

-- Delete an entry
run (DiaryRm cs tid) = do
    day <- toDay cs
    catch (runDB $ hdRm day tid) (\e@(HdNotFound _ _) -> printException e)


