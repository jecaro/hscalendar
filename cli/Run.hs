-- | Implement all the commands
module Run
  ( App(..)
  , run
  )
where

import           RIO
import           RIO.Orphans ()
import           RIO.Process (HasProcessContext)

import           Control.Monad (void)
import           Database.Persist.Sql (runMigration)

import           App.App
    ( App(..)
    , HasConfig(..)
    , HasConnPool(..)
    , runDB
    )
import           App.CommandLine (Cmd(..))
import           App.CustomDay (toDay)
import           App.WorkOption
    ( ProjCmdIsMandatory(..)
    , runWorkOptions
    )
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
    )
import           Db.Project (unProject)

import           Editor (editorToOptions)

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
    -- Get half-day
    eiHd <- try $ runDB $ hdGet day tid
    -- Display output
    case eiHd of
       Left e@(HdNotFound _ _) -> logInfo $ displayShow e
       Right hd -> logInfo $ display hd

-- Edit an entry
run (DiaryEdit cd tid) = do
    options <- editorToOptions hdGetFromDB cd tid
    run $ DiaryWork cd tid options
  where hdGetFromDB cd' tid' = do
            day <- toDay cd'
            runDB $ hdGet day tid'

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


