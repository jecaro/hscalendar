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
    , logException
    , runDB
    )
import           App.CommandLine (Cmd(..))
import           App.DayDesc (toDay)
import           App.MonthDesc (toMonth)
import           App.WeekDesc (toWeek)
import           App.Editor (editorToOptions)
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
    , hdSetOff
    , migrateAll
    , monthGet
    , projAdd
    , projList
    , projRename
    , projRm
    , weekGet
    )
import           Db.HalfDay as HalfDay (displayHdWithDate)
import           Db.Project (unProject)
import           Db.MonthF (stats)


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
                           (\e@(ProjExists _) -> logException e)

-- Remove a project
run (ProjRm project) = catches (runDB $ projRm project)
    [ Handler (\e@(ProjHasHd _)    -> logException e)
    , Handler (\e@(ProjNotFound _) -> logException e)
    ]

-- Rename a project
run (ProjRename p1 p2) = catches (runDB $ projRename p1 p2)
    [ Handler (\e@(ProjExists _)   -> logException e)
    , Handler (\e@(ProjNotFound _) -> logException e)
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
       Right hd -> logInfo $ displayHdWithDate hd

-- Display a complete week
run (DiaryWeek cw) = do
    -- Get actual week
    week <- toWeek cw
    hds <- runDB $ weekGet week
    logInfo $ display hds

-- Display a complete month
run (DiaryMonth cm) = do
    -- Get actual month
    month <- toMonth cm
    hds <- runDB $ monthGet month
    logInfo $ display hds
    logInfo $ display $ stats hds

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
        [ Handler (\e@TimesAreWrong      -> logException e)
        , Handler (\e@ProjCmdIsMandatory -> logException e)
        , Handler (\e@(ProjNotFound _)   -> logException e)
        ]
    -- Display new Half-Day
    run $ DiaryDisplay cd tid

-- Set a day off
run (DiaryOff cd tid hdt) = do
    day <- toDay cd
    runDB $ hdSetOff day tid hdt
    -- Display new Half-Day
    run $ DiaryDisplay cd tid

-- Delete an entry
run (DiaryRm cs tid) = do
    day <- toDay cs
    catch (runDB $ hdRm day tid) (\e@(HdNotFound _ _) -> logException e)


