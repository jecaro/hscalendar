{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Exception.Safe (MonadCatch, try, catch)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (runNoLoggingT)
import           Database.Persist.Sqlite
    ( SqlPersistT
    , runMigration
    , runSqlPool
    , withSqlitePool
    )
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (TimeOfDay(..))
import           Options.Applicative (execParser)
import           Text.Printf (printf)

import           CommandLine (WorkOption(..), Cmd(..), opts)
import           HalfDayType (HalfDayType(..))
import           Model
import           TimeInDay (TimeInDay(..))
import           ModelFcts
    ( ModelException(..)
    , hdHdwProjGet
    , hdRm
    , hdSetHoliday
    , hdSetWork
    , hdwSetArrived
    , hdwSetLeft
    , hdwSetNotes
    , hdwSetOffice
    , hdwSetProject
    , projAdd
    , projList
    , projRm
    )

-- Synopsis
-- hsmaster diary work date Morning|Afternoon [commands]
--   commands: 
--     -p project   set the project name 
--     -n note      set note
--     -a 00:00     set arrived time
--     -l 00:00     set left time
-- hsmaster diary holiday date morning|afternoon
-- hsmaster diary rm date morning|afternoon
-- hsmaster project list
-- hsmaster project remove project
-- hsmaster project add project

-- TODO:
-- - Error handling in parser -> show message
-- - Bug need to apply times in the same time
-- - Add import CSV
-- - Add stats for a year
-- - Add optional day/time
-- - Add keywords today, yesterday, tomorrow, change date 
--   for 22/03/1979, or 22/03 for current year, or 22 for 
--   current month
-- - Make diary date/TimeInDay optional default today
-- - Append note
-- - Launch editor
-- - Project empty string
-- - Add unit testing
-- - Remove public holiday
-- - Put -p as positional parameter
-- - Display entry after edit/new
-- - Use Lens instead of records
-- - Cascade delete
-- - Add project rename
-- - Use unliftio instead of safe-exceptions
-- - Try to remove mtl
-- - Handle exception from optparse-applicative
-- - Indentation 4

-- Ideas
-- - put default values for starting ending time in a config file
-- - put db file in a config file as well

dbInconstistency :: String
dbInconstistency = "Database inconsistency"

projCmdIsMandatory :: String
projCmdIsMandatory = "There should be one project command"

-- Find a project option in a list of options, gets its name and return 
-- remaining options
findProjCmd :: [WorkOption] -> (Maybe String, [WorkOption])
findProjCmd (SetProj x:xs) = (Just x, xs)
findProjCmd (x:xs) = (prjName, x:options)
   where (prjName, options) = findProjCmd xs
findProjCmd [] = (Nothing, [])

-- List projects
run :: (MonadIO m, MonadCatch m) => Cmd -> SqlPersistT m ()
run ProjList = projList >>= liftIO . mapM_ (putStrLn . projectName)

-- Add a project
run (ProjAdd name) =
    catch (void $ projAdd $ Project name) (\(ModelException msg) -> liftIO . putStrLn $ msg)

-- Remove a project
-- TODO ask for confirmation when erasing hdw
run (ProjRm name) = catch (projRm $ Project name) (\(ModelException msg) -> liftIO . putStrLn $ msg)

-- Display an entry
run (DiaryDisplay day tid) = do
    -- Display input date
    liftIO . putStrLn $ show day ++ " " ++ show tid
    -- Get half-day
    eiHdHdwProj <- try $ hdHdwProjGet day tid
    -- Analyse output to produce lines of text
    let hdStr = case eiHdHdwProj of
           Left (ModelException msg) -> [ msg ]
           Right (_, Nothing)        -> [ show Holiday ]
           Right (_, Just (HalfDayWorked notes arrived left office _ _, Project name)) ->
               [ show office ++ ":  " ++ showTime arrived ++ " - " ++ showTime left
               , "Project: " ++ name
               , "Notes:   " ++ notes
               ]
    -- Print it
    liftIO $ mapM_ putStrLn hdStr
  where showTime (TimeOfDay h m _) = printf "%02d" h ++ ":" ++ printf "%02d" m

-- Set a work entry 
run (DiaryWork day tid wopts) = do

    -- Get hdw
    eiHdHdwProj <- try $ hdHdwProjGet day tid
 
    -- Create it with a project if needed
    eiOtherOpts <- case (eiHdHdwProj, findProjCmd wopts) of
        -- Everything is there
        (Right (_, Just (_, _)), _) -> return $ Right wopts 
        -- Nothing or holiday
        (_, (Just proj, otherOpts)) -> do
            eiAdded <- try $ hdSetWork day tid $ Project proj
            case eiAdded of
                Right _ -> return $ Right otherOpts
                Left (ModelException msg) -> return $ Left msg
        -- Holiday but no project
        (Right (_, Nothing), (Nothing, _)) -> return $ Left projCmdIsMandatory
        -- No hd, but no project either
        (Left (ModelException _), (Nothing, _)) -> return $ Left projCmdIsMandatory
    
    -- Apply remaining options
    case eiOtherOpts of
        Left msg -> liftIO $ putStrLn msg
        Right otherOpts -> do
            mapM_ dispatchEditWithError otherOpts 
            -- Display new Half-Day
            run $ DiaryDisplay day tid
  where dispatchEditWithError x = catch (dispatchEdit day tid x) (\(ModelException msg) -> liftIO $ putStrLn msg)

-- Set a holiday entry
run (DiaryHoliday day tid) = do
    hdSetHoliday day tid
    -- Display new Half-Day
    run $ DiaryDisplay day tid

-- Delete an entry
run (DiaryRm day tid) = catch (hdRm day tid) (\(ModelException msg) -> liftIO $ putStrLn msg)

-- Dispatch edit - TODO handle modelexception (no project)
dispatchEdit
    :: (MonadIO m, MonadCatch m)
    => Day
    -> TimeInDay
    -> WorkOption
    -> SqlPersistT m()
-- Set arrived time
dispatchEdit day tid (SetArrived time)  = hdwSetArrived day tid time
-- Set left time
dispatchEdit day tid (SetLeft time)     =  hdwSetLeft day tid time
-- Simple actions handling
dispatchEdit day tid (SetNotes notes)   = hdwSetNotes day tid notes
dispatchEdit day tid (SetOffice office) = hdwSetOffice day tid office
dispatchEdit day tid (SetProj name)     = hdwSetProject day tid $ Project name

main :: IO ()
-- runNoLoggingT or runStdoutLoggingT
main = runNoLoggingT . withSqlitePool "file.db" 3 . runSqlPool $ do
    runMigration migrateAll
    liftIO (execParser opts) >>= run

