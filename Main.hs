{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Exception.Safe (MonadCatch, try, catch)
import           Control.Monad (void, join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (runNoLoggingT)
import           Database.Persist.Sqlite
   ( Entity(..)
   , SqlPersistT
   , getBy
   , insertEntity
   , replace
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
import           Office (Office(..))
import           TimeInDay (TimeInDay(..))
import           ModelFcts
   ( ModelException(..)
   , hdHdwProjGet
   , hdRm
   , hdSetHoliday
   , hdwSetArrived
   , hdwSetLeft
   , hdwSetNotes
   , hdwSetOffice
   , hdwSetProject
   , projAdd
   , projGet
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
-- - Use project type instead of string
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

-- Check if it is possible to create a new entry in HalfDayWorked.
-- We need a SetProj command with a valid project name
-- We try to find SetProj command
checkCreateConditions :: (MonadIO m, MonadCatch m) =>
   [WorkOption]
   -> SqlPersistT m (Either String (ProjectId, [WorkOption]))
checkCreateConditions wopts = case findProjCmd wopts of
   (Nothing, _) -> return $ Left projCmdIsMandatory
   (Just name, otherCmds) -> do
      eiProject <- try $ projGet $ Project name
      case eiProject of
         Left  (ModelException msg) -> return $ Left msg
         Right pId                  -> return $ Right (pId, otherCmds)

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
run (DiaryDisplay day time) = do
   -- Display input date
   liftIO . putStrLn $ show day ++ " " ++ show time
   -- Get half-day
   eiHdHdwProj <- try $ hdHdwProjGet day time
   -- Analyse output to produce lines of text
   let hdStr = case eiHdHdwProj of
          Left (ModelException msg) -> [ msg ]
          Right (_, Nothing)        -> [ show Holiday ]
          Right (_, Just (HalfDayWorked notes arrived left office _ _, Project name)) ->
             [ show office ++ ":  " ++ showTime arrived ++ " - " ++ showTime left
             , "Project: " ++ name
             , "Notes:   " ++ notes
             ]
                where showTime (TimeOfDay h m _) =
                         printf "%02d" h ++ ":" ++ printf "%02d" m
   -- Print it
   liftIO $ mapM_ putStrLn hdStr

-- Set a work entry TODO tid
run (DiaryWork day time wopts) = do

   -- Get half-day 
   mbHdE <- getBy $ DayAndTimeInDay day time
   -- Get half-day type
   let mbHdType = fmap (halfDayType . entityVal) mbHdE
   -- Get half-day worked 
   -- mapM goes inside the maybe monad and getBy returns maybe. This results
   -- maybe maybe that must be joined
   mbHdwE <- join <$> mapM (getBy . UniqueHalfDayId . entityKey) mbHdE
   -- Get conditions for creating new hdw  
   eiConditions <- checkCreateConditions wopts

   -- Get hd, hdw and options depending on the cases
   -- If we create hdw we need a set project option so it is removed from
   -- the list of commands
   eiHdEHdwE <- case (mbHdE, mbHdType, mbHdwE, eiConditions) of
      -- First case, hdw exists return it
      (Just hdE, Just Worked, Just hdwE, _) -> return $ Right (hdE, hdwE, wopts)
      -- Worked day but no hdw, error
      (Just _, Just Worked, Nothing, _)   -> return $ Left dbInconstistency
      -- The two following cases need conditions, but no condition here so error
      (_, _, _, Left msg)            -> return $ Left msg
      -- Holiday and condition, carry on
      (Just hdE@(Entity hdId hd), Just Holiday, _, Right (projId, otherOpts)) -> do
         -- Override HD
         let hd' = hd { halfDayType = Worked }
         replace hdId hd'
         -- Create Hdw
         hdwE <- runCreateHdw time hdId projId
         return $ Right (hdE, hdwE, otherOpts)
      -- No hd, create everything
      (Nothing, _, _, Right (projId, otherOpts)) -> do
         -- Create HD
         let hd = HalfDay day time Worked
         hdE@(Entity hdId _) <- insertEntity hd
         -- Create Hdw
         hdwE <- runCreateHdw time hdId projId
         return $ Right (hdE, hdwE, otherOpts)
      -- This should never happens
      _ -> undefined

   -- Need a special case for time

   -- Check output of last command and apply remaining commands
   case eiHdEHdwE of
      Left msg -> liftIO $ putStrLn msg
      Right (_, _, otherOpts) -> do
         mapM_ (dispatchEdit day time) otherOpts
         -- Display new Half-Day
         run $ DiaryDisplay day time

-- Set a holiday entry
run (DiaryHoliday day time) = do
   hdSetHoliday day time
   -- Display new Half-Day
   run $ DiaryDisplay day time

-- Delete an entry
run (DiaryRm day time) = catch (hdRm day time) (\(ModelException msg) -> liftIO $ putStrLn msg)

-- Create an entry
runCreateHdw :: (MonadIO m) =>
   TimeInDay
   -> HalfDayId
   -> ProjectId
   -> SqlPersistT m (Entity HalfDayWorked)
runCreateHdw time hdId pId = do
   -- Create half-day
   let notes           = ""
       (arrived, left) = if time == Morning
          then (TimeOfDay 8 20 0, TimeOfDay 12 0 0)
          else (TimeOfDay 13 0 0, TimeOfDay 17 0 0)
       office = Rennes
   -- Create half-day
   insertEntity $ HalfDayWorked notes arrived left office pId hdId

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

