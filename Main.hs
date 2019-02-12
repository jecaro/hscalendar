{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Database.Persist
import           Database.Persist.Sqlite
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Options.Applicative

import           Model
import           HalfDayType
import           TimeInDay
import           Office
import           CommandLine

-- Synopsis
-- hsmaster diary work date Morning|Afternoon [commands]
--   commands: 
--     -p project   set the project name 
--     -n note      set note
--     -a 00:00     set arrived time
--     -l 00:00     set left time
-- hsmaster diary holiday date Morning|Afternoon
-- hsmaster diary rm date Morning|Afternoon
-- hsmaster project list
-- hsmaster project remove project
-- hsmaster project add project

-- TODO:
-- - Add consistency check
-- - Add import CSV
-- - Add stats for a year
-- - Add optional day/time
-- - Add keywords today, yesterday, tomorrow
-- - Parse time as 12:00
-- - Make diary date/TimeInDay optional
-- - Append note
-- - Launch editor
-- - Project empty string
-- - Put model into a separate module
-- - Add unit testing

-- Ideas
-- - put default values for starting ending time in a config file
-- - put db file in a config file as well

projectNotFound name = "The project " ++ name ++ " is not in the database"
projectAlready name = "The project " ++ name ++ " is already in the database"
noEntry = "No entry"

-- List projects
run :: MonadIO m => Cmd -> SqlPersistT m ()
run ProjList = do
   projects <- selectList [] [Asc ProjectName]
   let names = map (projectName . entityVal) projects
   liftIO $ mapM_ putStrLn names

-- Add a project
run (ProjAdd name) = do
   maybeProject <- getBy $ UniqueName name
   case maybeProject of
      Nothing -> void . insert $ Project name
      Just (Entity _ _) -> liftIO . putStrLn $ projectAlready name

-- Remove a project
-- TODO: check in the HalfDay Worked Table before
run (ProjRm name) = do
   mbProject <- getBy $ UniqueName name
   case mbProject of
      Nothing -> liftIO $ putStrLn $ projectNotFound name
      Just (Entity projectId _) -> delete projectId

-- Display an entry
run (DiaryDisplay day time) = do
   -- Display input
   liftIO . putStrLn $ show day ++ " " ++ show time
   mbHalfDayId <- getBy $ DayAndTimeInDay day time
   liftIO $ putStrLn $ case mbHalfDayId of
      Nothing -> noEntry
      Just (Entity id halfDay) -> show $ halfDayType halfDay
         
-- Set a work entry
run (DiaryWork day time opts) = do
   mbHalfDayId <- getBy $ DayAndTimeInDay day time
   case mbHalfDayId of
      -- Edit an existing entry
      Just (Entity hdId _) -> do
         mbHalfDayWorkedId <- getBy $ UniqueHalfDayId hdId
         case mbHalfDayWorkedId of 
            Nothing -> undefined -- Error
            Just (Entity hdwId _) -> mapM_ (runEdit hdwId) opts
      -- Create a new entry - check if we got a project
      Nothing -> return ()

-- Set a holiday entry
run (DiaryHoliday day time) = do
   mbHalfDayId <- getBy $ DayAndTimeInDay day time
   case mbHalfDayId of
      -- Edit an existing entry 
      Just (Entity id _) -> do
         -- Delete entry from HalfDayWorked if it exists
         deleteWhere [HalfDayWorkedHalfDayId ==. id]
         -- Update entry
         update id [HalfDayType =. Holiday]
      -- Create a new entry 
      Nothing -> void $ insert $ HalfDay day time Holiday

-- Edit an entry
-- TODO: make sure arrived < left and it's coherent with morning/afternoon
runEdit :: MonadIO m => HalfDayWorkedId -> WorkOption -> SqlPersistT m()
runEdit id (SetProj name)     = do
   mbProjectId <- getBy $ UniqueName name
   case mbProjectId of 
      Nothing             -> liftIO . putStrLn $ projectNotFound name
      Just (Entity pId _) -> update id [HalfDayWorkedProjectId =. pId]
runEdit id (SetNote note)     = update id [HalfDayWorkedNotes   =. note]
runEdit id (SetArrived time)  = update id [HalfDayWorkedArrived =. time]
runEdit id (SetLeft time)     = update id [HalfDayWorkedLeft    =. time]
runEdit id (SetOffice office) = update id [HalfDayWorkedOffice  =. office]

main :: IO ()
-- runNoLoggingT or runStdoutLoggingT
main = runNoLoggingT . withSqlitePool "file.db" 3 . runSqlPool $ do
   runMigration migrateAll
   liftIO (execParser opts) >>= run

