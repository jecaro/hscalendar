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
timeShouldBeLowerThan arrived left = "Time " ++ show arrived ++ " should be lower than " ++ show left

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
run (ProjRm name) = do
   mbPId <- getBy $ UniqueName name
   case mbPId of
      Nothing -> liftIO $ putStrLn $ projectNotFound name
      Just (Entity pId _) -> do
         deleteWhere [HalfDayWorkedProjectId ==. pId]
         delete pId

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
   mbHDId <- getBy $ DayAndTimeInDay day time
   case mbHDId of
      -- Edit an existing entry
      Just (Entity hdId _) -> do
         mbHDWId <- getBy $ UniqueHalfDayId hdId
         case mbHDWId of 
            Nothing -> undefined -- Error
            Just (Entity hdwId hdw) -> mapM_ (runEdit hdwId hdw) opts
      -- Create a new entry - check if we got a project
      Nothing -> return ()

-- Set a holiday entry
run (DiaryHoliday day time) = do
   mbHDId <- getBy $ DayAndTimeInDay day time
   case mbHDId of
      -- Edit an existing entry 
      Just (Entity hdId _) -> do
         -- Delete entry from HalfDayWorked if it exists
         deleteWhere [HalfDayWorkedHalfDayId ==. hdId]
         -- Update entry
         update hdId [HalfDayType =. Holiday]
      -- Create a new entry 
      Nothing -> void $ insert $ HalfDay day time Holiday

-- Make sure arrived < left
checkTimeContraint :: HalfDayWorked -> Maybe String
checkTimeContraint hdw = if arrived > left
   then Just $ timeShouldBeLowerThan arrived left
   else Nothing
      where arrived = halfDayWorkedArrived hdw
            left = halfDayWorkedLeft hdw

-- Edit an entry
-- TODO: make sure time is coherent with morning/afternoon
--       pass Entity instead
--       factorize time edit
runEdit :: MonadIO m => HalfDayWorkedId -> HalfDayWorked -> WorkOption -> SqlPersistT m()
runEdit hdwId _ (SetProj name) = do
   mbPId <- getBy $ UniqueName name
   case mbPId of 
      Nothing             -> liftIO . putStrLn $ projectNotFound name
      Just (Entity pId _) -> update hdwId [HalfDayWorkedProjectId =. pId]
runEdit hdwId _ (SetNotes notes)   = update hdwId [HalfDayWorkedNotes   =. notes]
runEdit hdwId _ (SetOffice office) = update hdwId [HalfDayWorkedOffice  =. office]
runEdit hdwId hdw (SetArrived time) = do
   let hdw' = hdw { halfDayWorkedArrived = time }
   case checkTimeContraint hdw' of
      Just msg -> liftIO . putStrLn $ msg
      Nothing  -> replace hdwId hdw'
runEdit hdwId hdw (SetLeft time) = do
   let hdw' = hdw { halfDayWorkedLeft = time }
   case checkTimeContraint hdw' of
      Just msg -> liftIO . putStrLn $ msg
      Nothing  -> replace hdwId hdw'

main :: IO ()
-- runNoLoggingT or runStdoutLoggingT
main = runNoLoggingT . withSqlitePool "file.db" 3 . runSqlPool $ do
   runMigration migrateAll
   liftIO (execParser opts) >>= run

