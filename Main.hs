{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
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
dbInconstistency = "Database inconsistency"
timesAreWrong = "Times are wrong"

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
   -- Getting HaflDay from date/time
   mbHdId <- getBy $ DayAndTimeInDay day time
   case mbHdId of
      -- It exists, edit it 
      Just hd@(Entity hdId _) -> do
         -- In this case it is mandatory to have a hdw as well
         mbHdwId <- getBy $ UniqueHalfDayId hdId
         case mbHdwId of
            Nothing -> liftIO . putStrLn $ dbInconstistency
            Just hdw -> mapM_ (dispatchEdit hd hdw) opts
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

-- Return the times in the day in a list
timesOfDay :: HalfDayWorked -> [TimeOfDay]
timesOfDay hdw = [halfDayWorkedArrived hdw, halfDayWorkedLeft hdw]

-- Return true if the list is sorted
isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Check that the constraint on the times are valid between the two days
checkTimeConstraints :: TimeInDay -> HalfDayWorked -> Maybe HalfDayWorked -> Maybe String
checkTimeConstraints Morning hdw mbOtherHdw =
   if isSorted $ timesOfDay hdw ++ otherTimes
   then Nothing
   else Just timesAreWrong
      where otherTimes = case mbOtherHdw of
                           Nothing -> []
                           Just otherHdw -> timesOfDay otherHdw
-- We switch the arguments and call the same function
checkTimeConstraints Afternoon hdw (Just otherHdw) =
   checkTimeConstraints Morning otherHdw (Just hdw)

-- From a half-day return the other half-day
otherHdFromHd :: MonadIO m => HalfDay -> SqlPersistT m (Maybe (Entity HalfDay))
otherHdFromHd hd = getBy $ DayAndTimeInDay day tid
   where tid = other $ halfDayTimeInDay hd
         day = halfDayDay hd

-- From a half-day worked return the other half-day
otherHdFromHdw :: MonadIO m => HalfDayWorked -> SqlPersistT m (Maybe (Entity HalfDay))
otherHdFromHdw hdw = runMaybeT $ do
   hdId <- MaybeT $ get $ halfDayWorkedHalfDayId hdw
   MaybeT $ otherHdFromHd hdId

-- From a worked half-day, return the other worked half-day
otherHdwFromHdw :: MonadIO m => HalfDayWorked -> SqlPersistT m (Maybe (Entity HalfDayWorked))
otherHdwFromHdw hdw = runMaybeT $ do
   otherHdId <- MaybeT $ otherHdFromHdw hdw
   MaybeT $ getBy $ UniqueHalfDayId $ entityKey otherHdId

-- Simple edit action using only hdwid
editSimpleById :: MonadIO m => HalfDayWorkedId -> WorkOption -> SqlPersistT m()
editSimpleById hdwId (SetNotes notes)   = update hdwId [HalfDayWorkedNotes   =. notes]
editSimpleById hdwId (SetOffice office) = update hdwId [HalfDayWorkedOffice  =. office]
editSimpleById hdwId (SetProj name) = do
   mbPId <- getBy $ UniqueName name
   case mbPId of
      Nothing             -> liftIO . putStrLn $ projectNotFound name
      Just (Entity pId _) -> update hdwId [HalfDayWorkedProjectId =. pId]

-- Dispatch edit
dispatchEdit :: MonadIO m => 
   Entity HalfDay 
   -> Entity HalfDayWorked 
   -> WorkOption 
   -> SqlPersistT m()
-- Set arrived time
dispatchEdit eHd eHdw (SetArrived time) = editTime eHd eHdw $ setArrivedTime time
-- Set left time
dispatchEdit eHd eHdw (SetLeft time) = editTime eHd eHdw $ setLeftTime time
-- Simple actions handling
dispatchEdit _ (Entity hdwId _) action = editSimpleById hdwId action

-- Edit arrived time function
setArrivedTime :: TimeOfDay -> HalfDayWorked -> HalfDayWorked
setArrivedTime time hdw = hdw { halfDayWorkedArrived = time }

-- Edit left time function
setLeftTime :: TimeOfDay -> HalfDayWorked -> HalfDayWorked
setLeftTime time hdw = hdw { halfDayWorkedLeft = time }

-- Set arrived/left time
editTime :: MonadIO m =>
   Entity HalfDay
   -> Entity HalfDayWorked
   -> (HalfDayWorked -> HalfDayWorked)
   -> SqlPersistT m ()
editTime (Entity _ hd) (Entity hdwId hdw) setTime = do
   -- Getting time in day of current half day
   let tid = halfDayTimeInDay hd
   -- Apply time to arrived/left hdw
       hdw' = setTime hdw
   -- Getting other hdw
   otherHdwE <- otherHdwFromHdw hdw
   let otherHdw = fmap entityVal otherHdwE
   -- Check if it works
   case checkTimeConstraints tid hdw' otherHdw of
      Just msg -> liftIO . putStrLn $ msg
      Nothing  -> replace hdwId hdw'

main :: IO ()
-- runNoLoggingT or runStdoutLoggingT
main = runNoLoggingT . withSqlitePool "file.db" 3 . runSqlPool $ do
   runMigration migrateAll
   liftIO (execParser opts) >>= run

