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
import           Data.Foldable

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
--     -a 00:00:00  set arrived time
--     -l 00:00:00  set left time
-- hsmaster diary holiday date Morning|Afternoon
-- hsmaster diary rm date Morning|Afternoon
-- hsmaster project list
-- hsmaster project remove project
-- hsmaster project add project

-- TODO:
-- - Error handling in parser -> show message
-- - But need to apply times in the same time
-- - Dont display seconds
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
-- - Add unit testing
-- - Remove public holiday
-- - Put -p as positional parameter
-- - Display entry after edit/new
-- - Potential bug time before after
-- - Use Lens instead of records

-- Ideas
-- - put default values for starting ending time in a config file
-- - put db file in a config file as well

projectNotFound name = "The project " ++ name ++ " is not in the database"
projectAlready name = "The project " ++ name ++ " is already in the database"
noEntry = "No entry"
dbInconstistency = "Database inconsistency"
timesAreWrong = "Times are wrong"
projCmdIsMandatory = "There should be one project command"

-- Find a project option in a list of options, gets its name and return 
-- remaining options
findProjCmd :: [WorkOption] -> (Maybe String, [WorkOption])
findProjCmd (SetProj str:xs) = (Just str, xs)
findProjCmd (x:xs) = (prjName, x:options)
   where (prjName, options) = findProjCmd xs
findProjCmd [] = (Nothing, [])
 
-- Get a project return an error message if the project cannot be found
getProject :: MonadIO m => String -> SqlPersistT m (Either String (Entity Project))
getProject name = do
   mbPId <- getBy $ UniqueName name
   return $ case mbPId of
      Nothing -> Left $ projectNotFound name
      Just entity -> Right entity

-- Check if it is possible to create a new entry in HalfDayWorked.
-- We need a SetProj command with a valid project name
-- We try to find SetProj command
checkCreateConditions :: MonadIO m =>
   [WorkOption]
   -> SqlPersistT m (Either String (Key Project, [WorkOption]))
checkCreateConditions opts = case findProjCmd opts of 
   (Nothing, _) -> return $ Left projCmdIsMandatory
   (Just name, otherCmds) -> do
      eiProject <- getProject name
      case eiProject of
         Left msg -> return $ Left msg
         Right (Entity pId _) -> return $ Right (pId, otherCmds)

-- List projects
run :: MonadIO m => Cmd -> SqlPersistT m ()
run ProjList = do
   projects <- selectList [] [Asc ProjectName]
   let names = map (projectName . entityVal) projects
   liftIO $ mapM_ putStrLn names

-- Add a project
run (ProjAdd name) = do
   mbPId <- getBy $ UniqueName name
   case mbPId of
      Nothing -> void . insert $ Project name
      Just (Entity _ _) -> liftIO . putStrLn $ projectAlready name

-- Remove a project
-- TODO ask for confirmation when erasing hdw
run (ProjRm name) = do
   eiProject <- getProject name 
   case eiProject of
      Left msg -> liftIO . putStrLn $ msg
      Right (Entity pId _) -> do 
         deleteWhere [HalfDayWorkedProjectId ==. pId]
         delete pId

-- Display an entry
run (DiaryDisplay day time) = do
   -- Display input date
   liftIO . putStrLn $ show day ++ " " ++ show time
   -- Get Half-Day
   mbHdE <- getBy $ DayAndTimeInDay day time
   -- Get Half-Day worked
   mbHdwE <- join <$> mapM (getBy . UniqueHalfDayId . entityKey) mbHdE
   -- Get Project
   mbP <- join <$> mapM (get . halfDayWorkedProjectId . entityVal ) mbHdwE
   -- Get output lines
   let lines = case (mbHdE, mbHdwE, mbP) of
         (Nothing, _, _)                               -> [ noEntry ]
         (Just (Entity _ (HalfDay _ _ Holiday)), _, _) -> [ show Holiday ]
         (Just (Entity _ (HalfDay _ _ Worked)), 
            Just (Entity _ (HalfDayWorked notes arrived left office _ _)), 
            Just (Project name))                       -> 
               [ show office ++ ":  " ++ (show arrived) ++ " - " ++ (show left)
               , "Project: " ++ name
               , "Notes:   " ++ notes ]
         (_, _, _)                                     -> [ dbInconstistency ]
   -- Print it
   liftIO $ mapM_ putStrLn lines
 
-- Set a work entry
run (DiaryWork day time opts) = do

   -- Get half-day 
   mbHdE <- getBy $ DayAndTimeInDay day time
   -- Get half-day type
   let mbHdType = fmap (halfDayType . entityVal) mbHdE 
   -- Get half-day worked 
   -- mapM goes inside the maybe monad and getBy returns maybe. This results
   -- maybe maybe that must be joined
   mbHdwE <- join <$> mapM (getBy . UniqueHalfDayId . entityKey) mbHdE
   -- Get conditions for creating new hdw  
   eiConditions <- checkCreateConditions opts  
   
   -- Get hd, hdw and options depending on the cases
   -- If we create hdw we need a set project option so it is removed from
   -- the list of commands
   eiHdEHdwE <- case (mbHdE, mbHdType, mbHdwE, eiConditions) of
      -- First case, hdw exists return it
      (Just hdE, Just Worked, Just hdwE, _) -> return $ Right (hdE, hdwE, opts)
      -- Worked day but no hdw, error
      (Just hdE, Just Worked, Nothing, _)   -> return $ Left dbInconstistency
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
      
   -- Check output of last command and apply remaining commands
   case eiHdEHdwE of
      Left msg -> liftIO $ putStrLn msg
      Right (hdE, hdwE, opts) -> mapM_ (dispatchEdit hdE hdwE) opts

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

-- Create an entry
runCreateHdw :: (MonadIO m) =>
   TimeInDay
   -> Key HalfDay
   -> Key Project
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

-- Return the times in the day in a list
timesOfDay :: HalfDayWorked -> [TimeOfDay]
timesOfDay hdw = [halfDayWorkedArrived hdw, halfDayWorkedLeft hdw]

-- Return true if the list is sorted
isOrdered :: (Ord a) => [a] -> Bool
isOrdered []       = True
isOrdered [x]      = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)

-- Return Nothing if the times in the list are ordered. Return an error message
-- otherwise
timeAreOrdered :: [TimeOfDay] -> Maybe String
timeAreOrdered times = if isOrdered times
   then Nothing
   else Just timesAreWrong

-- Check that the constraints on the times are valid between the two days
timesAreOrderedInDay :: TimeInDay -> HalfDayWorked -> Maybe HalfDayWorked -> Maybe String
timesAreOrderedInDay Morning hdw mbOtherHdw = timeAreOrdered $ timesOfDay hdw ++ otherTimes
   where otherTimes = concatMap timesOfDay mbOtherHdw
-- We switch the arguments and call the same function
timesAreOrderedInDay Afternoon hdw (Just otherHdw) = 
   timesAreOrderedInDay Morning otherHdw (Just hdw)
-- Afternoon only, just need to check for half day
timesAreOrderedInDay Afternoon hdw Nothing = timeAreOrdered $ timesOfDay hdw

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
   eiProject <- getProject name
   case eiProject of 
      Left msg -> liftIO . putStrLn $ msg
      Right (Entity pId _) -> update hdwId [HalfDayWorkedProjectId =. pId]

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
   case timesAreOrderedInDay tid hdw' otherHdw of
      Just msg -> liftIO . putStrLn $ msg
      Nothing  -> replace hdwId hdw'

main :: IO ()
-- runNoLoggingT or runStdoutLoggingT
main = runNoLoggingT . withSqlitePool "file.db" 3 . runSqlPool $ do
   runMigration migrateAll
   liftIO (execParser opts) >>= run

