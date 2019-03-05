{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Exception.Safe (MonadCatch, try, catch)
import           Control.Monad (void, join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (runNoLoggingT)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Database.Persist.Sqlite 
   ( Entity(..)
   , SqlPersistT
   , delete
   , deleteWhere
   , get
   , getBy
   , insert
   , insertEntity
   , replace
   , runMigration
   , runSqlPool
   , update
   , withSqlitePool
   , (=.)
   , (==.)
   )
import           Data.Time.LocalTime (TimeOfDay(..))
import           Options.Applicative (execParser)
import           Text.Printf (printf) 

import           CommandLine (WorkOption(..), Cmd(..), opts)
import           HalfDayType (HalfDayType(..))
import           Model
import           Office (Office(..))
import           TimeInDay (TimeInDay(..), other)
import           ModelFcts 
   ( ModelException(..)
   , projAdd
   , projGet
   , projList
   , projRm
   , hdGet
   , hdHdwProjGet
   , hdwProjGet
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

-- Ideas
-- - put default values for starting ending time in a config file
-- - put db file in a config file as well

noEntry :: String
noEntry = "No entry"

dbInconstistency :: String
dbInconstistency = "Database inconsistency"

timesAreWrong :: String
timesAreWrong = "Times are wrong"

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
      eiProject <- try $ projGet name
      case eiProject of
         Left  (ModelException msg) -> return $ Left msg
         Right (Entity pId _)       -> return $ Right (pId, otherCmds)

-- List projects
run :: (MonadIO m, MonadCatch m) => Cmd -> SqlPersistT m ()
run ProjList = projList >>= liftIO . mapM_ putStrLn 

-- Add a project
run (ProjAdd name) = 
   catch (void $ projAdd name) (\(ModelException msg) -> liftIO . putStrLn $ msg)

-- Remove a project
-- TODO ask for confirmation when erasing hdw
run (ProjRm name) = catch (projRm name) (\(ModelException msg) -> liftIO . putStrLn $ msg)

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
   let hdStr = case (mbHdE, mbHdwE, mbP) of
         (Nothing, _, _)                               -> [ noEntry ]
         (Just (Entity _ (HalfDay _ _ Holiday)), _, _) -> [ show Holiday ]
         (Just (Entity _ (HalfDay _ _ Worked)), 
            Just (Entity _ (HalfDayWorked notes arrived left office _ _)), 
            Just (Project name))                       -> 
               [ show office ++ ":  " ++ showTime arrived ++ " - " ++ showTime left
               , "Project: " ++ name
               , "Notes:   " ++ notes 
               ]
                  where showTime (TimeOfDay h m _) = 
                           printf "%02d" h ++ ":" ++ printf "%02d" m
         (_, _, _)                                     -> [ dbInconstistency ]
   -- Print it
   liftIO $ mapM_ putStrLn hdStr
 
-- Set a work entry
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
      
   -- Check output of last command and apply remaining commands
   case eiHdEHdwE of
      Left msg -> liftIO $ putStrLn msg
      Right (hdE, hdwE, otherOpts) -> mapM_ (dispatchEdit hdE hdwE) otherOpts

   -- Display new Half-Day
   run $ DiaryDisplay day time

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

   -- Display new Half-Day
   run $ DiaryDisplay day time

-- Delete an entry
run (DiaryRm day time) = do
   mbHDId <- getBy $ DayAndTimeInDay day time
   case mbHDId of
      -- Nothing to do
      Nothing -> liftIO $ putStrLn noEntry
      -- Found an entry to delete 
      Just (Entity hdId _) -> do
         -- Delete entry from HalfDayWorked if it exists
         deleteWhere [HalfDayWorkedHalfDayId ==. hdId]
         delete hdId

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

-- Return the times in the day in a list
timesOfDay :: HalfDayWorked -> [TimeOfDay]
timesOfDay hdw = [halfDayWorkedArrived hdw, halfDayWorkedLeft hdw]

-- Return true if the list is sorted
isOrdered :: (Ord a) => [a] -> Bool
isOrdered []       = True
isOrdered [_]      = True
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
editSimpleById 
   :: (MonadIO m, MonadCatch m)
   => HalfDayWorkedId 
   -> WorkOption 
   -> SqlPersistT m()
editSimpleById hdwId (SetNotes notes)   = update hdwId [HalfDayWorkedNotes   =. notes]
editSimpleById hdwId (SetOffice office) = update hdwId [HalfDayWorkedOffice  =. office]
editSimpleById hdwId (SetProj name) = do
   eiProject <- try $ projGet name
   case eiProject of 
      Left (ModelException msg) -> liftIO . putStrLn $ msg
      Right (Entity pId _)      -> update hdwId [HalfDayWorkedProjectId =. pId]
editSimpleById _ (SetArrived _) = error "SetArrived command not handled by this function"
editSimpleById _ (SetLeft _)    = error "SetLeft command not handled by this function"

-- Dispatch edit
dispatchEdit 
   :: (MonadIO m, MonadCatch m) 
   => Entity HalfDay
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

