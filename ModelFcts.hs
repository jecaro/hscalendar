{-# LANGUAGE FlexibleInstances           #-}

module ModelFcts 
  ( ModelException(..)
  , hdHdwProjGet
  , hdRm
  , hdSetHoliday
  , hdwSetArrived
  , hdwSetArrivedAndLeft
  , hdwSetLeft
  , hdwSetNotes
  , hdwSetOffice
  , hdwSetProject
  , projAdd
  , projExists
  , projGet
  , projList
  , projRm
  ) where

import           Control.Exception.Safe 
  ( Exception
  , MonadCatch
  , MonadThrow
  , throwM
  , try
  )
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO)
import           Database.Persist.Sqlite 
   ( Entity(..)
   , SelectOpt(Asc)
   , SqlPersistT
   , delete
   , deleteWhere
   , get
   , getBy
   , insert
   , replace
   , selectList
   , update
   , (=.)
   , (==.)
   )

import           Data.Maybe (isJust)
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (TimeOfDay)

import           HalfDayType(HalfDayType(..))
import           Model
import           Office (Office(..))
import           TimeInDay(TimeInDay(..), other)

newtype ModelException = ModelException String deriving (Show)

instance Exception ModelException

errProjNotFound :: Project -> String
errProjNotFound (Project name) = "The project " ++ name ++ " is not in the database"

errProjExists :: Project -> String
errProjExists (Project name) = "The project " ++ name ++ " exists in the database"

errHdNotFound :: Day -> TimeInDay -> String
errHdNotFound day tid = "Nothing for " ++ show day ++ " " ++ show tid

errHdwIdNotFound :: HalfDayId -> String
errHdwIdNotFound hdwId = "No half-day worked entry for " ++ show hdwId

errProjIdNotFound :: ProjectId -> String
errProjIdNotFound pId = "No project entry for " ++ show pId

errDbInconsistency :: String
errDbInconsistency = "Warning db inconsistency"

errTimesAreWrong :: String
errTimesAreWrong = "Times are wrong"

-- TODO Should be internal only
projGet :: (MonadIO m, MonadThrow m) => Project -> SqlPersistT m (Key Project)
projGet project@(Project name) = do
  mbProj <- getBy $ UniqueName name 
  case mbProj of
    Nothing             -> throwM $ ModelException $ errProjNotFound project
    Just (Entity pId _) -> return pId

projExists :: MonadIO m => Project -> SqlPersistT m Bool
projExists (Project name) = isJust <$> getBy (UniqueName name)

projAdd :: (MonadIO m, MonadThrow m) => Project -> SqlPersistT m ()
projAdd project = do
  pExists <- projExists project
  if pExists
    then throwM $ ModelException $ errProjExists project
    else void $ insert project

projList :: MonadIO m => SqlPersistT m [Project]
projList = map entityVal <$> selectList [] [Asc ProjectName] 

projRm :: (MonadIO m, MonadThrow m) => Project -> SqlPersistT m ()
projRm project = do
  -- The following can throw exception same exception apply to this function
  -- so we dont catch it here
  pId <- projGet project 
  deleteWhere [HalfDayWorkedProjectId ==. pId]
  delete pId

-- Keep internal
-- Get a half day if it exists or raise an exception
hdGetInt :: (MonadIO m, MonadThrow m) => Day -> TimeInDay -> SqlPersistT m (Entity HalfDay)
hdGetInt day tid = do
  mbHd <- getBy $ DayAndTimeInDay day tid
  case mbHd of
    Nothing -> throwM $ ModelException $ errHdNotFound day tid
    Just e  -> return e

-- Keep internal 
hdwProjGetInt 
  :: (MonadIO m, MonadThrow m) 
  => HalfDayId 
  -> SqlPersistT m (Entity HalfDayWorked, Entity Project)
hdwProjGetInt hdId = do
  mbHdw <- getBy $ UniqueHalfDayId hdId
  case mbHdw of
    Nothing -> throwM $ ModelException $ errHdwIdNotFound hdId
    Just e@(Entity _ (HalfDayWorked _ _ _ _ pId _)) -> do
      mbProj <- get pId
      project <- case mbProj of 
        Nothing -> throwM $ ModelException $ errProjIdNotFound pId
        Just p  -> return (Entity pId p)
      return (e, project)

hdHdwProjGetInt 
  :: (MonadIO m, MonadThrow m) 
  => Day 
  -> TimeInDay 
  -> SqlPersistT m (Entity HalfDay, Entity HalfDayWorked, Entity Project)
hdHdwProjGetInt day tid = do
  eHd@(Entity hdId _) <- hdGetInt day tid
  (eHdw, eProj) <- hdwProjGetInt hdId
  return (eHd, eHdw, eProj)


-- Main request function
hdHdwProjGet
  :: (MonadIO m, MonadCatch m) 
  => Day
  -> TimeInDay 
  -> SqlPersistT m (HalfDay, Maybe (HalfDayWorked, Project))
hdHdwProjGet day tid = do
  (Entity hdId hd) <- hdGetInt day tid 
  eiHdwProj <- try $ hdwProjGetInt hdId
  let mbHdw = case eiHdwProj of 
        Left (ModelException _)                -> Nothing
        Right (Entity _ hdw, Entity _ project) -> Just (hdw, project)
  -- Check for consistency
  case (hd, mbHdw) of
    (HalfDay _ _ Worked, Nothing) -> throwM $ ModelException errDbInconsistency
    (HalfDay _ _ Holiday, Just _) -> throwM $ ModelException errDbInconsistency
    (_, _)                        -> return (hd, mbHdw)

hdwSetOffice :: (MonadIO m, MonadCatch m) => Day -> TimeInDay -> Office -> SqlPersistT m ()
hdwSetOffice day tid office = do
  (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
  update hdwId [HalfDayWorkedOffice =. office]

hdwSetNotes :: (MonadIO m, MonadCatch m) => Day -> TimeInDay -> String -> SqlPersistT m ()
hdwSetNotes day tid notes = do
  (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
  update hdwId [HalfDayWorkedNotes =. notes]

hdwSetProject :: (MonadIO m, MonadCatch m) => Day -> TimeInDay -> Project -> SqlPersistT m () 
hdwSetProject day tid project = do
  (_, Entity hdwId _, _) <- hdHdwProjGetInt day tid
  pId <- projGet project
  update hdwId [HalfDayWorkedProjectId =. pId]

-- Check that the constraints on the times are valid between the two days
timesAreOrderedInDay :: TimeInDay -> HalfDayWorked -> Maybe HalfDayWorked -> Maybe String
timesAreOrderedInDay Morning hdw mbOtherHdw = timeAreOrdered $ timesOfDay hdw ++ otherTimes
   where otherTimes = concatMap timesOfDay mbOtherHdw
-- We switch the arguments and call the same function
timesAreOrderedInDay Afternoon hdw (Just otherHdw) =
   timesAreOrderedInDay Morning otherHdw (Just hdw)
-- Afternoon only, just need to check for half day
timesAreOrderedInDay Afternoon hdw Nothing = timeAreOrdered $ timesOfDay hdw

-- Set arrived/left time
editTime :: (MonadIO m, MonadCatch m)
  => Entity HalfDay
  -> Entity HalfDayWorked
  -> (HalfDayWorked -> HalfDayWorked)
  -> SqlPersistT m ()
editTime (Entity _ (HalfDay day tid _)) (Entity hdwId hdw) setTime = do
  eiHdHdwProj <- try $ hdHdwProjGetInt day $ other tid
  let mbOtherHdw = case eiHdHdwProj of
        Left (ModelException _)     -> Nothing
        Right (_, Entity _ oHdw, _) -> Just oHdw
-- Apply time to arrived/left hdw
  let hdw' = setTime hdw
  -- Check if it works
  case timesAreOrderedInDay tid hdw' mbOtherHdw of
     Just msg -> throwM $ ModelException msg
     Nothing  -> replace hdwId hdw'

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
   else Just errTimesAreWrong

hdwSetArrived 
  :: (MonadIO m, MonadCatch m) 
  => Day 
  -> TimeInDay 
  -> TimeOfDay 
  -> SqlPersistT m () 
hdwSetArrived day tid tod = do
    (eHd, eHdw, _) <- hdHdwProjGetInt day tid
    editTime eHd eHdw $ setArrived tod
  where setArrived tod' hdw = hdw { halfDayWorkedArrived = tod' }

hdwSetLeft 
  :: (MonadIO m, MonadCatch m) 
  => Day 
  -> TimeInDay 
  -> TimeOfDay 
  -> SqlPersistT m () 
hdwSetLeft day tid tod = do
    (eHd, eHdw, _) <- hdHdwProjGetInt day tid
    editTime eHd eHdw $ setLeft tod
  where setLeft tod' hdw = hdw { halfDayWorkedLeft = tod' }

hdwSetArrivedAndLeft 
  :: (MonadIO m, MonadCatch m) 
  => Day 
  -> TimeInDay 
  -> TimeOfDay 
  -> TimeOfDay 
  -> SqlPersistT m () 
hdwSetArrivedAndLeft day tid arrived left = do
    (eHd, eHdw, _) <- hdHdwProjGetInt day tid
    editTime eHd eHdw $ setArrivedAndLeft arrived left
  where setArrivedAndLeft arrived' left' hdw = 
            hdw { halfDayWorkedArrived = arrived'
                , halfDayWorkedLeft    = left' }

hdSetHoliday :: (MonadIO m, MonadCatch m) => Day -> TimeInDay -> SqlPersistT m () 
hdSetHoliday day tid = do
  eiHd <- try $ hdGetInt day tid
  case eiHd of
    -- Create a new entry
    Left (ModelException _) -> void $ insert $ HalfDay day tid Holiday
    -- Edit existing entry
    Right (Entity hdId _)   -> do          
      -- Delete entry from HalfDayWorked if it exists
      deleteWhere [HalfDayWorkedHalfDayId ==. hdId]
      -- Update entry
      update hdId [HalfDayType =. Holiday]

hdRm :: (MonadIO m, MonadCatch m) => Day -> TimeInDay -> SqlPersistT m () 
hdRm day tid = do
  (Entity hdId _) <- hdGetInt day tid
  -- Delete entry from HalfDayWorked if it exists
  deleteWhere [HalfDayWorkedHalfDayId ==. hdId]
  delete hdId
 
