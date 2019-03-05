{-# LANGUAGE FlexibleInstances           #-}

module ModelFcts 
  ( ModelException(..)
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
   , selectList
   , (==.)
   )

import           Data.Maybe (isJust, isNothing)
import           Data.Time.Calendar (Day)

import           Model
import           TimeInDay(TimeInDay)
import           HalfDayType(HalfDayType(..))

newtype ModelException = ModelException String deriving (Show)

instance Exception ModelException

errProjNotFound :: String -> String
errProjNotFound name = "The project " ++ name ++ " is not in the database"

errProjExists :: String -> String
errProjExists name = "The project " ++ name ++ " exists in the database"

errHdNotFound :: Day -> TimeInDay -> String
errHdNotFound day tid = "Nothing for " ++ show day ++ " " ++ show tid

errHdwIdNotFound :: HalfDayId -> String
errHdwIdNotFound hdwId = "No half-day worked entry for " ++ show hdwId

errProjIdNotFound :: ProjectId -> String
errProjIdNotFound pId = "No project entry for " ++ show pId

errDbInconsistency :: String
errDbInconsistency = "Warning db inconsistency"

-- TODO Take a Project as Parameter return only Id 
--      Should be internal
projGet :: (MonadIO m, MonadThrow m) => String -> SqlPersistT m (Entity Project)
projGet name = do
  mbProj <- getBy $ UniqueName name 
  case mbProj of
    Nothing -> throwM $ ModelException $ errProjNotFound name 
    Just e  -> return e

-- TODO Take a Project as parameter
projExists :: MonadIO m => String -> SqlPersistT m Bool
projExists name = isJust <$> getBy (UniqueName name)

-- TODO Take a Project as parameter
projAdd :: (MonadIO m, MonadThrow m) => String -> SqlPersistT m ProjectId
projAdd name = do
  pExists <- projExists name
  if pExists
    then throwM $ ModelException $ errProjExists name
    else insert $ Project name

-- TODO do not return [String] but [Project]
projList :: MonadIO m => SqlPersistT m [String]
projList = map (projectName . entityVal) <$> selectList [] [Asc ProjectName] 

-- TODO Take a Project as parameter
projRm :: (MonadIO m, MonadThrow m) => String -> SqlPersistT m ()
projRm name = do
  -- The following can throw exception same exception apply to this function
  -- so we dont catch it here
  (Entity pId _) <- projGet name 
  deleteWhere [HalfDayWorkedProjectId ==. pId]
  delete pId

-- Keep internal
hdGet :: (MonadIO m, MonadThrow m) => Day -> TimeInDay -> SqlPersistT m (Entity HalfDay)
hdGet day tid = do
  mbHd <- getBy $ DayAndTimeInDay day tid
  case mbHd of
    Nothing  -> throwM $ ModelException $ errHdNotFound day tid
    Just e   -> return e

-- Keep internal
hdwProjGet :: (MonadIO m, MonadThrow m) => (Entity HalfDay) -> SqlPersistT m (Entity HalfDayWorked, String)
hdwProjGet (Entity hdId _) = do
  mbHdw <- getBy $ UniqueHalfDayId hdId
  case mbHdw of
    Nothing -> throwM $ ModelException $ errHdwIdNotFound hdId
    Just e@(Entity _ (HalfDayWorked _ _ _ _ pId _)) -> do
      mbProj <- get pId
      let name = case mbProj of 
                  Nothing -> throwM $ ModelException $ errProjIdNotFound pId
                  Just (Project name) -> name
      return (e, name)

-- Main function
hdHdwProjGet 
  :: (MonadIO m, MonadCatch m) 
  => Day
  -> TimeInDay 
  -> SqlPersistT m (Entity HalfDay, Maybe (Entity HalfDayWorked, String))
hdHdwProjGet day tid = do
  eHd@(Entity _ hd) <- hdGet day tid 
  eiHdwProj <- try $ hdwProjGet eHd
  let mbHdw = case eiHdwProj of 
                Left (ModelException _) -> Nothing
                Right e                 -> Just e
  -- Check for consistency
  case (hd, mbHdw) of
    (HalfDay _ _ Worked, Nothing) -> throwM $ ModelException errDbInconsistency
    (HalfDay _ _ Holiday, Just _) -> throwM $ ModelException errDbInconsistency
    (_, _) -> return (eHd, mbHdw)
