{-# LANGUAGE FlexibleInstances           #-}

module ModelFcts 
  ( ModelException(..)
  , projAdd
  , projExists
  , projGet
  , projList
  , projRm
  , hdGet
  , hdwGet
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

errHdwNotFound :: HalfDayId -> String
errHdwNotFound hdId = "No half-day worked entry for " ++ show hdId

errDbInconsistency :: String
errDbInconsistency = "Warning db inconsistency"

projGet :: (MonadIO m, MonadThrow m) => String -> SqlPersistT m (Entity Project)
projGet name = do
  mbProj <- getBy $ UniqueName name 
  case mbProj of
    Nothing -> throwM $ ModelException $ errProjNotFound name 
    Just e  -> return e

projExists :: MonadIO m => String -> SqlPersistT m Bool
projExists name = isJust <$> getBy (UniqueName name)

projAdd :: (MonadIO m, MonadThrow m) => String -> SqlPersistT m ProjectId
projAdd name = do
  pExists <- projExists name
  if pExists
    then throwM $ ModelException $ errProjExists name
    else insert $ Project name

projList :: MonadIO m => SqlPersistT m [String]
projList = map (projectName . entityVal) <$> selectList [] [Asc ProjectName] 

projRm :: (MonadIO m, MonadThrow m) => String -> SqlPersistT m ()
projRm name = do
  -- The following can throw exception same exception apply to this function
  -- so we dont catch it here
  (Entity pId _) <- projGet name 
  deleteWhere [HalfDayWorkedProjectId ==. pId]
  delete pId

hdGet :: (MonadIO m, MonadThrow m) => Day -> TimeInDay -> SqlPersistT m (Entity HalfDay)
hdGet day tid = do
  mbHd <- getBy $ DayAndTimeInDay day tid
  case mbHd of
    Nothing  -> throwM $ ModelException $ errHdNotFound day tid
    Just e   -> return e

-- todo unwrap entity
hdwGet :: (MonadIO m, MonadThrow m) => (Entity HalfDay) -> SqlPersistT m (Entity HalfDayWorked)
hdwGet (Entity hdId _) = do
  mbHdw <- getBy $ UniqueHalfDayId hdId
  case mbHdw of
    Nothing  -> throwM $ ModelException $ errHdwNotFound hdId
    Just e   -> return e

hdAndHdwGet 
  :: (MonadIO m, MonadCatch m) 
  => Day
  -> TimeInDay 
  -> SqlPersistT m (Entity HalfDay, Maybe (Entity HalfDayWorked))
hdAndHdwGet day tid = do
  hd@(Entity _ eHd) <- hdGet day tid 
  eiHdw <- try $ hdwGet hd
  let mbHdw = case eiHdw of 
                Left (ModelException _) -> Nothing
                Right e                 -> Just e
  -- Check for consistency
  case (eHd, mbHdw) of
    (HalfDay _ _ Worked, Nothing) -> throwM $ ModelException errDbInconsistency
    (HalfDay _ _ Holiday, Just _) -> throwM $ ModelException errDbInconsistency
    (_, _) -> return (hd, mbHdw)
