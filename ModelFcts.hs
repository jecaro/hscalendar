{-# LANGUAGE FlexibleInstances           #-}

module ModelFcts 
  ( projAdd
  , projExists
  , projGet
  , projList
  , projRm
  , ModelException(..) 
  ) where

import           Control.Exception.Safe 
  ( Exception
  , MonadThrow
  , throwM
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

import           Data.Maybe (isJust)

import Model

newtype ModelException = ModelException String deriving (Show)

instance Exception ModelException

errProjNotFound :: String -> String
errProjNotFound name = "The project " ++ name ++ " is not in the database"

errProjExists :: String -> String
errProjExists name = "The project " ++ name ++ " exists in the database"

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
