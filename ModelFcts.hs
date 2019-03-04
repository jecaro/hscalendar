{-# LANGUAGE FlexibleInstances           #-}

module ModelFcts 
  ( addProject
  , getProject
  , projectExists
  , projectList
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
   , getBy
   , insert
   , selectList
   )

import           Data.Maybe (isJust)

import Model

newtype ModelException = ModelException String deriving (Show)

instance Exception ModelException

errProjNotFound :: String -> String
errProjNotFound name = "The project " ++ name ++ " is not in the database"

errProjExists :: String -> String
errProjExists name = "The project " ++ name ++ " exists in the database"

getProject 
  :: (MonadIO m, MonadThrow m) 
  => String 
  -> SqlPersistT m (Entity Project)
getProject name = do
  mbProj <- getBy $ UniqueName name 
  case mbProj of
    Nothing -> throwM $ ModelException $ errProjNotFound name 
    Just e  -> return e

projectExists :: MonadIO m => String -> SqlPersistT m Bool
projectExists name = do
  mbProj <- getBy $ UniqueName name 
  return $ isJust mbProj

addProject 
  :: (MonadIO m, MonadThrow m)
  => String
  -> SqlPersistT m ProjectId
addProject name = do
  pExists <- projectExists name
  if pExists
    then throwM $ ModelException $ errProjExists name
    else insert $ Project name

projectList :: MonadIO m => SqlPersistT m [String]
projectList = do
  projects <- selectList [] [Asc ProjectName]
  return $ map (projectName . entityVal) projects
