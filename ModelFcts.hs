{-# LANGUAGE FlexibleInstances           #-}

module ModelFcts 
  ( getProject
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
   , SqlPersistT
   , getBy
   )

import Model

newtype ModelException = ModelException String deriving (Show)

instance Exception ModelException

projectNotFound :: String -> String
projectNotFound name = "The project " ++ name ++ " is not in the database"

getProject 
  :: (MonadIO m, MonadThrow m) 
  => String 
  -> SqlPersistT m (Entity Project)
getProject name = do
  mbProj <- getBy $ UniqueName name 
  case mbProj of
    Nothing -> throwM $ ModelException $ projectNotFound name 
    Just e  -> return e

