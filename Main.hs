{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Monoid
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Options.Applicative

import           HalfDayType
import           TimeInDay
import           Office
import           CommandLine

-- Synopsis
-- hsmaster diary date Morning|Afternoon [commands]
--   commands: 
--     -p project   set the project name 
--     -n note      set note
--     -a 00:00     set arrived time
--     -l 00:00     set left time
--     -d           delete record 
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

-- Ideas
-- - put default values for starting ending time in a config file
-- - put db file in a config file as well

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
   name String
   UniqueName name 
   deriving Show
HalfDay
   day         Day        
   timeInDay   TimeInDay   -- morning/afternoon
   halfDayType HalfDayType -- worked/holiday
   FullDay day timeInDay   -- One morning, one afternoon everyday
   deriving Show
HalfDayWorked -- Only for WorkedOpenDay
	notes     String
	arrived   TimeOfDay 
	left      TimeOfDay --Constraint Left > Arrived
   office    Office
   projectId ProjectId 
   halfDayId HalfDayId
   deriving Show
|]

run :: MonadIO m => Cmd -> SqlPersistT m ()
run ProjList = do 
   projects <- selectList [] [Asc ProjectName]
   let names = map (projectName . entityVal) projects
   liftIO $ print names

-- TODO add error handling checkUnique
run (ProjAdd name) = void . insert $ Project name 

-- Remove project
-- TODO: check in the HalfDay Worked Table before
run (ProjRm name) = do
   maybeProject <- getBy $ UniqueName name
   case maybeProject of
      Nothing -> liftIO $ putStrLn "Project not found"
      Just (Entity projectId project) -> delete projectId

run (DiaryDisplay day time) = liftIO . putStrLn $ "Display diary " ++ show day ++ " " ++ show time
run (DiaryEdit day time opts) = liftIO . putStrLn $ "Edit diary " ++ 
   show day ++ " " ++ show time ++ " " ++ show opts
   
main :: IO ()
-- runNoLoggingT or runStdoutLoggingT
main = runNoLoggingT . withSqlitePool "file.db" 3 . runSqlPool $ do 
   runMigration migrateAll
   cmd <- liftIO $ execParser opts
   run cmd

-- main :: IO ()
-- main = do

--     today <- localDay . zonedTimeToLocalTime <$> getZonedTime

--     runSqlite ":memory:" $ do
--         runMigration migrateAll

--         -- New project and half day
--         projectId <- insert $ Project "A project"

--         dayId <- insert $ HalfDay today Morning Worked  

--         let eight = TimeOfDay 08 15 00 

--         halfDayId <- insert $ HalfDayWorked "Note" eight midday Rennes projectId dayId

--         -- Getting content
--         halfDay <- get halfDayId

--         liftIO $ print halfDay

--         -- Update
--         update halfDayId [HalfDayWorkedNotes =. "Coucou"]

--         -- Delete 
--         delete halfDayId

