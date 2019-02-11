{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
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

-- Ideas
-- - put default values for starting ending time in a config file
-- - put db file in a config file as well

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
      Just (Entity _ _) -> liftIO . putStrLn $ "The project " ++ name ++
                                               " is already in the database"

-- Remove a project
-- TODO: check in the HalfDay Worked Table before
run (ProjRm name) = do
   mbProject <- getBy $ UniqueName name
   case mbProject of
      Nothing -> liftIO $ putStrLn $ "The project " ++ name ++
                                     " is not in the database"
      Just (Entity projectId _) -> delete projectId

-- Display an entry
run (DiaryDisplay day time) = do
   -- Display input
   liftIO . putStrLn $ show day ++ " " ++ show time
   mbHalfDayId <- getBy $ DayAndTimeInDay day time
   liftIO $ putStrLn $ case mbHalfDayId of
      Nothing -> "No entry"
      Just (Entity id halfDay) -> show $ halfDayType halfDay
         
-- Set a work entry
run (DiaryWork day time opts) = do
   mbHalfDayId <- getBy $ DayAndTimeInDay day time
   case mbHalfDayId of
      -- Edit an existing entry
      Just (Entity id _) -> mapM_ (runEdit id) opts
      -- Create a new entry - check if we got a project
      Nothing -> return ()

-- Set a holiday entry
run (DiaryHoliday day time) = do
   mbHalfDayId <- getBy $ DayAndTimeInDay day time
   case mbHalfDayId of
      -- Edit an existing entry
      Just (Entity id _) -> update id [HalfDayType =. Holiday]
      -- Create a new entry 
      Nothing -> void $ insert $ HalfDay day time Holiday

runEdit :: MonadIO m => HalfDayId -> WorkOption -> SqlPersistT m()
runEdit id (SetProj name)     = undefined
runEdit id (SetNote note)     = undefined
runEdit id (SetArrived time)  = undefined
runEdit id (SetLeft time)     = undefined
runEdit id (SetOffice office) = undefined

main :: IO ()
-- runNoLoggingT or runStdoutLoggingT
main = runNoLoggingT . withSqlitePool "file.db" 3 . runSqlPool $ do
   runMigration migrateAll
   liftIO (execParser opts) >>= run

