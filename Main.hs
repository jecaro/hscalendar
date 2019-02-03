{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class  (liftIO)
import           Data.List
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
-- - Command line into separate module

-- Ideas
-- - put default values for starting ending time in a config file
-- - put db file in a config file as well

data Cmd = ProjList                             |
           ProjRm String                        |
           ProjAdd String                       |
           DiaryDisplay Day TimeInDay           |
           DiaryEdit Day TimeInDay [EditOption] |
           DiaryDelete Day TimeInDay 
  deriving (Eq, Show)

data EditOption = SetProj String       |
                  SetNote String       |
                  SetArrived TimeOfDay |
                  SetLeft TimeOfDay    |
                  SetOffice Office
   deriving (Eq, Show)

projRm :: Parser Cmd
projRm = ProjRm <$> argument str (metavar "PROJECT...")

projAdd :: Parser Cmd
projAdd = ProjAdd <$> argument str (metavar "PROJECT...")

projCmd :: Parser Cmd
projCmd = subparser
   (  command "list" (info (pure ProjList) (progDesc "List current projects"))
   <> command "add"  (info projAdd         (progDesc "Add project"))
   <> command "rm"   (info projRm          (progDesc "Remove project"))
   )

diaryDisplay :: Parser Cmd
diaryDisplay = DiaryDisplay <$> 
   argument auto (metavar "DAY") <*> 
   argument auto (metavar "TIMEINDAY")
   
diaryDelete :: Parser Cmd
diaryDelete = DiaryDelete <$> 
   argument auto (metavar "DAY") <*> 
   argument auto (metavar "TIMEINDAY")

diaryEdit :: Parser Cmd
diaryEdit = DiaryEdit <$> 
   argument auto (metavar "DAY") <*> 
   argument auto (metavar "TIMEINDAY") <*>
   some editOption

diaryCmd :: Parser Cmd
diaryCmd = hsubparser
   (  command "display" (info diaryDisplay (progDesc "Display entry"))
   <> command "edit"    (info diaryEdit    (progDesc "Edit entry"))
   <> command "delete"  (info diaryDelete  (progDesc "Delete entry"))
   )

editOption :: Parser EditOption
editOption = editOptionSetProj    <|> 
             editOptionSetNote    <|>
             editOptionSetArrived <|>
             editOptionSetLeft    <|>
             editOptionSetOffice

editOptionSetProj :: Parser EditOption
editOptionSetProj = SetProj <$> strOption
   (  long "project"
   <> short 'p'
   <> metavar "PROJECT"
   <> help "Set the project" )

editOptionSetNote :: Parser EditOption
editOptionSetNote = SetNote <$> strOption
   (  long "note"
   <> short 'n'
   <> metavar "NOTE"
   <> help "Set the note" )

editOptionSetArrived :: Parser EditOption
editOptionSetArrived = SetArrived <$> option auto
   (  long "arrived"
   <> short 'a'
   <> metavar "TIME"
   <> help "Time of arrival" )

editOptionSetLeft :: Parser EditOption
editOptionSetLeft = SetLeft <$> option auto
   (  long "left"
   <> short 'l'
   <> metavar "TIME"
   <> help "Time of leaving" )

editOptionSetOffice :: Parser EditOption
editOptionSetOffice = SetOffice <$> option auto
   (  long "office"
   <> short 'o'
   <> metavar "OFFICE"
   <> help "Office" )

cmd :: Parser Cmd
cmd = hsubparser
   (  command "project" (info projCmd  (progDesc "Project"))
   <> command "diary"   (info diaryCmd (progDesc "Diary"))
   )

run :: Cmd -> IO()
run ProjList = putStrLn "List project"
run (ProjRm name) = putStrLn $ "Remove project " ++ name
run (ProjAdd name) = putStrLn $ "Adding project " ++ name
run (DiaryDisplay day time) = putStrLn $ "Display diary " ++ show day ++ " " ++ show time
run (DiaryEdit day time opts) = putStrLn $ "Edit diary " ++ 
   show day ++ " " ++ show time ++ " " ++ show opts
   
opts :: ParserInfo Cmd
opts = info (cmd <**> helper) idm

main :: IO ()
main = execParser opts >>= run

-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- Project
--     name String
--     UniqueName name 
--     deriving Show
-- HalfDay
--     day         Day        
--     timeInDay   TimeInDay   -- morning/afternoon
--     halfDayType HalfDayType -- worked/holiday
--     FullDay day timeInDay   -- One morning, one afternoon everyday
--     deriving Show
-- HalfDayWorked -- Only for WorkedOpenDay
-- 	notes     String
-- 	arrived   TimeOfDay 
-- 	left      TimeOfDay --Constraint Left > Arrived
--     office    Office
--     projectId ProjectId 
--     halfDayId HalfDayId
--     deriving Show
-- |]

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

