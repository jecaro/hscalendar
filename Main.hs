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
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Time.LocalTime

import HalfDayType
import TimeInDay
import Office

-- Synopsis
-- hsmaster [date] [morning|afternoon] [modifiers]
--   default date: today
--   no TimeInDay: both morning and afernoon
--   no modifiers: print the records
--   modifiers: 
--     -p project   set project by name works without TimeInDay
--     -n note      set note
--     -an note     append note
--     -a 00:00     set arrived time
--     -l 00:00     set left time
--     -d           delete record work without TimeInDay
--     -edit        launch editor
--  date specifier: 
--      today
--      tomorrow
--      yesterday   
--      12/02       Date on current year
--      12/02/19    Date on specific year

-- TODO:
-- - Add consistency check
-- - Add import CSV
-- - Add stats for a year

-- Ideas
-- - put default values for starting ending time in a config file
-- - put file db in a config file as well

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

main :: IO ()
main = do

    today <- localDay . zonedTimeToLocalTime <$> getZonedTime

    runSqlite ":memory:" $ do
        runMigration migrateAll

        -- New project and half day
        projectId <- insert $ Project "A project"

        dayId <- insert $ HalfDay today Morning Worked  

        let eight = TimeOfDay 08 15 00 

        halfDayId <- insert $ HalfDayWorked "Note" eight midday Rennes projectId dayId

        -- Getting content
        halfDay <- get halfDayId

        liftIO $ print halfDay

        -- Update
        update halfDayId [HalfDayWorkedNotes =. "Coucou"]

        -- Delete 
        delete halfDayId

