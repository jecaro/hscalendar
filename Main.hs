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

-- Ideas
-- - put default values for starting ending day in a config file

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age  Int Maybe
    deriving Show
BlogPost
    title    String
    authorId PersonId
    deriving Show
HalfDay
    day         Day        
    timeInDay   TimeInDay   -- morning/afternoon
    halfDayType HalfDayType -- worked/holiday
    FullDay day timeInDay   -- One morning, one afternoon everyday
    deriving Show
Project
    name String
    UniqueName name
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

        johnId <- insert $ Person "John Doe" $ Just 35
        janeId <- insert $ Person "Jane Doe" Nothing

        insert $ BlogPost "My fr1st p0st" johnId
        insert $ BlogPost "One more for good measure" johnId

        oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
        liftIO $ print (oneJohnPost :: [Entity BlogPost])

        john <- get johnId
        liftIO $ print (john :: Maybe Person)

        delete janeId
        deleteWhere [BlogPostAuthorId ==. johnId]

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

