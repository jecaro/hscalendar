{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances               #-}

module Model 
where

import           RIO
import qualified RIO.Text ()
import qualified RIO.Text as Text (all)
import qualified RIO.Time as Time (Day, TimeOfDay)
import qualified RIO.Char as C (isAlphaNum)

import           Data.Either.Combinators (rightToMaybe)
import           Data.Maybe (Maybe(..))
import           Data.Typeable (typeOf)

import           Generic.Random (genericArbitraryU)
import           Refined 
    ( Predicate
    , Refined
    , refine
    , throwRefineOtherException
    , unrefine
    , validate
    )
import           Test.QuickCheck (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances.Text()

import           Database.Persist.TH 
   ( mkMigrate
   , mkPersist
   , persistLowerCase
   , share
   , sqlSettings
   )

import           HalfDayType (HalfDayType)
import           TimeInDay (TimeInDay)
import           Office (Office)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Project
    -- Fields
    name       Text
    -- Constraint
    UniqueName name 
    deriving Show
    deriving Ord
    deriving Eq
    deriving Generic
HalfDay
    -- Fields
    day             Time.Day        
    timeInDay       TimeInDay   -- morning/afternoon
    type            HalfDayType -- worked/holiday
    -- Constraint
    DayAndTimeInDay day timeInDay   -- One morning, one afternoon everyday
    deriving Show
HalfDayWorked -- Only for WorkedOpenDay
    -- Fields
    notes     Text -- default empty string
    arrived   Time.TimeOfDay 
    left      Time.TimeOfDay --Constraint Left > Arrived
    office    Office
    -- Foreign keys
    projectId ProjectId 
    halfDayId HalfDayId
    -- Constraints
    UniqueHalfDayId halfDayId
    deriving Show
|]

-- Arbitrary instance for QuickCheck
instance Arbitrary Project where
    arbitrary = genericArbitraryU

-- We use the refined library to validate the project name
data AlphaNum

instance Predicate AlphaNum Text where
    validate p value = unless (Text.all C.isAlphaNum value) $ 
            throwRefineOtherException (typeOf p) "Not alpha num text"

type AlphaNumText = Refined AlphaNum Text

-- Smart constructor which cannot fail
mkProjectLit :: AlphaNumText -> Project
mkProjectLit = Project . unrefine 

-- Smart constructor which can fail
mkProject :: Text -> Maybe Project 
mkProject name = Project . (unrefine :: AlphaNumText -> Text) <$> 
    rightToMaybe (refine name)
