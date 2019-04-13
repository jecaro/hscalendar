-- | This is the internal Model. It defines the persistent data types with
-- template haskell.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Internal.Model
where

import           RIO
import qualified RIO.Text ()
import qualified RIO.Text as Text (all, length, pack)
import qualified RIO.Time as Time (Day, TimeOfDay)

import           Data.Either.Combinators (rightToMaybe)
import           Data.Maybe (Maybe(..))
import           Data.Typeable (typeOf)

import           Refined
    ( Predicate
    , Refined
    , refine
    , throwRefineOtherException
    , unrefine
    , validate
    )
import           Test.QuickCheck (Arbitrary, arbitrary, elements, choose, vectorOf, sized)
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

-- | Max length of project name
projNameMaxLength :: Int
projNameMaxLength = 20

-- | Allowed chars for project name
projNameAllowedChars :: String
projNameAllowedChars = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['_']

-- | Predicate to check if the project is valid
projNameValid :: Text -> Bool
projNameValid name = Text.length name <= projNameMaxLength &&
    Text.all (`elem` projNameAllowedChars) name

-- | Arbitrary instance for project. Only project with allowed characters
instance Arbitrary Project where
    arbitrary = sized $ \s -> do
        n <- choose (0, s `min` projNameMaxLength)
        xs <- vectorOf n (elements projNameAllowedChars)
        return (Project (Text.pack xs))

-- | Simple type to refine Text for project names
data ProjName

-- | The actual refined type
type ProjNameText = Refined ProjName Text

-- | Predicate instance to validate what is allowable for a project name
instance Predicate ProjName Text where
    validate p name = unless (projNameValid name) $
            throwRefineOtherException (typeOf p) "Not alpha num text"

-- | Smart constructor which cannot fail
mkProjectLit :: ProjNameText -> Project
mkProjectLit = Project . unrefine

-- | Smart constructor which can fail
mkProject :: Text -> Maybe Project
mkProject name = Project . (unrefine :: ProjNameText -> Text) <$>
    rightToMaybe (refine name)
