{-# LANGUAGE TemplateHaskell #-}
module NewModel
where

import           RIO

import qualified RIO.Text as Text (Text, all, length, pack)
import qualified RIO.Time as Time (Day, TimeOfDay)

import           Data.Either.Combinators (rightToMaybe)
import           Data.Typeable (typeOf)
import           Lens.Micro.Platform (makeFields)
import           Refined
    ( Predicate
    , Refined
    , refine
    , throwRefineOtherException
    , unrefine
    , validate
    )
import           Test.QuickCheck 
    ( Arbitrary
    , arbitrary
    , elements
    , choose
    , vectorOf
    , sized
    )
import           Test.QuickCheck.Instances.Text()

import           Office (Office)
import           TimeInDay (TimeInDay)

data IdleDayType = PayedLeave
                 | FamilyEvent
                 | RTTE
                 | RTTS
                 | UnpayedLeave
                 | PublicHoliday 
                 | PartTime

data Idle = MkIdle
    { _idleDay       :: Time.Day
    , _idleTimeInDay :: TimeInDay
    , _idleDayType   :: IdleDayType
    }
makeFields ''Idle

newtype Notes = MkNotes Text.Text
newtype Project = MkProject { unProject :: Text.Text }
    deriving (Eq, Show, Ord)

data Worked = MkWorked
    { _workedDay       :: Time.Day
    , _workedTimeInDay :: TimeInDay
    , _workedArrived   :: Time.TimeOfDay
    , _workedLeft      :: Time.TimeOfDay
    , _workedOffice    :: Office
    , _workedNotes     :: Notes
    , _workedProject   :: Project
    }
makeFields ''Worked

data HalfDay = HalfDayWorked Worked | HalfDayIdle Idle


-- | Max length of project name
projNameMaxLength :: Int
projNameMaxLength = 20

-- | Allowed chars for project name
projNameAllowedChars :: String
projNameAllowedChars = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['_']

-- | Predicate to check if the project is valid
projNameValid :: Text -> Bool
projNameValid name =  Text.length name > 0 
                   && Text.length name <= projNameMaxLength 
                   && Text.all (`elem` projNameAllowedChars) name

-- | Arbitrary instance for project. Only project with allowed characters
instance Arbitrary Project where
    arbitrary = sized $ \s -> do
        n <- choose (1, s `min` projNameMaxLength)
        xs <- vectorOf n (elements projNameAllowedChars)
        return (MkProject (Text.pack xs))

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
mkProjectLit = MkProject . unrefine

-- | Smart constructor which can fail
mkProject :: Text -> Maybe Project
mkProject name = MkProject . (unrefine :: ProjNameText -> Text) <$>
    rightToMaybe (refine name)


