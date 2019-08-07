{-# LANGUAGE TemplateHaskell #-}
module NewModel
where

import           RIO

import qualified RIO.Text as Text (Text, all, length, pack)
import qualified RIO.Time as Time (Day, TimeOfDay)

import           Data.Char (isPrint)
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
    , sized
    , suchThat
    , vectorOf
    )
import           Test.QuickCheck.Instances.Text()

import           IdleDayType
import           Office (Office)
import           TimeInDay (TimeInDay)

data Idle = MkIdle
    { _idleDay       :: Time.Day
    , _idleTimeInDay :: TimeInDay
    , _idleDayType   :: IdleDayType
    }
    deriving (Show, Eq)
makeFields ''Idle

newtype Notes = MkNotes { unNotes :: Text.Text }
    deriving (Eq, Show)

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
    deriving (Show, Eq)
makeFields ''Worked

data HalfDay = MkHalfDayWorked Worked | MkHalfDayIdle Idle
    deriving (Show, Eq)

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
mkProject name = mkProjectLit <$> rightToMaybe (refine name)

-- | Simple type to refine Text for notes
data NotesData

-- | The actual refined type
type NotesText = Refined NotesData Text

-- | Predicate instance to validate what is allowable for a project name
instance Predicate NotesData Text where
    validate p name = unless (notesValid name) $
            throwRefineOtherException (typeOf p) "Not alpha num text"

-- | Arbitrary instance for QuickCheck
instance Arbitrary Notes where
    arbitrary = sized $ \s -> do
        n <- choose (0, s `min` notesMaxLength)
        xs <- vectorOf n (arbitrary `suchThat` printableOrEOLOrTab)
        return $ MkNotes $ Text.pack xs

-- | Maximum length of a note
notesMaxLength :: Int
notesMaxLength = 500

-- | Allowed characters for a note: a printable char, eol or tabulation
printableOrEOLOrTab :: Char -> Bool
printableOrEOLOrTab x = isPrint x || elem x ['\n', '\t']

-- | Check the validity of a note
notesValid :: Text -> Bool
notesValid name = Text.length name <= notesMaxLength &&
    Text.all printableOrEOLOrTab name 

-- | Smart constructor which cannot fail
mkNotesLit :: NotesText -> Notes
mkNotesLit = MkNotes . unrefine

-- | Smart constructor which can fail
mkNotes :: Text -> Maybe Notes
mkNotes name = mkNotesLit <$> rightToMaybe (refine name)

