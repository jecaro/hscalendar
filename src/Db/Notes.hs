-- | Functions related to Notes
module Db.Notes
    ( Notes
    , mkNotes
    , mkNotesLit
    , unNotes
    )
where

import           RIO

import qualified RIO.Text as Text (Text, all, length, pack)

import           Data.Char (isPrint)
import           Data.Either.Combinators (rightToMaybe)
import           Data.Typeable (typeOf)
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
    , choose
    , sized
    , suchThat
    , vectorOf
    )
import           Test.QuickCheck.Instances.Text()

-- | The type for storing notes
newtype Notes = MkNotes { unNotes :: Text.Text }
    deriving (Eq, Show)

-- | Simple type to refine Text for Notes
data NotesData

-- | The actual refined type
type NotesText = Refined NotesData Text

-- | Predicate instance to validate what is allowable for notes
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

