-- | Functions related to 'Notes'
module Db.Notes
    ( Notes
    , mkNotes
    , mkNotesLit
    , unNotes
    , parser
    )
where

import           RIO

import qualified RIO.Text as Text (Text, all, length, pack)

import           Data.Char (isPrint)
import           Data.Aeson (FromJSON(..), ToJSON, withObject, (.:))
import           Data.Attoparsec.Text
    ( Parser
    , many1
    , satisfy
    )
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
newtype Notes = MkNotes
    { unNotes :: Text.Text -- ^ Unwrap the content
    }
    deriving (Generic, Eq, Show)

-- | Simple type to refine 'Text' for 'Notes'
data NotesData

-- | The actual refined type
type NotesText = Refined NotesData Text

-- | Predicate instance to validate what is allowable for notes
instance Predicate NotesData Text where
    validate p notes = unless (notesValid notes) $
        throwRefineOtherException (typeOf p) "Not a valid note"

-- | Arbitrary instance for QuickCheck
instance Arbitrary Notes where
    arbitrary = sized $ \s -> do
        n <- choose (0, s `min` notesMaxLength)
        xs <- vectorOf n (arbitrary `suchThat` printableOrEOLOrTab)
        return $ MkNotes $ Text.pack xs

instance ToJSON Notes
instance FromJSON Notes where
    parseJSON = withObject "Notes" $ \o -> do
        n <- o .: "unNotes"
        case mkNotes n of
            Nothing -> fail "Bad notes content"
            Just notes -> return notes

instance Display Notes where
    display = display . unNotes

-- | Maximum length of a note
notesMaxLength :: Int
notesMaxLength = 500

-- | Allowed characters for a note: a printable char, eol or tabulation
printableOrEOLOrTab :: Char -> Bool
printableOrEOLOrTab x = isPrint x || elem x ['\n', '\t']

-- | Check the validity of a note
notesValid :: Text -> Bool
notesValid notes = Text.length notes <= notesMaxLength &&
    Text.all printableOrEOLOrTab notes

-- | Smart constructor which cannot fail
mkNotesLit :: NotesText -> Notes
mkNotesLit = MkNotes . unrefine

-- | Smart constructor which can fail
mkNotes :: Text -> Maybe Notes
mkNotes notes = mkNotesLit <$> rightToMaybe (refine notes)

parser :: Parser Notes
parser = do
    str <- many1 $ satisfy printableOrEOLOrTab
    case mkNotes (Text.pack str) of
        Nothing -> fail "Unable to parse notes"
        Just p  -> return p

