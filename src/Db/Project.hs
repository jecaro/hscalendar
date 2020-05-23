-- | Functions related to the 'Project' type
module Db.Project
    ( Project,
      mkProject,
      mkProjectLit,
      parser,
      unProject,
    )
where

import Data.Aeson ((.:), FromJSON (..), ToJSON, withObject)
import Data.Attoparsec.Text
    ( Parser,
      inClass,
      many1,
      satisfy,
    )
import Data.Either.Combinators (rightToMaybe)
import Data.Typeable (typeOf)
import RIO
import qualified RIO.Text as Text (Text, all, length, pack)
import Refined
    ( Predicate,
      Refined,
      refine,
      throwRefineOtherException,
      unrefine,
      validate,
    )
import Test.QuickCheck
    ( Arbitrary,
      arbitrary,
      choose,
      elements,
      sized,
      vectorOf,
    )
import Test.QuickCheck.Instances.Text ()

-- | A type for a project name
newtype Project = MkProject
    { -- | Unwrap
      unProject :: Text.Text
    }
    deriving (Eq, Generic, Ord, Show)

instance Hashable Project

instance ToJSON Project

instance FromJSON Project where
    parseJSON = withObject "Project" $ \o -> do
        p <- o .: "unProject"
        case mkProject p of
            Nothing -> fail "Bad project name"
            Just project -> pure project

instance Display Project where
    display = display . unProject

-- | Max length of project name
projNameMaxLength :: Int
projNameMaxLength = 20

-- | Allowed chars for project name
projNameAllowedChars :: String
projNameAllowedChars = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> ['_']

-- | Predicate to check if the project is valid
projNameValid :: Text -> Bool
projNameValid name =
    Text.length name > 0
        && Text.length name <= projNameMaxLength
        && Text.all (`elem` projNameAllowedChars) name

-- | Arbitrary instance for project. Only project with allowed characters
instance Arbitrary Project where
    arbitrary = sized $ \s -> do
        let minSize = 1
        n <- choose (minSize, minSize `max` (s `min` projNameMaxLength))
        xs <- vectorOf n (elements projNameAllowedChars)
        pure (MkProject (Text.pack xs))

-- | Simple type to refine 'Text' for project names
data ProjName

-- | The actual refined type
type ProjNameText = Refined ProjName Text

-- | Predicate instance to validate what is allowable for a project name
instance Predicate ProjName Text where
    validate p name =
        unless (projNameValid name) $
            throwRefineOtherException (typeOf p) "Not a valid project name"

-- | Smart constructor which cannot fail
mkProjectLit :: ProjNameText -> Project
mkProjectLit = MkProject . unrefine

-- | Smart constructor which can fail
mkProject :: Text -> Maybe Project
mkProject name = mkProjectLit <$> rightToMaybe (refine name)

parser :: Parser Project
parser = do
    str <- many1 $ satisfy $ inClass projNameAllowedChars
    case mkProject (Text.pack str) of
        Nothing -> fail "Unable to parse project"
        Just p -> pure p
