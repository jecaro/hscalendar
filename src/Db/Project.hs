-- | Functions related to the 'Project' type
module Db.Project
    ( Project
    , mkProject
    , mkProjectLit
    , readProject
    , unProject
    )
where

import           RIO

import qualified RIO.Text as Text (Text, all, length, pack)

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Either.Combinators (rightToMaybe)
import           Data.Typeable (typeOf)
import           Options.Applicative (ReadM, maybeReader)
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
    , vectorOf
    )
import           Test.QuickCheck.Instances.Text()

-- | A type for a project name
newtype Project = MkProject
    { unProject :: Text.Text -- ^ Unwrap
    }
    deriving (Eq, Generic, Show, Ord)

instance ToJSON Project
instance FromJSON Project

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
        let minSize = 1
        n <- choose (minSize, minSize `max` (s `min` projNameMaxLength))
        xs <- vectorOf n (elements projNameAllowedChars)
        return (MkProject (Text.pack xs))

-- | Simple type to refine 'Text' for project names
data ProjName

-- | The actual refined type
type ProjNameText = Refined ProjName Text

-- | Predicate instance to validate what is allowable for a project name
instance Predicate ProjName Text where
    validate p name = unless (projNameValid name) $
        throwRefineOtherException (typeOf p) "Not a valid project name"

-- | Smart constructor which cannot fail
mkProjectLit :: ProjNameText -> Project
mkProjectLit = MkProject . unrefine

-- | Smart constructor which can fail
mkProject :: Text -> Maybe Project
mkProject name = mkProjectLit <$> rightToMaybe (refine name)

-- | Reader for a "Project" which may fail
readProject :: ReadM Project
readProject = maybeReader $ mkProject . Text.pack

