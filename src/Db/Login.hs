-- | Functions related to 'Login'
module Db.Login
    ( Login
    , mkLogin
    , mkLoginLit
    , unLogin
    )
where

import           RIO

import qualified RIO.Text as Text (Text, all, length, pack)

import           Data.Aeson (FromJSON, ToJSON)
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
    , elements
    , sized
    , vectorOf
    )
import           Test.QuickCheck.Instances.Text()

-- | The type for storing the login
newtype Login = MkLogin
    { unLogin :: Text.Text -- ^ Unwrap the content
    }
    deriving (Eq, Generic, Ord, Show)

-- | Simple type to refine 'Text' for 'Login'
data LoginData

-- | The actual refined type
type LoginText = Refined LoginData Text

-- | Predicate instance to validate what is allowable for login
instance Predicate LoginData Text where
    validate p name = unless (loginValid name) $
            throwRefineOtherException (typeOf p) "Not alpha num text"

-- | Arbitrary instance for QuickCheck
instance Arbitrary Login where
    arbitrary = sized $ \s -> do
        n <- choose (0, s `min` loginMaxLength)
        xs <- vectorOf n (elements loginAllowedChars)
        return $ MkLogin $ Text.pack xs

instance FromJSON Login
instance ToJSON Login

-- | Maximum length of a login
loginMaxLength :: Int
loginMaxLength = 20

-- | Allowed characters for a login
loginAllowedChars :: String
loginAllowedChars = ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['_']

-- | Check the validity of a login
loginValid :: Text -> Bool
loginValid name = Text.length name <= loginMaxLength &&
    Text.all (`elem` loginAllowedChars) name

-- | Smart constructor which cannot fail
mkLoginLit :: LoginText -> Login
mkLoginLit = MkLogin . unrefine

-- | Smart constructor which can fail
mkLogin :: Text -> Maybe Login
mkLogin name = mkLoginLit <$> rightToMaybe (refine name)

