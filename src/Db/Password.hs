-- | Functions related to 'Password
module Db.Password
    ( Password,
      mkPassword,
      mkPasswordLit,
      parser,
      unPassword,
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.Text
    ( Parser,
      many1,
      satisfy,
    )
import Data.Char (isPrint)
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
      sized,
      suchThat,
      vectorOf,
    )
import Test.QuickCheck.Instances.Text ()

-- | The type for storing the password
newtype Password = MkPassword
    { -- | Unwrap the content
      unPassword :: Text.Text
    }
    deriving (Eq, Generic, Ord, Show)

-- | Simple type to refine 'Text' for 'Password'
data PasswordData

-- | The actual refined type
type PasswordText = Refined PasswordData Text

-- | Predicate instance to validate what is allowable for a password
instance Predicate PasswordData Text where
    validate p password =
        unless (passwordValid password) $
            throwRefineOtherException (typeOf p) "Not a valid password"

-- | Arbitrary instance for QuickCheck
instance Arbitrary Password where
    arbitrary = sized $ \s -> do
        n <- choose (0, s `min` passwordMaxLength)
        xs <- vectorOf n (arbitrary `suchThat` isPrint)
        pure $ MkPassword $ Text.pack xs

instance FromJSON Password

instance ToJSON Password

-- | Maximum length of a password
passwordMaxLength :: Int
passwordMaxLength = 20

-- | Check the validity of a password
passwordValid :: Text -> Bool
passwordValid password =
    Text.length password <= passwordMaxLength
        && Text.all isPrint password

-- | Smart constructor which cannot fail
mkPasswordLit :: PasswordText -> Password
mkPasswordLit = MkPassword . unrefine

-- | Smart constructor which can fail
mkPassword :: Text -> Maybe Password
mkPassword password = mkPasswordLit <$> rightToMaybe (refine password)

parser :: Parser Password
parser = do
    str <- many1 $ satisfy isPrint
    case mkPassword (Text.pack str) of
        Nothing -> fail "Unable to parse password"
        Just p -> pure p
