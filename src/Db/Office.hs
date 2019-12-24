-- | Type to define the office used on working half-day
{-# LANGUAGE TemplateHaskell #-}
module Db.Office
    ( Office(..)
    , parser
    ) where
import           RIO
import           Data.Attoparsec.Text
    ( Parser
    , asciiCI
    )
import           Data.Yaml
    ( FromJSON
    , ToJSON
    )

import           Database.Persist.TH (derivePersistField)

import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)

-- | Simple sum type for defining a work location
data Office = Rennes | Home | Poool | OutOfOffice
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)
derivePersistField "Office"

instance FromJSON Office
instance ToJSON Office

-- | Arbitrary instance for QuickCheck
instance Arbitrary Office where
    arbitrary = arbitraryBoundedEnum

instance Display Office where
    textDisplay Rennes = "Rennes"
    textDisplay Home = "Home"
    textDisplay OutOfOffice = "Out of office"
    textDisplay Poool = "Poool"

-- | Parser for the 'Office' type
parser :: Parser Office
parser =   asciiCI "home"   $> Home
       <|> asciiCI "out"    $> OutOfOffice
       <|> asciiCI "poool"  $> Poool
       <|> asciiCI "rennes" $> Rennes

