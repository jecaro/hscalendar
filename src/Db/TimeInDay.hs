-- | Simple sum type to define the time in the day
{-# LANGUAGE TemplateHaskell #-}
module Db.TimeInDay where

import           RIO

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Attoparsec.Text
    ( Parser
    , asciiCI
    )
import           Database.Persist.TH (derivePersistField)
import           Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum)
import           Servant.API (FromHttpApiData(..), ToHttpApiData(..))
import           Servant.API.Extended (runAtto)

-- | Simple sum type of defining the time in the day
data TimeInDay = Morning | Afternoon
    deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)
derivePersistField "TimeInDay"

-- | Arbitrary instance for QuickCheck
instance Arbitrary TimeInDay where
    arbitrary = arbitraryBoundedEnum

instance FromJSON TimeInDay
instance ToJSON TimeInDay

instance Display TimeInDay where
    display Morning = "morning"
    display Afternoon = "afternoon"

instance FromHttpApiData TimeInDay where
    parseQueryParam = runAtto parser

instance ToHttpApiData TimeInDay where
    toQueryParam Morning   = "morning"
    toQueryParam Afternoon = "afternoon"

-- | Switch to the other 'TimeInDay'
other :: TimeInDay -> TimeInDay
other Morning   = Afternoon
other Afternoon = Morning

-- | Parser for the 'TimeInDay' type
parser :: Parser TimeInDay
parser =   asciiCI "morning"   $> Morning
       <|> asciiCI "afternoon" $> Afternoon
