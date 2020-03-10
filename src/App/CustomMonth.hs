module App.CustomMonth
    ( CustomMonth(..)
    , parser
    , toMonth
    )
where

import           RIO
import qualified RIO.Time as Time (toGregorian)
import qualified RIO.Text as Text (intercalate)

import           Data.Attoparsec.Text
    ( Parser
    , asciiCI
    , char
    , decimal
    )
import           Formatting.Extended (formatTwoDigitsPadZero)
import           Servant.API (FromHttpApiData(..), ToHttpApiData(..))
import           Servant.API.Extended (runAtto)

import           App.CustomDay (today)
import           Db.Month (Month(..))

data CustomMonth = MkCustomMonth Month  | -- ^ Fully defined month
                   MkCustomMonthInt Int | -- ^ Month in current year
                   CurrentMonth           -- ^ The current month

    deriving (Eq, Show)

instance FromHttpApiData CustomMonth where
    parseQueryParam = runAtto parser

instance ToHttpApiData CustomMonth where
    toQueryParam (MkCustomMonth (MkMonth year month)) =
        Text.intercalate "-" (fmap formatTwoDigitsPadZero [yearInt, month])
        where yearInt = fromIntegral year
    toQueryParam (MkCustomMonthInt month) = formatTwoDigitsPadZero month
    toQueryParam CurrentMonth = "current"



parser :: Parser CustomMonth
parser =   asciiCI "current" $> CurrentMonth
       <|> mkMonth <$> decimal <*> (char '-' *> decimal)
       <|> MkCustomMonthInt <$> decimal
    where mkMonth year month = MkCustomMonth (MkMonth year month)

toMonth :: (MonadIO m) => CustomMonth -> m Month
toMonth (MkCustomMonth month) = pure month
toMonth (MkCustomMonthInt m) = mkMonth . Time.toGregorian <$> today
  where mkMonth (y, _, _) = MkMonth y m
toMonth CurrentMonth = mkMonth . Time.toGregorian <$> today
  where mkMonth (y, m, _) = MkMonth y m


