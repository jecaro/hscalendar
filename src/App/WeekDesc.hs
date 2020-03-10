module App.WeekDesc
    ( WeekDesc(..)
    , parser
    , toWeek
    )
where

import           RIO
import qualified RIO.Text as Text (intercalate)
import qualified RIO.Time as Time (toGregorian)

import           Data.Attoparsec.Text
    ( Parser
    , asciiCI
    , decimal
    , char
    )
import           Formatting.Extended (formatTwoDigitsPadZero)
import           Servant.API (FromHttpApiData(..), ToHttpApiData(..))
import           Servant.API.Extended (runAtto)

import           App.DayDesc (today)
import           Db.Week (Week(..), fromDay)

-- | Define a week that could be related to current day
data WeekDesc = MkWeekDesc Week   | -- ^ Fully defined week
                MkWeekDescNum Int | -- ^ The year is not defined
                CurrentWeek           -- ^ The current week

    deriving (Eq, Show)

instance FromHttpApiData WeekDesc where
    parseQueryParam = runAtto parser

instance ToHttpApiData WeekDesc where
    toQueryParam (MkWeekDesc (MkWeek year week)) =
        Text.intercalate "-" (fmap formatTwoDigitsPadZero [yearInt, week])
        where yearInt = fromIntegral year
    toQueryParam (MkWeekDescNum week) = formatTwoDigitsPadZero week
    toQueryParam CurrentWeek = "current"

parser :: Parser WeekDesc
parser =   asciiCI "current" $> CurrentWeek
       <|> mkWeek <$> decimal <*> (char '-' *> decimal)
       <|> MkWeekDescNum <$> decimal
    where mkWeek year weekNum = MkWeekDesc (MkWeek year weekNum)

toWeek :: (MonadIO m) => WeekDesc -> m Week
toWeek (MkWeekDesc week) = pure week
toWeek (MkWeekDescNum weekNum) = do
    day <- today
    let (year, _, _) = Time.toGregorian day
    pure $ MkWeek year weekNum
toWeek CurrentWeek = fst . fromDay <$> today

