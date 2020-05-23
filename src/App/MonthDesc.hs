module App.MonthDesc
    ( MonthDesc (..),
      parser,
      toMonth,
    )
where

import App.DayDesc (today)
import Data.Attoparsec.Text
    ( Parser,
      asciiCI,
      char,
      decimal,
    )
import Db.Month (Month (..))
import Formatting.Extended (formatTwoDigitsPadZero)
import RIO
import qualified RIO.Text as Text (intercalate)
import qualified RIO.Time as Time (toGregorian)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import Servant.API.Extended (runAtto)

data MonthDesc
    = -- | Fully defined month
      MkMonthDesc Month
    | -- | Month in current year
      MkMonthDescNum Int
    | -- | The current month
      CurrentMonth
    deriving (Eq, Show)

instance FromHttpApiData MonthDesc where
    parseQueryParam = runAtto parser

instance ToHttpApiData MonthDesc where
    toQueryParam (MkMonthDesc (MkMonth year month)) =
        Text.intercalate "-" (fmap formatTwoDigitsPadZero [yearInt, month])
        where
            yearInt = fromIntegral year
    toQueryParam (MkMonthDescNum month) = formatTwoDigitsPadZero month
    toQueryParam CurrentMonth = "current"

parser :: Parser MonthDesc
parser =
    asciiCI "current" $> CurrentMonth
        <|> mkMonth <$> decimal <*> (char '-' *> decimal)
        <|> MkMonthDescNum <$> decimal
    where
        mkMonth year month = MkMonthDesc (MkMonth year month)

toMonth :: (MonadIO m) => MonthDesc -> m Month
toMonth (MkMonthDesc month) = pure month
toMonth (MkMonthDescNum m) = mkMonth . Time.toGregorian <$> today
    where
        mkMonth (y, _, _) = MkMonth y m
toMonth CurrentMonth = mkMonth . Time.toGregorian <$> today
    where
        mkMonth (y, m, _) = MkMonth y m
