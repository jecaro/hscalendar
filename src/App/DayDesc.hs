-- | Contains functions related to 'DayDesc': a data type which can handle
--   any calendar day or today, yesterday and tomorrow. It can also use an easy
--   numbering system.
module App.DayDesc
    ( DayDesc(..)
    , parser
    , toDay
    , today
    ) where

import           RIO
import qualified RIO.Text as Text (intercalate)
import qualified RIO.Time as Time
    ( Day
    , addDays
    , fromGregorian
    , toGregorian
    , getZonedTime
    , localDay
    , zonedTimeToLocalTime
    )

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Attoparsec.Text
    ( Parser
    , asciiCI
    , decimal
    , char
    )
import           Formatting.Extended (formatTwoDigitsPadZero)
import           Servant.API (FromHttpApiData(..), ToHttpApiData(..))
import           Servant.API.Extended (runAtto)


-- | The day description
data DayDesc = MkDayDesc Time.Day        | -- ^ Fully defined day
               MkDayDescNum Int          | -- ^ The year and the month are not present
               MkDayDescMonthNum Int Int | -- ^ The year is not present
               Today                     |
               Yesterday                 |
               Tomorrow
    deriving (Eq, Show)

instance FromHttpApiData DayDesc where
    parseQueryParam = runAtto parser

instance ToHttpApiData DayDesc where
    toQueryParam (MkDayDesc day) = Text.intercalate "-" (fmap formatTwoDigitsPadZero [d, m, intY])
        where (y, m, d) = Time.toGregorian day
              intY = fromIntegral y
    toQueryParam (MkDayDescNum d)        = formatTwoDigitsPadZero d
    toQueryParam (MkDayDescMonthNum d m) = Text.intercalate "-" (fmap formatTwoDigitsPadZero [d, m])
    toQueryParam Today                   = "today"
    toQueryParam Yesterday               = "yesterday"
    toQueryParam Tomorrow                = "tomorrow"

-- | Get today
today :: (MonadIO m) => m Time.Day
today = liftIO $ Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime

-- | Convert 'DayDesc' to 'Time.Day'
toDay :: (MonadIO m) => DayDesc -> m Time.Day
toDay Today            = today
toDay Yesterday        = Time.addDays (-1) <$> today
toDay Tomorrow         = Time.addDays 1 <$> today
toDay (MkDayDesc x)    = pure x
toDay (MkDayDescNum d) = do
    (y, m, _) <- Time.toGregorian <$> today
    pure $ Time.fromGregorian y m d
toDay (MkDayDescMonthNum d m) = do
    (y, _, _) <- Time.toGregorian <$> today
    pure $ Time.fromGregorian y m d

-- | Parser for a 'DayDesc'
parser :: Parser DayDesc
parser =   asciiCI "today"     $> Today
       <|> asciiCI "yesterday" $> Yesterday
       <|> asciiCI "tomorrow"  $> Tomorrow
       <|> mkDayFromGregorian <$> decimal
                              <*> (char '-' *> decimal)
                              <*> (char '-' *> decimal)
       <|> MkDayDescMonthNum <$> decimal <*> (char '-' *> decimal)
       <|> MkDayDescNum <$> decimal
  where mkDayFromGregorian d m y = MkDayDesc $ Time.fromGregorian y m d

