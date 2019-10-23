-- | Contains functions related to CustomDay: a data type which can handle
--   any calendar day or today, yesterday and tomorrow. It can also use an easy
--   numbering system.
module App.CustomDay
    ( CustomDay(..)
    , parser
    , toDay
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


-- | The custom day
data CustomDay = MkDay Time.Day        | -- ^ Fully defined day
                 MkDayNum Int          | -- ^ The year and the month are not present
                 MkDayMonthNum Int Int | -- ^ The year is not present
                 Today                 |
                 Yesterday             |
                 Tomorrow
    deriving (Eq, Show)

instance FromHttpApiData CustomDay where
    parseQueryParam = runAtto parser

instance ToHttpApiData CustomDay where
    toQueryParam (MkDay day) = Text.intercalate "-" (fmap formatTwoDigitsPadZero [d, m, intY]) 
        where (y, m, d) = Time.toGregorian day
              intY = fromIntegral y
    toQueryParam (MkDayNum d)        = formatTwoDigitsPadZero d        
    toQueryParam (MkDayMonthNum d m) = Text.intercalate "-" (fmap formatTwoDigitsPadZero [d, m]) 
    toQueryParam Today               = "today"   
    toQueryParam Yesterday           = "yesterday"   
    toQueryParam Tomorrow            = "tomorrow"

-- | Get today
today :: (MonadIO m) => m Time.Day
today = liftIO $ Time.localDay . Time.zonedTimeToLocalTime <$> Time.getZonedTime

-- | Convert CustomDay to Day
toDay :: (MonadIO m) => CustomDay -> m Time.Day 
toDay Today        = today
toDay Yesterday    = Time.addDays (-1) <$> today
toDay Tomorrow     = Time.addDays 1 <$> today
toDay (MkDay x)    = return x
toDay (MkDayNum d) = do
    (y, m, _) <- Time.toGregorian <$> today
    return $ Time.fromGregorian y m d
toDay (MkDayMonthNum d m) = do
    (y, _, _) <- Time.toGregorian <$> today
    return $ Time.fromGregorian y m d

parser :: Parser CustomDay
parser =   asciiCI "today"     $> Today
       <|> asciiCI "yesterday" $> Yesterday
       <|> asciiCI "tomorrow"  $> Tomorrow
       <|> mkDayFromGregorian <$> decimal
                              <*> (char '-' *> decimal)
                              <*> (char '-' *> decimal)
       <|> MkDayMonthNum <$> decimal <*> (char '-' *> decimal)
       <|> MkDayNum <$> decimal
  where mkDayFromGregorian d m y = MkDay $ Time.fromGregorian y m d

