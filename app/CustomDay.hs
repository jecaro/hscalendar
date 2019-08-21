-- | Contains functions related to CustomDay: a data type which can handle
--   any calendar day or today, yesterday and tomorrow. It can also use an easy
--   numbering system.
module CustomDay
    ( CustomDay(..)
    , parser
    , toDay
    ) where

import           RIO
import qualified RIO.Time as Time 
    ( Day
    , addDays
    , fromGregorian
    , toGregorian
    , getZonedTime
    , localDay
    , zonedTimeToLocalTime
    )

import           Data.Attoparsec.Text
    ( Parser
    , asciiCI 
    , decimal
    , char
    )
import           Control.Monad.IO.Class (MonadIO, liftIO)

-- | The custom day
data CustomDay = MkDay Time.Day        | -- ^ Fully defined day
                 MkDayNum Int          | -- ^ The year and the month are not present
                 MkDayMonthNum Int Int | -- ^ The year is not present
                 Today                 |
                 Yesterday             |
                 Tomorrow
    deriving (Eq, Show)

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

