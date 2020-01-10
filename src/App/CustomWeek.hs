module App.CustomWeek
    ( CustomWeek(..)
    , parser
    , toWeek
    )
where

import           RIO
import qualified RIO.Time as Time (toGregorian)

import           Data.Attoparsec.Text
    ( Parser
    , asciiCI
    , decimal
    , char
    )

import           App.CustomDay (today)
import           Db.Week (Week(..), fromDay)

-- | Define a week that could be related to current day
data CustomWeek = MkCustomWeek Week   | -- ^ Fully defined week
                  MkCustomWeekNum Int | -- ^ The year is not defined
                  CurrentWeek           -- ^ The current week

    deriving (Eq, Show)

parser :: Parser CustomWeek
parser =   asciiCI "current" $> CurrentWeek
       <|> mkWeek <$> decimal <*> (char '-' *> decimal)
       <|> MkCustomWeekNum <$> decimal
    where mkWeek year weekNum = MkCustomWeek (MkWeek year weekNum)

toWeek :: (MonadIO m) => CustomWeek -> m Week
toWeek (MkCustomWeek week) = return week
toWeek (MkCustomWeekNum weekNum) = do
    day <- today
    let (year, _, _) = Time.toGregorian day
    return $ MkWeek year weekNum
toWeek CurrentWeek = fromDay <$> today

