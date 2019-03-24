module CustomDay
    ( CustomDay(..)
    , toDay
    ) where

import           RIO

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Time.Calendar (Day, addDays, fromGregorian, toGregorian)
import           Data.Time.LocalTime 
    ( getZonedTime
    , localDay
    , zonedTimeToLocalTime
    )

data CustomDay = MkDay Day             |
                 MkDayNum Int          |
                 MkDayMonthNum Int Int |
                 Today                 |
                 Yesterday             |
                 Tomorrow
    deriving (Eq, Show)

-- Get today
today :: (MonadIO m) => m Day
today = liftIO $ localDay . zonedTimeToLocalTime <$> getZonedTime

-- Convert CustomDay to Day
toDay :: (MonadIO m) => CustomDay -> m Day 
toDay Today        = today
toDay Yesterday    = addDays (-1) <$> today
toDay Tomorrow     = addDays 1 <$> today
toDay (MkDay x)    = return x
toDay (MkDayNum d) = do
    (y, m, _) <- toGregorian <$> today
    return $ fromGregorian y m d
toDay (MkDayMonthNum d m) = do
    (y, _, _) <- toGregorian <$> today
    return $ fromGregorian y m d
