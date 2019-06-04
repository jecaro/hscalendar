module DefaultHours
    ( DefaultHours(..)
    , DefaultHoursForDay(..)
    )
where

import           RIO
import qualified RIO.Time as Time (TimeOfDay)
import           Data.Yaml (FromJSON, ToJSON)

data DefaultHours = DefaultHours { arrived :: Time.TimeOfDay
                                 , left    :: Time.TimeOfDay }
    deriving (Show, Generic)

instance FromJSON DefaultHours
instance ToJSON DefaultHours

data DefaultHoursForDay = DefaultHoursForDay { morning   :: DefaultHours
                                             , afternoon :: DefaultHours }
    deriving (Show, Generic)

instance FromJSON DefaultHoursForDay
instance ToJSON DefaultHoursForDay
