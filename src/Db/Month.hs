-- | Simple data type to represent a Month
module Db.Month (Month(..))
where

import           RIO

data Month = MkMonth { _year :: !Integer, _month :: !Int}
    deriving (Eq, Generic, Show)



