{-# LANGUAGE TemplateHaskell #-}
module Db.HalfDay
where

import           RIO

import           Db.Idle (Idle(..))
import           Db.Worked (Worked)

data HalfDay = MkHalfDayWorked Worked | MkHalfDayIdle Idle
    deriving (Show, Eq)

