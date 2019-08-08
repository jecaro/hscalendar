{-# LANGUAGE TemplateHaskell #-}
module HalfDay
where

import           RIO

import           Idle (Idle(..))
import           Worked (Worked)

data HalfDay = MkHalfDayWorked Worked | MkHalfDayIdle Idle
    deriving (Show, Eq)

