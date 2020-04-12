{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Db.Stats (Stats(..), empty, off, worked, week)
where

import           RIO
import qualified RIO.HashMap as HM (HashMap, empty, foldrWithKey)

import           Lens.Micro.Platform (makeFields)

import           Db.OffDayType (OffDayType)
import           Db.Project (Project)

data Stats = MkStats
    { _statsOff :: !(HM.HashMap OffDayType Int)
    , _statsWorked :: !(HM.HashMap Project Int)
    , _statsWeek :: !Int
    }
makeFields ''Stats

instance Display Stats where
    display s =  "Stats for the month:\n"
              <> "Week days: " <> displayHdNb (s ^. week) <> "\n"
              <> "Projects:\n"
              <> HM.foldrWithKey foldFct "" (s ^. worked)
              <> "Days off:\n"
              <> HM.foldrWithKey foldFct "" (s ^. off)
        where foldFct p nbHds builder
                  =  builder <> "\t" <> display p <> ": "
                  <> displayHdNb nbHds <> "\n"
              displayHdNb n = display (fromIntegral n / 2 :: Double)

empty :: Stats
empty = MkStats HM.empty HM.empty 0
