{-# OPTIONS_GHC -fno-warn-orphans #-}
module Db.Stats (Stats)
where

import           RIO
import qualified RIO.HashMap as HM (HashMap, foldrWithKey)

import           Db.Project (Project)

type Stats = HM.HashMap Project Int

instance Display Stats where
    display s =  "Stats for the month:\n"
            <> HM.foldrWithKey foldFct "" s
        where foldFct project nbHds b
                  =  b <> display project <> ": "
                  <> display (nbDays nbHds :: Double) <> "\n"
              nbDays nbHds = fromIntegral nbHds / 2
