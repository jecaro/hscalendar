import           RIO

import           Options.Applicative (execParser)

import           App.App (initAppAndRun)
import           CommandLine 
    ( Options(..)
    , opts
    )
import           Run (run)

-- Synopsis
-- hsmaster diary work date -m|-a [commands]
--   commands: 
--     -p project   set the project name 
--     -n note      set note
--     -a 00:00     set arrived time
--     -l 00:00     set left time
-- hsmaster diary holiday date -m|-a [cp|ef|rtte|rtts|ss|ph|tp]
-- hsmaster diary rm date -m|-a
-- hsmaster diary display date -m|-a
-- hsmaster diary edit date -m|-a
-- hsmaster project list
-- hsmaster project rm project
-- hsmaster project add project
-- hsmaster project rename project1 project2 

-- TODO:
-- - Add import CSV
-- - Add stats for a year
-- - Add show week
-- - Add show month
-- - Make diary date/TimeInDay optional default today
-- - Put defaults values in config file for a week
-- - Add unit test for editor features
-- - Add cleanUp command
-- - Make FromJSON instance safe for Project/Note
-- - Factorize attoReadM
-- - Add constraint to displayHd 

-- | Main function
main :: IO ()
main = do
    -- Parse command line
    (Options verbose level, cmd) <- execParser opts
    initAppAndRun verbose level (run cmd)
