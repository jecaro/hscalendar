import           RIO

import           Options.Applicative (execParser)

import           App.App (initAppAndRun)
import           CommandLine
    ( Options(..)
    , opts
    )
import           Run (run)

-- | Main function
main :: IO ()
main = do
    -- Parse command line
    (Options verbose level, cmd) <- execParser opts
    initAppAndRun verbose level (run cmd)
