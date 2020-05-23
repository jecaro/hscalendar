module Main where

import App.App (initAppAndRun)
import App.CommandLine (Cmd, Options (..), cmd, options)
import Options.Applicative
    ( (<**>),
      Parser,
      ParserInfo,
      execParser,
      helper,
      idm,
      info,
    )
import RIO
import Run (run)

optionsAndCmd :: Parser (Options, Cmd)
optionsAndCmd = curry id <$> options <*> cmd

optionsInfo :: ParserInfo (Options, Cmd)
optionsInfo = info (optionsAndCmd <**> helper) idm

-- | Main function
main :: IO ()
main = do
    -- Parse command line
    (Options verbose level, cmd') <- execParser optionsInfo
    initAppAndRun verbose level (run cmd')
