module CommandLine
    ( Cmd(..)
    , SetArrived(..)
    , SetLeft(..)
    , SetNotes(..)
    , SetOffice(..)
    , SetProj(..)
    , WorkOption(..)
    , cmd
    , opts
    ) where

import           RIO
import qualified RIO.Text as Text (pack)
import qualified RIO.Time as Time (TimeOfDay(..), fromGregorian)

import           Data.Attoparsec.Text as Atto
    ( Parser
    , string
    , decimal
    , endOfInput
    , char
    , parseOnly
    )
import           Data.Functor (($>))
import           Options.Applicative as Opt
    ( Parser
    , ParserInfo
    , ReadM
    , argument
    , command
    , eitherReader
    , flag'
    , help
    , helper
    , hsubparser
    , idm
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , some
    , str
    , strOption
    , subparser
    , (<**>)
    , (<|>)
    )

import           CustomDay (CustomDay(..))
import           Office (Office(..))
import           TimeInDay (TimeInDay(..))

data Cmd = DiaryDisplay CustomDay TimeInDay           |
           DiaryHoliday CustomDay TimeInDay           |
           DiaryRm CustomDay TimeInDay                |
           DiaryWork CustomDay TimeInDay [WorkOption] |
           ProjAdd Text                               |
           ProjList                                   |
           ProjRename Text Text                       |
           ProjRm Text
    deriving (Eq, Show)

newtype SetProj = SetProj Text
    deriving (Eq, Show)

newtype SetNotes = SetNotes Text
    deriving (Eq, Show)

newtype SetArrived = SetArrived Time.TimeOfDay
    deriving (Eq, Show)

newtype SetLeft = SetLeft Time.TimeOfDay
    deriving (Eq, Show)

newtype SetOffice = SetOffice Office
    deriving (Eq, Show)

data WorkOption = MkSetArrived SetArrived |
                  MkSetLeft SetLeft       |
                  MkSetNotes SetNotes     |
                  MkSetOffice SetOffice   |
                  MkSetProj SetProj
    deriving (Eq, Show)

attoReadM :: Atto.Parser a -> ReadM a
attoReadM p = eitherReader (parseOnly (p <* endOfInput) . Text.pack)

parseOffice :: ReadM Office
parseOffice = attoReadM parser
  where parser =   string "rennes" $> Rennes
               <|> string "home"   $> Home
               <|> string "poool"  $> Poool

parseTimeOfDay :: ReadM Time.TimeOfDay
parseTimeOfDay = attoReadM parser
  where parser = (\h m -> Time.TimeOfDay h m 0) <$> decimal <*> (char ':' *> decimal)

parseCustomDay :: ReadM CustomDay
parseCustomDay = attoReadM parser
  where parser =   string "today"     $> Today
               <|> string "yesterday" $> Yesterday
               <|> string "tomorrow"  $> Tomorrow
               <|> mkDayFromGregorian <$> decimal
                                      <*> (char '-' *> decimal)
                                      <*> (char '-' *> decimal)
               <|> MkDayMonthNum <$> decimal <*> (char '-' *> decimal)
               <|> MkDayNum <$> decimal
        mkDayFromGregorian d m y = MkDay $ Time.fromGregorian y m d

projRm :: Opt.Parser Cmd
projRm = ProjRm <$> argument str (metavar "PROJECT...")

projRename :: Opt.Parser Cmd
projRename = ProjRename
    <$> argument str (metavar "PROJECT...")
    <*> argument str (metavar "PROJECT...")

projAdd :: Opt.Parser Cmd
projAdd = ProjAdd <$> argument str (metavar "PROJECT...")

projCmd :: Opt.Parser Cmd
projCmd = subparser
    (  command "list"   (info (pure ProjList) (progDesc "List current projects"))
    <> command "add"    (info projAdd         (progDesc "Add project"))
    <> command "rm"     (info projRm          (progDesc "Remove project"))
    <> command "rename" (info projRename      (progDesc "Rename project"))
    )

diaryDisplay :: Opt.Parser Cmd
diaryDisplay = DiaryDisplay 
    <$> argument parseCustomDay (metavar "DAY")
    <*> tidOption 

diaryRm :: Opt.Parser Cmd
diaryRm = DiaryRm
    <$> argument parseCustomDay (metavar "DAY")
    <*> tidOption

diaryHoliday :: Opt.Parser Cmd
diaryHoliday = DiaryHoliday
    <$> argument parseCustomDay (metavar "DAY")
    <*> tidOption

diaryWork :: Opt.Parser Cmd
diaryWork = DiaryWork
    <$> argument parseCustomDay (metavar "DAY")
    <*> tidOption
    <*> some workOption

tidOption :: Opt.Parser TimeInDay
tidOption =   flag' Morning (long "morning" <> short 'm')
          <|> flag' Afternoon (long "afternoon" <> short 'a')

diaryCmd :: Opt.Parser Cmd
diaryCmd = hsubparser
    (  command "display" (info diaryDisplay (progDesc "Display entry"))
    <> command "work"    (info diaryWork    (progDesc "Set work entry"))
    <> command "holiday" (info diaryHoliday (progDesc "Set holiday entry"))
    <> command "rm"      (info diaryRm      (progDesc "Remove entry"))
    )

workOption :: Opt.Parser WorkOption
workOption = workOptionSetProj    <|>
             workOptionSetNotes   <|>
             workOptionSetArrived <|>
             workOptionSetLeft    <|>
             workOptionSetOffice

workOptionSetProj :: Opt.Parser WorkOption
workOptionSetProj = MkSetProj . SetProj <$> strOption
    (  long "project"
    <> short 'p'
    <> metavar "PROJECT"
    <> help "Set the project" )

workOptionSetNotes :: Opt.Parser WorkOption
workOptionSetNotes = MkSetNotes . SetNotes <$> strOption
    (  long "notes"
    <> short 'n'
    <> metavar "NOTES"
    <> help "Set the notes" )

workOptionSetArrived :: Opt.Parser WorkOption
workOptionSetArrived = MkSetArrived . SetArrived <$> option parseTimeOfDay
    (  long "arrived"
    <> short 'a'
    <> metavar "TIME"
    <> help "Time of arrival" )

workOptionSetLeft :: Opt.Parser WorkOption
workOptionSetLeft = MkSetLeft . SetLeft <$> option parseTimeOfDay
    (  long "left"
    <> short 'l'
    <> metavar "TIME"
    <> help "Time of leaving" )

workOptionSetOffice :: Opt.Parser WorkOption
workOptionSetOffice = MkSetOffice . SetOffice <$> option parseOffice
    (  long "office"
    <> short 'o'
    <> metavar "OFFICE"
    <> help "Office" )

cmd :: Opt.Parser Cmd
cmd = hsubparser
    (  command "project" (info projCmd  (progDesc "Project commands"))
    <> command "diary"   (info diaryCmd (progDesc "Diary commands"))
    )

opts :: ParserInfo Cmd
opts = info (cmd <**> helper) idm

