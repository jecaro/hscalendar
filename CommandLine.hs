module CommandLine 
    ( Cmd(..)
    , WorkOption(..)
    , SetArrived(..)
    , SetLeft(..)
    , SetNotes(..)
    , SetOffice(..)
    , SetProj(..)
    , cmd
    , opts
    ) where

import           Data.Attoparsec.Text as Atto
    ( Parser
    , string
    , decimal
    , char
    , parseOnly
    )
import           Data.Functor (($>))
import           Data.Text (pack)
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime (TimeOfDay(..))
import           Options.Applicative as Opt
    ( Parser
    , ParserInfo
    , ReadM
    , argument
    , auto
    , command
    , eitherReader 
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

import           Office (Office(..))
import           TimeInDay (TimeInDay(..))

data Cmd = ProjList                             |
           ProjRm String                        |
           ProjAdd String                       |
           DiaryDisplay Day TimeInDay           |
           DiaryRm Day TimeInDay                |
           DiaryHoliday Day TimeInDay           |
           DiaryWork Day TimeInDay [WorkOption]
    deriving (Eq, Show)

newtype SetProj = SetProj String
    deriving (Eq, Show)

newtype SetNotes = SetNotes String
    deriving (Eq, Show)

newtype SetArrived = SetArrived TimeOfDay
    deriving (Eq, Show)

newtype SetLeft = SetLeft TimeOfDay
    deriving (Eq, Show)

newtype SetOffice = SetOffice Office
    deriving (Eq, Show)

data WorkOption = OptSetProj SetProj       |
                  OptSetNotes SetNotes     |
                  OptSetArrived SetArrived |
                  OptSetLeft SetLeft       |
                  OptSetOffice SetOffice
    deriving (Eq, Show)

attoReadM :: Atto.Parser a -> ReadM a
attoReadM p = eitherReader (parseOnly p . pack)

parseOffice :: ReadM Office
parseOffice = attoReadM parser
  where parser = string "rennes" $> Rennes
             <|> string "home"   $> Home

parseTimeInDay :: ReadM TimeInDay
parseTimeInDay = attoReadM parser
  where parser = string "morning"   $> Morning
             <|> string "afternoon" $> Afternoon

parseTimeOfDay :: ReadM TimeOfDay
parseTimeOfDay = attoReadM parser
  where parser = do
            h <- decimal
            _ <- char ':'
            m <- decimal
            return $ TimeOfDay h m 0


projRm :: Opt.Parser Cmd
projRm = ProjRm <$> argument str (metavar "PROJECT...")

projAdd :: Opt.Parser Cmd
projAdd = ProjAdd <$> argument str (metavar "PROJECT...")

projCmd :: Opt.Parser Cmd
projCmd = subparser
    (  command "list" (info (pure ProjList) (progDesc "List current projects"))
    <> command "add"  (info projAdd         (progDesc "Add project"))
    <> command "rm"   (info projRm          (progDesc "Remove project"))
    )

diaryDisplay :: Opt.Parser Cmd
diaryDisplay = DiaryDisplay <$>
    argument auto (metavar "DAY") <*>
    argument parseTimeInDay (metavar "TIMEINDAY")

diaryRm :: Opt.Parser Cmd
diaryRm = DiaryRm <$>
    argument auto (metavar "DAY") <*>
    argument parseTimeInDay (metavar "TIMEINDAY")

diaryHoliday :: Opt.Parser Cmd
diaryHoliday = DiaryHoliday <$>
    argument auto (metavar "DAY") <*>
    argument parseTimeInDay (metavar "TIMEINDAY")

diaryWork :: Opt.Parser Cmd
diaryWork = DiaryWork <$>
    argument auto (metavar "DAY") <*>
    argument parseTimeInDay (metavar "TIMEINDAY") <*>
    some workOption

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
workOptionSetProj = OptSetProj . SetProj <$> strOption
    (  long "project"
    <> short 'p'
    <> metavar "PROJECT"
    <> help "Set the project" )

workOptionSetNotes :: Opt.Parser WorkOption
workOptionSetNotes = OptSetNotes . SetNotes <$> strOption
    (  long "notes"
    <> short 'n'
    <> metavar "NOTES"
    <> help "Set the notes" )

workOptionSetArrived :: Opt.Parser WorkOption
workOptionSetArrived = OptSetArrived . SetArrived <$> option parseTimeOfDay
    (  long "arrived"
    <> short 'a'
    <> metavar "TIME"
    <> help "Time of arrival" )

workOptionSetLeft :: Opt.Parser WorkOption
workOptionSetLeft = OptSetLeft . SetLeft <$> option parseTimeOfDay
    (  long "left"
    <> short 'l'
    <> metavar "TIME"
    <> help "Time of leaving" )

workOptionSetOffice :: Opt.Parser WorkOption
workOptionSetOffice = OptSetOffice . SetOffice <$> option parseOffice
    (  long "office"
    <> short 'o'
    <> metavar "OFFICE"
    <> help "Office" )

cmd :: Opt.Parser Cmd
cmd = hsubparser
    (  command "project" (info projCmd  (progDesc "Project"))
    <> command "diary"   (info diaryCmd (progDesc "Diary"))
    )

opts :: ParserInfo Cmd
opts = info (cmd <**> helper) idm

