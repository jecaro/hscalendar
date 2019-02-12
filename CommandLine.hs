module CommandLine (
  Cmd(..),
  WorkOption(..),
  cmd,
  opts
) where

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Options.Applicative

import           Office
import           TimeInDay


data Cmd = ProjList                             |
           ProjRm String                        |
           ProjAdd String                       |
           DiaryDisplay Day TimeInDay           |
           DiaryRm Day TimeInDay                |
           DiaryHoliday Day TimeInDay           |
           DiaryWork Day TimeInDay [WorkOption] |
           DiaryDelete Day TimeInDay 
  deriving (Eq, Show)

data WorkOption = SetProj String       |
                  SetNotes String      |
                  SetArrived TimeOfDay |
                  SetLeft TimeOfDay    |
                  SetOffice Office
   deriving (Eq, Show)

projRm :: Parser Cmd
projRm = ProjRm <$> argument str (metavar "PROJECT...")

projAdd :: Parser Cmd
projAdd = ProjAdd <$> argument str (metavar "PROJECT...")

projCmd :: Parser Cmd
projCmd = subparser
   (  command "list" (info (pure ProjList) (progDesc "List current projects"))
   <> command "add"  (info projAdd         (progDesc "Add project"))
   <> command "rm"   (info projRm          (progDesc "Remove project"))
   )

diaryDisplay :: Parser Cmd
diaryDisplay = DiaryDisplay <$> 
   argument auto (metavar "DAY") <*> 
   argument auto (metavar "TIMEINDAY")
   
diaryRm :: Parser Cmd
diaryRm = DiaryRm <$> 
   argument auto (metavar "DAY") <*> 
   argument auto (metavar "TIMEINDAY")

diaryHoliday :: Parser Cmd
diaryHoliday = DiaryHoliday <$> 
   argument auto (metavar "DAY") <*> 
   argument auto (metavar "TIMEINDAY")

diaryWork :: Parser Cmd
diaryWork = DiaryWork <$> 
   argument auto (metavar "DAY") <*> 
   argument auto (metavar "TIMEINDAY") <*>
   some workOption

diaryCmd :: Parser Cmd
diaryCmd = hsubparser
   (  command "display" (info diaryDisplay (progDesc "Display entry"))
   <> command "work"    (info diaryWork    (progDesc "Set work entry"))
   <> command "holiday" (info diaryHoliday (progDesc "Set holiday entry"))
   <> command "rm"      (info diaryRm      (progDesc "Remove entry"))
   )

workOption :: Parser WorkOption
workOption = workOptionSetProj    <|> 
             workOptionSetNotes   <|>
             workOptionSetArrived <|>
             workOptionSetLeft    <|>
             workOptionSetOffice

workOptionSetProj :: Parser WorkOption
workOptionSetProj = SetProj <$> strOption
   (  long "project"
   <> short 'p'
   <> metavar "PROJECT"
   <> help "Set the project" )

workOptionSetNotes :: Parser WorkOption
workOptionSetNotes = SetNotes <$> strOption
   (  long "notes"
   <> short 'n'
   <> metavar "NOTES"
   <> help "Set the notes" )

workOptionSetArrived :: Parser WorkOption
workOptionSetArrived = SetArrived <$> option auto
   (  long "arrived"
   <> short 'a'
   <> metavar "TIME"
   <> help "Time of arrival" )

workOptionSetLeft :: Parser WorkOption
workOptionSetLeft = SetLeft <$> option auto
   (  long "left"
   <> short 'l'
   <> metavar "TIME"
   <> help "Time of leaving" )

workOptionSetOffice :: Parser WorkOption
workOptionSetOffice = SetOffice <$> option auto
   (  long "office"
   <> short 'o'
   <> metavar "OFFICE"
   <> help "Office" )

cmd :: Parser Cmd
cmd = hsubparser
   (  command "project" (info projCmd  (progDesc "Project"))
   <> command "diary"   (info diaryCmd (progDesc "Diary"))
   )

opts :: ParserInfo Cmd
opts = info (cmd <**> helper) idm
   
   