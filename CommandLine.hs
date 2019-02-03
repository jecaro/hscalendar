module CommandLine (
  Cmd(..),
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
           DiaryEdit Day TimeInDay [EditOption] |
           DiaryDelete Day TimeInDay 
  deriving (Eq, Show)

data EditOption = SetProj String       |
                  SetNote String       |
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
   
diaryDelete :: Parser Cmd
diaryDelete = DiaryDelete <$> 
   argument auto (metavar "DAY") <*> 
   argument auto (metavar "TIMEINDAY")

diaryEdit :: Parser Cmd
diaryEdit = DiaryEdit <$> 
   argument auto (metavar "DAY") <*> 
   argument auto (metavar "TIMEINDAY") <*>
   some editOption

diaryCmd :: Parser Cmd
diaryCmd = hsubparser
   (  command "display" (info diaryDisplay (progDesc "Display entry"))
   <> command "edit"    (info diaryEdit    (progDesc "Edit entry"))
   <> command "delete"  (info diaryDelete  (progDesc "Delete entry"))
   )

editOption :: Parser EditOption
editOption = editOptionSetProj    <|> 
             editOptionSetNote    <|>
             editOptionSetArrived <|>
             editOptionSetLeft    <|>
             editOptionSetOffice

editOptionSetProj :: Parser EditOption
editOptionSetProj = SetProj <$> strOption
   (  long "project"
   <> short 'p'
   <> metavar "PROJECT"
   <> help "Set the project" )

editOptionSetNote :: Parser EditOption
editOptionSetNote = SetNote <$> strOption
   (  long "note"
   <> short 'n'
   <> metavar "NOTE"
   <> help "Set the note" )

editOptionSetArrived :: Parser EditOption
editOptionSetArrived = SetArrived <$> option auto
   (  long "arrived"
   <> short 'a'
   <> metavar "TIME"
   <> help "Time of arrival" )

editOptionSetLeft :: Parser EditOption
editOptionSetLeft = SetLeft <$> option auto
   (  long "left"
   <> short 'l'
   <> metavar "TIME"
   <> help "Time of leaving" )

editOptionSetOffice :: Parser EditOption
editOptionSetOffice = SetOffice <$> option auto
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
   
   