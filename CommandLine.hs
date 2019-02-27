{-# LANGUAGE OverloadedStrings          #-}

module CommandLine (
  Cmd(..),
  WorkOption(..),
  cmd,
  opts
) where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Char
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

attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . T.pack)  

parseOffice :: ReadM Office
parseOffice = attoReadM parser
   where parser = A.string "rennes" *> pure Rennes
              <|> A.string "home"   *> pure Home
   
parseTimeInDay :: ReadM TimeInDay
parseTimeInDay = attoReadM parser
   where parser = A.string "morning"   *> pure Morning
              <|> A.string "afternoon" *> pure Afternoon

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
   argument parseTimeInDay (metavar "TIMEINDAY")
   
diaryRm :: Parser Cmd
diaryRm = DiaryRm <$> 
   argument auto (metavar "DAY") <*> 
   argument parseTimeInDay (metavar "TIMEINDAY")

diaryHoliday :: Parser Cmd
diaryHoliday = DiaryHoliday <$> 
   argument auto (metavar "DAY") <*> 
   argument parseTimeInDay (metavar "TIMEINDAY")

diaryWork :: Parser Cmd
diaryWork = DiaryWork <$> 
   argument auto (metavar "DAY") <*> 
   argument parseTimeInDay (metavar "TIMEINDAY") <*>
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
workOptionSetOffice = SetOffice <$> option parseOffice
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
   
   