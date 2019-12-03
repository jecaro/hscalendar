module App.API (HSCalendarApi, ProtectedHSCalendarApi, RenameArgs(..))
where

import           RIO

import           Data.Aeson (FromJSON, ToJSON)
import           Servant.API
    ( BasicAuth
    , Get
    , Capture
    , DeleteNoContent
    , JSON
    , NoContent(..)
    , PostNoContent
    , PutNoContent
    , ReqBody
    , Summary
    , (:>)
    , (:<|>)(..))

import           App.CustomDay (CustomDay(..))
import           App.WorkOption (WorkOption(..))
import           Db.HalfDay (HalfDay(..))
import           Db.IdleDayType (IdleDayType(..))
import           Db.Login (Login)
import           Db.Project (Project)
import           Db.TimeInDay (TimeInDay(..))


data RenameArgs = MkRenameArgs { from :: Project, to :: Project }
    deriving (Eq, Generic, Show, Ord)

instance FromJSON RenameArgs
instance ToJSON RenameArgs

type HSCalendarApi =
        Summary "Initialize the database"
           :> "migrate"
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "List all projects"
           :> "project"
           :> Get '[JSON] [Project]
   :<|> Summary "Add a project"
           :> "project"
           :> ReqBody '[JSON] Project
           :> PostNoContent '[JSON] NoContent
   :<|> Summary "Delete a project"
           :> "project"
           :> ReqBody '[JSON] Project
           :> DeleteNoContent '[JSON] NoContent
   :<|> Summary "Rename a project"
           :> "project"
           :> ReqBody '[JSON] RenameArgs
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "Display a half-day"
           :> "diary"
           :> Capture "day" CustomDay
           :> Capture "time in day" TimeInDay
           :> Get '[JSON] HalfDay
   :<|> Summary "Set a non-working half-day"
           :> "diary"
           :> "idle"
           :> Capture "day" CustomDay
           :> Capture "time in day" TimeInDay
           :> ReqBody '[JSON] IdleDayType
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "Set a working half-day"
           :> "diary"
           :> Capture "day" CustomDay
           :> Capture "time in day" TimeInDay
           :> ReqBody '[JSON] [WorkOption]
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "Delete a half-day"
           :> "diary"
           :> Capture "day" CustomDay
           :> Capture "time in day" TimeInDay
           :> DeleteNoContent '[JSON] NoContent

type ProtectedHSCalendarApi = BasicAuth "basic-realm" Login :> HSCalendarApi

