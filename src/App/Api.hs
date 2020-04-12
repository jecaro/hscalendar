module App.Api
    ( HSBasicAuth
    , HSCalendarApi
    , RenameArgs(..)
    )
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

import           App.DayDesc (DayDesc(..))
import           App.MonthDesc (MonthDesc(..))
import           App.WeekDesc (WeekDesc(..))
import           App.WorkOption (WorkOption(..))

import           Db.HalfDay (HalfDay(..))
import           Db.OffDayType (OffDayType(..))
import           Db.Login (Login)
import           Db.MonthF (MonthWithDays)
import           Db.Project (Project)
import           Db.TimeInDay (TimeInDay(..))
import           Db.WeekF (WeekWithDays)


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
           :> Capture "day" DayDesc
           :> Capture "time in day" TimeInDay
           :> Get '[JSON] HalfDay
   :<|> Summary "Display a week"
           :> "week"
           :> Capture "week" WeekDesc
           :> Get '[JSON] WeekWithDays
   :<|> Summary "Display a month"
           :> "month"
           :> Capture "month" MonthDesc
           :> Get '[JSON] MonthWithDays
   :<|> Summary "Set a non-working half-day"
           :> "diary"
           :> "off"
           :> Capture "day" DayDesc
           :> Capture "time in day" TimeInDay
           :> ReqBody '[JSON] OffDayType
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "Set a working half-day"
           :> "diary"
           :> Capture "day" DayDesc
           :> Capture "time in day" TimeInDay
           :> ReqBody '[JSON] [WorkOption]
           :> PutNoContent '[JSON] NoContent
   :<|> Summary "Delete a half-day"
           :> "diary"
           :> Capture "day" DayDesc
           :> Capture "time in day" TimeInDay
           :> DeleteNoContent '[JSON] NoContent

type HSBasicAuth = BasicAuth "basic-realm" Login


