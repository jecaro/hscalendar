module Request exposing
    ( addProject
    , delete
    , deleteProject
    , getHalfDay
    , getMonth
    , getProjects
    , renameProject
    , setArrived
    , setOffType
    , setLeft
    , setNotes
    , setOffice
    , setProject
    )

import Api
import Api.HalfDay as HalfDay exposing (decoder)
import Api.OffDayType as OffDayType exposing (encoder)
import Api.Month.Extended as Month exposing (toString)
import Api.MonthWithDays as MonthWithDays exposing (decoder)
import Api.Project as Project exposing (decoder)
import Api.RenameArgs as RenameArgs exposing (decoder)
import Api.TimeInDay.Extended as TimeInDay exposing (toString)
import Api.TimeOfDay exposing (TimeOfDay)
import Api.WorkOption as WorkOption exposing (encoder)
import Date exposing (Date, format)
import Http
    exposing
        ( Error(..)
        , emptyBody
        , expectJson
        , expectWhatever
        , get
        , jsonBody
        , post
        , request
        )
import Json.Decode as Decode exposing (list)
import Json.Encode as Encode exposing (list)
import RemoteData exposing (RemoteData(..), WebData, fromResult)


getMonth : (WebData Api.MonthWithDays -> m) -> Api.Month -> Cmd m
getMonth toMsg month =
    get
        { url = "/month/" ++ Month.toString month
        , expect = Http.expectJson (fromResult >> toMsg) MonthWithDays.decoder
        }


projectUrl : String
projectUrl =
    "/project"


getProjects : (WebData (List Api.Project) -> m) -> Cmd m
getProjects toMsg =
    get
        { url = projectUrl
        , expect =
            expectJson
                (fromResult >> toMsg)
                (Decode.list Project.decoder)
        }


deleteProject : (WebData () -> m) -> Api.Project -> Cmd m
deleteProject toMsg project =
    request
        { method = "DELETE"
        , headers = []
        , url = projectUrl
        , body = jsonBody <| Project.encoder project
        , expect = expectWhatever <| toMsg << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }


renameProject : (WebData () -> m) -> Api.Project -> Api.Project -> Cmd m
renameProject toMsg from to =
    request
        { method = "PUT"
        , headers = []
        , url = projectUrl
        , body = jsonBody <| RenameArgs.encoder { from = from, to = to }
        , expect = expectWhatever <| toMsg << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }


addProject : (WebData () -> m) -> Api.Project -> Cmd m
addProject toMsg project =
    post
        { url = projectUrl
        , body = jsonBody <| Project.encoder project
        , expect = expectWhatever <| toMsg << RemoteData.fromResult
        }


toInvertIsoString : Date -> String
toInvertIsoString =
    format "dd-MM-yyyy"


diaryUrl : Date -> Api.TimeInDay -> String
diaryUrl date timeInDay =
    "/diary/"
        ++ toInvertIsoString date
        ++ "/"
        ++ TimeInDay.toString timeInDay


offUrl : Date -> Api.TimeInDay -> String
offUrl date timeInDay =
    "/diary/off/"
        ++ toInvertIsoString date
        ++ "/"
        ++ TimeInDay.toString timeInDay


getHalfDay : (WebData Api.HalfDay -> m) -> Date -> Api.TimeInDay -> Cmd m
getHalfDay toMsg date timeInDay =
    get
        { url = diaryUrl date timeInDay
        , expect = Http.expectJson (fromResult >> toMsg) HalfDay.decoder
        }


setOffice : (WebData () -> m) -> Date -> Api.TimeInDay -> Api.Office -> Cmd m
setOffice toMsg date timeInDay office =
    let
        workOption =
            Api.MkSetOffice <| Api.SetOffice office
    in
    setWorkOption toMsg date timeInDay workOption


setNotes : (WebData () -> m) -> Date -> Api.TimeInDay -> String -> Cmd m
setNotes toMsg date timeInDay notes =
    let
        workOption =
            Api.MkSetNotes <| Api.SetNotes <| Api.Notes notes
    in
    setWorkOption toMsg date timeInDay workOption


setProject : (WebData () -> m) -> Date -> Api.TimeInDay -> String -> Cmd m
setProject toMsg date timeInDay project =
    let
        workOption =
            Api.MkSetProj <| Api.SetProj <| Api.Project project
    in
    setWorkOption toMsg date timeInDay workOption


setArrived : (WebData () -> m) -> Date -> Api.TimeInDay -> TimeOfDay -> Cmd m
setArrived toMsg date timeInDay timeOfDay =
    let
        workOption =
            Api.MkSetArrived <| Api.SetArrived timeOfDay
    in
    setWorkOption toMsg date timeInDay workOption


setLeft : (WebData () -> m) -> Date -> Api.TimeInDay -> TimeOfDay -> Cmd m
setLeft toMsg date timeInDay timeOfDay =
    let
        workOption =
            Api.MkSetLeft <| Api.SetLeft timeOfDay
    in
    setWorkOption toMsg date timeInDay workOption


setWorkOption : (WebData () -> m) -> Date -> Api.TimeInDay -> Api.WorkOption -> Cmd m
setWorkOption toMsg date timeInDay option =
    request
        { method = "PUT"
        , headers = []
        , url = diaryUrl date timeInDay
        , body = jsonBody <| Encode.list WorkOption.encoder [ option ]
        , expect = expectWhatever <| toMsg << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }


setOffType : (WebData () -> m) -> Date -> Api.TimeInDay -> Api.OffDayType -> Cmd m
setOffType toMsg date timeInDay offDayType =
    request
        { method = "PUT"
        , headers = []
        , url = offUrl date timeInDay
        , body = jsonBody <| OffDayType.encoder offDayType
        , expect = expectWhatever <| toMsg << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }


delete : (WebData () -> m) -> Date -> Api.TimeInDay -> Cmd m
delete toMsg date timeInDay =
    request
        { method = "DELETE"
        , headers = []
        , url = diaryUrl date timeInDay
        , body = emptyBody
        , expect = expectWhatever <| toMsg << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }
