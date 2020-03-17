module Request exposing 
    ( addProject
    , delete
    , deleteProject
    , getHalfDay
    , getProjects
    , setArrived
    , setIdleDayType
    , setLeft
    , setNotes
    , setOffice
    , setProject
    )


import Date exposing (Date, format)
import Http exposing 
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

import Api exposing 
    ( HalfDay
    , IdleDayType
    , Notes
    , Office
    , Project
    , SetArrived(..)
    , SetLeft(..)
    , SetNotes(..)
    , SetOffice(..)
    , SetProj(..)
    , TimeInDay
    , WorkOption(..)
    )
import Api.HalfDay as HalfDay exposing (decoder)
import Api.IdleDayType as IdleDayType exposing (encoder)
import Api.Project as Project exposing (decoder)
import Api.TimeInDay.Extended as TimeInDay exposing (toString)
import Api.TimeOfDay exposing (TimeOfDay)
import Api.WorkOption as WorkOption exposing (encoder)

projectUrl : String
projectUrl = "/project"

getProjects : (WebData (List Project) -> m) -> Cmd m
getProjects toMsg =
    get
        { url = projectUrl
        , expect = expectJson 
            (fromResult >> toMsg) 
            (Decode.list Project.decoder)
        }

deleteProject : (WebData () -> m) -> Project -> Cmd m
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

addProject : (WebData () -> m) -> Project -> Cmd m
addProject toMsg project =
    post
        { url = projectUrl
        , body = jsonBody <| Project.encoder project
        , expect = expectWhatever <| toMsg << RemoteData.fromResult
        }




toInvertIsoString : Date -> String
toInvertIsoString = format "dd-MM-yyyy"

diaryUrl : Date -> TimeInDay -> String
diaryUrl date timeInDay 
    = "/diary/" 
    ++ toInvertIsoString date 
    ++ "/" 
    ++ TimeInDay.toString timeInDay

idleUrl : Date -> TimeInDay -> String
idleUrl date timeInDay
    = "/diary/idle/" 
    ++ toInvertIsoString date 
    ++ "/" 
    ++ TimeInDay.toString timeInDay

getHalfDay : ((WebData HalfDay) -> m) -> Date -> TimeInDay -> Cmd m
getHalfDay toMsg date timeInDay =
    get
        { url = diaryUrl date timeInDay
        , expect = Http.expectJson (fromResult >> toMsg) HalfDay.decoder
        }

setOffice : (WebData () -> m) -> Date -> TimeInDay -> Office -> Cmd m
setOffice toMsg date timeInDay office = 
    let
        workOption = MkSetOffice <| SetOffice office
    in
        setWorkOption toMsg date timeInDay workOption


setNotes : (WebData () -> m) -> Date -> TimeInDay -> String -> Cmd m
setNotes toMsg date timeInDay notes = 
    let
        workOption = MkSetNotes <| SetNotes <| Notes notes
    in
        setWorkOption toMsg date timeInDay workOption


setProject : (WebData () -> m) -> Date -> TimeInDay -> String -> Cmd m
setProject toMsg date timeInDay project =
    let
        workOption = MkSetProj <| SetProj <| Project project
    in
        setWorkOption toMsg date timeInDay workOption


setArrived : (WebData() -> m) -> Date -> TimeInDay -> TimeOfDay -> Cmd m
setArrived toMsg date timeInDay timeOfDay =
    let
        workOption = MkSetArrived <| SetArrived timeOfDay
    in
        setWorkOption toMsg date timeInDay workOption


setLeft : (WebData () -> m) -> Date -> TimeInDay -> TimeOfDay -> Cmd m
setLeft toMsg date timeInDay timeOfDay =
    let
        workOption = MkSetLeft <| SetLeft timeOfDay
    in
        setWorkOption toMsg date timeInDay workOption


setWorkOption : (WebData () -> m) -> Date -> TimeInDay -> WorkOption -> Cmd m
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

setIdleDayType : (WebData () -> m) -> Date -> TimeInDay -> IdleDayType -> Cmd m
setIdleDayType toMsg date timeInDay idleDayType =
    request
        { method = "PUT"
        , headers = []
        , url = idleUrl date timeInDay
        , body = jsonBody <| IdleDayType.encoder idleDayType
        , expect = expectWhatever <| toMsg << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }

delete : (WebData() -> m) -> Date -> TimeInDay -> Cmd m
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


