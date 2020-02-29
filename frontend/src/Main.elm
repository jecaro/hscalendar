module Main exposing (main)

import Browser
import Browser.Events exposing (onMouseDown)
import Browser.Dom exposing (focus)
import Date exposing 
    ( Date
    , Unit(..)
    , add
    , format
    , fromCalendarDate
    , today
    , toIsoString
    )
import Html exposing 
    ( Html
    , button
    , div
    , h1
    , input
    , nav
    , option
    , p
    , section
    , select
    , table
    , tbody
    , textarea
    , td
    , th
    , tr
    , text
    )
import Html.Attributes exposing 
    ( class
    , disabled
    , id
    , selected
    , type_
    , value
    )
import Html.Events exposing (onBlur, onClick, onDoubleClick, onInput)
import Html.Events.Extended exposing (onEnter)
import Html.Extra exposing (viewIf, viewMaybe)
import Http exposing 
    ( Error(..)
    , emptyBody
    , expectJson
    , expectWhatever
    , get
    , jsonBody
    , request
    )
import Json.Decode as Decode exposing (list)
import Json.Encode as Encode exposing (list)
import List exposing (member)
import Maybe.Extra exposing (isJust, isNothing)
import Platform.Cmd exposing (batch)
import Result exposing (withDefault)
import RemoteData exposing (RemoteData(..), WebData, fromResult)
import String exposing (concat)
import Task exposing (attempt, perform)
import Time exposing (Month(..))

import Api exposing 
    ( HalfDay(..)
    , Idle
    , IdleDayType(..)
    , Notes
    , Office(..)
    , Project
    , SetArrived(..)
    , SetLeft(..)
    , SetNotes(..)
    , SetOffice(..)
    , SetProj(..)
    , TimeInDay(..)
    , WorkOption(..)
    , Worked
    )
import Api.HalfDay as HalfDay exposing (decoder)
import Api.IdleDayType as IdleDayType exposing (encoder)
import Api.IdleDayType.Extended as IdleDayType exposing (fromString, toString)
import Api.Office.Extended as Office exposing (toString)
import Api.Project as Project exposing (decoder)
import Api.TimeInDay.Extended as TimeInDay exposing (fromString, toString)
import Api.TimeOfDay as TimeOfDay exposing (TimeOfDay, fromString, toString)
import Api.WorkOption as WorkOption exposing (encoder)

-- Types

type Mode
    = View
    | EditOffice
    | EditProject
    | EditNotes String
    | EditIdleDayType
    | EditArrived
    | EditLeft


type alias Model =
    { morning : ModelHalfDay
    , afternoon : ModelHalfDay
    }

type alias ModelHalfDay = 
    { date : Date
    , timeInDay : TimeInDay
    , halfDay : WebData HalfDay
    , projects : WebData (List Project)
    , edit : WebData ()
    , mode : Mode
    }

type Msg 
    = GotProjectsResponse (WebData (List Project))
    | DateChanged Date
    | MorningMsg MsgHalfDay 
    | AfternoonMsg MsgHalfDay

type MsgHalfDay
    = GotHalfDayResponse (WebData HalfDay)
    | GotEditResponse (WebData ())
    | ModeChanged Mode
    | EditHalfDaySent (Cmd MsgHalfDay)
    | EditWasCanceled
    | NoOp

-- Main

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

-- Init

init : flags -> ( Model, Cmd Msg )
init _ =
    let
        baseModel = 
            { date = fromCalendarDate 2020 Jan 1
            , timeInDay = Morning
            , halfDay = NotAsked
            , projects = NotAsked
            , mode = View
            , edit = NotAsked
            }
    in
    ( { morning = { baseModel | timeInDay = Morning }
      , afternoon = { baseModel | timeInDay = Afternoon }
      }
    , batch 
        [ perform (\d -> DateChanged d) today
        , getProjects
        ]
    )

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        DateChanged date ->
            let 
                morning = model.morning
                morning_ = 
                    { morning
                    | date = date
                    , halfDay = Loading
                    , edit = NotAsked
                    , mode = View
                    }
                afternoon = model.afternoon
                afternoon_ = 
                    { afternoon
                    | date = date
                    , halfDay = Loading
                    , edit = NotAsked
                    , mode = View
                    }
                model_ = 
                    { model
                    | morning = morning_
                    , afternoon = afternoon_
                    }
                cmdGetMorning = 
                    Cmd.map MorningMsg (getHalfDay model_.morning.date model_.morning.timeInDay) 
                cmdGetAfternoon = 
                    Cmd.map AfternoonMsg (getHalfDay model_.afternoon.date model_.afternoon.timeInDay) 
            in 
                ( model_, batch [ cmdGetMorning, cmdGetAfternoon ] )

        GotProjectsResponse response ->
            let
                morning = model.morning
                morning_ = 
                    { morning
                    | projects = response
                    }                
                afternoon = model.afternoon
                afternoon_ = 
                    { afternoon
                    | projects = response
                    }                
            in
            
            ( { model | morning = morning_, afternoon = afternoon_ }, Cmd.none )
        
        MorningMsg morningMsg -> 
            let
                (morning, cmd) = updateHalfDay morningMsg model.morning
            in
                ( {model | morning = morning}, Cmd.map MorningMsg cmd )

        AfternoonMsg afternoonMsg -> 
            let
                (afternoon, cmd) = updateHalfDay afternoonMsg model.afternoon
            in
                ( {model | afternoon = afternoon}, Cmd.map AfternoonMsg cmd )




updateHalfDay : MsgHalfDay -> ModelHalfDay -> ( ModelHalfDay, Cmd MsgHalfDay )
updateHalfDay msg model =
    case msg of
        GotHalfDayResponse response ->
            ( { model | halfDay = response, mode = View }, Cmd.none )

        GotEditResponse response ->
            ( { model | edit = response }
            , getHalfDay model.date model.timeInDay )

        ModeChanged mode ->
            let 
                cmd = 
                    if mode == View 
                    then Cmd.none
                    else attempt (\_ -> NoOp) (focus "edit")
            in
                ( { model | mode = mode }, cmd )

        EditHalfDaySent request -> 
            ( { model | edit = Loading }, request )
        
        EditWasCanceled -> 
            ( { model | mode = View }, Cmd.none )

        NoOp -> ( model, Cmd.none )

-- View

viewHero : Html msg
viewHero =
    section [ class "hero", class "is-primary" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ]
                    [ text "HSCalendar"
                    ]
                ]
            ]
        ]

viewNav : Date -> Html Msg
viewNav date =
    let
        previous = DateChanged (add Days -1 date)
        next = DateChanged (add Days 1 date)
        weekdayString = format "EEEE" date
    in
        nav [ class "level" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ]
                    [ div [ class "field", class "has-addons" ]
                        [ div [ class "control" ]
                            [ button
                                [ class "button"
                                , onClick previous
                                ]
                                [ text "Prev" ]
                            ]
                        , div [ class "control" ] 
                            [ button
                                [ class "button"
                                , onClick next
                                ]
                                [ text "Next" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "level-item" ]
                [ p [ class "subtitle" ]
                    [ text <| toIsoString date ++ " " ++ weekdayString ]
                ]
            ]


viewChangeHalfDayType : Date -> TimeInDay -> Maybe HalfDay -> List Project -> Html MsgHalfDay
viewChangeHalfDayType date timeInDay halfDay projects =
    let
        isWorked = 
            case halfDay of
                Nothing -> False
                Just (MkHalfDayWorked _) -> True
                Just (MkHalfDayIdle _) -> False
        isIdle = 
            case halfDay of
                Nothing -> False
                Just (MkHalfDayWorked _) -> False
                Just (MkHalfDayIdle _) -> True

        viewDelete =
            [ div [ class "level-item" ]
                [ button 
                    [ class "button"
                    , onClick <| EditHalfDaySent <| delete date timeInDay
                    , disabled <| isNothing halfDay
                    ] [ text "Delete" ]
                ]
            ]

        viewSetIdle =
            [ div [ class "level-item" ] 
                [ div [ class "label" ] [ text "Set as holiday" ]
                ]
            , div [ class "level-item" ]
                [ idleDayTypeSelect 
                    date 
                    timeInDay 
                    Nothing 
                    (isNothing halfDay || isWorked) 
                ]
            ]

        viewSetWorked =
            [ div [ class "level-item" ] 
                [ div [ class "label" ] [ text "Set as working" ] 
                ]
            , div [ class "level-item" ]
                [ projectSelect 
                    date 
                    timeInDay 
                    projects
                    Nothing
                    (isNothing halfDay || isIdle)
                ]
            ]
    in
        div [ class "level" ] 
            [ div [ class "level-left" ] (viewSetIdle ++ viewSetWorked)
            , div [ class "level-right" ] viewDelete
            ]


viewStatus : WebData HalfDay -> Mode -> List Project -> Html MsgHalfDay
viewStatus halfDay mode projects =
    case halfDay of
        NotAsked -> p [] []
        Loading -> p [] [ text "Loading ..." ]
        Failure (BadStatus 404) -> viewNoEntry
        Failure _ -> p [] [ text "Error loading half-day" ]
        Success (MkHalfDayWorked worked) ->
            viewWorked mode projects worked
        Success (MkHalfDayIdle idle) ->
            viewIdle mode idle


officeSelect : Date -> TimeInDay -> Office -> Html MsgHalfDay
officeSelect date timeInDay current =
    let
        toOption office =
            option [ selected <| office == current ]
                [ text <| Office.toString office ]
        setEditHalfDay =
            EditHalfDaySent
                << setOffice date timeInDay
                << withDefault Rennes
                << Office.fromString
    in
    div [ class "field" ]
        [ div [ class "control" ]
            [ div [ class "select" ]
                [ select 
                    [ id "edit"
                    , onInput setEditHalfDay
                    , onBlur EditWasCanceled
                    ]
                    [ toOption Rennes
                    , toOption Home
                    , toOption Poool
                    , toOption OutOfOffice
                    ]
                ]
            ]
        ]

projectSelect : Date -> TimeInDay -> List Project -> Maybe Project -> Bool -> Html MsgHalfDay
projectSelect date timeInDay projects current enabled =
    let
        defaultValue = 
            case current of
                Nothing -> Project ""
                Just current_ -> current_
        projects_ =
            case current of
                Nothing -> Project "" :: projects
                Just _ -> projects
        toOption project =
            option
                [ value project.unProject
                , selected <| project == defaultValue
                ]
                [ text project.unProject ]
    in
        div [ class "field" ]
            [ div [ class "control" ]
                [ div [ class "select" ]
                    [ select 
                        [ disabled <| not enabled
                        , onBlur EditWasCanceled
                        , onInput <| EditHalfDaySent << setProject date timeInDay
                        , value defaultValue.unProject
                        -- if there is a default value, it means that it is the
                        -- select used for editing current halfday
                        , id <| if isJust current
                            then "edit" 
                            else ""
                        ]
                        <| List.map toOption projects_ 
                    ]
                ]
            ]

{- Create select for all the IdleDayType. If Maybe IdleDayType is Nothing, the
function prepend an empty item before the different types.
-}
idleDayTypeSelect : Date -> TimeInDay -> Maybe IdleDayType -> Bool -> Html MsgHalfDay
idleDayTypeSelect date timeInDay current enabled =
    let
        defaultValue = 
            case current of
                Nothing -> ""
                Just current_ -> IdleDayType.toString current_

        toOption idleDayType =
            let
                idleDayTypeStr = IdleDayType.toString idleDayType
            in
                option 
                    [ value idleDayTypeStr
                    , selected (idleDayTypeStr == defaultValue)
                    ]
                    [ text <| IdleDayType.toString idleDayType ]

        setEditHalfDay =
            EditHalfDaySent
                << setIdleDayType date timeInDay
                << withDefault PaidLeave
                << IdleDayType.fromString
    in
        div [ class "field" ]
            [ div [ class "control" ]
                [ div
                    [ class "select"
                    ]
                    [ select 
                        [ disabled <| not enabled 
                        , onBlur EditWasCanceled
                        , onInput setEditHalfDay
                        , value defaultValue
                        -- if there is a default value, it means that it is the
                        -- select used for editing current halfday
                        , id <| if isJust current
                            then "edit" 
                            else ""
                        ] <|
                        -- Prepend empty case if needed
                        [ viewIf (isNothing current) 
                            <| option [ value "" ] [ text "" ]
                        , toOption PaidLeave
                        , toOption FamilyEvent
                        , toOption RTTE
                        , toOption RTTS
                        , toOption UnpaidLeave
                        , toOption PublicHoliday
                        , toOption PartTime
                        ]
                    ]
                ]
            ]

arrivedOrLeftInput : (String -> MsgHalfDay) -> TimeOfDay -> Html MsgHalfDay
arrivedOrLeftInput callback timeOfDay =
    div [ class "field", class "is-inline-block" ]
        [ div [ class "control" ]
            [ input
                [ class "input"
                , type_ "text"
                , value <| TimeOfDay.toString timeOfDay
                , onEnter callback
                , id "edit"
                , onBlur EditWasCanceled
                ]
                []
            ]
        ]


arrivedInput : Date -> TimeInDay -> TimeOfDay -> Html MsgHalfDay
arrivedInput date timeInDay timeOfDay =
    let
        setEditHalfDay =
            EditHalfDaySent
                << setArrived date timeInDay
                << withDefault timeOfDay
                << TimeOfDay.fromString
    in
    arrivedOrLeftInput setEditHalfDay timeOfDay

leftInput : Date -> TimeInDay -> TimeOfDay -> Html MsgHalfDay
leftInput date timeInDay timeOfDay =
    let
        setEditHalfDay =
            EditHalfDaySent
                << setLeft date timeInDay
                << withDefault timeOfDay
                << TimeOfDay.fromString
    in
    arrivedOrLeftInput setEditHalfDay timeOfDay


viewWorked : Mode -> List Project -> Worked -> Html MsgHalfDay
viewWorked mode projects
    { workedDay
    , workedTimeInDay
    , workedArrived
    , workedLeft
    , workedOffice
    , workedNotes
    , workedProject } =
    let
        cellOffice =
            case mode of
                EditOffice ->
                    th [] [ officeSelect workedDay workedTimeInDay workedOffice ]
                _ ->
                    th [ onDoubleClick <| ModeChanged EditOffice ]
                        [ text <| Office.toString workedOffice ]
        cellNotes =
            case mode of
                EditNotes notes ->
                    td [ ]
                        [ div [ class "field" ]
                            [ div [ class "control" ]
                                [ textarea
                                    [ class "textarea"
                                    , id "edit"
                                    , onInput <| ModeChanged << EditNotes 
                                    ]
                                    [ text notes
                                    ]
                                ]
                            ]
                        , div [ class "field" ]
                            [ div [ class "control" ]
                                [ button
                                    [ class "button"
                                    , id "submit"
                                    , onClick 
                                        <| EditHalfDaySent 
                                        <| setNotes workedDay workedTimeInDay notes
                                    ]
                                    [ text "Submit"
                                    ]
                                ]
                            ]
                        ]
                _ ->
                    td [ onDoubleClick <| ModeChanged (EditNotes workedNotes.unNotes) ]
                        [ text <| workedNotes.unNotes ]
        cellProject =
            case mode of
                EditProject ->
                    th [] 
                        [ projectSelect 
                            workedDay 
                            workedTimeInDay 
                            projects 
                            (Just workedProject)
                            True 
                        ]
                _ ->
                    th [ onDoubleClick <| ModeChanged EditProject ]
                        [ text <| workedProject.unProject ]
        divViewArrivedOrLeft mode_ timeOfDay =
            div
                [ class "is-inline-block"
                , onDoubleClick <| ModeChanged mode_
                ]
                [ text <| TimeOfDay.toString timeOfDay ]
        divArrived =
            case mode of
                EditArrived ->
                    arrivedInput workedDay workedTimeInDay workedArrived
                _ ->
                    divViewArrivedOrLeft EditArrived workedArrived
        divLeft =
            case mode of
                EditLeft ->
                    leftInput workedDay workedTimeInDay workedLeft
                _ ->
                    divViewArrivedOrLeft EditLeft workedLeft
    in
    table [ class "table" ]
        [ tbody []
            [ tr []
                [ cellOffice
                , td []
                    [ divArrived
                    , div [ class "is-inline-block" ]
                        [ text "\u{00A0}-\u{00A0}" ]
                    , divLeft
                    ]
                ]
            , tr []
                [ cellProject
                , cellNotes
                ]
            ]
        ]


viewIdle : Mode -> Idle -> Html MsgHalfDay
viewIdle mode { idleDay, idleTimeInDay, idleDayType } =
    let
        cellIdleDayType =
            case mode of
                EditIdleDayType ->
                    th [] 
                        [ idleDayTypeSelect 
                            idleDay 
                            idleTimeInDay 
                            (Just idleDayType) 
                            True 
                        ]
                _ ->
                    th [ onDoubleClick <| ModeChanged EditIdleDayType ]
                        [ text <| IdleDayType.toString idleDayType ]
    in
        table [ class "table" ]
            [ tbody []
                [ tr [] [ cellIdleDayType]
                ]
            ]

viewNoEntry : Html msg
viewNoEntry =
    table [ class "table" ]
        [ tbody []
            [ tr []
                [ th [] [ text "No entry" ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        viewMorningChangeHalfDayType_ = 
            viewMaybe 
                (\projects -> 
                    viewChangeHalfDayType 
                        model.morning.date 
                        model.morning.timeInDay 
                        (RemoteData.toMaybe model.morning.halfDay) 
                        projects 
                )
                (RemoteData.toMaybe model.morning.projects)
        viewAfternoonChangeHalfDayType_ = 
            viewMaybe 
                (\projects -> 
                    viewChangeHalfDayType 
                        model.afternoon.date 
                        model.afternoon.timeInDay 
                        (RemoteData.toMaybe model.afternoon.halfDay) 
                        projects 
                )
                (RemoteData.toMaybe model.morning.projects)
        viewMorningStatus_ =
            viewMaybe 
                (\projects -> viewStatus model.morning.halfDay model.morning.mode projects)
                (RemoteData.toMaybe model.morning.projects)
        viewAfternoonStatus_ =
            viewMaybe 
                (\projects -> viewStatus model.afternoon.halfDay model.afternoon.mode projects)
                (RemoteData.toMaybe model.afternoon.projects)

    in
        div [] 
            [ viewHero 
            , section [ class "section" ] 
                [ div [ class "content" ] [ viewNav model.morning.date ]
                ]
            , section [ class "section" ] 
                [ div [ class "content" ] [ Html.map MorningMsg viewMorningStatus_ ]
                ]
            , section [ class "section" ] 
                [ div [ class "content" ] [ Html.map MorningMsg viewMorningChangeHalfDayType_ ]
                ]
            , section [ class "section" ] 
                [ div [ class "content" ] [ Html.map AfternoonMsg viewAfternoonStatus_ ]
                ]
            , section [ class "section" ] 
                [ div [ class "content" ] [ Html.map AfternoonMsg viewAfternoonChangeHalfDayType_ ]
                ]
            ]

-- Requests

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

getHalfDay : Date -> TimeInDay -> Cmd MsgHalfDay
getHalfDay date timeInDay =
    get
        { url = diaryUrl date timeInDay
        , expect = Http.expectJson (fromResult >> GotHalfDayResponse) HalfDay.decoder
        }

getProjects : Cmd Msg
getProjects =
    get
        { url = "/project/"
        , expect = Http.expectJson 
            (fromResult >> GotProjectsResponse) 
            (Decode.list Project.decoder)
        }

setOffice : Date -> TimeInDay -> Office -> Cmd MsgHalfDay
setOffice date timeInDay office = 
    let
        workOption = MkSetOffice <| SetOffice office
    in
        setWorkOption date timeInDay workOption


setNotes : Date -> TimeInDay -> String -> Cmd MsgHalfDay
setNotes date timeInDay notes = 
    let
        workOption = MkSetNotes <| SetNotes <| Notes notes
    in
        setWorkOption date timeInDay workOption


setProject : Date -> TimeInDay -> String -> Cmd MsgHalfDay
setProject date timeInDay project =
    let
        workOption = MkSetProj <| SetProj <| Project project
    in
    setWorkOption date timeInDay workOption


setArrived : Date -> TimeInDay -> TimeOfDay -> Cmd MsgHalfDay
setArrived date timeInDay timeOfDay =
    let
        workOption = MkSetArrived <| SetArrived timeOfDay
    in
    setWorkOption date timeInDay workOption


setLeft : Date -> TimeInDay -> TimeOfDay -> Cmd MsgHalfDay
setLeft date timeInDay timeOfDay =
    let
        workOption = MkSetLeft <| SetLeft timeOfDay
    in
    setWorkOption date timeInDay workOption


setWorkOption : Date -> TimeInDay -> WorkOption -> Cmd MsgHalfDay
setWorkOption date timeInDay option =
    request
        { method = "PUT"
        , headers = []
        , url = diaryUrl date timeInDay
        , body = jsonBody <| Encode.list WorkOption.encoder [ option ]
        , expect = expectWhatever <| GotEditResponse << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }

setIdleDayType : Date -> TimeInDay -> IdleDayType -> Cmd MsgHalfDay
setIdleDayType date timeInDay idleDayType =
    request
        { method = "PUT"
        , headers = []
        , url = idleUrl date timeInDay
        , body = jsonBody <| IdleDayType.encoder idleDayType
        , expect = expectWhatever <| GotEditResponse << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }

delete : Date -> TimeInDay -> Cmd MsgHalfDay
delete date timeInDay = 
    request
        { method = "DELETE"
        , headers = []
        , url = diaryUrl date timeInDay
        , body = emptyBody
        , expect = expectWhatever <| GotEditResponse << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.morning.mode of
       EditNotes _ -> Sub.map MorningMsg (onMouseDown (outsideTarget [ "submit", "edit" ]))
       _ -> Sub.none


{-This code has been found here
https://dev.to/margaretkrutikova/elm-dom-node-decoder-to-detect-click-outside-3ioh 
-}
outsideTarget : List String -> Decode.Decoder MsgHalfDay
outsideTarget domEltIds =
    Decode.field "target" (isOutsideDomEltId domEltIds)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed EditWasCanceled
                else
                    Decode.fail <| "inside " ++ concat domEltIds
            )


isOutsideDomEltId : List String -> Decode.Decoder Bool
isOutsideDomEltId domEltIds =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if member id domEltIds then
                        -- found match by id
                        Decode.succeed False
                    else
                        -- try next decoder
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> isOutsideDomEltId domEltIds |> Decode.field "parentNode")
        -- fallback if all previous decoders failed
        , Decode.succeed True
        ]

