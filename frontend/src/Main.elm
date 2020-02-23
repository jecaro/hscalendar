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

type Mode
    = View
    | EditOffice
    | EditProject
    | EditNotes String
    | EditIdleDayType
    | EditArrived
    | EditLeft


type alias Model =
    { date : Date
    , timeInDay : TimeInDay
    , halfDay : WebData HalfDay
    , projects : WebData (List Project)
    , edit : WebData ()
    , mode : Mode
    }


type Msg
    = SetDateAndTimeInDay Date TimeInDay
    | HalfDayResponse (WebData HalfDay)
    | EditResponse (WebData ())
    | ProjectsResponse (WebData (List Project))
    | SetMode Mode
    | SetEditHalfDay (Cmd Msg)
    | NoOp
    | Cancel


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { date = fromCalendarDate 2020 Jan 1
      , timeInDay = Morning
      , halfDay = NotAsked
      , projects = NotAsked
      , mode = View
      , edit = NotAsked
      }
    , batch 
        [ perform (\d -> SetDateAndTimeInDay d Morning) today
        , sendGetProjects
        ]
    )

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

sendGetHalfDay : Date -> TimeInDay -> Cmd Msg
sendGetHalfDay date timeInDay =
    get
        { url = diaryUrl date timeInDay
        , expect = Http.expectJson (fromResult >> HalfDayResponse) HalfDay.decoder
        }

sendGetProjects : Cmd Msg
sendGetProjects =
    get
        { url = "/project/"
        , expect = Http.expectJson 
            (fromResult >> ProjectsResponse) 
            (Decode.list Project.decoder)
        }

sendSetOffice : Date -> TimeInDay -> Office -> Cmd Msg
sendSetOffice date timeInDay office = 
    let
        workOption = MkSetOffice <| SetOffice office
    in
        sendSetWorkOption date timeInDay workOption


sendSetNotes : Date -> TimeInDay -> String -> Cmd Msg
sendSetNotes date timeInDay notes = 
    let
        workOption = MkSetNotes <| SetNotes <| Notes notes
    in
        sendSetWorkOption date timeInDay workOption


sendSetProject : Date -> TimeInDay -> String -> Cmd Msg
sendSetProject date timeInDay project =
    let
        workOption = MkSetProj <| SetProj <| Project project
    in
    sendSetWorkOption date timeInDay workOption


sendSetArrived : Date -> TimeInDay -> TimeOfDay -> Cmd Msg
sendSetArrived date timeInDay timeOfDay =
    let
        workOption = MkSetArrived <| SetArrived timeOfDay
    in
    sendSetWorkOption date timeInDay workOption


sendSetLeft : Date -> TimeInDay -> TimeOfDay -> Cmd Msg
sendSetLeft date timeInDay timeOfDay =
    let
        workOption = MkSetLeft <| SetLeft timeOfDay
    in
    sendSetWorkOption date timeInDay workOption


sendSetWorkOption : Date -> TimeInDay -> WorkOption -> Cmd Msg
sendSetWorkOption date timeInDay option =
    request
        { method = "PUT"
        , headers = []
        , url = diaryUrl date timeInDay
        , body = jsonBody <| Encode.list WorkOption.encoder [ option ]
        , expect = expectWhatever <| EditResponse << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }

sendSetIdleDayType : Date -> TimeInDay -> IdleDayType -> Cmd Msg
sendSetIdleDayType date timeInDay idleDayType =
    request
        { method = "PUT"
        , headers = []
        , url = idleUrl date timeInDay
        , body = jsonBody <| IdleDayType.encoder idleDayType
        , expect = expectWhatever <| EditResponse << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }

sendDelete : Date -> TimeInDay -> Cmd Msg
sendDelete date timeInDay = 
    request
        { method = "DELETE"
        , headers = []
        , url = diaryUrl date timeInDay
        , body = emptyBody
        , expect = expectWhatever <| EditResponse << RemoteData.fromResult
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDateAndTimeInDay date timeInDay ->
            let 
                model_ = 
                    { model 
                    | date = date
                    , timeInDay = timeInDay
                    , halfDay = Loading
                    , edit = NotAsked
                    , mode = View
                    }
            in 
                ( model_, sendGetHalfDay model_.date model_.timeInDay )
        
        HalfDayResponse response ->
            ( { model | halfDay = response, mode = View }, Cmd.none )

        ProjectsResponse response ->
            ( { model | projects = response }, Cmd.none )

        EditResponse response ->
            ( { model | edit = response }
            , sendGetHalfDay model.date model.timeInDay )

        SetMode mode ->
            let 
                cmd = 
                    if mode == View 
                    then Cmd.none
                    else attempt (\_ -> NoOp) (focus "edit")
            in
                ( { model | mode = mode }, cmd )

        SetEditHalfDay request -> 
            ( { model | edit = Loading }, request )
        
        Cancel -> 
            ( { model | mode = View }, Cmd.none )

        NoOp -> ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mode of
       EditNotes _ -> onMouseDown (outsideTarget [ "submit", "edit" ])
       _ -> Sub.none


{-This code has been found here
https://dev.to/margaretkrutikova/elm-dom-node-decoder-to-detect-click-outside-3ioh 
-}
outsideTarget : List String -> Decode.Decoder Msg
outsideTarget domEltIds =
    Decode.field "target" (isOutsideDomEltId domEltIds)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed Cancel
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

toInvertIsoString : Date -> String
toInvertIsoString = format "dd-MM-yyyy"


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

viewNav : Date -> TimeInDay -> Html Msg
viewNav date timeInDay =
    let
        previous = 
            case timeInDay of
                Morning -> SetDateAndTimeInDay (add Days -1 date) Afternoon
                Afternoon -> SetDateAndTimeInDay date Morning
        next = 
            case timeInDay of
                Morning -> SetDateAndTimeInDay date Afternoon
                Afternoon -> SetDateAndTimeInDay (add Days 1 date) Morning
        weekdayString = format "EEEE" date
        toOption timeInDay_ = 
            let
                timeInDayStr = TimeInDay.toString timeInDay_
            in
                option [ value timeInDayStr ] [ text timeInDayStr ]
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
            , div [ class "level-right" ]
                [ div [ class "level-item" ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ div
                                [ class "select"
                                , onInput 
                                    <| SetDateAndTimeInDay date 
                                        << withDefault Morning 
                                        << TimeInDay.fromString 
                                ]
                                [ select [
                                    value <| TimeInDay.toString timeInDay
                                    ]
                                    [ toOption Morning
                                    , toOption Afternoon
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]


viewChangeHalfDayType : Date -> TimeInDay -> Maybe HalfDay -> List Project -> Html Msg
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
                    , onClick <| SetEditHalfDay <| sendDelete date timeInDay
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


viewStatus : WebData HalfDay -> Mode -> List Project -> Html Msg
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


officeSelect : Date -> TimeInDay -> Office -> Html Msg
officeSelect date timeInDay current =
    let
        toOption office =
            option [ selected <| office == current ]
                [ text <| Office.toString office ]
        setEditHalfDay =
            SetEditHalfDay
                << sendSetOffice date timeInDay
                << withDefault Rennes
                << Office.fromString
    in
    div [ class "field" ]
        [ div [ class "control" ]
            [ div [ class "select" ]
                [ select 
                    [ id "edit"
                    , onInput setEditHalfDay
                    , onBlur Cancel
                    ]
                    [ toOption Rennes
                    , toOption Home
                    , toOption Poool
                    , toOption OutOfOffice
                    ]
                ]
            ]
        ]

projectSelect : Date -> TimeInDay -> List Project -> Maybe Project -> Bool -> Html Msg
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
                        , onBlur Cancel
                        , onInput <| SetEditHalfDay << sendSetProject date timeInDay
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
idleDayTypeSelect : Date -> TimeInDay -> Maybe IdleDayType -> Bool -> Html Msg
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
            SetEditHalfDay
                << sendSetIdleDayType date timeInDay
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
                        , onBlur Cancel
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

arrivedOrLeftInput : (String -> Msg) -> TimeOfDay -> Html Msg
arrivedOrLeftInput callback timeOfDay =
    div [ class "field", class "is-inline-block" ]
        [ div [ class "control" ]
            [ input
                [ class "input"
                , type_ "text"
                , value <| TimeOfDay.toString timeOfDay
                , onEnter callback
                , id "edit"
                , onBlur Cancel
                ]
                []
            ]
        ]


arrivedInput : Date -> TimeInDay -> TimeOfDay -> Html Msg
arrivedInput date timeInDay timeOfDay =
    let
        setEditHalfDay =
            SetEditHalfDay
                << sendSetArrived date timeInDay
                << withDefault timeOfDay
                << TimeOfDay.fromString
    in
    arrivedOrLeftInput setEditHalfDay timeOfDay

leftInput : Date -> TimeInDay -> TimeOfDay -> Html Msg
leftInput date timeInDay timeOfDay =
    let
        setEditHalfDay =
            SetEditHalfDay
                << sendSetLeft date timeInDay
                << withDefault timeOfDay
                << TimeOfDay.fromString
    in
    arrivedOrLeftInput setEditHalfDay timeOfDay


viewWorked : Mode -> List Project -> Worked -> Html Msg
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
                    th [ onDoubleClick <| SetMode EditOffice ]
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
                                    , onInput <| SetMode << EditNotes 
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
                                        <| SetEditHalfDay 
                                        <| sendSetNotes workedDay workedTimeInDay notes
                                    ]
                                    [ text "Submit"
                                    ]
                                ]
                            ]
                        ]
                _ ->
                    td [ onDoubleClick <| SetMode (EditNotes workedNotes.unNotes) ]
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
                    th [ onDoubleClick <| SetMode EditProject ]
                        [ text <| workedProject.unProject ]
        divViewArrivedOrLeft mode_ timeOfDay =
            div
                [ class "is-inline-block"
                , onDoubleClick <| SetMode mode_
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


viewIdle : Mode -> Idle -> Html Msg
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
                    th [ onDoubleClick <| SetMode EditIdleDayType ]
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
        viewChangeHalfDayType_ = 
            viewMaybe 
                (\projects -> 
                    viewChangeHalfDayType 
                        model.date 
                        model.timeInDay 
                        (RemoteData.toMaybe model.halfDay) 
                        projects 
                )
                (RemoteData.toMaybe model.projects)
        viewStatus_ =
            viewMaybe 
                (\projects -> viewStatus model.halfDay model.mode projects)
                (RemoteData.toMaybe model.projects)

    in
        div [] 
            [ viewHero 
            , section [ class "section" ] 
                [ div [ class "content" ] [ viewNav model.date model.timeInDay ]
                ]
            , section [ class "section" ] 
                [ div [ class "content" ] [ viewStatus_ ]
                ]
            , section [ class "section" ] 
                [ div [ class "content" ] [ viewChangeHalfDayType_ ]
                ]
            ]

