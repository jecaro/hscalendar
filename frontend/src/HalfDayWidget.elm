module HalfDayWidget exposing 
    ( State
    , Mode(..)
    , Msg(..)
    , init
    , setDate
    , viewChangeHalfDayType
    , view
    , update
    )

import Browser.Dom exposing (focus)
import Date exposing (Date, Unit(..), fromCalendarDate)
import Html exposing 
    ( Html
    , button
    , div
    , input
    , label
    , option
    , p
    , select
    , textarea
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
import Html.Extra exposing (viewIf)
import Http exposing (Error(..))
import List exposing (map)
import Maybe.Extra exposing (isJust, isNothing)
import Result exposing (withDefault)
import RemoteData exposing (RemoteData(..), WebData)
import String exposing (length)
import Task exposing (attempt)
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
import Api.IdleDayType.Extended as IdleDayType exposing (fromString, toString)
import Api.Office.Extended as Office exposing (offices, toString)
import Api.TimeOfDay as TimeOfDay exposing (TimeOfDay, fromString, toString)

import Request exposing 
    ( getHalfDay
    , setArrived
    , setIdleDayType
    , setLeft
    , setNotes
    , setOffice
    , setProject
    , delete
    )

-- Types

type Mode
    = View
    | EditOffice
    | EditProject
    | EditNotes String
    | EditIdleDayType
    | EditArrived
    | EditLeft


type alias State = 
    { date : Date
    , timeInDay : TimeInDay
    , halfDay : WebData HalfDay
    , edit : WebData ()
    , mode : Mode
    }

type Msg
    = GotHalfDayResponse (WebData HalfDay)
    | GotEditResponse (WebData ())
    | ModeChanged Mode
    | EditHalfDaySent (Cmd Msg)
    | EditWasCanceled
    | NoOp

-- init

init : TimeInDay -> State
init timeInDay = 
    { date = fromCalendarDate 2020 Jan 1
    , timeInDay = timeInDay
    , halfDay = NotAsked
    , mode = View
    , edit = NotAsked
    }

setDate : State -> Date -> ( State, Cmd Msg )
setDate state date = 
    let
        state_ =
            { state
            | date = date
            , halfDay = Loading
            , edit = NotAsked
            , mode = View
            }
        cmd = getHalfDay GotHalfDayResponse state_.date state_.timeInDay
    in
        ( state_, cmd)

-- Update

update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        GotHalfDayResponse response ->
            ( { model | halfDay = response, mode = View }, Cmd.none )

        GotEditResponse response ->
            ( { model | edit = response }
            , getHalfDay GotHalfDayResponse model.date model.timeInDay )

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


viewChangeHalfDayType 
  : Date 
  -> TimeInDay 
  -> Maybe HalfDay 
  -> List Project 
  -> Html Msg
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
            div [ class "field" ]
                [ div [ class "control" ]
                    [ button 
                        [ class "button"
                        , onClick <| EditHalfDaySent <| delete GotEditResponse date timeInDay
                        , disabled <| isNothing halfDay
                        ]
                        [ text "Delete" ]
                    ]
                ]
            

        viewSetIdle =
            div [ class "field" ] 
                [ label [ class "label" ] [ text "Set as holiday" ] 
                , idleDayTypeSelect 
                        date 
                        timeInDay 
                        Nothing 
                        (isNothing halfDay || isWorked) 
                ]
           

        viewSetWorked =
            div [ class "field" ] 
                [ label [ class "label" ] [ text "Set as working" ]
                , projectSelect 
                        date 
                        timeInDay 
                        projects
                        Nothing
                        (isNothing halfDay || isIdle)
                ]
            
    in
        div [ ] 
            [ viewSetWorked
            , viewSetIdle 
            , viewDelete
            ]


view : WebData HalfDay -> Mode -> List Project -> Html Msg
view halfDay mode projects =
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
            EditHalfDaySent
                << setOffice GotEditResponse date timeInDay
                << withDefault Rennes
                << Office.fromString
    in
        div [ class "control" ]
            [ div [ class "select" ]
                [ select 
                    [ id "edit"
                    , onInput setEditHalfDay
                    , onBlur EditWasCanceled
                    ]
                    (map toOption offices)
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
        div [ class "control" ]
          [ div [ class "select" ]
              [ select 
                  [ disabled <| not enabled
                  , onBlur EditWasCanceled
                  , onInput 
                      <| EditHalfDaySent 
                      << setProject GotEditResponse date timeInDay
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
            EditHalfDaySent
                << setIdleDayType GotEditResponse date timeInDay
                << withDefault PaidLeave
                << IdleDayType.fromString
    in
        div [ class "control" ]
            [ div [ class "select" ]
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


arrivedOrLeftInput : (String -> Msg) -> TimeOfDay -> Html Msg
arrivedOrLeftInput callback timeOfDay =
    div [ class "control" ]
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


arrivedInput : Date -> TimeInDay -> TimeOfDay -> Html Msg
arrivedInput date timeInDay timeOfDay =
    let
        setEditHalfDay =
            EditHalfDaySent
                << setArrived GotEditResponse date timeInDay
                << withDefault timeOfDay
                << TimeOfDay.fromString
    in
        arrivedOrLeftInput setEditHalfDay timeOfDay


leftInput : Date -> TimeInDay -> TimeOfDay -> Html Msg
leftInput date timeInDay timeOfDay =
    let
        setEditHalfDay =
            EditHalfDaySent
                << setLeft GotEditResponse date timeInDay
                << withDefault timeOfDay
                << TimeOfDay.fromString
    in
        arrivedOrLeftInput setEditHalfDay timeOfDay


viewOffice : Mode -> Date -> TimeInDay -> Office -> Html Msg
viewOffice mode day timeInDay office = 
    div [ class "field" ]
        [ label [ class "label" ] [ text "Office" ]
        , case mode of
            EditOffice ->
                officeSelect day timeInDay office
            _ ->
                div [ onDoubleClick <| ModeChanged EditOffice ]
                    [ text <| Office.toString office ]
        ]


viewProject : Mode -> List Project -> Date -> TimeInDay -> Project -> Html Msg
viewProject mode projects day timeInDay project =
    div [ class "field" ]
        [ label [ class "label" ] [ text "Project" ]
        , case mode of
            EditProject -> 
                projectSelect 
                    day timeInDay projects (Just project) True 
            _ ->
                div [ onDoubleClick <| ModeChanged EditProject ]
                    [ text <| project.unProject ]
        ]


viewArrivedOrLeft : Mode -> TimeOfDay -> Html Msg
viewArrivedOrLeft mode timeOfDay =
    div [ onDoubleClick <| ModeChanged mode ]
        [ text <| TimeOfDay.toString timeOfDay ]


viewArrived : Mode -> Date -> TimeInDay -> TimeOfDay -> Html Msg
viewArrived mode day timeInDay arrived =
    div [ class "field" ]
        [ label [ class "label" ] [ text "Arrived" ]
        , case mode of
            EditArrived ->
                arrivedInput day timeInDay arrived
            _ ->
                viewArrivedOrLeft EditArrived arrived
        ]


viewLeft : Mode -> Date -> TimeInDay -> TimeOfDay -> Html Msg
viewLeft mode day timeInDay left =
    div [ class "field" ]
        [ label [ class "label" ] [ text "Left" ]
        , case mode of
            EditLeft ->
                leftInput day timeInDay left
            _ ->
                viewArrivedOrLeft EditLeft left
        ]


viewNotes : Mode -> Date -> TimeInDay -> Notes -> Html Msg
viewNotes mode day timeInDay notes =
    div [ class "field" ]
        [ label [class "label"] [ text "Notes" ]
        , case mode of
            EditNotes notes_ ->
                div [ ]
                    [ div [ class "field" ]
                        [ div [ class "control" ]
                            [ textarea
                                [ class "textarea"
                                , id "edit"
                                , onInput <| ModeChanged << EditNotes 
                                ]
                                [ text notes_ ]
                            ]
                        ]
                    , div [ class "field" ]
                        [ div [ class "control" ]
                            [ button
                                [ class "button"
                                , id "submit"
                                , onClick 
                                    <| EditHalfDaySent 
                                    <| setNotes GotEditResponse day timeInDay notes_
                                ]
                                [ text "Submit" ]
                            ]
                        ]
                    ]
            _ ->
                div [ onDoubleClick <| ModeChanged (EditNotes notes.unNotes) ]
                    [ text <|
                        if length notes.unNotes /= 0 
                            then notes.unNotes
                            else "Double click to edit"
                    ]
        ]


viewWorked : Mode -> List Project -> Worked -> Html Msg
viewWorked mode projects
    { workedDay
    , workedTimeInDay
    , workedArrived
    , workedLeft
    , workedOffice
    , workedNotes
    , workedProject } =
        div [ ]
            [ viewOffice mode workedDay workedTimeInDay workedOffice
            , viewArrived mode workedDay workedTimeInDay workedArrived
            , viewLeft mode workedDay workedTimeInDay workedLeft
            , viewProject mode projects workedDay workedTimeInDay workedProject
            , viewNotes mode workedDay workedTimeInDay workedNotes
            ]


viewIdle : Mode -> Idle -> Html Msg
viewIdle mode { idleDay, idleTimeInDay, idleDayType } =
    div [ class "field" ]
        [ label [ class "label" ] [ text "Day off" ]
        , case mode of
            EditIdleDayType -> 
                idleDayTypeSelect idleDay idleTimeInDay (Just idleDayType) True 
            _ ->
                div [ onDoubleClick <| ModeChanged EditIdleDayType ] 
                    [ text <| IdleDayType.toString idleDayType ]
        ]

viewNoEntry : Html msg
viewNoEntry = text "No entry"