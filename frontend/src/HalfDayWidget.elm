module HalfDayWidget exposing 
    ( State
    , Mode(..)
    , Msg(..)
    , init
    , setDate
    , view
    , update
    )

import Browser.Dom exposing (focus)
import Date exposing (Date, Unit(..), fromCalendarDate)
import Html exposing 
    ( Attribute
    , Html
    , button
    , div
    , header
    , i
    , input
    , label
    , option
    , p
    , select
    , span
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
import Html.Extra exposing (nothing, viewIf)
import Http exposing (Error(..))
import List exposing (map)
import Maybe.Extra exposing (isJust, isNothing)
import Result exposing (withDefault)
import RemoteData exposing (RemoteData(..), WebData, isLoading, toMaybe)
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
import Api.TimeInDay.Extended as TimeInDay exposing (toString)
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
    , halfDayDisplayed : Maybe HalfDay
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
    , halfDayDisplayed = Nothing
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
            , halfDayDisplayed = toMaybe state.halfDay
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
            ( { model 
                | halfDayDisplayed = toMaybe response
                , halfDay = response
                , mode = View 
                }
            , Cmd.none 
            )

        GotEditResponse response ->
            ( { model | edit = response, halfDay = Loading }
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
            viewHorizontalForm "Set as holiday"
                [ idleDayTypeSelect 
                        date 
                        timeInDay 
                        Nothing 
                        (isNothing halfDay || isWorked) 
                    ]

        viewSetWorked =
            viewHorizontalForm "Set as working"
                [ projectSelect 
                        date 
                        timeInDay 
                        projects
                        Nothing
                        (isNothing halfDay || isIdle)
                ]
    in
        div [ ] 
            [ viewIf (isIdle || isNothing halfDay) viewSetWorked
            , viewIf (isWorked || isNothing halfDay) viewSetIdle 
            , viewDelete
            ]


view : State -> List Project -> Html Msg
view state projects =
    let
        halfDayHtml = 
            case state.halfDayDisplayed of
                Nothing -> [ viewNoEntry ]
                Just (MkHalfDayWorked worked) ->
                    viewWorked state.mode projects worked
                Just (MkHalfDayIdle idle) ->
                    viewIdle state.mode idle
        changeHalDayHtml = 
            [ viewChangeHalfDayType 
                state.date 
                state.timeInDay 
                state.halfDayDisplayed 
                projects 
            ]
        loadingIcon = 
            if isLoading state.halfDay || isLoading state.edit
                then 
                    div [ class "card-header-icon" ]
                        [ span [ class "icon" ] 
                            [ i [ class "fas fa-spinner fa-spin" ] [ ] ] 
                        ]
                else
                    nothing 
    in
        div [ class "card" ] 
            [ header [ class "card-header" ] 
                [ div [ class "card-header-title" ] 
                    [ div [ class "title", class "is-4" ]
                        [ text <| TimeInDay.toString state.timeInDay ] 
                    ]
                , loadingIcon
                ]
            , div [ class "card-content" ]
                [ div [ class "content" ] 
                    (halfDayHtml ++ changeHalDayHtml)
                ]
            ] 
    

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
    viewHorizontalForm "Office"
        [ case mode of
            EditOffice ->
                officeSelect day timeInDay office
            _ ->
                div [ onDoubleClick <| ModeChanged EditOffice ]
                    [ text <| Office.toString office ]
        ]


viewProject : Mode -> List Project -> Date -> TimeInDay -> Project -> Html Msg
viewProject mode projects day timeInDay project =
    viewHorizontalForm "Project"
        [ case mode of
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
    viewHorizontalForm "Arrived" 
        [ case mode of
                EditArrived ->
                    arrivedInput day timeInDay arrived
                _ ->
                    viewArrivedOrLeft EditArrived arrived
        ]


viewLeft : Mode -> Date -> TimeInDay -> TimeOfDay -> Html Msg
viewLeft mode day timeInDay left =
    viewHorizontalForm "Left"
        [ case mode of
                EditLeft ->
                    leftInput day timeInDay left
                _ ->
                    viewArrivedOrLeft EditLeft left
        ]


viewNotes : Mode -> Date -> TimeInDay -> Notes -> List (Html Msg)
viewNotes mode day timeInDay notes =
        case mode of
            EditNotes notes_ ->
                [ viewHorizontalForm "Notes"
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
                    ]
                , viewHorizontalForm ""
                    [ div [ class "field" ]
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
                ]
            _ ->
                [ viewHorizontalFormWithAttr 
                    [ onDoubleClick <| ModeChanged (EditNotes notes.unNotes) ]
                    "Notes"
                    [ text <|
                            if length notes.unNotes /= 0 
                                then notes.unNotes
                                else "Double click to edit"
                    ]
                ]


viewWorked : Mode -> List Project -> Worked -> List (Html Msg)
viewWorked mode projects
    { workedDay
    , workedTimeInDay
    , workedArrived
    , workedLeft
    , workedOffice
    , workedNotes
    , workedProject } =
        [ viewOffice mode workedDay workedTimeInDay workedOffice
        , viewArrived mode workedDay workedTimeInDay workedArrived
        , viewLeft mode workedDay workedTimeInDay workedLeft
        , viewProject mode projects workedDay workedTimeInDay workedProject
        ] ++ viewNotes mode workedDay workedTimeInDay workedNotes


viewIdle : Mode -> Idle -> List (Html Msg)
viewIdle mode { idleDay, idleTimeInDay, idleDayType } =
    [ viewHorizontalForm "Day off"
        [ case mode of
            EditIdleDayType -> 
                idleDayTypeSelect idleDay idleTimeInDay (Just idleDayType) True 
            _ ->
                div [ onDoubleClick <| ModeChanged EditIdleDayType ] 
                    [ text <| IdleDayType.toString idleDayType ]
        ]
    ]

viewNoEntry : Html msg
viewNoEntry = viewHorizontalForm "No entry" []

viewHorizontalFormWithAttr : List (Attribute msg) -> String -> List (Html msg) -> Html msg
viewHorizontalFormWithAttr attributes label_ content =
    div [ class "field", class "is-horizontal" ]
        [ div [ class "field-label" ] 
            [ label [class "label"] [ text label_ ] ]
        , div (class "field-body" :: attributes) 
            content
        ]

viewHorizontalForm : String -> List (Html msg) -> Html msg
viewHorizontalForm = viewHorizontalFormWithAttr []
