module HalfDayWidget exposing
    ( Mode(..)
    , Msg(..)
    , State
    , init
    , setDate
    , update
    , view
    )

import Api
import Api.OffDayType.Extended as OffDayType 
import Api.Office.Extended as Office 
import Api.TimeInDay.Extended as TimeInDay 
import Api.TimeOfDay as TimeOfDay 
import Browser.Dom as Dom
import Common 
import Date 
import Html
    exposing
        ( Attribute
        , Html
        , br
        , button
        , div
        , header
        , i
        , input
        , label
        , option
        , select
        , span
        , text
        , textarea
        )
import Html.Attributes
    exposing
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
import Http 
import List 
import Maybe.Extra as Maybe
import RemoteData 
import Request
import Result 
import String 
import Task 
import Time 



-- Types


type Mode
    = View
    | EditOffice
    | EditProject
    | EditNotes String
    | EditOffDayType
    | EditArrived
    | EditLeft


type alias State =
    { date : Date.Date
    , timeInDay : Api.TimeInDay
    , halfDayDisplayed : Maybe Api.HalfDay
    , halfDay : RemoteData.WebData Api.HalfDay
    , edit : RemoteData.WebData ()
    , mode : Mode
    }


type Msg
    = GotHalfDayResponse (RemoteData.WebData Api.HalfDay)
    | GotEditResponse (RemoteData.WebData ())
    | ModeChanged Mode
    | EditHalfDaySent (Cmd Msg)
    | EditWasCanceled
    | NoOp



-- init


init : Api.TimeInDay -> State
init timeInDay =
    { date = Date.fromCalendarDate 2020 Time.Jan 1
    , timeInDay = timeInDay
    , halfDayDisplayed = Nothing
    , halfDay = RemoteData.NotAsked
    , mode = View
    , edit = RemoteData.NotAsked
    }


setDate : State -> Date.Date -> ( State, Cmd Msg )
setDate state date =
    let
        state_ =
            { state
                | date = date
                , halfDayDisplayed = RemoteData.toMaybe state.halfDay
                , halfDay = RemoteData.Loading
                , edit = RemoteData.NotAsked
                , mode = View
            }

        cmd =
            Request.getHalfDay GotHalfDayResponse state_.date state_.timeInDay
    in
    ( state_, cmd )



-- Update


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        GotHalfDayResponse response ->
            ( { model
                | halfDayDisplayed =
                    case response of
                        -- Copy the data
                        RemoteData.Success a ->
                            Just a

                        -- 404 is not an error, there is no entry
                        RemoteData.Failure (Http.BadStatus 404) ->
                            Nothing

                        -- Other than that is an error, we keep displaying
                        -- backup data
                        _ ->
                            model.halfDayDisplayed
                , halfDay = response
                , mode = View
              }
            , Cmd.none
            )

        GotEditResponse ((RemoteData.Success _) as response) ->
            ( { model | edit = response, halfDay = RemoteData.Loading }
            , Request.getHalfDay GotHalfDayResponse model.date model.timeInDay
            )

        -- We don't ask for up to date half-day in case of an error
        GotEditResponse response ->
            ( { model | edit = response }, Cmd.none )

        ModeChanged mode ->
            let
                cmd =
                    if mode == View then
                        Cmd.none

                    else
                        Task.attempt (\_ -> NoOp) (Dom.focus "edit")
            in
            ( { model | mode = mode }, cmd )

        EditHalfDaySent request ->
            ( { model | edit = RemoteData.Loading }, request )

        EditWasCanceled ->
            ( { model | mode = View }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- View


viewChangeHalfDayType :
    Date.Date
    -> Api.TimeInDay
    -> Maybe Api.HalfDay
    -> List Api.Project
    -> List (Html Msg)
viewChangeHalfDayType date timeInDay halfDay projects =
    let
        isWorked =
            case halfDay of
                Nothing ->
                    False

                Just (Api.MkHalfDayWorked _) ->
                    True

                Just (Api.MkHalfDayOff _) ->
                    False

        isOff =
            case halfDay of
                Nothing ->
                    False

                Just (Api.MkHalfDayWorked _) ->
                    False

                Just (Api.MkHalfDayOff _) ->
                    True

        viewDelete =
            viewHorizontalForm ""
                [ div [ class "control" ]
                    [ button
                        [ class "button"
                        , onClick <| EditHalfDaySent <| Request.delete GotEditResponse date timeInDay
                        , disabled <| Maybe.isNothing halfDay
                        ]
                        [ text "Delete" ]
                    ]
                ]

        viewSetOff =
            viewHorizontalForm "Day off"
                [ offDayTypeSelect
                    date
                    timeInDay
                    Nothing
                    (Maybe.isNothing halfDay || isWorked)
                ]

        viewSetWorked =
            viewHorizontalForm "Worked on"
                [ projectSelect
                    date
                    timeInDay
                    projects
                    Nothing
                    (Maybe.isNothing halfDay || isOff)
                ]
    in
    [ viewIf (isOff || Maybe.isNothing halfDay) viewSetWorked
    , viewIf (isWorked || Maybe.isNothing halfDay) viewSetOff
    , viewDelete
    ]


view : State -> List Api.Project -> Html Msg
view state projects =
    let
        halfDayHtml =
            case state.halfDayDisplayed of
                Nothing ->
                    [ viewNoEntry ]

                Just (Api.MkHalfDayWorked worked) ->
                    viewWorked state.mode projects worked

                Just (Api.MkHalfDayOff off) ->
                    viewOff state.mode off

        changeHalDayHtml =
            viewChangeHalfDayType
                state.date
                state.timeInDay
                state.halfDayDisplayed
                projects

        loadingIcon =
            if RemoteData.isLoading state.halfDay || RemoteData.isLoading state.edit then
                div [ class "card-header-icon" ]
                    [ span [ class "icon" ]
                        [ i [ class "fas fa-spinner fa-spin" ] [] ]
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
                (halfDayHtml
                    ++ changeHalDayHtml
                    ++ [ viewErrorHalfDay state.halfDay
                       , viewErrorEdit state.edit
                       ]
                )
            ]
        ]


viewErrorHalfDay : RemoteData.WebData a -> Html msg
viewErrorHalfDay halfDay =
    case halfDay of
        -- 404 is not an error, there is no entry
        RemoteData.Failure (Http.BadStatus 404) ->
            nothing

        RemoteData.Failure error ->
            Common.viewErrorFromError error "Error getting data from the server"

        _ ->
            nothing


viewErrorEdit : RemoteData.WebData a -> Html msg
viewErrorEdit edit =
    Common.viewErrorFromWebData edit "Edit command returned an error"


officeSelect : Date.Date -> Api.TimeInDay -> Api.Office -> Html Msg
officeSelect date timeInDay current =
    let
        toOption office =
            option [ selected <| office == current ]
                [ text <| Office.toString office ]

        setEditHalfDay =
            EditHalfDaySent
                << Request.setOffice GotEditResponse date timeInDay
                << Result.withDefault Api.Rennes
                << Office.fromString
    in
    div [ class "control" ]
        [ div [ class "select" ]
            [ select
                [ id "edit"
                , onInput setEditHalfDay
                , onBlur EditWasCanceled
                ]
                (List.map toOption Office.offices)
            ]
        ]


projectSelect : Date.Date -> Api.TimeInDay -> List Api.Project -> Maybe Api.Project -> Bool -> Html Msg
projectSelect date timeInDay projects current enabled =
    let
        defaultValue =
            case current of
                Nothing ->
                    Api.Project ""

                Just current_ ->
                    current_

        projects_ =
            case current of
                Nothing ->
                    Api.Project "" :: projects

                Just _ ->
                    projects

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
                , onInput <|
                    EditHalfDaySent
                        << Request.setProject GotEditResponse date timeInDay
                , value defaultValue.unProject

                -- if there is a default value, it means that it is the
                -- select used for editing current halfday
                , id <|
                    if Maybe.isJust current then
                        "edit"

                    else
                        ""
                ]
              <|
                List.map toOption projects_
            ]
        ]



{- Create select for all the OffDayType. If Maybe OffDayType is Nothing, the
   function prepend an empty item before the different types.
-}


offDayTypeSelect : Date.Date -> Api.TimeInDay -> Maybe Api.OffDayType -> Bool -> Html Msg
offDayTypeSelect date timeInDay current enabled =
    let
        defaultValue =
            case current of
                Nothing ->
                    ""

                Just current_ ->
                    OffDayType.toString current_

        toOption offDayType =
            let
                offDayTypeStr =
                    OffDayType.toString offDayType
            in
            option
                [ value offDayTypeStr
                , selected (offDayTypeStr == defaultValue)
                ]
                [ text <| OffDayType.toString offDayType ]

        setEditHalfDay =
            EditHalfDaySent
                << Request.setOffType GotEditResponse date timeInDay
                << Result.withDefault Api.PaidLeave
                << OffDayType.fromString
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
                , id <|
                    if Maybe.isJust current then
                        "edit"

                    else
                        ""
                ]
              <|
                -- Prepend empty case if needed
                [ viewIf (Maybe.isNothing current) <|
                    option [ value "" ] [ text "" ]
                , toOption Api.PaidLeave
                , toOption Api.FamilyEvent
                , toOption Api.RTTE
                , toOption Api.RTTS
                , toOption Api.UnpaidLeave
                , toOption Api.PublicHoliday
                , toOption Api.PartTime
                ]
            ]
        ]


arrivedOrLeftInput : (String -> Msg) -> TimeOfDay.TimeOfDay -> Html Msg
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


arrivedInput : Date.Date -> Api.TimeInDay -> TimeOfDay.TimeOfDay -> Html Msg
arrivedInput date timeInDay timeOfDay =
    let
        setEditHalfDay =
            EditHalfDaySent
                << Request.setArrived GotEditResponse date timeInDay
                << Result.withDefault timeOfDay
                << TimeOfDay.fromString
    in
    arrivedOrLeftInput setEditHalfDay timeOfDay


leftInput : Date.Date -> Api.TimeInDay -> TimeOfDay.TimeOfDay -> Html Msg
leftInput date timeInDay timeOfDay =
    let
        setEditHalfDay =
            EditHalfDaySent
                << Request.setLeft GotEditResponse date timeInDay
                << Result.withDefault timeOfDay
                << TimeOfDay.fromString
    in
    arrivedOrLeftInput setEditHalfDay timeOfDay


viewOffice : Mode -> Date.Date -> Api.TimeInDay -> Api.Office -> Html Msg
viewOffice mode day timeInDay office =
    viewHorizontalForm "Office"
        [ case mode of
            EditOffice ->
                officeSelect day timeInDay office

            _ ->
                div [ onDoubleClick <| ModeChanged EditOffice ]
                    [ text <| Office.toString office ]
        ]


viewProject : Mode -> List Api.Project -> Date.Date -> Api.TimeInDay -> Api.Project -> Html Msg
viewProject mode projects day timeInDay project =
    viewHorizontalForm "Project"
        [ case mode of
            EditProject ->
                projectSelect
                    day
                    timeInDay
                    projects
                    (Just project)
                    True

            _ ->
                div [ onDoubleClick <| ModeChanged EditProject ]
                    [ text <| project.unProject ]
        ]


viewArrivedOrLeft : Mode -> TimeOfDay.TimeOfDay -> Html Msg
viewArrivedOrLeft mode timeOfDay =
    div [ onDoubleClick <| ModeChanged mode ]
        [ text <| TimeOfDay.toString timeOfDay ]


viewArrived : Mode -> Date.Date -> Api.TimeInDay -> TimeOfDay.TimeOfDay -> Html Msg
viewArrived mode day timeInDay arrived =
    viewHorizontalForm "Arrived"
        [ case mode of
            EditArrived ->
                arrivedInput day timeInDay arrived

            _ ->
                viewArrivedOrLeft EditArrived arrived
        ]


viewLeft : Mode -> Date.Date -> Api.TimeInDay -> TimeOfDay.TimeOfDay -> Html Msg
viewLeft mode day timeInDay left =
    viewHorizontalForm "Left"
        [ case mode of
            EditLeft ->
                leftInput day timeInDay left

            _ ->
                viewArrivedOrLeft EditLeft left
        ]


viewNotes : Mode -> Date.Date -> Api.TimeInDay -> Api.Notes -> List (Html Msg)
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
                            , onClick <|
                                EditHalfDaySent <|
                                    Request.setNotes GotEditResponse day timeInDay notes_
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
                (if String.length notes.unNotes /= 0 then
                    List.intersperse (br [] [])
                        (List.map text (String.lines notes.unNotes))

                 else
                    [ text "Double click to edit" ]
                )
            ]


viewWorked : Mode -> List Api.Project -> Api.Worked -> List (Html Msg)
viewWorked mode projects { workedDay, workedTimeInDay, workedArrived, workedLeft, workedOffice, workedNotes, workedProject } =
    [ viewOffice mode workedDay workedTimeInDay workedOffice
    , viewArrived mode workedDay workedTimeInDay workedArrived
    , viewLeft mode workedDay workedTimeInDay workedLeft
    , viewProject mode projects workedDay workedTimeInDay workedProject
    ]
        ++ viewNotes mode workedDay workedTimeInDay workedNotes


viewOff : Mode -> Api.Off -> List (Html Msg)
viewOff mode { offDay, offTimeInDay, offDayType } =
    [ viewHorizontalForm "Day off"
        [ case mode of
            EditOffDayType ->
                offDayTypeSelect offDay offTimeInDay (Just offDayType) True

            _ ->
                div [ onDoubleClick <| ModeChanged EditOffDayType ]
                    [ text <| OffDayType.toString offDayType ]
        ]
    ]


viewNoEntry : Html msg
viewNoEntry =
    viewHorizontalForm "No entry" []


viewHorizontalFormWithAttr : List (Attribute msg) -> String -> List (Html msg) -> Html msg
viewHorizontalFormWithAttr attributes label_ content =
    div [ class "field", class "is-horizontal" ]
        [ div [ class "field-label" ]
            [ label [ class "label" ] [ text label_ ] ]
        , div (class "field-body" :: attributes)
            content
        ]


viewHorizontalForm : String -> List (Html msg) -> Html msg
viewHorizontalForm =
    viewHorizontalFormWithAttr []
