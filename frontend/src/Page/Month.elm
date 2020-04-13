module Page.Month exposing (Model, Msg, init, update, view)

import Api
import Api.Month.Extended as Month
import Api.OffDayType.Extended as OffDayType
import Api.Office.Extended as Office
import Array
import Browser exposing (Document)
import Common
import Date
import Dict exposing (Dict)
import Html exposing (Html, a, br, div, header, i, li, span, table, td, text, th, tr, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Html.Extra exposing (nothing)
import RemoteData
import Request
import String
import Task


type Msg
    = GotResponse (RemoteData.WebData Api.MonthWithDays)
    | MonthChanged Api.Month


type alias Model =
    RemoteData.WebData Api.MonthWithDays


init : ( Model, Cmd Msg )
init =
    ( RemoteData.Loading
    , Task.perform (MonthChanged << Month.fromDate) Date.today
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        MonthChanged month ->
            ( RemoteData.Loading, Request.getMonth GotResponse month )

        GotResponse response ->
            ( response, Cmd.none )


viewHalfDay : Maybe Api.HalfDay -> List (Html msg)
viewHalfDay maybeHalfDay =
    case maybeHalfDay of
        Nothing ->
            []

        Just (Api.MkHalfDayWorked worked) ->
            [ text <| Office.toString worked.workedOffice
            , br [] []
            , text worked.workedProject.unProject
            ]

        Just (Api.MkHalfDayOff off) ->
            [ text <| OffDayType.toString off.offDayType ]


viewDay : Api.Month -> Api.DayWithHalfDays -> List (Html msg)
viewDay month { dayFDay, dayFMorning, dayFAfternoon } =
    let
        header =
            a [ href <| Common.dateUrl dayFDay ]
                [ text <| String.fromInt <| Date.day dayFDay ]
    in
    if Date.monthNumber dayFDay == month.month then
        header
            :: br [] []
            :: viewHalfDay dayFMorning
            ++ br [] []
            :: viewHalfDay dayFAfternoon

    else
        [ header ]


trForDays : Api.Month -> List Api.DayWithHalfDays -> List (Html msg)
trForDays month days =
    case days of
        x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: xs ->
            let
                sevenDays =
                    [ x1, x2, x3, x4, x5, x6, x7 ]
            in
            tr []
                (List.map (\d -> td [] (viewDay month d)) sevenDays)
                :: trForDays month xs

        [] ->
            []

        _ ->
            []



-- This should never happens


beforeMonth : Date.Date -> Array.Array Date.Date
beforeMonth date =
    let
        nbDatesToAdd =
            Date.weekdayNumber date - 1
    in
    Array.initialize nbDatesToAdd (\i -> Date.add Date.Days (i - nbDatesToAdd) date)


afterMonth : Date.Date -> Array.Array Date.Date
afterMonth date =
    let
        nbDatesToAdd =
            7 - Date.weekdayNumber date
    in
    Array.initialize nbDatesToAdd (\i -> Date.add Date.Days (i + 1) date)


normalizeMonth : Array.Array Api.DayWithHalfDays -> Array.Array Api.DayWithHalfDays
normalizeMonth days =
    let
        dateToDayWithHalfDays date =
            Api.DayWithHalfDays date Nothing Nothing

        getPadding i fct =
            case Array.get i days of
                Just day ->
                    let
                        arrayOfDates =
                            fct day.dayFDay
                    in
                    Array.map dateToDayWithHalfDays arrayOfDates

                Nothing ->
                    Array.empty

        before =
            getPadding 0 beforeMonth

        after =
            getPadding (Array.length days - 1) afterMonth
    in
    Array.append (Array.append before days) after


getId : Api.HalfDay -> String
getId hd =
    case hd of
        Api.MkHalfDayWorked { workedProject } ->
            workedProject.unProject

        Api.MkHalfDayOff { offDayType } ->
            OffDayType.toString offDayType


incOrSetToOne : Maybe Float -> Float
incOrSetToOne maybeNumber =
    case maybeNumber of
        Just n ->
            n + 0.5

        Nothing ->
            0.5


updateDictWithId : Maybe String -> Dict String Float -> Dict String Float
updateDictWithId maybeId dict =
    case maybeId of
        Nothing ->
            dict

        Just id ->
            Dict.update id (Just << incOrSetToOne) dict


stats : Array.Array Api.DayWithHalfDays -> Dict String Float
stats days =
    let
        foldHalfDayWithDict { dayFMorning, dayFAfternoon } d =
            let
                maybeIdMorning =
                    Maybe.map getId dayFMorning

                maybeIdAfternoon =
                    Maybe.map getId dayFAfternoon
            in
            updateDictWithId maybeIdMorning d |> updateDictWithId maybeIdAfternoon
    in
    Array.foldr foldHalfDayWithDict Dict.empty days


viewMonth : Api.MonthWithDays -> List (Html msg)
viewMonth { monthFMonth, monthFDays } =
    let
        daysOfWeek =
            [ "Monday"
            , "Tuesday"
            , "Wednesday"
            , "Thursday"
            , "Friday"
            , "Saturday"
            , "Sunday"
            ]

        rowLabel =
            tr [] <| List.map (\d -> th [] [ text d ]) daysOfWeek

        monthTable =
            table [] <|
                rowLabel
                    :: (trForDays monthFMonth <| Array.toList <| normalizeMonth monthFDays)

        monthStats =
            let
                monthList =
                    Dict.toList <| stats monthFDays
            in
            ul [] <|
                List.map (\( k, v ) -> li [] [ text <| k ++ ": " ++ String.fromFloat v ]) monthList
    in
    [ monthTable, br [] [], monthStats ]


viewNav : Api.Month -> Html Msg
viewNav month =
    let
        items =
            [ div [ class "navbar-item" ]
                [ div [ class "buttons", class "has-addons" ]
                    [ div
                        [ class "button"
                        , onClick <| MonthChanged <| Month.previous month
                        ]
                        [ text "Prev" ]
                    , div
                        [ class "button"
                        , onClick <| MonthChanged <| Month.next month
                        ]
                        [ text "Next" ]
                    ]
                ]
            , div [ class "navbar-item" ]
                [ text <| Month.toString month ]
            ]
    in
    Common.viewNavBar items


view : Model -> Document Msg
view model =
    let
        loadingIcon =
            if RemoteData.isLoading model then
                div [ class "card-header-icon" ]
                    [ span [ class "icon" ]
                        [ i [ class "fas fa-spinner fa-spin" ] [] ]
                    ]

            else
                nothing

        viewNavWithDefault =
            RemoteData.withDefault (Common.viewNavBar []) <|
                RemoteData.map (viewNav << .monthFMonth) model

        monthTitleWithDefault =
            RemoteData.withDefault "" <|
                RemoteData.map (Month.toString << .monthFMonth) model

        monthHtml =
            case model of
                RemoteData.Success month ->
                    viewMonth month

                RemoteData.Failure _ ->
                    [ text "Error" ]

                _ ->
                    []

        body =
            [ viewNavWithDefault
            , div [ class "section" ]
                [ div [ class "column" ]
                    [ div [ class "card" ]
                        [ header [ class "card-header" ]
                            [ div [ class "card-header-title" ]
                                [ div [ class "title", class "is-4" ]
                                    [ text monthTitleWithDefault ]
                                ]
                            , loadingIcon
                            ]
                        , div [ class "card-content" ]
                            [ div [ class "content" ] monthHtml
                            ]
                        ]
                    ]
                ]
            ]
    in
    { title = "Month", body = body }
