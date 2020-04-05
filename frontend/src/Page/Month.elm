module Page.Month exposing (Model, Msg, init, update, view)

import Array exposing (Array, append, empty, initialize, get, map, toList)
import Browser exposing (Document)
import Date exposing (Date, Unit(..), add, day, monthNumber, today, weekdayNumber)
import Html exposing (Html, br, div, header, i, span, table, td, th, tr, text)
import Html.Extra exposing (nothing)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (length)
import RemoteData exposing (RemoteData(..), WebData, isLoading)
import String exposing (fromInt)
import Task exposing (perform)

import Api exposing (DayWithHalfDays, HalfDay(..), Month, MonthWithDays)
import Api.IdleDayType.Extended as IdleDayType exposing (toString)
import Api.Month.Extended as Month exposing (fromDate, next, previous, toString)
import Common exposing (viewNavBar)
import Request exposing (getMonth)

type Msg
    = GotResponse (WebData MonthWithDays)
    | MonthChanged Month



type alias Model = WebData MonthWithDays

init : ( Model, Cmd Msg )
init = ( Loading, perform (MonthChanged << fromDate) today )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MonthChanged month -> (model, getMonth GotResponse month)
        GotResponse response -> (response, Cmd.none)


viewHalfDay : Maybe HalfDay -> Html msg
viewHalfDay maybeHalfDay =
    case maybeHalfDay of
        Nothing -> text "Nothing"
        Just (MkHalfDayWorked worked) -> text worked.workedProject.unProject
        Just (MkHalfDayIdle idle) -> text <| IdleDayType.toString idle.idleDayType

viewDay : Month -> DayWithHalfDays -> List (Html msg)
viewDay month { dayFDay, dayFMorning, dayFAfternoon } =
    let
        header = text <| fromInt <| day dayFDay
    in
        if monthNumber dayFDay == month.month
            then
                [ header
                , br [] []
                , viewHalfDay dayFMorning
                , br [] []
                , viewHalfDay dayFAfternoon
                ]
            else
                [ header ]

trForDays : Month -> List DayWithHalfDays -> List (Html msg)
trForDays month days =
    case days of
        x1::x2::x3::x4::x5::x6::x7::xs ->
            let
                sevenDays = [x1, x2, x3, x4, x5, x6, x7]
            in
            tr []
                (List.map (\d -> td [] ( viewDay month d ) ) sevenDays)
                :: trForDays month xs

        [] -> []
        _ -> [] -- This should never happens


beforeMonth : Date -> Array Date
beforeMonth date =
    let
        nbDatesToAdd = weekdayNumber date - 1
    in
    initialize nbDatesToAdd (\i -> add Days (i - nbDatesToAdd) date)

afterMonth : Date -> Array Date
afterMonth date =
    let
        nbDatesToAdd = 7 - weekdayNumber date
    in
    initialize nbDatesToAdd (\i -> add Days (i + 1) date)

normalizeMonth : Array DayWithHalfDays -> Array DayWithHalfDays
normalizeMonth days =
    let
        dateToDayWithHalfDays date = DayWithHalfDays date Nothing Nothing
        getPadding i fct =
            case get i days of
                Just day ->
                    let arrayOfDates = fct day.dayFDay
                    in map dateToDayWithHalfDays arrayOfDates
                Nothing -> empty

        before = getPadding 0 beforeMonth
        after = getPadding (Array.length days - 1) afterMonth
    in append (append before days) after


viewMonth : MonthWithDays -> Html msg
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

        rowLabel = tr [] <| List.map (\d -> th [] [ text d ]) daysOfWeek
    in
    table [] <|
        rowLabel :: (trForDays monthFMonth <| toList <| normalizeMonth monthFDays)

viewNav : Month -> Html Msg
viewNav month =
    let
        items =
            [ div [ class "navbar-item" ]
                [ div [ class "buttons", class "has-addons" ]
                    [ div
                        [ class "button"
                        , onClick <| MonthChanged <| previous month
                        ]
                        [ text "Prev" ]
                    , div
                        [ class "button"
                        , onClick <| MonthChanged <| next month
                        ]
                        [ text "Next" ]
                    ]
                ]
            , div [ class "navbar-item" ]
                [ text <| Month.toString month ]
            ]
    in
    viewNavBar items

view : Model -> Document Msg
view model =
    let
        loadingIcon =
            if isLoading model
                then
                    div [ class "card-header-icon" ]
                        [ span [ class "icon" ]
                            [ i [ class "fas fa-spinner fa-spin" ] [ ] ]
                        ]
                else
                    nothing
        monthTitleString =
            case model of
               Success month -> Month.toString month.monthFMonth
               _ -> "Month"
        monthHtml =
            case model of
               Success month -> viewMonth month
               _ -> text "Error"
        navBarHtml =
            case model of
                Success month -> viewNav month.monthFMonth
                _ -> viewNavBar []
        body =
            [ navBarHtml
            , div [ class "section" ]
                [ div [ class "column" ]
                    [ div [ class "card" ]
                        [ header [ class "card-header" ]
                            [ div [ class "card-header-title" ]
                                [ div [ class "title", class "is-4" ]
                                    [ text monthTitleString ]
                                ]
                            , loadingIcon
                            ]
                        , div [ class "card-content" ]
                            [ div [ class "content" ]
                                [ monthHtml ]
                            ]
                        ]
                    ]
                ]
            ]
    in
    { title = "Month", body = body }

