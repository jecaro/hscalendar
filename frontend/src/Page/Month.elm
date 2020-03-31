module Page.Month exposing (Model, Msg, init, update, view)

import Array exposing (Array, map, toList)
import Browser exposing (Document)
import Date exposing (today)
import Date.Extended exposing (toStringWithWeekday)
import Html exposing (Html, div, header, i, span, text, ul, li)
import Html.Extra exposing (nothing)
import Html.Attributes exposing (class)
import RemoteData exposing (RemoteData(..), WebData, isLoading)
import Task exposing (perform)

import Api exposing (DayWithHalfDays, HalfDay(..), Month, MonthWithDays)
import Api.IdleDayType.Extended as IdleDayType exposing (toString)
import Api.Month.Extended as Month exposing (fromDate, toString)
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

viewDay : DayWithHalfDays -> Html msg
viewDay { dayFDay, dayFMorning, dayFAfternoon } =
    li [] 
        [ text <| toStringWithWeekday dayFDay
        , ul []
            [ li [] [ viewHalfDay dayFMorning ]
            , li [] [ viewHalfDay dayFAfternoon ]
            ]
        ]

viewMonth : Array DayWithHalfDays -> Html msg
viewMonth days = 
    ul [] <| toList <| map viewDay days
        

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
               Success month -> viewMonth month.monthFDays
               _ -> text "Error"
        body = 
            [ viewNavBar []
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

