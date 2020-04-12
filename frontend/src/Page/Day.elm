module Page.Day exposing
    ( Model
    , Msg(..)
    , initWithDate
    , initWithToday
    , subscriptions
    , update
    , view
    )

import Api
    exposing
        ( HalfDay(..)
        , IdleDayType(..)
        , Office(..)
        , Project
        , SetArrived(..)
        , SetLeft(..)
        , SetNotes(..)
        , SetOffice(..)
        , SetProj(..)
        , TimeInDay(..)
        , WorkOption(..)
        )
import Browser exposing (Document, UrlRequest(..))
import Browser.Events exposing (onMouseDown)
import Common exposing (dateUrl, outsideTarget, viewNavBar)
import Date exposing (Date, Unit(..), add, today)
import Date.Extended exposing (toStringWithWeekday)
import HalfDayWidget as HDW
    exposing
        ( Mode(..)
        , Msg(..)
        , setDate
        , update
        , view
        )
import Html exposing (Html, a, div, section, text)
import Html.Attributes exposing (class, href)
import Platform.Cmd exposing (batch)
import RemoteData exposing (RemoteData(..))
import Task exposing (perform, succeed)
import Time exposing (Month(..))



-- Types


type alias Model =
    { morning : HDW.State
    , afternoon : HDW.State
    }


type Msg
    = DateChanged Date
    | MorningMsg HDW.Msg
    | AfternoonMsg HDW.Msg



-- Init


initWithToday : ( Model, Cmd Msg )
initWithToday =
    ( { morning = HDW.init Morning
      , afternoon = HDW.init Afternoon
      }
    , perform (\d -> DateChanged d) today
    )


initWithDate : Date -> ( Model, Cmd Msg )
initWithDate date =
    ( { morning = HDW.init Morning
      , afternoon = HDW.init Afternoon
      }
    , perform identity <| succeed <| DateChanged date
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DateChanged date ->
            let
                ( morning, cmdMorning ) =
                    setDate model.morning date

                ( afternoon, cmdAfternoon ) =
                    setDate model.afternoon date

                cmdGetMorning =
                    Cmd.map MorningMsg cmdMorning

                cmdGetAfternoon =
                    Cmd.map AfternoonMsg cmdAfternoon

                model_ =
                    { model | morning = morning, afternoon = afternoon }
            in
            ( model_, batch [ cmdGetMorning, cmdGetAfternoon ] )

        MorningMsg morningMsg ->
            let
                ( morning, cmd ) =
                    HDW.update morningMsg model.morning
            in
            ( { model | morning = morning }, Cmd.map MorningMsg cmd )

        AfternoonMsg afternoonMsg ->
            let
                ( afternoon, cmd ) =
                    HDW.update afternoonMsg model.afternoon
            in
            ( { model | afternoon = afternoon }, Cmd.map AfternoonMsg cmd )



-- View


viewNav : Date -> Html Msg
viewNav date =
    let
        previous =
            dateUrl (add Days -1 date)

        next =
            dateUrl (add Days 1 date)

        items =
            [ div [ class "navbar-item" ]
                [ div [ class "buttons", class "has-addons" ]
                    [ a [ class "button", href previous ] [ text "Prev" ]
                    , a [ class "button", href next ] [ text "Next" ]
                    ]
                ]
            , div [ class "navbar-item" ]
                [ text <| toStringWithWeekday date ]
            ]
    in
    viewNavBar items


view : Model -> List Project -> Document Msg
view model projects =
    let
        viewBody =
            div []
                [ viewNav model.morning.date
                , section [ class "section" ]
                    [ div [ class "column" ]
                        [ Html.map MorningMsg (HDW.view model.morning projects) ]
                    , div [ class "column" ]
                        [ Html.map AfternoonMsg (HDW.view model.afternoon projects) ]
                    ]
                ]
    in
    { title = toStringWithWeekday model.morning.date
    , body = [ viewBody ]
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case ( model.morning.mode, model.afternoon.mode ) of
        ( EditNotes _, _ ) ->
            onMouseDown
                (outsideTarget
                    (MorningMsg EditWasCanceled)
                    [ "submit", "edit" ]
                )

        ( _, EditNotes _ ) ->
            onMouseDown
                (outsideTarget
                    (AfternoonMsg EditWasCanceled)
                    [ "submit", "edit" ]
                )

        _ ->
            Sub.none
