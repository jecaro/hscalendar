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
import Browser
import Browser.Events as Events
import Common
import Date
import Date.Extended as Date
import HalfDayWidget as HDW
import Html exposing (Html, a, div, section, text)
import Html.Attributes exposing (class, href)
import Task



-- Types


type alias Model =
    { morning : HDW.State
    , afternoon : HDW.State
    }


type Msg
    = DateChanged Date.Date
    | MorningMsg HDW.Msg
    | AfternoonMsg HDW.Msg



-- Init


initWithToday : ( Model, Cmd Msg )
initWithToday =
    ( { morning = HDW.init Api.Morning
      , afternoon = HDW.init Api.Afternoon
      }
    , Task.perform (\d -> DateChanged d) Date.today
    )


initWithDate : Date.Date -> ( Model, Cmd Msg )
initWithDate date =
    ( { morning = HDW.init Api.Morning
      , afternoon = HDW.init Api.Afternoon
      }
    , Task.perform identity <| Task.succeed <| DateChanged date
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DateChanged date ->
            let
                ( morning, cmdMorning ) =
                    HDW.setDate model.morning date

                ( afternoon, cmdAfternoon ) =
                    HDW.setDate model.afternoon date

                cmdGetMorning =
                    Cmd.map MorningMsg cmdMorning

                cmdGetAfternoon =
                    Cmd.map AfternoonMsg cmdAfternoon

                model_ =
                    { model | morning = morning, afternoon = afternoon }
            in
            ( model_, Cmd.batch [ cmdGetMorning, cmdGetAfternoon ] )

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


viewNav : Date.Date -> Html Msg
viewNav date =
    let
        previous =
            Common.dateUrl (Date.add Date.Days -1 date)

        next =
            Common.dateUrl (Date.add Date.Days 1 date)

        items =
            [ div [ class "navbar-item" ]
                [ div [ class "buttons", class "has-addons" ]
                    [ a [ class "button", href previous ] [ text "Prev" ]
                    , a [ class "button", href next ] [ text "Next" ]
                    ]
                ]
            , div [ class "navbar-item" ]
                [ text <| Date.toStringWithWeekday date ]
            ]
    in
    Common.viewNavBar items


view : Model -> List Api.Project -> Browser.Document Msg
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
    { title = Date.toStringWithWeekday model.morning.date
    , body = [ viewBody ]
    }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case ( model.morning.mode, model.afternoon.mode ) of
        ( HDW.EditNotes _, _ ) ->
            Events.onMouseDown
                (Common.outsideTarget
                    (MorningMsg HDW.EditWasCanceled)
                    [ "submit", "edit" ]
                )

        ( _, HDW.EditNotes _ ) ->
            Events.onMouseDown
                (Common.outsideTarget
                    (AfternoonMsg HDW.EditWasCanceled)
                    [ "submit", "edit" ]
                )

        _ ->
            Sub.none
