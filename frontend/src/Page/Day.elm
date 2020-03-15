module Page.Day exposing (Model, Msg(..), init, update, view)

import Browser exposing (Document, UrlRequest(..))
import Date exposing 
    ( Date
    , Unit(..)
    , add
    , today
    )
import Date.Extended exposing (toStringWithWeekday)
import Html exposing 
    ( Html
    , a
    , div
    , nav
    , section
    , text
    )
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (batch)
import RemoteData exposing (RemoteData(..))
import Task exposing (perform)
import Time exposing (Month(..))

import Api exposing 
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

import HalfDayWidget as HDW exposing 
    ( Mode(..)
    , Msg(..)
    , setDate
    , update
    , view
    )

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

init : ( Model, Cmd Msg )
init  =
    ( { morning = HDW.init Morning
      , afternoon = HDW.init Afternoon
      }
    , perform (\d -> DateChanged d) today
    )
    
-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        DateChanged date ->
            let 
                (morning, cmdMorning) = setDate model.morning date
                (afternoon, cmdAfternoon) = setDate model.afternoon date
                
                cmdGetMorning = Cmd.map MorningMsg cmdMorning
                cmdGetAfternoon = Cmd.map AfternoonMsg cmdAfternoon
                
                model_ = { model | morning = morning, afternoon = afternoon }
            in 
                ( model_, batch [ cmdGetMorning, cmdGetAfternoon ] )

        MorningMsg morningMsg -> 
            let
                (morning, cmd) = HDW.update morningMsg model.morning
            in
                ( { model | morning = morning }, Cmd.map MorningMsg cmd )

        AfternoonMsg afternoonMsg -> 
            let
                (afternoon, cmd) = HDW.update afternoonMsg model.afternoon
            in
                ( { model | afternoon = afternoon }, Cmd.map AfternoonMsg cmd )

-- View

viewNav : Date -> Html Msg
viewNav date =
    let
        previous = DateChanged (add Days -1 date)
        next = DateChanged (add Days 1 date)
    in
        nav [ class "navbar", class "is-fixed-top", class "is-primary" ]
            [ div [ class "navbar-brand" ] 
                [ div [ class "navbar-item" ] 
                    [ div [ class "buttons", class "has-addons" ]
                        [ div [ class "button", onClick previous ]
                            [ text "Prev" ]
                        , div [ class "button", onClick next ]
                            [ text "Next" ]
                        ]
                    ]
                , div [ class "navbar-item" ] 
                    [ text <| toStringWithWeekday date ]
                , a [ class "navbar-item", href "/projects" ] [ text "Project" ]
                ]
            ]


view : Model -> List Project -> Document Msg
view model projects =
    let
        viewBody = 
            div [ ] 
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
        , body = [ viewBody ] }
