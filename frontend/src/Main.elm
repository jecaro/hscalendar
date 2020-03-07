module Main exposing (main)

import Browser
import Browser.Events exposing (onMouseDown)
import Date exposing 
    ( Date
    , Unit(..)
    , add
    , format
    , today
    , toIsoString
    )
import Html exposing 
    ( Html
    , button
    , div
    , h1
    , nav
    , p
    , section
    , text
    )
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewMaybe)
import Json.Decode as Decode
import List exposing (member)
import Platform.Cmd exposing (batch)
import RemoteData exposing (RemoteData(..), WebData)
import String exposing (concat)
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
    , viewChangeHalfDayType
    , view
    )
import Request exposing (getProjects)

-- Types


type alias Model =
    { morning : HDW.State
    , afternoon : HDW.State
    , projects : WebData (List Project)
    }

type Msg 
    = GotProjectsResponse (WebData (List Project))
    | DateChanged Date
    | MorningMsg HDW.Msg 
    | AfternoonMsg HDW.Msg


-- Main

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

-- Init

init : flags -> ( Model, Cmd Msg )
init _ =
    ( { morning = HDW.init Morning
      , afternoon = HDW.init Afternoon
      , projects = Loading
      }
    , batch 
        [ perform (\d -> DateChanged d) today
        , getProjects GotProjectsResponse
        ]
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

        GotProjectsResponse response -> 
            ( { model | projects = response }, Cmd.none )
        
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

viewNav : Date -> Html Msg
viewNav date =
    let
        previous = DateChanged (add Days -1 date)
        next = DateChanged (add Days 1 date)
        weekdayString = format "EEEE" date
    in
        nav [ class "navbar", class "is-fixed-top", class "is-primary" ]
            [ div [ class "navbar-brand" ] 
                [ div [ class "navbar-item" ] 
                    [ div [ class "field", class "has-addons" ]
                        [ div [ class "control" ]
                            [ button
                                [ class "button" , onClick previous ]
                                [ text "Prev" ]
                            ]
                        , div [ class "control" ]
                            [ button
                                [ class "button" , onClick next ]
                                [ text "Next" ]
                            ]
                        ]
                    ]
                , div [ class "navbar-item" ] 
                    [ text <| toIsoString date ++ " " ++ weekdayString]
                ]
            ]



view : Model -> Html Msg
view model =
    let
        viewMorningChangeHalfDayType_ = 
            viewMaybe 
                (\projects -> 
                    viewChangeHalfDayType 
                        model.morning.date 
                        model.morning.timeInDay 
                        (RemoteData.toMaybe model.morning.halfDay) 
                        projects 
                )
                (RemoteData.toMaybe model.projects)
        viewAfternoonChangeHalfDayType_ = 
            viewMaybe 
                (\projects -> 
                    viewChangeHalfDayType 
                        model.afternoon.date 
                        model.afternoon.timeInDay 
                        (RemoteData.toMaybe model.afternoon.halfDay) 
                        projects 
                )
                (RemoteData.toMaybe model.projects)
        viewMorning =
            viewMaybe 
                (\projects -> HDW.view model.morning.halfDay model.morning.mode projects)
                (RemoteData.toMaybe model.projects)
        viewAfternoon =
            viewMaybe 
                (\projects -> HDW.view model.afternoon.halfDay model.afternoon.mode projects)
                (RemoteData.toMaybe model.projects)

    in
        div [] 
            [ viewNav model.morning.date
            , section [ class "section" ] 
                [ div [ class "box" ] 
                    [ p [ class "title", class "is-4" ] [ text "Morning" ]
                    , Html.map MorningMsg viewMorning 
                    , Html.map MorningMsg viewMorningChangeHalfDayType_
                    ]
                , div [ class "box" ] 
                    [ p [ class "title", class "is-4" ] [ text "Afternoon" ]
                    , Html.map AfternoonMsg viewAfternoon 
                    , Html.map AfternoonMsg viewAfternoonChangeHalfDayType_
                    ]
                ]
            ]

-- Requests


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    case (model.morning.mode, model.afternoon.mode) of
        (EditNotes _, _) -> 
            Sub.map MorningMsg (onMouseDown (outsideTarget [ "submit", "edit" ]))
        (_, EditNotes _) -> 
            Sub.map AfternoonMsg (onMouseDown (outsideTarget [ "submit", "edit" ]))
        _ -> Sub.none


{-This code has been found here
https://dev.to/margaretkrutikova/elm-dom-node-decoder-to-detect-click-outside-3ioh 
-}
outsideTarget : List String -> Decode.Decoder HDW.Msg
outsideTarget domEltIds =
    Decode.field "target" (isOutsideDomEltId domEltIds)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed EditWasCanceled
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

