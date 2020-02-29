module Main exposing (main)

import Browser
import Browser.Events exposing (onMouseDown)
import Date exposing 
    ( Date
    , Unit(..)
    , add
    , format
    , fromCalendarDate
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
import Http exposing (Error(..), expectJson, get)
import Json.Decode as Decode exposing (list)
import List exposing (member)
import Platform.Cmd exposing (batch)
import RemoteData exposing (RemoteData(..), WebData, fromResult)
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
import Api.Project as Project exposing (decoder)
import Api.TimeInDay.Extended as TimeInDay exposing (toString)

import HalfDayWidget exposing 
    ( ModelHalfDay
    , Mode(..)
    , MsgHalfDay(..)
    , getHalfDay
    , updateHalfDay
    , viewChangeHalfDayType
    , viewStatus
    )

-- Types


type alias Model =
    { morning : ModelHalfDay
    , afternoon : ModelHalfDay
    }

type Msg 
    = GotProjectsResponse (WebData (List Project))
    | DateChanged Date
    | MorningMsg MsgHalfDay 
    | AfternoonMsg MsgHalfDay


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
    let
        baseModel = 
            { date = fromCalendarDate 2020 Jan 1
            , timeInDay = Morning
            , halfDay = NotAsked
            , projects = NotAsked
            , mode = View
            , edit = NotAsked
            }
    in
    ( { morning = { baseModel | timeInDay = Morning }
      , afternoon = { baseModel | timeInDay = Afternoon }
      }
    , batch 
        [ perform (\d -> DateChanged d) today
        , getProjects
        ]
    )

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        DateChanged date ->
            let 
                morning = model.morning
                morning_ = 
                    { morning
                    | date = date
                    , halfDay = Loading
                    , edit = NotAsked
                    , mode = View
                    }
                afternoon = model.afternoon
                afternoon_ = 
                    { afternoon
                    | date = date
                    , halfDay = Loading
                    , edit = NotAsked
                    , mode = View
                    }
                model_ = 
                    { model
                    | morning = morning_
                    , afternoon = afternoon_
                    }
                cmdGetMorning = 
                    Cmd.map MorningMsg (getHalfDay model_.morning.date model_.morning.timeInDay) 
                cmdGetAfternoon = 
                    Cmd.map AfternoonMsg (getHalfDay model_.afternoon.date model_.afternoon.timeInDay) 
            in 
                ( model_, batch [ cmdGetMorning, cmdGetAfternoon ] )

        GotProjectsResponse response ->
            let
                morning = model.morning
                morning_ = 
                    { morning
                    | projects = response
                    }                
                afternoon = model.afternoon
                afternoon_ = 
                    { afternoon
                    | projects = response
                    }                
            in
            
            ( { model | morning = morning_, afternoon = afternoon_ }, Cmd.none )
        
        MorningMsg morningMsg -> 
            let
                (morning, cmd) = updateHalfDay morningMsg model.morning
            in
                ( {model | morning = morning}, Cmd.map MorningMsg cmd )

        AfternoonMsg afternoonMsg -> 
            let
                (afternoon, cmd) = updateHalfDay afternoonMsg model.afternoon
            in
                ( {model | afternoon = afternoon}, Cmd.map AfternoonMsg cmd )




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
        nav [ class "level" ]
            [ div [ class "level-left" ]
                [ div [ class "level-item" ]
                    [ div [ class "field", class "has-addons" ]
                        [ div [ class "control" ]
                            [ button
                                [ class "button"
                                , onClick previous
                                ]
                                [ text "Prev" ]
                            ]
                        , div [ class "control" ] 
                            [ button
                                [ class "button"
                                , onClick next
                                ]
                                [ text "Next" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "level-item" ]
                [ p [ class "subtitle" ]
                    [ text <| toIsoString date ++ " " ++ weekdayString ]
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
                (RemoteData.toMaybe model.morning.projects)
        viewAfternoonChangeHalfDayType_ = 
            viewMaybe 
                (\projects -> 
                    viewChangeHalfDayType 
                        model.afternoon.date 
                        model.afternoon.timeInDay 
                        (RemoteData.toMaybe model.afternoon.halfDay) 
                        projects 
                )
                (RemoteData.toMaybe model.morning.projects)
        viewMorningStatus_ =
            viewMaybe 
                (\projects -> viewStatus model.morning.halfDay model.morning.mode projects)
                (RemoteData.toMaybe model.morning.projects)
        viewAfternoonStatus_ =
            viewMaybe 
                (\projects -> viewStatus model.afternoon.halfDay model.afternoon.mode projects)
                (RemoteData.toMaybe model.afternoon.projects)

    in
        div [] 
            [ viewHero 
            , section [ class "section" ] 
                [ div [ class "content" ] [ viewNav model.morning.date ]
                ]
            , section [ class "section" ] 
                [ div [ class "content" ] [ Html.map MorningMsg viewMorningStatus_ ]
                ]
            , section [ class "section" ] 
                [ div [ class "content" ] [ Html.map MorningMsg viewMorningChangeHalfDayType_ ]
                ]
            , section [ class "section" ] 
                [ div [ class "content" ] [ Html.map AfternoonMsg viewAfternoonStatus_ ]
                ]
            , section [ class "section" ] 
                [ div [ class "content" ] [ Html.map AfternoonMsg viewAfternoonChangeHalfDayType_ ]
                ]
            ]

-- Requests

getProjects : Cmd Msg
getProjects =
    get
        { url = "/project/"
        , expect = Http.expectJson 
            (fromResult >> GotProjectsResponse) 
            (Decode.list Project.decoder)
        }


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.morning.mode of
       EditNotes _ -> Sub.map MorningMsg (onMouseDown (outsideTarget [ "submit", "edit" ]))
       _ -> Sub.none


{-This code has been found here
https://dev.to/margaretkrutikova/elm-dom-node-decoder-to-detect-click-outside-3ioh 
-}
outsideTarget : List String -> Decode.Decoder MsgHalfDay
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

