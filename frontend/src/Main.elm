module Main exposing (main)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Events exposing (onMouseDown)
import Browser.Navigation exposing (Key, load, pushUrl)
import Date exposing (Unit(..))
import Html
import Html.Extra exposing (nothing)
import Json.Decode as Decode
import List exposing (member)
import Platform.Cmd exposing (batch)
import Platform.Sub exposing (none)
import RemoteData exposing (RemoteData(..), WebData)
import String exposing (concat)
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Parser exposing (map, oneOf, parse, s)

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
import Page.Day as PD exposing (Model, Msg(..), init, view)
import HalfDayWidget as HDW exposing (Mode(..), Msg(..))
import Request exposing (getProjects)

-- Types

type Page 
    = NotFound
    | PageDay PD.Model
    | PageProject

type alias Model =
    { projects : WebData (List Project)
    , page : Page
    , key : Key
    }

type Msg 
    = GotProjectsResponse (WebData (List Project))
    | DayMsg PD.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url



-- Main

main : Program () Model Msg
main =
    application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

-- Init

init : flags -> Url -> Key -> ( Model, Cmd Msg )
init _ _ key =
    let
        (dayModel, dayCmd) = PD.init 
    in
    ( { projects = Loading
      , page = PageDay dayModel
      , key = key
      }
    , batch [ getProjects GotProjectsResponse, Cmd.map DayMsg dayCmd ]
    )

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of

        GotProjectsResponse response -> 
            ( { model | projects = response }, Cmd.none )
        
        DayMsg dayMsg -> 
            case model.page of
                PageDay dayModel -> 
                    let
                        (dayModel_, cmd) = PD.update dayMsg dayModel
                    in
                        ( { model | page = PageDay dayModel_ }, Cmd.map DayMsg cmd )
                _ -> ( model, Cmd.none )

        -- Code found here
        -- https://github.com/elm/package.elm-lang.org/blob/master/src/frontend/Main.elm
        LinkClicked urlRequest ->
            case urlRequest of
                Internal url -> 
                    ( model
                    , pushUrl model.key (Url.toString url)
                    )
                External href ->
                    ( model
                    , load href
                    )      

        UrlChanged url -> stepUrl url model


-- View

view : Model -> Document Msg
view model =
    case model.projects of
        Success projects ->
            case model.page of
                PageDay dayModel -> 
                    let 
                        document = PD.view dayModel projects
                    in 
                        { title = document.title
                        , body = List.map (Html.map DayMsg) document.body
                        }
                _ -> { title = "No found", body = [ nothing ] }
        _ -> { title = "Loading projects", body = [ nothing ] }


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = 
    case model.page of
        PageDay dayModel ->
            case (dayModel.morning.mode, dayModel.afternoon.mode) of
                (EditNotes _, _) -> 
                        onMouseDown 
                            ( outsideTarget 
                                (DayMsg <| PD.MorningMsg EditWasCanceled) 
                                [ "submit", "edit" ]
                            )
                (_, EditNotes _) -> 
                        onMouseDown 
                            ( outsideTarget 
                                (DayMsg <| PD.AfternoonMsg EditWasCanceled) 
                                [ "submit", "edit" ]
                            )
                _ -> Sub.none
        _ -> none


{-This code has been found here
https://dev.to/margaretkrutikova/elm-dom-node-decoder-to-detect-click-outside-3ioh 
-}
outsideTarget : msg -> List String -> Decode.Decoder msg
outsideTarget toMsg domEltIds =
    Decode.field "target" (isOutsideDomEltId domEltIds)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed toMsg
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

stepUrl : Url -> Model -> (Model, Cmd Msg)
stepUrl url model =
    let
        ( dayModel, dayCmd ) = PD.init
        parser = oneOf 
            [ map ( { model | page = PageDay dayModel }, Cmd.map DayMsg dayCmd) (s "editday")
            , map ( { model | page = PageProject }, Cmd.none ) (s "projects")
            ]
    in
        case parse parser url of
            Just answer ->
                answer

            Nothing ->
                ( { model | page = NotFound }
                , Cmd.none
                )
