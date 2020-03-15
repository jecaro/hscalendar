module Main exposing (main)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Date exposing (Unit(..))
import Html
import Html.Extra exposing (nothing)
import Platform.Cmd exposing (batch)
import Platform.Sub exposing (none)
import RemoteData exposing (RemoteData(..), WebData)
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
import Page.Day as PD exposing (Model, Msg(..), init, subscriptions, view)
import Page.Projects as PP exposing (view)
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
                PageProject -> PP.view projects
                _ -> { title = "No found", body = [ nothing ] }
        _ -> { title = "Loading projects", body = [ nothing ] }


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = 
    case model.page of
        PageDay dayModel -> Sub.map DayMsg (PD.subscriptions dayModel)
        _ -> none

stepUrl : Url -> Model -> (Model, Cmd Msg)
stepUrl url model =
    let
        ( dayModel, dayCmd ) = PD.init
        parser = oneOf 
            [ map ( { model | page = PageDay dayModel }, Cmd.map DayMsg dayCmd) (s "today")
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
