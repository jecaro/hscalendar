module Main exposing (main)

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
import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Date exposing (Date, Unit(..), fromIsoString)
import Html
import Html.Extra exposing (nothing)
import Page.Day as PD exposing (Model, Msg(..), initWithToday, subscriptions, view)
import Page.Month as PM exposing (Model, Msg, init, view)
import Page.Projects as PP exposing (Model, projectsModified, view)
import Platform.Cmd exposing (batch)
import Platform.Sub exposing (none)
import RemoteData exposing (RemoteData(..), WebData)
import Request exposing (getProjects)
import Result exposing (toMaybe)
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Parser exposing (Parser, custom, map, oneOf, parse, s)



-- Types


type Page
    = NotFound
    | PageDay PD.Model
    | PageMonth PM.Model
    | PageProject PP.Model


type alias Model =
    { projects : WebData (List Project)
    , page : Page
    , key : Key
    }


type Msg
    = GotProjectsResponse (WebData (List Project))
    | DayMsg PD.Msg
    | MonthMsg PM.Msg
    | ProjectMsg PP.Msg
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
init _ url key =
    let
        url_ =
            if url.path == "/" then
                { url | path = "/diary" }

            else
                url

        ( model, cmd ) =
            stepUrl url_ { projects = Loading, page = NotFound, key = key }
    in
    ( model
    , batch [ getProjects GotProjectsResponse, cmd ]
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
                        ( dayModel_, cmd ) =
                            PD.update dayMsg dayModel
                    in
                    ( { model | page = PageDay dayModel_ }
                    , Cmd.map DayMsg cmd
                    )

                _ ->
                    ( model, Cmd.none )

        MonthMsg monthMsg ->
            case model.page of
                PageMonth monthModel ->
                    let
                        ( monthModel_, cmd ) =
                            PM.update monthMsg monthModel
                    in
                    ( { model | page = PageMonth monthModel_ }
                    , Cmd.map MonthMsg cmd
                    )

                _ ->
                    ( model, Cmd.none )

        ProjectMsg projectMsg ->
            case model.page of
                PageProject projectModel ->
                    let
                        ( projectModel_, cmd ) =
                            PP.update projectMsg projectModel

                        getProjectsIfNeeded =
                            if projectsModified projectMsg then
                                getProjects GotProjectsResponse

                            else
                                Cmd.none
                    in
                    ( { model | page = PageProject projectModel_ }
                    , batch [ Cmd.map ProjectMsg cmd, getProjectsIfNeeded ]
                    )

                _ ->
                    ( model, Cmd.none )

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

        UrlChanged url ->
            stepUrl url model



-- View


view : Model -> Document Msg
view model =
    case model.projects of
        Success projects ->
            case model.page of
                PageDay dayModel ->
                    let
                        document =
                            PD.view dayModel projects
                    in
                    { title = document.title
                    , body = List.map (Html.map DayMsg) document.body
                    }

                PageMonth monthModel ->
                    let
                        document =
                            PM.view monthModel
                    in
                    { title = document.title
                    , body = List.map (Html.map MonthMsg) document.body
                    }

                PageProject projectModel ->
                    let
                        document =
                            PP.view projectModel projects
                    in
                    { title = document.title
                    , body = List.map (Html.map ProjectMsg) document.body
                    }

                _ ->
                    { title = "Not found", body = [ nothing ] }

        _ ->
            { title = "Loading projects", body = [ nothing ] }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        PageDay dayModel ->
            Sub.map DayMsg <| PD.subscriptions dayModel

        PageProject _ ->
            Sub.map ProjectMsg PP.subscriptions

        _ ->
            none


stepDayWithToday : Model -> ( Model, Cmd Msg )
stepDayWithToday model =
    let
        ( dayModel, dayCmd ) =
            PD.initWithToday
    in
    ( { model | page = PageDay dayModel }, Cmd.map DayMsg dayCmd )


stepDayWithDate : Model -> Date -> ( Model, Cmd Msg )
stepDayWithDate model date =
    let
        ( dayModelWithDate, dayCmdWithDate ) =
            PD.initWithDate date
    in
    ( { model | page = PageDay dayModelWithDate }, Cmd.map DayMsg dayCmdWithDate )


stepMonth : Model -> ( Model, Cmd Msg )
stepMonth model =
    let
        ( monthModel, monthCmd ) =
            PM.init
    in
    ( { model | page = PageMonth monthModel }, Cmd.map MonthMsg monthCmd )


stepProject : Model -> ( Model, Cmd Msg )
stepProject model =
    let
        projectModel =
            PP.init
    in
    ( { model | page = PageProject projectModel }, Cmd.none )


diaryTodayParser : Parser a a
diaryTodayParser =
    s "diary"


diaryDateParser : Parser (Date -> a) a
diaryDateParser =
    custom "DATE" (toMaybe << fromIsoString)


monthParser : Parser a a
monthParser =
    s "month"


projectsParser : Parser a a
projectsParser =
    s "projects"


stepUrl : Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            oneOf
                [ map (stepDayWithToday model) diaryTodayParser
                , map (stepDayWithDate model) diaryDateParser
                , map (stepMonth model) monthParser
                , map (stepProject model) projectsParser
                ]
    in
    case parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )
