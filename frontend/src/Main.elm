module Main exposing (main)

import Api
import Browser 
import Browser.Navigation as Navigation
import Date 
import Html
import Html.Extra exposing (nothing)
import Page.Day as PD
import Page.Month as PM
import Page.Projects as PP 
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import RemoteData 
import Request 
import Result 
import Url 
import Url.Parser as Parser



-- Types


type Page
    = NotFound
    | PageDay PD.Model
    | PageMonth PM.Model
    | PageProject PP.Model


type alias Model =
    { projects : RemoteData.WebData (List Api.Project)
    , page : Page
    , key : Navigation.Key
    }


type Msg
    = GotProjectsResponse (RemoteData.WebData (List Api.Project))
    | DayMsg PD.Msg
    | MonthMsg PM.Msg
    | ProjectMsg PP.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url



-- Main


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Init


init : flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        url_ =
            if url.path == "/" then
                { url | path = "/diary" }

            else
                url

        ( model, cmd ) =
            stepUrl url_ { projects = RemoteData.Loading, page = NotFound, key = key }
    in
    ( model
    , Cmd.batch [ Request.getProjects GotProjectsResponse, cmd ]
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
                            if PP.projectsModified projectMsg then
                                Request.getProjects GotProjectsResponse

                            else
                                Cmd.none
                    in
                    ( { model | page = PageProject projectModel_ }
                    , Cmd.batch [ Cmd.map ProjectMsg cmd, getProjectsIfNeeded ]
                    )

                _ ->
                    ( model, Cmd.none )

        -- Code found here
        -- https://github.com/elm/package.elm-lang.org/blob/master/src/frontend/Main.elm
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        UrlChanged url ->
            stepUrl url model



-- View


view : Model -> Browser.Document Msg
view model =
    case model.projects of
        RemoteData.Success projects ->
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
            Sub.none


stepDayWithToday : Model -> ( Model, Cmd Msg )
stepDayWithToday model =
    let
        ( dayModel, dayCmd ) =
            PD.initWithToday
    in
    ( { model | page = PageDay dayModel }, Cmd.map DayMsg dayCmd )


stepDayWithDate : Model -> Date.Date -> ( Model, Cmd Msg )
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


diaryTodayParser : Parser.Parser a a
diaryTodayParser =
    Parser.s "diary"


diaryDateParser : Parser.Parser (Date.Date -> a) a
diaryDateParser =
    Parser.custom "DATE" (Result.toMaybe << Date.fromIsoString)


monthParser : Parser.Parser a a
monthParser =
    Parser.s "month"


projectsParser : Parser.Parser a a
projectsParser =
    Parser.s "projects"


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        parser =
            Parser.oneOf
                [ Parser.map (stepDayWithToday model) diaryTodayParser
                , Parser.map (stepDayWithDate model) diaryDateParser
                , Parser.map (stepMonth model) monthParser
                , Parser.map (stepProject model) projectsParser
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )
