module Page.Projects exposing (Model, Msg, init, projectsModified, subscriptions, update, view)

import Api 
import Browser 
import Browser.Events as Events
import Common 
import Html exposing (Html, button, div, header, i, input, span, text)
import Html.Attributes exposing (class, classList, id, placeholder, readonly, value)
import Html.Events exposing (onBlur, onClick, onDoubleClick, onInput)
import Html.Events.Extended exposing (onEnter)
import Html.Extra exposing (nothing)
import List
import RemoteData 
import Request 


type Msg
    = ProjectAdded
    | ProjectDeleted Api.Project
    | ProjectRenamed Api.Project Api.Project
    | ProjectEdited Api.Project
    | ProjectToAddEdited String
    | EditWasCanceled
    | GotResponse (RemoteData.WebData ())
    | NoOp


type alias Model =
    { response : RemoteData.WebData ()
    , projectToAdd : String
    , editedProject : Maybe EditedProject
    }


type alias EditedProject =
    { project : Api.Project
    , currentName : String
    }


projectsModified : Msg -> Bool
projectsModified msg =
    case msg of
        GotResponse (RemoteData.Success _) ->
            True

        _ ->
            False


init : Model
init =
    { response = RemoteData.NotAsked, projectToAdd = "", editedProject = Nothing }


viewProject : Maybe EditedProject -> Api.Project -> Html Msg
viewProject maybeEditedProject project =
    let
        ( edited, current, renamed ) =
            let
                noOp =
                    ( False, project.unProject, always NoOp )
            in
            case maybeEditedProject of
                Nothing ->
                    noOp

                Just editedProject ->
                    if project == editedProject.project then
                        ( True
                        , editedProject.currentName
                        , ProjectRenamed project << Api.Project
                        )

                    else
                        noOp
    in
    div [ class "field", class "is-grouped" ]
        [ div [ class "control", class "is-expanded" ]
            [ input
                [ classList
                    [ ( "input", True )
                    , ( "is-static", not edited )
                    ]
                , value current
                , readonly (not edited)
                , onBlur EditWasCanceled
                , onDoubleClick (ProjectEdited project)
                , onEnter renamed
                ]
                []
            ]
        , div [ class "control" ]
            [ button
                [ class "button"
                , class "is-danger"
                , class "is-outlined"
                , onClick (ProjectDeleted project)
                ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-times" ] [] ]
                ]
            ]
        ]


viewNewProject : String -> Html Msg
viewNewProject projectToAdd =
    div [ class "field", class "is-grouped" ]
        [ div [ class "control", class "is-expanded" ]
            [ input
                [ class "input"
                , onInput ProjectToAddEdited
                , onEnter (always ProjectAdded)
                , placeholder "Add new project"
                , value projectToAdd
                , id "edit"
                ]
                []
            ]
        , div [ class "control" ]
            [ button
                [ class "button"
                , class "is-success"
                , class "is-outlined"
                , onClick ProjectAdded
                , id "submit"
                ]
                [ span [ class "icon" ]
                    [ i [ class "fas fa-plus" ] [] ]
                ]
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ProjectAdded ->
            ( { model | projectToAdd = "", response = RemoteData.Loading }
            , Request.addProject GotResponse (Api.Project model.projectToAdd)
            )

        ProjectDeleted project ->
            ( { model | response = RemoteData.Loading }
            , Request.deleteProject GotResponse project )

        ProjectRenamed from to ->
            ( { model | response = RemoteData.Loading }
            , Request.renameProject GotResponse from to )

        GotResponse response ->
            ( { model | response = response }, Cmd.none )

        ProjectEdited project ->
            let
                editedProject =
                    Just { project = project, currentName = project.unProject }
            in
            ( { model | editedProject = editedProject }, Cmd.none )

        ProjectToAddEdited projectToAdd ->
            ( { model | projectToAdd = projectToAdd }, Cmd.none )

        EditWasCanceled ->
            ( { model | editedProject = Nothing, projectToAdd = "" }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> List Api.Project -> Browser.Document Msg
view model projects =
    let
        loadingIcon =
            if RemoteData.isLoading model.response then
                div [ class "card-header-icon" ]
                    [ span [ class "icon" ]
                        [ i [ class "fas fa-spinner fa-spin" ] [] ]
                    ]

            else
                nothing

        body =
            [ Common.viewNavBar []
            , div [ class "section" ]
                [ div [ class "column" ]
                    [ div [ class "card" ]
                        [ header [ class "card-header" ]
                            [ div [ class "card-header-title" ]
                                [ div [ class "title", class "is-4" ]
                                    [ text "Projects" ]
                                ]
                            , loadingIcon
                            ]
                        , div [ class "card-content" ]
                            [ div [ class "content" ] <|
                                List.map (viewProject model.editedProject) projects
                                    ++ [ viewNewProject model.projectToAdd
                                       , Common.viewErrorFromWebData 
                                            model.response "The command returned an error"
                                       ]
                            ]
                        ]
                    ]
                ]
            ]
    in
    { title = "Projects", body = body }


subscriptions : Sub Msg
subscriptions =
    Events.onMouseDown <| Common.outsideTarget EditWasCanceled [ "submit", "edit" ]
