module Page.Projects exposing (Model, Msg, init, projectsModified, update, view)

import Browser exposing (Document)
import Html exposing (Html, button, div, header, i, input, span, text)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Html.Events.Extended exposing (onEnter)
import Html.Attributes exposing (class, classList, placeholder, readonly, value)
import List exposing (map)
import RemoteData exposing (RemoteData(..), WebData)

import Api exposing (Project)

import NavBar as NB exposing (view)
import Request exposing (addProject, deleteProject, renameProject)


type Msg
    = ProjectAdded
    | ProjectDeleted Project
    | ProjectEdited Project
    | ProjectRenamed Project Project
    | ProjectToAddEdited String
    | GotResponse (WebData ())
    | NoOp


type alias Model = 
    { response : WebData ()
    , projectToAdd : String
    , editedProject : Maybe EditedProject
    }

type alias EditedProject =
    { project : Project
    , currentName : String
    }

projectsModified : Msg -> Bool
projectsModified msg =
    case msg of
        GotResponse (Success _) -> True
        _ -> False

init : Model
init = { response = NotAsked, projectToAdd = "", editedProject = Nothing }

viewProject : Maybe EditedProject -> Project -> Html Msg
viewProject maybeEditedProject project =
    let
        (edited, current, renamed) = 
            let
                noOp = (False, project.unProject, always NoOp)
            in
            
            case maybeEditedProject of
                Nothing -> noOp
                Just editedProject -> 
                    if project == editedProject.project
                        then 
                            ( True
                            , editedProject.currentName
                            , ProjectRenamed project << Project 
                            )
                        else noOp
    in
    div [ class "field", class "is-grouped" ]
        [ div [ class "control", class "is-expanded" ]
            [ input 
                [ classList 
                    [ ("input", True)
                    , ("is-static", not edited) 
                    ]
                , value current
                , readonly (not edited)
                , onDoubleClick (ProjectEdited project)
                , onEnter renamed
                ] []
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

viewNewProject : Html Msg
viewNewProject = 
    div [ class "field", class "is-grouped" ]
        [ div [ class "control", class "is-expanded" ]
            [ input 
                [ class "input"
                , onInput ProjectToAddEdited
                , onEnter (always ProjectAdded)
                , placeholder "Add new project"
                ] []
            ]
        , div [ class "control" ]
            [ button 
                [ class "button"
                , class "is-success"
                , class "is-outlined" 
                , onClick ProjectAdded
                ]
                [ span [ class "icon" ] 
                    [ i [ class "fas fa-plus" ] [] ]
                ]
            ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        ProjectDeleted project -> 
            (model, deleteProject GotResponse project)
        -- Error
        GotResponse response -> 
            ( { model | response = response }, Cmd.none )

        ProjectAdded ->
            ( { model | projectToAdd = "" }
              , addProject GotResponse (Project model.projectToAdd)
            )

        ProjectEdited project ->
            let
                editedProject = Just { project = project, currentName = project.unProject}
            in
            ( { model | editedProject = editedProject }, Cmd.none )

        ProjectToAddEdited projectToAdd ->
            ( { model | projectToAdd = projectToAdd }, Cmd.none )

        ProjectRenamed from to -> 
            ( model, renameProject GotResponse from to )

        NoOp -> ( model, Cmd.none )


view : Model -> List Project -> Document Msg
view model projects = 
    let 
        body = 
            [ NB.view []
            , div [ class "section" ]
                [ div [ class "column" ] 
                    [ div [ class "card" ] 
                        [ header [ class "card-header" ] 
                            [ div [ class "card-header-title" ] 
                                [ div [ class "title", class "is-4" ]
                                    [ text "Projects" ] 
                                ]
                            ]
                        , div [ class "card-content" ]
                            [ div [ class "content" ] 
                                <| map (viewProject model.editedProject) projects 
                                ++ [ viewNewProject ]
                            ]
                        ]
                    ]
                ]
            ]
    in
    { title = "Projects", body = body }