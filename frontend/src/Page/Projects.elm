module Page.Projects exposing (Model, Msg, init, projectsModified, update, view)

import Browser exposing (Document)
import Html exposing (Html, button, div, header, i, input, span, text)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extended exposing (onEnter)
import Html.Attributes exposing (class, placeholder, readonly, value)
import List exposing (map)
import RemoteData exposing (RemoteData(..), WebData)

import Api exposing (Project)

import NavBar as NB exposing (view)
import Request exposing (addProject, deleteProject)


type Msg
    = ProjectDeleted Project
    | ProjectAdded
    | ProjectToAddEdited String
    | GotResponse (WebData ())


type alias Model = 
    { response : WebData ()
    , projectToAdd : String
    }

projectsModified : Msg -> Bool
projectsModified msg =
    case msg of
        GotResponse (Success _) -> True
        _ -> False

init : Model
init = { response = NotAsked, projectToAdd = "" }

viewProject : Project -> Html Msg
viewProject project = 
    div [ class "field", class "is-grouped" ]
        [ div [ class "control", class "is-expanded" ]
            [ input 
                [ class "input"
                , class "is-static"
                , value project.unProject
                , readonly True 
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
                , onClick <| ProjectAdded
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
        ProjectToAddEdited projectToAdd ->
            ( { model | projectToAdd = projectToAdd }, Cmd.none )


view : List Project -> Document Msg
view projects = 
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
                                <| map viewProject projects 
                                ++ [ viewNewProject ]
                            ]
                        ]
                    ]
                ]
            ]
    in
    { title = "Projects", body = body }