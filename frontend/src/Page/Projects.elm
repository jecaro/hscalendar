module Page.Projects exposing (Model, Msg, init, projectsModified, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Events exposing (onMouseDown)
import Html exposing (Html, button, div, header, i, input, span, text)
import Html.Attributes exposing (class, classList, id, placeholder, readonly, value)
import Html.Events exposing (onBlur, onClick, onDoubleClick, onInput)
import Html.Events.Extended exposing (onEnter)
import List exposing (map)
import RemoteData exposing (RemoteData(..), WebData)

import Api exposing (Project)
import Common exposing (outsideTarget, viewErrorFromWebData, viewNavBar)
import Request exposing (addProject, deleteProject, renameProject)


type Msg
    = ProjectAdded
    | ProjectDeleted Project
    | ProjectRenamed Project Project
    | ProjectEdited Project
    | ProjectToAddEdited String
    | EditWasCanceled
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
                , onBlur EditWasCanceled
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
                ] []
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

        EditWasCanceled -> 
            ( { model | editedProject = Nothing, projectToAdd = "" }, Cmd.none )

        NoOp -> ( model, Cmd.none )


view : Model -> List Project -> Document Msg
view model projects = 
    let 
        body = 
            [ viewNavBar []
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
                                ++ 
                                [ viewNewProject model.projectToAdd 
                                , viewErrorFromWebData model.response "The command returned an error"
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
    onMouseDown <| outsideTarget EditWasCanceled [ "submit", "edit" ]


