module Page.Projects exposing (view)

import Browser exposing (Document)
import Html exposing (Html, button, div, header, i, input, span, text)
import Html.Attributes exposing (class, placeholder, readonly, value)
import List exposing (map)

import Api exposing (Project)

import NavBar as NB exposing (view)

viewProject : Project -> Html msg
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
            [ button [ class "button", class "is-danger",class "is-outlined" ]
                [ span [ class "icon" ] 
                    [ i [ class "fas fa-times" ] [] ]
                ]
            ]
        ]

viewNewProject : Html msg
viewNewProject = 
    div [ class "field", class "is-grouped" ]
        [ div [ class "control", class "is-expanded" ]
            [ input [ class "input", placeholder "Add new project" ] []
            ]
        , div [ class "control" ]
            [ button [ class "button", class "is-success", class "is-outlined" ]
                [ span [ class "icon" ] 
                    [ i [ class "fas fa-plus" ] [] ]
                ]
            ]
        ]

view : List Project -> Document msg
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