module Page.Projects exposing (view)

import Browser exposing (Document)
import Html exposing (div, header, li, text, ul)
import Html.Attributes exposing (class)
import List exposing (map)

import Api exposing (Project)

import NavBar as NB exposing (view)

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
                                [ ul [] (map (\p -> li [] [ text p.unProject ]) projects)
                                ]
                            ]
                        ]
                    ]
                ]
            ]
    in
    { title = "Projects", body = body }