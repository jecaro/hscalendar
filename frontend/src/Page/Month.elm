module Page.Month exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (div, header, text)
import Html.Attributes exposing (class)

import Common exposing (viewNavBar)


type alias Msg = ()


type alias Model = ()

init : Model
init = ()

update : Msg -> Model -> (Model, Cmd Msg)
update _ model = ( model, Cmd.none)


view : Model -> Document Msg
view _ = 
    let 
        body = 
            [ viewNavBar []
            , div [ class "section" ]
                [ div [ class "column" ] 
                    [ div [ class "card" ] 
                        [ header [ class "card-header" ] 
                            [ div [ class "card-header-title" ] 
                                [ div [ class "title", class "is-4" ]
                                    [ text "Month" ] 
                                ]
                            ]
                        , div [ class "card-content" ]
                            [ div [ class "content" ] 
                                [ text "Show month content here" ]
                            ]
                        ]
                    ]
                ]
            ]
    in
    { title = "Month", body = body }

