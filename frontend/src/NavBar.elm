module NavBar exposing (view)

import Html exposing (Html, a, div, nav, text)
import Html.Attributes exposing (class, href)

view : List (Html msg) -> Html msg
view items =
    nav [ class "navbar", class "is-fixed-top", class "is-primary" ]
        [ div [ class "navbar-brand" ] <|
            [ a [ class "navbar-item", href "/today" ] [ text "Today" ]
            , a [ class "navbar-item", href "/projects" ] [ text "Projects" ]
            ] ++ items
        ]
