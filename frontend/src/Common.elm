module Common exposing (outsideTarget, viewNavBar)

import Html exposing (Html, a, div, nav, text)
import Html.Attributes exposing (class, href)
import Json.Decode as Decode
import List exposing (member)
import String exposing (concat)

viewNavBar : List (Html msg) -> Html msg
viewNavBar items =
    nav [ class "navbar", class "is-fixed-top", class "is-primary" ]
        [ div [ class "navbar-brand" ] <|
            [ a [ class "navbar-item", href "/today" ] [ text "Today" ]
            , a [ class "navbar-item", href "/projects" ] [ text "Projects" ]
            ] ++ items
        ]

{-This code has been found here
https://dev.to/margaretkrutikova/elm-dom-node-decoder-to-detect-click-outside-3ioh 
-}
outsideTarget : msg -> List String -> Decode.Decoder msg
outsideTarget toMsg domEltIds =
    Decode.field "target" (isOutsideDomEltId domEltIds)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed toMsg
                else
                    Decode.fail <| "inside " ++ concat domEltIds
            )


isOutsideDomEltId : List String -> Decode.Decoder Bool
isOutsideDomEltId domEltIds =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if member id domEltIds then
                        -- found match by id
                        Decode.succeed False
                    else
                        -- try next decoder
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> isOutsideDomEltId domEltIds |> Decode.field "parentNode")
        -- fallback if all previous decoders failed
        , Decode.succeed True
        ]
