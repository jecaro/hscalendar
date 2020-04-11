module Common exposing 
    ( dateUrl
    , outsideTarget
    , viewError
    , viewErrorFromError
    , viewErrorFromWebData
    , viewNavBar
    )

import Date exposing (Date, toIsoString)
import Html exposing (Html, a, article, br, div, nav, p, text)
import Html.Attributes exposing (class, href)
import Html.Extra exposing (nothing)
import Http exposing (Error)
import Http.Extended exposing (errorToString)
import Json.Decode as Decode
import List exposing (concatMap, member)
import RemoteData exposing (RemoteData(..), WebData)
import String exposing (concat)


dateUrl : Date -> String
dateUrl date = "/" ++ toIsoString date


viewNavBar : List (Html msg) -> Html msg
viewNavBar items =
    nav [ class "navbar", class "is-fixed-top", class "is-primary" ]
        [ div [ class "navbar-brand" ] <|
            [ a [ class "navbar-item", href "/diary" ] [ text "Diary" ]
            , a [ class "navbar-item", href "/month" ] [ text "Month" ]
            , a [ class "navbar-item", href "/projects" ] [ text "Projects" ]
            ] ++ items
        ]

viewError : List String -> Html msg
viewError messages =
    article [ class "message", class "is-danger" ] 
        [ div [ class "message-header" ]
            [ p [] [ text "Error" ] ]
        , div [ class "message-body" ] 
            [ p [] <|
                concatMap (\t -> [ text t, br [] [] ]) messages 
            ]
        ]

viewErrorFromError : Error -> String -> Html msg
viewErrorFromError error msg = viewError [ msg, errorToString error ]

viewErrorFromWebData : WebData a -> String -> Html msg
viewErrorFromWebData data msg =
    case data of
       Failure error -> viewError [ msg, errorToString error ]
       _ -> nothing


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
