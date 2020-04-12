module Api.Office.Extended exposing (fromString, offices, toString)

import Api exposing (Office(..))


offices : List Office
offices =
    [ Home, Rennes, Poool, OutOfOffice ]


toString : Office -> String
toString office =
    case office of
        Home ->
            "Home"

        Rennes ->
            "Rennes"

        Poool ->
            "Poool"

        OutOfOffice ->
            "Out of office"


fromString : String -> Result String Office
fromString str =
    case str of
        "Home" ->
            Ok Home

        "Rennes" ->
            Ok Rennes

        "Poool" ->
            Ok Poool

        "Out of office" ->
            Ok OutOfOffice

        _ ->
            Err "Bad string"
