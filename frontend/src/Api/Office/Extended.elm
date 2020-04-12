module Api.Office.Extended exposing (fromString, offices, toString)

import Api


offices : List Api.Office
offices =
    [ Api.Home, Api.Rennes, Api.Poool, Api.OutOfOffice ]


toString : Api.Office -> String
toString office =
    case office of
        Api.Home ->
            "Home"

        Api.Rennes ->
            "Rennes"

        Api.Poool ->
            "Poool"

        Api.OutOfOffice ->
            "Out of office"


fromString : String -> Result String Api.Office
fromString str =
    case str of
        "Home" ->
            Ok Api.Home

        "Rennes" ->
            Ok Api.Rennes

        "Poool" ->
            Ok Api.Poool

        "Out of office" ->
            Ok Api.OutOfOffice

        _ ->
            Err "Bad string"
