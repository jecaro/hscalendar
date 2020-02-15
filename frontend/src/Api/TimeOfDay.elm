module Api.TimeOfDay exposing (TimeOfDay, decoder, encode, fromString, toString)

import Json.Decode
import Json.Encode

import String exposing (fromInt, toInt, split, pad)

type alias TimeOfDay = 
  { hours : Int
  , minutes : Int
  , seconds : Int
  }

toString : TimeOfDay -> String
toString { hours, minutes, seconds} = 
  let pad2 x = pad 2 '0' <| fromInt x
  in pad2 hours ++ ":" ++ pad2 minutes ++ ":"  ++ pad2 seconds

fromString : String -> Result String TimeOfDay
fromString str = 
  let
      items = split ":" str 
      maybeInts = List.map toInt items
  in
    case maybeInts of
      [Just hours, Just minutes, Just seconds] -> 
        Ok (TimeOfDay hours minutes seconds)
      _ -> Err "Error"
  

encode : TimeOfDay -> Json.Encode.Value
encode = toString >> Json.Encode.string

decoder : Json.Decode.Decoder TimeOfDay
decoder = Json.Decode.string
    |> Json.Decode.andThen
        (\str ->
            case fromString str of
                Result.Err e ->
                    Json.Decode.fail e
                Result.Ok a ->
                    Json.Decode.succeed a
        )