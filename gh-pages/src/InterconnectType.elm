module InterconnectType exposing
    ( InterconnectType(..)
    , decode
    , id
    )

import Json.Decode exposing (Decoder, Value)


type InterconnectType
    = C4
    | Input
    | Local
    | Logic
    | R4


decode : Decoder InterconnectType
decode =
    Json.Decode.string
        |> Json.Decode.andThen decodeString


decodeString : String -> Decoder InterconnectType
decodeString s =
    case s of
        "c4" ->
            Json.Decode.succeed C4

        "input" ->
            Json.Decode.succeed Input

        "local" ->
            Json.Decode.succeed Local

        "logic" ->
            Json.Decode.succeed Logic

        "r4" ->
            Json.Decode.succeed R4

        _ ->
            Json.Decode.fail ("Invalid interconnect type: " ++ s)


id : InterconnectType -> String
id t =
    case t of
        C4 ->
            "c4"

        Input ->
            "input"

        Local ->
            "local"

        Logic ->
            "logic"

        R4 ->
            "r4"
