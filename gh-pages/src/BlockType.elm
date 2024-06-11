module BlockType exposing
    ( BlockType(..)
    , decode
    , toString
    )

import Json.Decode exposing (Decoder, Value)


type BlockType
    = Logic
    | Row
    | Column
    | Global
    | Other


decode : Decoder BlockType
decode =
    Json.Decode.map fromString Json.Decode.string


fromString : String -> BlockType
fromString s =
    case s of
        "logic" ->
            Logic

        "row" ->
            Row

        "column" ->
            Column

        "global" ->
            Global

        _ ->
            Other


toString : BlockType -> String
toString t =
    case t of
        Logic ->
            "logic"

        Row ->
            "row"

        Column ->
            "column"

        Global ->
            "global"

        Other ->
            "other"
