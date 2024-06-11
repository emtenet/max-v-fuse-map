module BlockIndex exposing
    ( BlockIndex
    , decode
    , from
    , id
    )

import Json.Decode exposing (Decoder, Value)


type alias BlockIndex =
    { x : Int
    , y : Int
    }


decode : Decoder BlockIndex
decode =
    Json.Decode.map2 BlockIndex
        (Json.Decode.index 0 Json.Decode.int)
        (Json.Decode.index 1 Json.Decode.int)


from : { block | x : Int, y : Int } -> BlockIndex
from block =
    { x = block.x, y = block.y }


id : BlockIndex -> String
id block =
    String.concat
        [ "block-"
        , block.x |> String.fromInt
        , "-"
        , block.y |> String.fromInt
        ]
