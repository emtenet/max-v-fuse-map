module BlockIndex exposing
    ( BlockIndex
    , decode
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
