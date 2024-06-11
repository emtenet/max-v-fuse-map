module Device exposing
    ( Device
    , decode
    , empty
    )

import BlockIndex exposing (BlockIndex)
import Blocks exposing (Blocks)
import Json.Decode exposing (Decoder, Value)


type alias Device =
    { width : Int
    , height : Int
    , top : Int
    , left : Int
    , title : String
    , global : BlockIndex
    , blocks : Blocks
    }


decode : Decoder Device
decode =
    Json.Decode.map7 Device
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)
        (Json.Decode.field "top" Json.Decode.int)
        (Json.Decode.field "left" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "global" BlockIndex.decode)
        (Json.Decode.field "blocks" Blocks.decode)


empty : String -> Device
empty title =
    { width = 10
    , height = 1
    , left = 0
    , top = 0
    , title = title
    , global = { x = 0, y = 0 }
    , blocks = Blocks.empty
    }
