module InterconnectIndex exposing
    ( InterconnectIndex
    , decode
    , join
    )

import InterconnectType exposing (InterconnectType)
import Json.Decode exposing (Decoder, Value)


type alias InterconnectIndex =
    { x : Int
    , y : Int
    , t : InterconnectType
    , i : Int
    }


decode : Decoder InterconnectIndex
decode =
    Json.Decode.map4 InterconnectIndex
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)
        (Json.Decode.field "t" InterconnectType.decode)
        (Json.Decode.field "i" Json.Decode.int)


join :
    { interconnects | x : Int, y : Int, t : InterconnectType }
    -> Int
    -> InterconnectIndex
join interconnects i =
    { x = interconnects.x
    , y = interconnects.y
    , t = interconnects.t
    , i = i
    }
