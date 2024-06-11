module InterconnectsIndex exposing
    ( InterconnectsIndex
    , from
    , id
    , join
    , member
    )

import InterconnectIndex exposing (InterconnectIndex)
import InterconnectType exposing (InterconnectType)
import Json.Decode exposing (Decoder, Value)


type alias InterconnectsIndex =
    { x : Int
    , y : Int
    , t : InterconnectType
    }


member : InterconnectIndex -> InterconnectsIndex -> Bool
member p ps =
    (p.x == ps.x) && (p.y == ps.y) && (p.t == ps.t)


from : InterconnectIndex -> InterconnectsIndex
from p =
    { x = p.x, y = p.y, t = p.t }


join :
    { block | x : Int, y : Int }
    -> InterconnectType
    -> InterconnectsIndex
join block t =
    { x = block.x, y = block.y, t = t }


id : InterconnectsIndex -> String
id interconnects =
    String.concat
        [ interconnects.t |> InterconnectType.id
        , "-"
        , interconnects.x |> String.fromInt
        , "-"
        , interconnects.y |> String.fromInt
        ]
