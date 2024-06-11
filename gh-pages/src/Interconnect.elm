module Interconnect exposing
    ( Interconnect
    , decode
    )

import InterconnectIndexSet exposing (InterconnectIndexSet)
import Json.Decode exposing (Decoder)


type alias Interconnect =
    { i : Int
    , froms : InterconnectIndexSet
    , thrus : InterconnectIndexSet
    }


decode : Decoder Interconnect
decode =
    Json.Decode.map3 Interconnect
        (Json.Decode.field "i" Json.Decode.int)
        (Json.Decode.field "froms" InterconnectIndexSet.decode)
        (Json.Decode.field "thrus" InterconnectIndexSet.decode)
