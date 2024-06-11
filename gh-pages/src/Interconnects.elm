module Interconnects exposing
    ( Interconnects
    , decode
    , empty
    , fold
    , froms
    , get
    , isEmpty
    , map
    , member
    , thrus
    )

import Dict exposing (Dict)
import Interconnect exposing (Interconnect)
import InterconnectIndexSet exposing (InterconnectIndexSet)
import Json.Decode exposing (Decoder, Value)


type Interconnects
    = Interconnects (Dict Int Interconnect)


empty : Interconnects
empty =
    Interconnects
        Dict.empty


decode : Decoder Interconnects
decode =
    let
        keyValue mux =
            ( mux.i, mux )
    in
    Interconnect.decode
        |> Json.Decode.map keyValue
        |> Json.Decode.list
        |> Json.Decode.map Dict.fromList
        |> Json.Decode.map Interconnects


isEmpty : Interconnects -> Bool
isEmpty (Interconnects set) =
    Dict.isEmpty set


member : Int -> Interconnects -> Bool
member i (Interconnects set) =
    Dict.member i set


get : Int -> Interconnects -> Maybe Interconnect
get i (Interconnects set) =
    Dict.get i set


fold : (Interconnect -> a -> a) -> a -> Interconnects -> a
fold with init (Interconnects set) =
    Dict.foldl (\k v a -> with v a) init set


map : (Interconnect -> t) -> Interconnects -> List t
map with (Interconnects set) =
    let
        f k v acc =
            with v :: acc
    in
    Dict.foldl f [] set


froms : Interconnects -> InterconnectIndexSet
froms (Interconnects set) =
    Dict.foldl fromsUnion InterconnectIndexSet.empty set


fromsUnion :
    Int
    -> Interconnect
    -> InterconnectIndexSet
    -> InterconnectIndexSet
fromsUnion _ m =
    InterconnectIndexSet.union m.froms


thrus : Interconnects -> InterconnectIndexSet
thrus (Interconnects set) =
    Dict.foldl thrusUnion InterconnectIndexSet.empty set


thrusUnion :
    Int
    -> Interconnect
    -> InterconnectIndexSet
    -> InterconnectIndexSet
thrusUnion _ m =
    InterconnectIndexSet.union m.thrus
