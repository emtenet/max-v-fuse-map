module Block exposing
    ( Block
    , decode
    , froms
    , get
    , thrus
    )

import BlockIndex exposing (BlockIndex)
import BlockType exposing (BlockType)
import Dict exposing (Dict)
import InterconnectIndexSet exposing (InterconnectIndexSet)
import InterconnectType exposing (InterconnectType)
import Interconnects exposing (Interconnects)
import Json.Decode exposing (Decoder, Value)


type alias Block =
    { x : Int
    , y : Int
    , t : BlockType
    , c4s : Interconnects
    , inputs : Interconnects
    , locals : Interconnects
    , logics : Interconnects
    , r4s : Interconnects
    }


decode : Decoder Block
decode =
    Json.Decode.map8 Block
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)
        (Json.Decode.field "t" BlockType.decode)
        (Json.Decode.field "c4s" Interconnects.decode)
        (Json.Decode.field "inputs" Interconnects.decode)
        (Json.Decode.field "locals" Interconnects.decode)
        (Json.Decode.field "logics" Interconnects.decode)
        (Json.Decode.field "r4s" Interconnects.decode)


get : InterconnectType -> Block -> Interconnects
get t block =
    case t of
        InterconnectType.C4 ->
            block.c4s

        InterconnectType.Input ->
            block.inputs

        InterconnectType.Local ->
            block.locals

        InterconnectType.Logic ->
            block.logics

        InterconnectType.R4 ->
            block.r4s


froms : Block -> InterconnectIndexSet
froms block =
    InterconnectIndexSet.unions
        [ Interconnects.froms block.c4s
        , Interconnects.froms block.inputs
        , Interconnects.froms block.locals
        , Interconnects.froms block.logics
        , Interconnects.froms block.r4s
        ]


thrus : Block -> InterconnectIndexSet
thrus block =
    InterconnectIndexSet.unions
        [ Interconnects.thrus block.c4s
        , Interconnects.thrus block.inputs
        , Interconnects.thrus block.locals
        , Interconnects.thrus block.logics
        , Interconnects.thrus block.r4s
        ]
