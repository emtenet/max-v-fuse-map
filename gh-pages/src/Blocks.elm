module Blocks exposing
    ( Blocks
    , decode
    , empty
    , get
    , map
    )

import Block exposing (Block)
import Dict exposing (Dict)
import Json.Decode exposing (Decoder, Value)


type Blocks
    = Blocks (Dict Key Block)


type alias Key =
    ( Int, Int )


type alias KeyValue =
    ( Key, Block )


empty : Blocks
empty =
    Blocks Dict.empty


decode : Decoder Blocks
decode =
    let
        keyValue block =
            ( ( block.x, block.y ), block )
    in
    Block.decode
        |> Json.Decode.map keyValue
        |> Json.Decode.list
        |> Json.Decode.map Dict.fromList
        |> Json.Decode.map Blocks


get : { index | x : Int, y : Int } -> Blocks -> Maybe Block
get index (Blocks dict) =
    Dict.get ( index.x, index.y ) dict


map : (Block -> t) -> Blocks -> List t
map f (Blocks dict) =
    let
        fold _ block acc =
            f block :: acc
    in
    Dict.foldr fold [] dict
