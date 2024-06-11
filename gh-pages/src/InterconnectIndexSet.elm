module InterconnectIndexSet exposing
    ( InterconnectIndexSet
    , decode
    , empty
    , insert
    , isEmpty
    , member
    , union
    )

import Dict exposing (Dict)
import InterconnectIndex exposing (InterconnectIndex)
import InterconnectType exposing (InterconnectType)
import Json.Decode exposing (Decoder, Value)


type alias Key =
    ( Int, Int, Int )


type InterconnectIndexSet
    = InterconnectIndexSet
        { c4s : Dict Key InterconnectIndex
        , inputs : Dict Key InterconnectIndex
        , locals : Dict Key InterconnectIndex
        , logics : Dict Key InterconnectIndex
        , r4s : Dict Key InterconnectIndex
        }


empty : InterconnectIndexSet
empty =
    InterconnectIndexSet
        { c4s = Dict.empty
        , inputs = Dict.empty
        , locals = Dict.empty
        , logics = Dict.empty
        , r4s = Dict.empty
        }


key : InterconnectIndex -> Key
key index =
    ( index.x, index.y, index.i )


decode : Decoder InterconnectIndexSet
decode =
    Json.Decode.list InterconnectIndex.decode
        |> Json.Decode.map fromList


fromList : List InterconnectIndex -> InterconnectIndexSet
fromList list =
    List.foldl insert empty list


insert : InterconnectIndex -> InterconnectIndexSet -> InterconnectIndexSet
insert index (InterconnectIndexSet set) =
    case index.t of
        InterconnectType.C4 ->
            InterconnectIndexSet
                { set | c4s = Dict.insert (key index) index set.c4s }

        InterconnectType.Input ->
            InterconnectIndexSet
                { set | inputs = Dict.insert (key index) index set.inputs }

        InterconnectType.Local ->
            InterconnectIndexSet
                { set | locals = Dict.insert (key index) index set.locals }

        InterconnectType.Logic ->
            InterconnectIndexSet
                { set | logics = Dict.insert (key index) index set.logics }

        InterconnectType.R4 ->
            InterconnectIndexSet
                { set | r4s = Dict.insert (key index) index set.r4s }


isEmpty : InterconnectIndexSet -> Bool
isEmpty (InterconnectIndexSet set) =
    Dict.isEmpty set.c4s
        && Dict.isEmpty set.inputs
        && Dict.isEmpty set.locals
        && Dict.isEmpty set.logics
        && Dict.isEmpty set.r4s


member : InterconnectIndex -> InterconnectIndexSet -> Bool
member index (InterconnectIndexSet set) =
    case index.t of
        InterconnectType.C4 ->
            Dict.member (key index) set.c4s

        InterconnectType.Input ->
            Dict.member (key index) set.inputs

        InterconnectType.Local ->
            Dict.member (key index) set.locals

        InterconnectType.Logic ->
            Dict.member (key index) set.logics

        InterconnectType.R4 ->
            Dict.member (key index) set.r4s


union : InterconnectIndexSet -> InterconnectIndexSet -> InterconnectIndexSet
union (InterconnectIndexSet left) (InterconnectIndexSet right) =
    InterconnectIndexSet
        { c4s = Dict.union left.c4s right.c4s
        , inputs = Dict.union left.inputs right.inputs
        , locals = Dict.union left.locals right.locals
        , logics = Dict.union left.logics right.logics
        , r4s = Dict.union left.r4s right.r4s
        }
