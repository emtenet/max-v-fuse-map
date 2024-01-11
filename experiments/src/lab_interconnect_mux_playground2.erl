-module(lab_interconnect_mux_playground2).

-export([run/0]).

% Are there additional fuses involved in the LAB interconnect MUXs
% there where not found in the first playground?
%
% We might find special cases for direct-links and globals.
%
% Found like this:
%   {X, Y, 3, 3, cell, 4} -> {{lab, X, Y}, {interconnect,12}, direct_link}
%   {X, Y, 4, 0, cell, 4} -> {{lab, X, Y}, {interconnect,12}, from4, gclk}
%
% Sometimes there where other candidate fuses, but not consistent
% enough to think they directly relate to the LAB interconnect MUX.
%
% What are these fuses?
%
% max_v_240z {lab,2,1} {interconnect,25} <-- {io_data_in,1,1,2,0}
%  * {2,1,0,0,cell,23}
%  * {2,1,0,0,cell,25}
%
% max_v_240z {lab,2,3} {interconnect,10} <-- {io_data_in,1,3,2,0}
%  * {2,3,0,3,cell,1}
%
% max_v_240z {lab,7,1} {interconnect,15} <-- {io_data_in,8,1,0,0}
%  * {7,1,7,3,cell,27}
%
% max_v_240z {lab,7,1} {interconnect,16} <-- {io_data_in,8,1,1,0}
%  * {7,1,7,3,cell,27}
%
% max_v_240z {lab,7,1} {interconnect,17} <-- {io_data_in,8,1,2,0}
%  * {7,1,7,3,cell,27}
%
% max_v_570z {lab,1,4} {interconnect,21} <-- {io_data_in,0,4,6,0}
%  * {1,4,5,2,cell,0}
%
% max_v_570z {lab,1,5} {interconnect,19} <-- {io_data_in,0,5,5,0}
%  * {1,5,1,0,cell,22}
%  * {1,5,1,0,cell,25}
%
% max_v_570z {lab,1,7} {interconnect,6} <-- {io_data_in,0,7,0,0}
%  * {1,7,6,0,cell,22}
%  * {1,7,6,0,cell,24}
%
% max_v_2210z {lab,20,7} {interconnect,0} <-- {io_data_in,21,7,0,0}
%  * {20,7,2,0,cell,27}


%%====================================================================
%% run
%%====================================================================

run() ->
    _Interconnects = density(max_v_2210z, #{}),
    %Interconnects = lists:foldl(fun density/2, #{}, density:list()),
    %
    ok.

%%--------------------------------------------------------------------

density(Density, Acc0) ->
    {ok, Cache} = route_cache:open(Density),
    route_cache:fold_blocks(
        local_interconnect,
        fun (Block, Indexes, Acc) ->
            block(Density, Block, Indexes, Acc)
        end,
        Acc0,
        Cache
    ).

%%--------------------------------------------------------------------

block(Density, {local_interconnect, X, Y}, Indexes, Acc0) ->
    case density:is_lab(X, Y, Density) of
        true ->
            LAB = {lab, X, Y},
            io:format(" ==> ~s ~w~n", [Density, LAB]),
            route_cache:fold_indexes(
                fun (Index, Froms, Acc) ->
                    interconnect(Density, LAB, Index, Froms, Acc)
                end,
                Acc0,
                Indexes
            );

        false ->
            Acc0
    end.

%%--------------------------------------------------------------------

interconnect(Density, LAB, Index, Froms, Acc0) ->
    io:format(" ==> ~p ~p~n", [LAB, Index]),
    case experiments(Froms) of
        [] ->
            Acc0;

        Experiments0 ->
            froms(Density, LAB, Index, Experiments0, Acc0)
    end.

%%--------------------------------------------------------------------

froms(Density, LAB = {lab, X, Y}, Index, Experiments0, Acc0) ->
    Minimal = density:minimal_fuses(Density),
    Experiments = [{minimal, Minimal} | Experiments0],
    Matrix0 = matrix:build(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun (Fuse) ->
        remove_fuse(X, Y, Index, Fuse)
    end),
    %matrix:print(Matrix),
    Ns = lists:seq(1, length(Experiments)),
    from(2, Ns, Experiments0, Matrix, LAB, Index, Acc0).

%%--------------------------------------------------------------------

from(_, _, [], _, _, _, Acc) ->
    Acc;
from(N, Ns, [{From, _} | Froms], Matrix, LAB, Index, Acc0) ->
    Pattern = lists:map(fun (NN) when NN =:= N -> 0; (_) -> x end, Ns),
    Fuses = [
        Fuse
        ||
        {_, Fuse} <- matrix:pattern_match(Matrix, Pattern)
    ],
    fuses(From, Fuses, LAB, {interconnect, Index}, Matrix),
    from(N + 1, Ns, Froms, Matrix, LAB, Index, Acc0).

%%--------------------------------------------------------------------

remove_fuse(X, Y, _, {X, Y, _, _, _, _}) -> false;
remove_fuse(X, Y, I, {{lab, X, Y}, {interconnect, I}, _}) -> false;
remove_fuse(X, Y, I, {{lab, X, Y}, {interconnect, I}, _, _}) -> false;
remove_fuse(_, _, _, _) -> true.

%%--------------------------------------------------------------------

fuses(From, Fuses, LAB, Interconnect, Matrix) ->
    Fuse3 = lists:filter(fun
        ({L, I, from3, _}) -> L =:= LAB andalso I =:= Interconnect;
        (_) -> false
    end, Fuses),
    Fuse4 = lists:filter(fun
        ({L, I, from4, _}) -> L =:= LAB andalso I =:= Interconnect;
        (_) -> false
    end, Fuses),
    Direct = lists:filter(fun
        ({L, I, direct_link}) -> L =:= LAB andalso I =:= Interconnect;
        (_) -> false
    end, Fuses),
    case {Fuse3, Fuse4, Direct} of
        {[_], [_], []} ->
            ok;

        {[], [], [_]} ->
            ok;

        _ ->
            matrix:print(Matrix),
            io:format("     ~p ~p <-- ~p~n", [LAB, Interconnect, From]),
            io:format("     ~p~n", [Fuses]),
            throw(stop)
    end.

%%--------------------------------------------------------------------

experiments(Froms) ->
    route_cache:fold_froms(
        fun (From, Cached, Acc) ->
            experiment(From, Cached, Acc)
        end,
        [],
        Froms
    ).


%%--------------------------------------------------------------------

experiment(From, Cached, Experiments) ->
    case experiment_reduce(Cached) of
        {ok, Fuses} ->
            Experiment = {From, Fuses},
            [Experiment | Experiments];

        false ->
            Experiments
    end.

%%--------------------------------------------------------------------

experiment_reduce(Cached) ->
    route_cache:fold_cached(
        fun experiment_reduce_zero/2,
        zero,
        fun experiment_reduce_rest/2,
        Cached,
        #{limit => 50}
    ).

%%--------------------------------------------------------------------

experiment_reduce_zero(Experiment, zero) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    Fuses.

%%--------------------------------------------------------------------

experiment_reduce_rest(Experiment, Fuses0) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    fuses:intersect(Fuses0, Fuses).

