-module(r4_interconnect_mux_playground).

-export([run/0]).

% This experiment looks for pairs of fuses that could form a
% R4's interconnect mux.

%%====================================================================
%% run
%%====================================================================

run() ->
    Zero = #{},
    %density(max_v_240z, Zero),
    %density(max_v_570z, Zero),
    %density(max_v_1270z, Zero),
    %density(max_v_2210z, Zero),
    lists:foldl(fun density/2, Zero, density:list()),
    %
    ok.

%%--------------------------------------------------------------------

density(Density, Acc0) ->
    {ok, Cache} = route_cache:open(Density),
    route_cache:fold_blocks(
        r4,
        fun (Block, Indexes, Acc) ->
            block(Density, Block, Indexes, Acc)
        end,
        Acc0,
        Cache
    ).

%%--------------------------------------------------------------------

block(Density, R4, Indexes, Acc0) ->
    L = density:left_io(4, Density),
    R = density:right_lab(Density),
    %io:format(" ==> ~s ~w~n", [Density, R4]),
    route_cache:fold_indexes(
        fun (Index, Froms, Acc) ->
            interconnect({Density, R4, L, R, Index}, Froms, Acc)
        end,
        Acc0,
        Indexes
    ).

%%--------------------------------------------------------------------

interconnect(Block, Froms, Acc0) ->
    io:format(" ==> ~p         \r", [Block]),
    case experiments(Froms) of
        [] ->
            Acc0;

        Experiments0 ->
            froms(Block, Experiments0, Acc0)
    end.

%%--------------------------------------------------------------------

froms(Block = {Density, _, _, _, _}, Experiments0, Acc0) ->
    Minimal = density:minimal_fuses(Density),
    Experiments = [{minimal, Minimal} | Experiments0],
    Matrix0 = matrix:build_with_location(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun (Fuse) ->
        remove_fuse(Block, Fuse)
    end),
    %
    %matrix:print(Matrix),
    %
    Ns = lists:seq(1, length(Experiments)),
    from(2, Ns, Experiments0, Matrix, Block, Acc0).

%%--------------------------------------------------------------------

from(_, _, [], _, _, Acc) ->
    Acc;
from(N, Ns, [_ | Froms], Matrix, Block, Acc0) ->
    Pattern = lists:map(fun (NN) when NN =:= N -> 0; (_) -> x end, Ns),
    case matrix:pattern_match(Matrix, Pattern) of
        [{_, Fuse}] ->
            Acc = interconnects_add_one(Block, Fuse, Acc0),
            from(N + 1, Ns, Froms, Matrix, Block, Acc);

        [{_, Fuse1}, {_, Fuse2}] ->
            Acc = interconnects_add_pair(Block, Fuse1, Fuse2, Acc0),
            from(N + 1, Ns, Froms, Matrix, Block, Acc);

        _ ->
            from(N + 1, Ns, Froms, Matrix, Block, Acc0)
    end.

%%--------------------------------------------------------------------

remove_fuse({Density, _, _, _, _}, Location) ->
    case fuse_map:to_name(Location, Density) of
        {ok, {r4, _, _}} ->
            false;

        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

%%--------------------------------------------------------------------

interconnects_add_pair({Density, _, _, _, Index}, Fuse1, Fuse2, Acc) ->
    case {fuse_map:to_name(Fuse1, Density), fuse_map:to_name(Fuse2, Density)} of
        {{ok, _}, {ok, _}} ->
            ok;

        {{ok, {r4, direct, _}}, {error, _}} ->
            ok;

        {{ok, {r4, Paired, _}}, {error, _}} ->
            io:format("   ~-18w <- ~s ~w +++ ~w~n", [
                {interconnect, Index}, Paired, Fuse1, Fuse2]
            );

        {{error, _}, {ok, {r4, direct, _}}} ->
            ok;

        {{error, _}, {ok, {r4, Paired, _}}} ->
            io:format("   ~-18w <- ~s ~w +++ ~w~n", [
                {interconnect, Index}, Paired, Fuse2, Fuse1]
            );

        {{error, _}, {error, _}} ->
            io:format("   ~-18w <- ~w ~w~n", [
                {interconnect, Index}, Fuse1, Fuse2]
            )
            %ok
    end,
    Acc.

%%--------------------------------------------------------------------

interconnects_add_one({Density, _, _, _, Index}, Fuse, Acc) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, _} ->
            ok;

        {error, _} ->
            io:format("   ~-18w <- ~w~n", [
                {interconnect, Index}, Fuse]
            )
    end,
    Acc.

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

