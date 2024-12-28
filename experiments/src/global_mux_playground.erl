-module(global_mux_playground).

-export([run/0]).

% This experiment looks for pairs of fuses that could form a
% Global clock's mux.
%
% MAX V 240Z has its fuses in a different place than the other
% densities so process it separately.

%%====================================================================
%% run
%%====================================================================

run() ->
    [max_v_240z | Densitys] = density:list(),
    run([max_v_240z]),
    run(Densitys),
    [
        expect_small(max_v_240z, N)
        ||
        N <- [0, 1, 2, 3]
    ],
    [
        expect_large(Density, N)
        ||
        Density <- Densitys,
        N <- [0, 1, 2, 3]
    ],
    ok.

%%--------------------------------------------------------------------

expect_small(Density, N) ->
    X = 1,
    Y = 3,
    G = {global, X, Y},
    {I, A, B} = expect_small(N),
    expect(Density, {G, N, from3, mux0}, {X, Y, I, A, side, 3}),
    expect(Density, {G, N, from3, mux1}, {X, Y, I, A, side, 2}),
    expect(Density, {G, N, from3, mux2}, {X, Y, I, B, side, 3}),
    expect(Density, {G, N, from6, mux0}, {X, Y, I, A, side, 4}),
    expect(Density, {G, N, from6, mux1}, {X, Y, I, B, side, 4}),
    expect(Density, {G, N, from6, mux2}, {X, Y, I, A, side, 5}),
    expect(Density, {G, N, from6, mux3}, {X, Y, I, B, side, 5}),
    expect(Density, {G, N, from6, mux4}, {X, Y, I, A, side, 6}),
    expect(Density, {G, N, from6, mux5}, {X, Y, I, B, side, 6}),
    ok.

%%--------------------------------------------------------------------

expect_small(0) -> {6, 0, 1};
expect_small(1) -> {6, 3, 2};
expect_small(2) -> {7, 0, 1};
expect_small(3) -> {7, 3, 2}.

%%--------------------------------------------------------------------

expect_large(Density, N) ->
    {X, Y} = density:global_block(Density),
    G = {global, X, Y},
    expect(Density, {G, N, from3, mux1}, {X, Y, N, 0, cell, 20}),
    expect(Density, {G, N, from3, mux0}, {X, Y, N, 1, cell, 20}),
    expect(Density, {G, N, from3, mux2}, {X, Y, N, 0, cell, 21}),
    expect(Density, {G, N, from4, mux0}, {X, Y, N, 2, cell, 20}),
    expect(Density, {G, N, from4, mux1}, {X, Y, N, 3, cell, 20}),
    expect(Density, {G, N, from4, mux2}, {X, Y, N, 2, cell, 21}),
    expect(Density, {G, N, from4, mux3}, {X, Y, N, 3, cell, 21}),
    ok.

%%--------------------------------------------------------------------

expect(Density, Name, Location) ->
    %io:format(" --> ~s ~w ~w~n", [Density, Name, Location]),
    {ok, Name} = fuse_map:to_name(Location, Density),
    {ok, Fuse} = fuse_map:from_name(Name, Density),
    {ok, Name} = fuse_map:to_name(Fuse, Density),
    ok.

%%--------------------------------------------------------------------

run(Densitys) ->
    Zero = #{},
    Selects = lists:foldl(fun density/2, Zero, Densitys),
    %
    Indexes = lists:sort(maps:keys(Selects)),
    lists:foreach(fun (Index) ->
        selects_index(Index, maps:get(Index, Selects))
    end, Indexes),
    ok.

%%--------------------------------------------------------------------

selects_index(Index, Keys) ->
    io:format(" ==> {global, ~w}~n", [Index]),
    case axis(Keys) of
        {Left, Right} ->
            io:format("     ~w <---> ~w~n", [Left, Right]);

        false ->
            Sorted = lists:sort(maps:keys(Keys)),
            lists:foreach(fun (Key) ->
                selects_key(Key, maps:get(Key, Keys))
            end, Sorted)
    end,
    ok.

%%--------------------------------------------------------------------

selects_key(Key, Values) ->
    io:format("     ~12w <--->", [Key]),
    lists:foreach(fun (Value) ->
        io:format(" ~12w", [Value])
    end, Values),
    io:format("~n", []),
    ok.

%%--------------------------------------------------------------------

density(Density, Acc0) ->
    {ok, Cache} = route_cache:open(Density),
    route_cache:fold_blocks(
        global_clk_mux,
        fun (Block, Indexes, Acc) ->
            block(Density, Block, Indexes, Acc)
        end,
        Acc0,
        Cache
    ).

%%--------------------------------------------------------------------

block(Density, {global_clk_mux, _, _}, Indexes, Acc0) ->
    io:format(" ==> ~s~n", [Density]),
    route_cache:fold_indexes(
        fun (Index, Froms, Acc) ->
            interconnect(Density, Index, Froms, Acc)
        end,
        Acc0,
        Indexes
    ).

%%--------------------------------------------------------------------

interconnect(Density, Index, Froms, Acc0) ->
    %io:format(" ==> ~p~n", [Index]),
    case experiments(Froms) of
        [] ->
            Acc0;

        Experiments0 ->
            froms(Density, Index, Experiments0, Acc0)
    end.

%%--------------------------------------------------------------------

froms(Density, Index, Experiments0, Acc0) ->
    Minimal = density:minimal_fuses(Density),
    Experiments = [{minimal, Minimal} | Experiments0],
    Matrix0 = matrix:build_with_location(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun (Fuse) ->
        remove_fuse(Density, Fuse)
    end),
    %
    %matrix:print(Matrix),
    %
    Ns = lists:seq(1, length(Experiments)),
    from(2, Ns, Experiments0, Matrix, Index, Acc0).

%%--------------------------------------------------------------------

from(_, _, [], _, _, Acc) ->
    Acc;
from(N, Ns, [_ | Froms], Matrix, Index, Acc0) ->
    Pattern = lists:map(fun (NN) when NN =:= N -> 0; (_) -> x end, Ns),
    case matrix:pattern_match(Matrix, Pattern) of
        [{_, Fuse1}, {_, Fuse2}] ->
            Acc = selects_add_pair(Index, Fuse1, Fuse2, Acc0),
            from(N + 1, Ns, Froms, Matrix, Index, Acc);

        _ ->
            from(N + 1, Ns, Froms, Matrix, Index, Acc0)
    end.

%%--------------------------------------------------------------------

remove_fuse(max_v_240z, {1, 3, 6, _, side, Sector}) ->
    Sector < 2 orelse Sector > 6;
remove_fuse(max_v_240z, {1, 3, 7, _, side, Sector}) ->
    Sector < 2 orelse Sector > 6;
remove_fuse(Density, Location) ->
    case fuse_map:to_name(Location, Density) of
        {ok, {{global, _, _}, _, from3, _}} ->
            false;

        {ok, {{global, _, _}, _, from4, _}} ->
            false;

        {ok, {{global, _, _}, _, from6, _}} ->
            false;

        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

%%--------------------------------------------------------------------

selects_add_pair(Index, Fuse1, Fuse2, Acc) ->
    Key1 = fuse_key(Fuse1),
    Key2 = fuse_key(Fuse2),
    %io:format("   ~-18w <- ~w ~w~n", [{select, Index}, Key1, Key2]),
    Acc1 = selects_add(Index, Key1, Key2, Acc),
    selects_add(Index, Key2, Key1, Acc1).

%%--------------------------------------------------------------------

selects_add(Index, Key, Value, Acc) ->
    case Acc of
        #{Index := Keys = #{Key := Values}} ->
            case lists:member(Value, Values) of
                true ->
                    Acc;

                false ->
                    Merged = lists:umerge([Value], Values),
                    Acc#{Index => Keys#{Key => Merged}}
            end;

        #{Index := Keys} ->
            Acc#{Index => Keys#{Key => [Value]}};

        _ ->
            Acc#{Index => #{Key => [Value]}}
    end.

%%--------------------------------------------------------------------

fuse_key({_, _, N, I, side, Sector}) ->
    {Sector, N, I};
fuse_key({_, _, N, I, cell, Sector}) ->
    {Sector, N, I}.

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

%%--------------------------------------------------------------------

axis(Interconnect) ->
    Keys = [Key | _] = lists:sort(maps:keys(Interconnect)),
    Sides = #{Key => left},
    axis(0, Keys, [], Sides, Interconnect, [], []).

%%--------------------------------------------------------------------

axis(_, [], [], _, _, Left, Right) ->
    {lists:sort(Left), lists:sort(Right)};
axis(N, [], Keys, Sides, Interconnect, Left, Right) when N < 5 ->
    axis(N + 1, Keys, [], Sides, Interconnect, Left, Right);
axis(_, [], _, _, _, _, _) ->
    false;
axis(N, [Key | Keys], Again, Sides0, Interconnect, Left, Right) ->
    case Sides0 of
        #{Key := left} ->
            Sides = axis(maps:get(Key, Interconnect), right, Sides0),
            axis(N, Keys, Again, Sides, Interconnect, [Key | Left], Right);

        #{Key := right} ->
            Sides = axis(maps:get(Key, Interconnect), left, Sides0),
            axis(N, Keys, Again, Sides, Interconnect, Left, [Key | Right]);

        _ ->
            axis(N, Keys, [Key | Again], Sides0, Interconnect, Left, Right)
    end.

%%--------------------------------------------------------------------

axis([], _, Sides) ->
    Sides;
axis([Key | Keys], Side, Sides) ->
    case Sides of
        #{Key := Side} ->
            axis(Keys, Side, Sides);

        #{Key := Existing} ->
            throw({axis, Key, side, Side, existing, Existing});

        _ ->
            axis(Keys, Side, Sides#{Key => Side})
    end.

