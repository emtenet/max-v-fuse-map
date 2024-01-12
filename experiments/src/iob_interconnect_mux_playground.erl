-module(iob_interconnect_mux_playground).

-export([run/0]).

% This experiment looks for pairs of fuses that could form a
% IOB's interconnect mux.
%
%  * look for fuses in the IOB's block {X, Y, _, _, cell, _}
%  * only collect pairs of fuses
%
% Then for each interconnect 0..25 try and split the fuses into
% two sides (left & right) of the two-dimentional mux.
%
% We find muxes of 3 x 4 dimentions.

%%====================================================================
%% run
%%====================================================================

run() ->
    Zero = {#{}, #{}},
    %Pair = density(max_v_570z, Zero),
    Pair = lists:foldl(fun density/2, Zero, density:list()),
    %
    {Head, Side} = Pair,
    interconnects(head, Head),
    interconnects(side, Side),
    ok.

%%--------------------------------------------------------------------

interconnects(Side, Interconnects) ->
    Indexes = lists:sort(maps:keys(Interconnects)),
    lists:foreach(fun (Index) ->
        interconnects_index(Side, Index, maps:get(Index, Interconnects))
    end, Indexes),
    ok.

%%--------------------------------------------------------------------

interconnects_index(Side, Index, Keys) ->
    io:format(" ==> ~s {interconnect, ~2w}~n", [Side, Index]),
    case axis(Keys) of
        {Left, Right} ->
            io:format("     ~w <---> ~w~n", [Left, Right]);

        false ->
            Sorted = lists:sort(maps:keys(Keys)),
            lists:foreach(fun (Key) ->
                interconnects_key(Key, maps:get(Key, Keys))
            end, Sorted)
    end,
    ok.

%%--------------------------------------------------------------------

interconnects_key(Key, Values) ->
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
        local_interconnect,
        fun (Block, Indexes, Acc) ->
            block(Density, Block, Indexes, Acc)
        end,
        Acc0,
        Cache
    ).

%%--------------------------------------------------------------------

block(Density, {local_interconnect, X, Y}, Indexes, Acc0) ->
    case density:is_iob(X, Y, Density) of
        true ->
            IOB = {iob, X, Y},
            io:format(" ==> ~s ~w~n", [Density, IOB]),
            route_cache:fold_indexes(
                fun (Index, Froms, Acc) ->
                    interconnect(Density, IOB, Index, Froms, Acc)
                end,
                Acc0,
                Indexes
            );

        false ->
            Acc0
    end.

%%--------------------------------------------------------------------

interconnect(Density, IOB, Index, Froms, Acc0) ->
    io:format(" ==> ~p ~p~n", [IOB, Index]),
    case experiments(Froms) of
        [] ->
            Acc0;

        Experiments0 ->
            froms(Density, IOB, Index, Experiments0, Acc0)
    end.

%%--------------------------------------------------------------------

froms(Density, {iob, X, Y}, Index, Experiments0, Acc0) ->
    Minimal = density:minimal_fuses(Density),
    Experiments = [{minimal, Minimal} | Experiments0],
    Matrix0 = matrix:build_with_location(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun (Fuse) ->
        remove_fuse(Density, X, Y, Fuse)
    end),
    %
    %matrix:print(Matrix),
    %
    Ns = lists:seq(1, length(Experiments)),
    from(2, Ns, Experiments0, Matrix, X, Y, Index, Acc0).

%%--------------------------------------------------------------------

from(_, _, [], _, _, _, _, Acc) ->
    Acc;
from(N, Ns, [_ | Froms], Matrix, X, Y, Index, Acc0) ->
    Pattern = lists:map(fun (NN) when NN =:= N -> 0; (_) -> x end, Ns),
    case matrix:pattern_match(Matrix, Pattern) of
        [{_, Fuse}] ->
            Acc = interconnects_add_one(X, Y, Index, Fuse, Acc0),
            from(N + 1, Ns, Froms, Matrix, X, Y, Index, Acc);

        [{_, Fuse1}, {_, Fuse2}] ->
            Acc = interconnects_add_pair(X, Y, Index, Fuse1, Fuse2, Acc0),
            from(N + 1, Ns, Froms, Matrix, X, Y, Index, Acc);

        _ ->
            from(N + 1, Ns, Froms, Matrix, X, Y, Index, Acc0)
    end.

%%--------------------------------------------------------------------

remove_fuse(_, _, _, {_, _, 2, 3, cell, 19}) -> true; % manual discard
remove_fuse(max_v_570z, _, _, {9, 3, _, _, cell, 20}) -> true; % UFM
remove_fuse(max_v_570z, _, _, {9, 3, _, _, cell, 21}) -> true; % UFM
remove_fuse(max_v_1270z, _, _, {11, 3, _, _, cell, 20}) -> true; % UFM
remove_fuse(max_v_1270z, _, _, {11, 3, _, _, cell, 21}) -> true; % UFM
remove_fuse(max_v_2210z, _, _, {13, 3, _, _, cell, 20}) -> true; % UFM
remove_fuse(max_v_2210z, _, _, {13, 3, _, _, cell, 21}) -> true; % UFM
remove_fuse(_, X, _, {XX, head, _, cell, 26})
        when XX < X - 1 orelse XX > X + 1 ->
    true;
remove_fuse(Density, _, _, Location) ->
    case fuse_map:to_name(Location, Density) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%%--------------------------------------------------------------------

interconnects_add_pair(X, Y, Index, Fuse1, Fuse2, Acc) ->
    {Side, Key1} = fuse_key(X, Y, Fuse1),
    {Side, Key2} = fuse_key(X, Y, Fuse2),
    io:format("   ~-18w <- ~s ~w ~w~n", [{interconnect, Index}, Side, Key1, Key2]),
    Acc1 = interconnects_add(Side, Index, Key1, Key2, Acc),
    interconnects_add(Side, Index, Key2, Key1, Acc1).

%%--------------------------------------------------------------------

interconnects_add_one(X, Y, Index, Fuse, Acc) ->
    {Side, Key} = fuse_key(X, Y, Fuse),
    io:format("   ~-18w <- ~s ~w~n", [{interconnect, Index}, Side, Key]),
    interconnects_add(Side, Index, Key, direct_link, Acc).

%%--------------------------------------------------------------------

interconnects_add(head, Index, Key, Value, Acc = {Head, Side}) ->
    case Head of
        #{Index := Keys = #{Key := Values}} ->
            case lists:member(Value, Values) of
                true ->
                    Acc;

                false ->
                    Merged = lists:umerge([Value], Values),
                    {Head#{Index => Keys#{Key => Merged}}, Side}
            end;

        #{Index := Keys} ->
            {Head#{Index => Keys#{Key => [Value]}}, Side};

        _ ->
            {Head#{Index => #{Key => [Value]}}, Side}
    end;
interconnects_add(side, Index, Key, Value, Acc = {Head, Side}) ->
    case Side of
        #{Index := Keys = #{Key := Values}} ->
            case lists:member(Value, Values) of
                true ->
                    Acc;

                false ->
                    Merged = lists:umerge([Value], Values),
                    {Head, Side#{Index => Keys#{Key => Merged}}}
            end;

        #{Index := Keys} ->
            {Head, Side#{Index => Keys#{Key => [Value]}}};

        _ ->
            {Head, Side#{Index => #{Key => [Value]}}}
    end.

%%--------------------------------------------------------------------

fuse_key(X, Y, {X, Y, N, I, side, Sector}) ->
    {side, {Sector, N, I}};
fuse_key(X, _, {X, head, I, cell, Sector}) ->
    {head, {Sector, head, I}};
fuse_key(X, _, {X, tail, I, cell, Sector}) ->
    {head, {Sector, head, I}}.

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

