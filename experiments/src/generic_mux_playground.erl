-module(generic_mux_playground).

-export([run/2]).

% This experiment looks for pairs of fuses that could form a mux.

%%====================================================================
%% run
%%====================================================================

run(Densitys, Opts = #{}) ->
    Zero = #{},
    Selects = lists:foldl(fun (Density, Acc) ->
        density(Density, Opts, Acc)
    end, Zero, Densitys),
    %
    Indexes = lists:sort(maps:keys(Selects)),
    lists:foreach(fun (Index) ->
        selects_index(Index, maps:get(Index, Selects), Opts)
    end, Indexes),
    ok.

%%--------------------------------------------------------------------

selects_index(Index, Keys, #{block_type := BlockType}) ->
    io:format(" ==> {~s, ~w}~n", [BlockType, Index]),
    case axis(Keys) of
        {Left, Right} ->
            io:format("     ~w~n", [Left]),
            io:format("         <--->~n", []),
            io:format("     ~w~n", [Right]);

        false ->
            Sorted = lists:sort(maps:keys(Keys)),
            lists:foreach(fun (Key) ->
                selects_key(Key, maps:get(Key, Keys))
            end, Sorted)
    end,
    ok.

%%--------------------------------------------------------------------

selects_key(Key, Values) ->
    io:format("     ~w <--->", [Key]),
    lists:foreach(fun (Value) ->
        io:format(" ~w", [Value])
    end, Values),
    io:format("~n", []),
    ok.

%%--------------------------------------------------------------------

density(Density, Opts = #{block_type := BlockType}, Acc0) ->
    {ok, Cache} = route_cache:open(Density),
    route_cache:fold_blocks(
        BlockType,
        fun (Block, Indexes, Acc) ->
            block(Density, Block, Indexes, Opts, Acc)
        end,
        Acc0,
        Cache
    ).

%%--------------------------------------------------------------------

block(Density, _Block, Indexes, Opts, Acc0) ->
    %io:format(" ==> ~s ~p~n", [Density, _Block]),
    route_cache:fold_indexes(
        fun (Index, Froms, Acc) ->
            interconnect(Density, Index, Froms, Opts, Acc)
        end,
        Acc0,
        Indexes
    ).

%%--------------------------------------------------------------------

interconnect(Density, Index, Froms, Opts, Acc0) ->
    %io:format(" ==> ~p~n", [Index]),
    case experiments(Froms) of
        [] ->
            Acc0;

        Experiments0 ->
            froms(Density, Index, Experiments0, Opts, Acc0)
    end.

%%--------------------------------------------------------------------

froms(Density, Index, Experiments0, Opts = #{remove_fuse := Remove}, Acc0) ->
    Minimal = density:minimal_fuses(Density),
    Experiments = [{minimal, Minimal} | Experiments0],
    %Matrix0 = matrix:build_with_location(Density, Experiments),
    Matrix0 = matrix:build(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun (Fuse) ->
        Remove(Fuse, Opts)
    end),
    %
    matrix:print(Matrix),
    %
    Ns = lists:seq(1, length(Experiments)),
    from(2, Ns, Experiments0, Matrix, Index, Opts, Acc0).

%%--------------------------------------------------------------------

from(_, _, [], _, _, _, Acc) ->
    Acc;
from(N, Ns, [_ | Froms], Matrix, Index, Opts, Acc0) ->
    Pattern = lists:map(fun (NN) when NN =:= N -> 0; (_) -> x end, Ns),
    case matrix:pattern_match(Matrix, Pattern) of
        [{_, Fuse1}, {_, Fuse2}] ->
            Acc = selects_add_pair(Index, Fuse1, Fuse2, Opts, Acc0),
            from(N + 1, Ns, Froms, Matrix, Index, Opts, Acc);

        _ ->
            from(N + 1, Ns, Froms, Matrix, Index, Opts, Acc0)
    end.

%%--------------------------------------------------------------------

selects_add_pair(Index, Fuse1, Fuse2, #{fuse_key := FuseKey}, Acc) ->
    Key1 = FuseKey(Fuse1),
    Key2 = FuseKey(Fuse2),
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

