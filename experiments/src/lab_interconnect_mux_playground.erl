-module(lab_interconnect_mux_playground).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    %Interconnects = density(max_v_240z, #{}),
    Interconnects = lists:foldl(fun density/2, #{}, density:list()),
    %
    Indexes = lists:sort(maps:keys(Interconnects)),
    lists:foreach(fun (Index) ->
        interconnects_index(Index, maps:get(Index, Interconnects))
    end, Indexes),
    ok.

%%--------------------------------------------------------------------

interconnects_index(Index, Keys) ->
    io:format(" ==> {interconnect, ~2w}~n", [Index]),
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
    io:format("     ~8w <--->", [Key]),
    lists:foreach(fun (Value) ->
        io:format(" ~8w", [Value])
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
    %io:format(" ==> ~p ~p~n", [LAB, Index]),
    case experiments(Froms) of
        [] ->
            Acc0;

        Experiments0 ->
            froms(Density, LAB, Index, Experiments0, Acc0)
    end.

%%--------------------------------------------------------------------

froms(Density, {lab, X, Y}, Index, Experiments0, Acc0) ->
    Minimal = density:minimal_fuses(Density),
    Experiments = [{minimal, Minimal} | Experiments0],
    Matrix0 = matrix:build_with_location(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun (Fuse) ->
        remove_fuse(X, Y, Fuse)
    end),
    %matrix:print(Matrix),
    Ns = lists:seq(1, length(Experiments)),
    from(2, Ns, Experiments0, Matrix, Index, Acc0).

%%--------------------------------------------------------------------

from(_, _, [], _, _, Acc) ->
    Acc;
from(N, Ns, [_ | Froms], Matrix, Index, Acc0) ->
    Pattern = lists:map(fun (NN) when NN =:= N -> 0; (_) -> x end, Ns),
    case matrix:pattern_match(Matrix, Pattern) of
        [{_, Fuse1}, {_, Fuse2}] ->
            Acc = interconnects_add_pair(Index, Fuse1, Fuse2, Acc0),
            from(N + 1, Ns, Froms, Matrix, Index, Acc);

        _ ->
            from(N + 1, Ns, Froms, Matrix, Index, Acc0)
    end.

%%--------------------------------------------------------------------

remove_fuse(X, Y, {X, Y, _, _, _, _}) -> false;
%remove_fuse(X, Y, {{lab, X, Y}, {interconnect, _}, _}) -> false;
%remove_fuse(X, Y, {{lab, X, Y}, {interconnect, _}, _, _}) -> false;
remove_fuse(_, _, _) -> true.

%%--------------------------------------------------------------------

interconnects_add_pair(Index, Fuse1, Fuse2, Acc) ->
    Key1 = fuse_key(Fuse1),
    Key2 = fuse_key(Fuse2),
    %io:format("   ~-18w <- ~w ~w~n", [{interconnect, Index}, Key1, Key2]),
    Acc1 = interconnects_add(Index, Key1, Key2, Acc),
    interconnects_add(Index, Key2, Key1, Acc1).

%%--------------------------------------------------------------------

interconnects_add(Index, Key, Value, Acc) ->
    case Acc of
        #{Index := Keys = #{Key := Values}} ->
            case lists:member(Value, Values) of
                true ->
                    Acc;

                false ->
                    Acc#{Index => Keys#{Key => lists:umerge([Value], Values)}}
            end;

        #{Index := Keys} ->
            Acc#{Index => Keys#{Key => [Value]}};

        _ ->
            Acc#{Index => #{Key => [Value]}}
    end.


%%--------------------------------------------------------------------

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

