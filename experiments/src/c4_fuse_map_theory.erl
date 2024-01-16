-module(c4_fuse_map_theory).

-export([run/0]).

% This experiment checks that 100% of fuses can be matched to each
% C4 interconnect.
%
% This relies heavily on the fuse_map & c4_fuse_map generated
% previoisly.
%
% Initially the c4_fuse_map is incomplete and this aids manually
% populating the holes.

%-define(DENSITY, max_v_240z).

%%====================================================================
%% run
%%====================================================================

-ifdef(DENSITY).
run() ->
    Zero = #{},
    fold_density(?DENSITY, Zero),
    %
    io:format("                                            ~n", []),
    ok.
-else.
run() ->
    Zero = #{},
    lists:foldl(fun fold_density/2, Zero, density:list()),
    %
    io:format("                                            ~n", []),
    ok.
-endif.

%%====================================================================
%% fold
%%====================================================================

fold_density(Density, Acc0) ->
    {ok, Cache} = route_cache:open(Density),
    route_cache:fold_blocks(
        c4,
        fun (Block, Indexes, Acc) ->
            fold_block(Density, Block, Indexes, Acc)
        end,
        Acc0,
        Cache
    ).

%%--------------------------------------------------------------------

fold_block(Density, C4, Indexes, Acc0) ->
    %io:format(" ==> ~s ~w~n", [Density, C4]),
    route_cache:fold_indexes(
        fun (Index, Froms, Acc) when Index < 70 ->
                fold_interconnect({Density, C4, Index}, Froms, Acc);
            (_, _, Acc) ->
                Acc
        end,
        Acc0,
        Indexes
    ).

%%--------------------------------------------------------------------

fold_interconnect(Block, Froms, Acc0) ->
    io:format(" ==> ~p         \r", [Block]),
    case experiments(Froms) of
        [] ->
            Acc0;

        Experiments0 ->
            interconnects(Block, Experiments0, Acc0)
    end.

%%====================================================================
%% experiments
%%====================================================================

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

%%====================================================================
%% interconnects
%%====================================================================

interconnects(Block = {Density, _, _}, Experiments0, Acc0) ->
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
    interconnects(2, Ns, Experiments0, Block, Matrix, Matrix0, Acc0).

%%--------------------------------------------------------------------

interconnects(_, _, [], _, _, _, Acc) ->
    Acc;
interconnects(N, Ns, [Experiment | Experiments], Block, Matrix, Matrix0, Acc) ->
    Pattern = lists:map(fun (NN) when NN =:= N -> 0; (_) -> x end, Ns),
    interconnect(Block, Experiment, Matrix, Pattern, Matrix0),
    interconnects(N + 1, Ns, Experiments, Block, Matrix, Matrix0, Acc).

%%--------------------------------------------------------------------

remove_fuse(Block = {Density, {c4, X, _}, _}, Fuse) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, Mux = {{c4, X, _}, {mux, _}, _}} ->
            remove_fuse(Block, Fuse, Mux);

        {ok, Mux = {{c4, X, _}, {mux, _}, _, _}} ->
            remove_fuse(Block, Fuse, Mux);

        _ ->
            true
    end.

%%--------------------------------------------------------------------

remove_fuse(_Block = {Density, C4, I}, _Fuse, Mux) ->
    case c4_fuse_map:to_name(Mux, Density) of
        {ok, {C4, {interconnect, I}, _}} ->
            false;

        {ok, {C4, {interconnect, I}, _, _}} ->
            false;

        {ok, _} ->
            true;

        false ->
            false % throw({Block, Fuse, Mux})
    end.

%%====================================================================
%% interconnect
%%====================================================================

interconnect(Block, Experiment, Matrix, Pattern, _Matrix0) ->
    case matrix:pattern_match(Matrix, Pattern) of
        [{_, Fuse}] ->
            interconnect_direct(Block, Experiment, Fuse);

        [{_, Fuse1}, {_, Fuse2}] ->
            interconnect_pair(Block, Experiment, Fuse1, Fuse2);

        Fuses ->
            %matrix:print(Matrix0),
            interconnect_not_found(Block, Experiment, [
                Fuse
                ||
                {_, Fuse} <- Fuses
            ])
    end.

%%--------------------------------------------------------------------

interconnect_direct(Block = {Density, C4, Index}, Experiment, Fuse) ->
    Interconnect = {interconnect, Index},
    case fuse_map:to_name(Fuse, Density) of
        {ok, Mux = {_, {mux, _}, _}} ->
            case c4_fuse_map:to_name(Mux, Density) of
                {ok, {C4, Interconnect, direct_link}} ->
                    ok;

                {ok, {C4, Interconnect, io_data_in0}} ->
                    ok;

                {ok, {C4, Interconnect, io_data_in1}} ->
                    ok;

                _ ->
                    interconnect_not_found(Block, Experiment, [Fuse])
            end;

        _ ->
            interconnect_not_found(Block, Experiment, [Fuse])
    end.

%%--------------------------------------------------------------------

interconnect_pair(Block = {Density, C4, Index}, Experiment, Fuse1, Fuse2) ->
    Interconnect = {interconnect, Index},
    case {fuse_map:to_name(Fuse1, Density), fuse_map:to_name(Fuse2, Density)} of
        {{ok, Mux1 = {_, {mux, _}, _, _}},
         {ok, Mux2 = {_, {mux, _}, _, _}}} ->
            case {c4_fuse_map:to_name(Mux1, Density),
                  c4_fuse_map:to_name(Mux2, Density)} of
                {{ok, {C4, Interconnect, from3, _}},
                 {ok, {C4, Interconnect, from4, _}}} ->
                    ok;

                {{ok, {C4, Interconnect, from4, _}},
                 {ok, {C4, Interconnect, from3, _}}} ->
                    ok;

                _ ->
                    interconnect_not_found(Block, Experiment, [Fuse1, Fuse2])
            end;

        _ ->
            interconnect_not_found(Block, Experiment, [Fuse1, Fuse2])
    end.

%%--------------------------------------------------------------------

interconnect_not_found(Block, {From, _}, Fuses) ->
    {Density, _, _} = Block,
    throw({Block, From, [
        case fuse_map:to_name(Fuse, Density) of
            {ok, Mux} ->
                case c4_fuse_map:to_name(Mux, Density) of
                    {ok, Name} ->
                        {Fuse, Mux, Name};

                    false ->
                        {Fuse, Mux}
                end;

            {error, _} ->
                Fuse
        end
        ||
        Fuse <- Fuses
    ]}).

