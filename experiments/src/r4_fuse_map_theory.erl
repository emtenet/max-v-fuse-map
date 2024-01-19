-module(r4_fuse_map_theory).

-export([run/0]).

% This experiment checks that 100% of fuses can be matched to each
% R4 interconnect.
%
% This relies heavily on the fuse_map & r4_fuse_map generated
% previoisly.
%
% Initially the r4_fuse_map is incomplete and this aids manually
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
        r4,
        fun (Block, Indexes, Acc) ->
            fold_block(Density, Block, Indexes, Acc)
        end,
        Acc0,
        Cache
    ).

%%--------------------------------------------------------------------

fold_block(Density, R4, Indexes, Acc0) ->
    %io:format(" ==> ~s ~w~n", [Density, R4]),
    route_cache:fold_indexes(
        fun (Index, Froms, Acc) ->
            fold_interconnect({Density, R4, Index}, Froms, Acc)
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
    interconnects(2, Ns, Experiments0, Block, Matrix, Acc0).

%%--------------------------------------------------------------------

interconnects(_, _, [], _, _, Acc) ->
    Acc;
interconnects(N, Ns, [Experiment | Experiments], Block, Matrix, Acc) ->
    Pattern = lists:map(fun (NN) when NN =:= N -> 0; (_) -> x end, Ns),
    interconnect(Block, Experiment, Matrix, Pattern),
    interconnects(N + 1, Ns, Experiments, Block, Matrix, Acc).

%%--------------------------------------------------------------------

remove_fuse(Block = {Density, {r4, _, Y}, _}, Fuse) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, Mux = {{r4, _, Y}, {mux, _}, _}} ->
            remove_fuse(Block, Fuse, Mux);

        {ok, Mux = {{r4, _, Y}, {mux, _}, _, _}} ->
            remove_fuse(Block, Fuse, Mux);

        _ ->
            true
    end.

%%--------------------------------------------------------------------

remove_fuse(Block = {Density, R4, I}, Fuse, Mux) ->
    case r4_fuse_map:to_name(Mux, Density) of
        {ok, {R4, {interconnect, I}, _}} ->
            false;

        {ok, {R4, {interconnect, I}, _, _}} ->
            false;

        {ok, _} ->
            true;

        false ->
            io:format("UNKNOWN ~w FUSE ~w MUX ~w~n", [Block, Fuse, Mux]),
            false
    end.

%%====================================================================
%% interconnect
%%====================================================================

interconnect(Block, Experiment, Matrix, Pattern) ->
    case matrix:pattern_match(Matrix, Pattern) of
        [{_, Fuse}] ->
            interconnect_direct(Block, Experiment, Fuse);

        [{_, Fuse1}, {_, Fuse2}] ->
            interconnect_pair(Block, Experiment, Fuse1, Fuse2);

        Fuses0 ->
            Fuses = [
                Fuse
                ||
                {_, Fuse} <- Fuses0
            ],
            interconnect_not_found(Block, Experiment, Fuses)
    end.

%%--------------------------------------------------------------------

interconnect_direct(Block = {Density, R4, Index}, Experiment, Fuse) ->
    Interconnect = {interconnect, Index},
    case fuse_map:to_name(Fuse, Density) of
        {ok, Mux = {_, {mux, _}, _}} ->
            case r4_fuse_map:to_name(Mux, Density) of
                {ok, {R4, Interconnect, direct_link}} ->
                    ok;

                {ok, {R4, Interconnect, io_data_in0}} ->
                    ok;

                {ok, {R4, Interconnect, io_data_in1}} ->
                    ok;

                _ ->
                    interconnect_not_found(Block, Experiment, [Fuse])
            end;

        _ ->
            interconnect_not_found(Block, Experiment, [Fuse])
    end.

%%--------------------------------------------------------------------

interconnect_pair(Block = {Density, R4, Index}, Experiment, Fuse1, Fuse2) ->
    Interconnect = {interconnect, Index},
    case {fuse_map:to_name(Fuse1, Density), fuse_map:to_name(Fuse2, Density)} of
        {{ok, Mux1 = {_, {mux, _}, _, _}},
         {ok, Mux2 = {_, {mux, _}, _, _}}} ->
            case {r4_fuse_map:to_name(Mux1, Density),
                  r4_fuse_map:to_name(Mux2, Density)} of
                {{ok, {R4, Interconnect, from3, _}},
                 {ok, {R4, Interconnect, from4, _}}} ->
                    ok;

                {{ok, {R4, Interconnect, from4, _}},
                 {ok, {R4, Interconnect, from3, _}}} ->
                    ok;

                _ ->
                    interconnect_not_found(Block, Experiment, [Fuse1, Fuse2])
            end;

        _ ->
            interconnect_not_found(Block, Experiment, [Fuse1, Fuse2])
    end.

%%--------------------------------------------------------------------

interconnect_not_found(Block, Experiment = {From, _}, Fuses) ->
    case interconnect_maybe_found(Block, Fuses) of
        true ->
            {Density, _, _} = Block,
            io:format("SKIP ~p~n", [{Block, From, [
                case fuse_map:to_name(Fuse, Density) of
                    {ok, Mux} ->
                        case r4_fuse_map:to_name(Mux, Density) of
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
            ]}]);

        false ->
            interconnect_realy_not_found(Block, Experiment, Fuses)
    end.

%%--------------------------------------------------------------------

interconnect_realy_not_found(Block, {From, _}, Fuses) ->
    {Density, _, _} = Block,
    throw({Block, From, [
        case fuse_map:to_name(Fuse, Density) of
            {ok, Mux} ->
                case r4_fuse_map:to_name(Mux, Density) of
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

%%--------------------------------------------------------------------

interconnect_maybe_found({Density, R4, Index}, Fuses) ->
    Interconnect = {interconnect, Index},
    case interconnect_maybe(Density, R4, Interconnect, Fuses, []) of
        [{from3, _}, {from4, _}] ->
            true;

        [{from4, _}, {from3, _}] ->
            true;

        [direct_link] ->
            true;

        [io_data_in0] ->
            true;

        [io_data_in1] ->
            true;

        _ ->
            false
    end.

%%--------------------------------------------------------------------

interconnect_maybe(_, _, _, [], Names) ->
    Names;
interconnect_maybe(Density, R4, Interconnect, [Fuse | Fuses], Names) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, Mux} ->
            case r4_fuse_map:to_name(Mux, Density) of
                {ok, {R4, Interconnect, Key, Value}} ->
                    interconnect_maybe(Density, R4, Interconnect, Fuses,
                        [{Key, Value} | Names]
                    );

                {ok, {R4, Interconnect, Value}} ->
                    interconnect_maybe(Density, R4, Interconnect, Fuses,
                        [Value | Names]
                    );

                _ ->
                    interconnect_maybe(Density, R4, Interconnect, Fuses, Names)
            end;

        _ ->
             interconnect_maybe(Density, R4, Interconnect, Fuses, Names)
    end.

