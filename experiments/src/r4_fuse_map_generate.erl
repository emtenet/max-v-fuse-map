-module(r4_fuse_map_generate).

-export([run/0]).

% This experiment generates a first draft of r4_fuse_map:to_name/2
%
% fuse_map:to_name/2 produces MUX block indexes:
%
%  * {{r4, XX, YY}, {mux, MM}, _}
%  * {{r4, XX, YY}, {mux, MM}, _, _}
%
% Where XX, YY & MM come almost directly from the fuse locations.
%
% But, they do correspond with the interconnect numbering from RCF files
%
%  * {{r4, X, Y}, {interconnect, I}, _}
%  * {{r4, X, Y}, {interconnect, I}, _, _}
%
% We match up fuses to interconnects and then record the mux to interconnect
% mapping and write a draft version of src/r4_fuse_map.erl
%
% NOTE: We do not get 100% accuracy when matching, non-matches or too many
% matches are ignored and leave holes in the mapping tables.
% These holes will be filled in mannualy when running r4_fuse_map_theory.

%-define(DENSITY, max_v_240z).

%%====================================================================
%% run
%%====================================================================

-ifdef(DENSITY).
run() ->
    Zero = #{},
    Map = fold_density(?DENSITY, Zero),
    %
    io:format("                                            ~n", []),
    %
    generate(Map),
    ok.
-else.
run() ->
    Zero = #{},
    Map = lists:foldl(fun fold_density/2, Zero, density:list()),
    %
    io:format("                                            ~n", []),
    %
    generate(Map),
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
interconnects(N, Ns, [Experiment | Experiments], Block, Matrix, Acc0) ->
    Pattern = lists:map(fun (NN) when NN =:= N -> 0; (_) -> x end, Ns),
    Acc = interconnect(Block, Experiment, Matrix, Pattern, Acc0),
    interconnects(N + 1, Ns, Experiments, Block, Matrix, Acc).

%%--------------------------------------------------------------------

remove_fuse(_, {_, _, _, _, cell, 19}) ->
    true;
remove_fuse({Density, {r4, _, Y}, _}, Location) ->
    case fuse_map:to_name(Location, Density) of
        {ok, {{r4, _, Y}, _, _}} ->
            false;

        {ok, {{r4, _, Y}, _, _, _}} ->
            false;

        {ok, {r4, _, _}} ->
            false;

        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

%%====================================================================
%% interconnect
%%====================================================================

interconnect(Block, Experiment, Matrix, Pattern, Acc0) ->
    case matrix:pattern_match(Matrix, Pattern) of
        [{_, Fuse}] ->
            interconnect_direct(Block, Fuse, Acc0);

        [{_, Fuse1}, {_, Fuse2}] ->
            interconnect_pair(Block, Fuse1, Fuse2, Acc0);

        Fuses ->
            interconnect_not_found(Block, Experiment, Fuses, Acc0)
    end.

%%--------------------------------------------------------------------

interconnect_not_found(_, _, _, Acc) ->
    Acc.

%%--------------------------------------------------------------------

interconnect_pair({Density, R4, Index}, Fuse1, Fuse2, Acc) ->
    case {fuse_map:to_name(Fuse1, Density), fuse_map:to_name(Fuse2, Density)} of
        {{ok, Mux1 = {{r4, X, Y}, {mux, Mux}, _, _}},
         {ok, Mux2 = {{r4, X, Y}, {mux, Mux}, _, _}}} ->
            Acc1 = mux_map(Density, R4, Index, Mux1, Acc),
            mux_map(Density, R4, Index, Mux2, Acc1);

        _ ->
            Acc
    end.

%%--------------------------------------------------------------------

interconnect_direct({Density, R4, Index}, Fuse, Acc) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, Mux = {_, {mux, _}, _}} ->
            mux_map(Density, R4, Index, Mux, Acc);

        _ ->
            Acc
    end.

%%====================================================================
%% mux_map
%%====================================================================

mux_map(Density, {r4, X, Y}, I, {{r4, XX, YY}, {mux, MM}, _}, Acc) ->
    Y = YY,
    mux_map(Density, X, I, XX, YY, MM, Acc);
mux_map(Density, {r4, X, Y}, I, {{r4, XX, YY}, {mux, MM}, _, _}, Acc) ->
    Y = YY,
    mux_map(Density, X, I, XX, YY, MM, Acc).

%%--------------------------------------------------------------------

mux_map(Density, X, I, XX, YY, MM, Acc) ->
    case Acc of
        #{Density := #{MM := #{YY := #{XX := {X, I}}}}} ->
            Acc;

        #{Density := #{MM := #{YY := #{XX := Existing}}}} ->
            throw({Density, XX, YY, mux, MM, to, {X, I}, existing, Existing});

        #{Density := MMs = #{MM := YYs = #{YY := XXs}}} ->
            Acc#{Density => MMs#{MM => YYs#{YY => XXs#{XX => {X, I}}}}};

        #{Density := MMs = #{MM := YYs}} ->
            Acc#{Density => MMs#{MM => YYs#{YY => #{XX => {X, I}}}}};

        #{Density := MMs} ->
            Acc#{Density => MMs#{MM => #{YY => #{XX => {X, I}}}}};

        _ ->
            Acc#{Density => #{MM => #{YY => #{XX => {X, I}}}}}
    end.

%%====================================================================
%% generate
%%====================================================================

-define(SINGLE, <<"%%--------------------------------------------------------------------\n">>).
-define(DOUBLE, <<"%%====================================================================\n">>).

generate(Map) ->
    {ok, Header} = file:read_file("priv/r4_fuse_map.erl"),
    ok = file:write_file("src/r4_fuse_map.erl", [
        Header,
        [
            generate_density(Density, maps:get(Density, Map, #{}))
            ||
            Density <- density:list()
        ]
    ]),
    ok.

%%--------------------------------------------------------------------

generate_density(Density, MMs) ->
    Name = atom_to_binary(Density),
    {MaxX, MaxY} = generate_max(Density),
    Keys = lists:sort(maps:keys(MMs)),
    [
        ?DOUBLE,
        <<"%% to_">>, Name, <<"\n">>,
        ?DOUBLE,
        <<"\n"
        "to_">>, Name, <<"(XX, YY, _)\n"
        "        when XX > ">>, integer_to_binary(MaxX), <<" orelse"
                    " YY > ">>, integer_to_binary(MaxY), <<" ->\n"
        "    {x, x};\n">>,
        [
            generate_mux(Name, MaxX, MaxY, MM, map_get(MM, MMs))
            ||
            MM <- Keys
        ], <<
        "to_">>, Name, <<"(_, _, _) ->\n"
        "    {x, x}.\n"
        "\n"
    >>].

%%--------------------------------------------------------------------

generate_max(max_v_240z) -> {8, 4};
generate_max(max_v_570z) -> {13, 7};
generate_max(max_v_1270z) -> {17, 10};
generate_max(max_v_2210z) -> {21, 13}.

%%--------------------------------------------------------------------

generate_mux(Name, MaxX, MaxY, MM, YYs) ->
    Heading = generate_heading(MaxX),
    [<<
        "to_">>, Name, <<"(XX, YY, ">>, integer_to_binary(MM), <<") ->\n"
        "    X = element(XX + 1, element(YY + 1, {\n">>,
        Heading,
        [
            generate_row(YY, MaxX, MaxY, maps:get(YY, YYs, #{}), fun generate_x/1)
            ||
            YY <- lists:seq(0, MaxY)
        ], <<
        "    })),\n"
        "    I = element(XX + 1, element(YY + 1, {\n">>,
        Heading,
        [
            generate_row(YY, MaxX, MaxY, maps:get(YY, YYs, #{}), fun generate_i/1)
            ||
            YY <- lists:seq(0, MaxY)
        ], <<
        "    })),\n"
        "    {X, I};\n"
     >>].

%%--------------------------------------------------------------------

generate_heading(MaxX) ->
    [
        <<"        % 0">>,
        [
            io_lib:format(" ~2b", [X])
            ||
            X <- lists:seq(1, MaxX)
        ],
        <<"\n">>
    ].

%%--------------------------------------------------------------------

generate_i({_, I}) -> I.

%%--------------------------------------------------------------------

generate_x({X, _}) -> X.

%%--------------------------------------------------------------------

generate_row(YY, MaxX, MaxY, XXs, Get) ->
    [
        <<"        ">>,
        [
            generate_cell(X, Get(maps:get(X, XXs, {x, x})))
            ||
            X <- lists:seq(0, MaxX)
        ],
        case YY of
            MaxY ->
                io_lib:format("}  % ~2b\n", [YY]);

            _ ->
                io_lib:format("}, % ~2b\n", [YY])
        end
    ].

%%--------------------------------------------------------------------

generate_cell(0, x) ->
    <<"{ x">>;
generate_cell(_, x) ->
    <<", x">>;
generate_cell(0, Value) ->
    io_lib:format("{~2b", [Value]);
generate_cell(_, Value) ->
    io_lib:format(",~2b", [Value]).

