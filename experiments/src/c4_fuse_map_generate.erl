-module(c4_fuse_map_generate).

-export([run/0]).

% This experiment generates a first draft of c4_fuse_map:to_name/2
%
% fuse_map:to_name/2 produces MUX block indexes:
%
%  * {{c4, XX, YY}, {mux, MM}, _}
%  * {{c4, XX, YY}, {mux, MM}, _, _}
%
% Where XX, YY & MM come almost directly from the fuse locations.
%
% But, they do correspond with the interconnect numbering from RCF files
%
%  * {{c4, X, Y}, {interconnect, I}, _}
%  * {{c4, X, Y}, {interconnect, I}, _, _}
%
% We match up fuses to interconnects and then record the mux to interconnect
% mapping and write a draft version of src/c4_fuse_map.erl
%
% NOTE: We do not get 100% accuracy when matching, non-matches or too many
% matches are ignored and leave holes in the mapping tables.
% These holes will be filled in mannualy when running c4_fuse_map_theory.

%%====================================================================
%% run
%%====================================================================

run() ->
    Zero = #{},
    Map = lists:foldl(fun fold_density/2, Zero, density:list()),
    %
    io:format("                                            ~n", []),
    %
    generate(Map),
    ok.

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
remove_fuse({Density, {c4, X, _}, _}, Location) ->
    case fuse_map:to_name(Location, Density) of
        {ok, {{c4, X, _}, _, _}} ->
            false;

        {ok, {{c4, X, _}, _, _, _}} ->
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

interconnect_pair({Density, C4, Index}, Fuse1, Fuse2, Acc) ->
    case {fuse_map:to_name(Fuse1, Density), fuse_map:to_name(Fuse2, Density)} of
        {{ok, Mux1 = {{c4, X, Y}, {mux, Mux}, _, _}},
         {ok, Mux2 = {{c4, X, Y}, {mux, Mux}, _, _}}} ->
            Acc1 = mux_map(Density, C4, Index, Mux1, Acc),
            mux_map(Density, C4, Index, Mux2, Acc1);

        _ ->
            Acc
    end.

%%--------------------------------------------------------------------

interconnect_direct({Density, C4, Index}, Fuse, Acc) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, Mux = {_, {mux, _}, _}} ->
            mux_map(Density, C4, Index, Mux, Acc);

        _ ->
            Acc
    end.

%%====================================================================
%% mux_map
%%====================================================================

mux_map(Density, {c4, X, Y}, I, {{c4, XX, YY}, {mux, MM}, _}, Acc) ->
    X = XX,
    mux_map(Density, Y, I, XX, YY, MM, Acc);
mux_map(Density, {c4, X, Y}, I, {{c4, XX, YY}, {mux, MM}, _, _}, Acc) ->
    X = XX,
    mux_map(Density, Y, I, XX, YY, MM, Acc).

%%--------------------------------------------------------------------

mux_map(Density, Y, I, XX, YY, MM, Acc) ->
    case Acc of
        #{Density := #{MM := #{YY := #{XX := {Y, I}}}}} ->
            Acc;

        #{Density := #{MM := #{YY := #{XX := Existing}}}} ->
            throw({Density, XX, YY, mux, MM, to, {Y, I}, existing, Existing});

        #{Density := MMs = #{MM := YYs = #{YY := XXs}}} ->
            Acc#{Density => MMs#{MM => YYs#{YY => XXs#{XX => {Y, I}}}}};

        #{Density := MMs = #{MM := YYs}} ->
            Acc#{Density => MMs#{MM => YYs#{YY => #{XX => {Y, I}}}}};

        #{Density := MMs} ->
            Acc#{Density => MMs#{MM => #{YY => #{XX => {Y, I}}}}};

        _ ->
            Acc#{Density => #{MM => #{YY => #{XX => {Y, I}}}}}
    end.

%%====================================================================
%% generate
%%====================================================================

-define(SINGLE, <<"%%--------------------------------------------------------------------\n">>).
-define(DOUBLE, <<"%%====================================================================\n">>).

generate(Map) ->
    {ok, Header} = file:read_file("priv/c4_fuse_map.erl"),
    ok = file:write_file("src/c4_fuse_map.erl", [
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

generate_max(max_v_240z) -> {7, 5};
generate_max(max_v_570z) -> {12, 8};
generate_max(max_v_1270z) -> {16, 11};
generate_max(max_v_2210z) -> {20, 14}.

%%--------------------------------------------------------------------

generate_mux(Name, MaxX, MaxY, MM, YYs) ->
    Heading = generate_heading(MaxX),
    [<<
        "to_">>, Name, <<"(XX, YY, ">>, integer_to_binary(MM), <<") ->\n"
        "    Y = element(XX + 1, element(YY + 1, {\n">>,
        Heading,
        [
            generate_row(YY, MaxX, MaxY, maps:get(YY, YYs, #{}), fun generate_y/1)
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
        "    {Y, I};\n"
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

generate_y({Y, _}) -> Y.

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

