-module(r4_fuse_map_inverse).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    Zero = #{},
    Map = lists:foldl(fun inverse_density/2, Zero, density:list()),
    %
    io:format("                                            ~n", []),
    %
    generate(Map),
    ok.

%%====================================================================
%% inverst
%%====================================================================

inverse_density(Density, Acc0) ->
    {MaxX, MaxY} = generate_max(Density),
    XXs = lists:seq(0, MaxX),
    YYs = lists:seq(0, MaxY),
    MMs = lists:seq(0, 15),
    lists:foldl(fun (XX, Acc) ->
        inverse_x(Density, XX, YYs, MMs, Acc)
    end, Acc0, XXs).

%%--------------------------------------------------------------------

inverse_x(Density, XX, YYs, MMs, Acc0) ->
    lists:foldl(fun (YY, Acc) ->
        inverse_y(Density, XX, YY, MMs, Acc)
    end, Acc0, YYs).

%%--------------------------------------------------------------------

inverse_y(Density, XX, YY, MMs, Acc0) ->
    lists:foldl(fun (MM, Acc) ->
        inverse_m(Density, XX, YY, MM, Acc)
    end, Acc0, MMs).

%%--------------------------------------------------------------------

inverse_m(Density, XX, YY, MM, Acc) ->
    case r4_fuse_map:to_name({{r4, XX, YY}, {mux, MM}, direct_link}, Density) of
        {ok, {{r4, X, Y}, {interconnect, I}, direct_link}} ->
            Y = YY,
            inverse(Density, XX, MM, X, Y, I, Acc);

        false ->
            Acc
    end.

%%--------------------------------------------------------------------

inverse(Density, XX, MM, X, Y, I, Acc) ->
    case Acc of
        #{Density := #{I := #{Y := #{X := {XX, MM}}}}} ->
            Acc;

        #{Density := #{I := #{Y := #{X := {XXX, MMM}}}}} ->
            throw({
                Density, {{r4, XX, Y}, {mux, MM}},
                interconnect, I,
                existing, {{r4, XXX, Y}, {mux, MMM}}
            });

        #{Density := Is = #{I := Ys = #{Y := Xs}}} ->
            Acc#{Density => Is#{I => Ys#{Y => Xs#{X => {XX, MM}}}}};

        #{Density := Is = #{I := Ys}} ->
            Acc#{Density => Is#{I => Ys#{Y => #{X => {XX, MM}}}}};

        #{Density := Is} ->
            Acc#{Density => Is#{I => #{Y => #{X => {XX, MM}}}}};

        _ ->
            Acc#{Density => #{I => #{Y => #{X => {XX, MM}}}}}
    end.

%%====================================================================
%% generate
%%====================================================================

-define(SINGLE, <<"%%--------------------------------------------------------------------\n">>).
-define(DOUBLE, <<"%%====================================================================\n">>).

generate(Map) ->
    io:format("~s", [[
        generate_density(Density, maps:get(Density, Map, #{}))
        ||
        Density <- density:list()
    ]]),
    ok.

%%--------------------------------------------------------------------

generate_density(Density, Is) ->
    Name = atom_to_binary(Density),
    {MaxX, MaxY} = generate_max(Density),
    Keys = lists:sort(maps:keys(Is)),
    [
        ?DOUBLE,
        <<"%% from_">>, Name, <<"\n">>,
        ?DOUBLE,
        <<"\n"
        "from_">>, Name, <<"(X, Y, _)\n"
        "        when X > ">>, integer_to_binary(MaxX), <<" orelse"
                    " Y > ">>, integer_to_binary(MaxY), <<" ->\n"
        "    {x, x};\n">>,
        [
            generate_mux(Name, MaxX, MaxY, I, map_get(I, Is))
            ||
            I <- Keys
        ], <<
        "from_">>, Name, <<"(_, _, _) ->\n"
        "    {x, x}.\n"
        "\n"
    >>].

%%--------------------------------------------------------------------

generate_max(max_v_240z) -> {8, 4};
generate_max(max_v_570z) -> {13, 7};
generate_max(max_v_1270z) -> {17, 10};
generate_max(max_v_2210z) -> {21, 13}.

%%--------------------------------------------------------------------

generate_mux(Name, MaxX, MaxY, I, Ys) ->
    Heading = generate_heading(MaxX),
    [<<
        "from_">>, Name, <<"(X, Y, ">>, integer_to_binary(I), <<") ->\n"
        "    XX = element(X + 1, element(Y + 1, {\n">>,
        Heading,
        [
            generate_row(Y, MaxX, MaxY, maps:get(Y, Ys, #{}), fun generate_y/1)
            ||
            Y <- lists:seq(0, MaxY)
        ], <<
        "    })),\n"
        "    MM = element(X + 1, element(Y + 1, {\n">>,
        Heading,
        [
            generate_row(Y, MaxX, MaxY, maps:get(Y, Ys, #{}), fun generate_i/1)
            ||
            Y <- lists:seq(0, MaxY)
        ], <<
        "    })),\n"
        "    {XX, MM};\n"
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

generate_row(Y, MaxX, MaxY, Xs, Get) ->
    [
        <<"        ">>,
        [
            generate_cell(X, Get(maps:get(X, Xs, {x, x})))
            ||
            X <- lists:seq(0, MaxX)
        ],
        case Y of
            MaxY ->
                io_lib:format("}  % ~2b\n", [Y]);

            _ ->
                io_lib:format("}, % ~2b\n", [Y])
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

