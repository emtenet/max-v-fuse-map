-module(lut_experiment).

-export([run/0]).

% This experiment locates the 16 fuses for each LUT's look-up-table.

%%====================================================================
%% run
%%====================================================================

run() ->
    {Rows, Verilogs} = pattern(),
    [
        density(Density, Rows, Verilogs)
        ||
        Density <- density:list()
    ],
    ok.

%%--------------------------------------------------------------------

density(Density, Rows, Verilogs) ->
    Device = density:largest_device(Density),
    Pins = device:pins(Device),
    labs(device:labs(Device), Device, {[], Pins}, Rows, Verilogs),
    ok.

%%--------------------------------------------------------------------

labs([], _, Pins, _, _) ->
    Pins;
labs([LAB | LABs], Device, Pins0, Rows, Verilogs) ->
    Pins = lab(LAB, Device, Pins0, Rows, Verilogs),
    labs(LABs, Device, Pins, Rows, Verilogs).

%%--------------------------------------------------------------------

lab(LAB, Device, Pins0, Rows, Verilogs) ->
    io:format(" => ~s ~p~n", [Device, LAB]),
    Is = lists:seq(0, 9),
    {Sources, Pins} = lists:foldr(
        fun (I, {S0, P0}) ->
            lc_sources(LAB, I, Device, S0, P0, Verilogs)
        end,
        {[], Pins0},
        Is
    ),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf(Sources),
    lcs(LAB, Is, Device, Experiments, Rows),
    Pins.

%%--------------------------------------------------------------------

lc_sources(LAB, I, Device, Sources0, Pins0, Verilogs) ->
    LC = lab:lc(LAB, I),
    {A, B, C, D, Q, Pins} = pins(Pins0),
    Settings = [
        {location, lut, LC},
        {location, a, A},
        {location, b, B},
        {location, c, C},
        {location, d, D},
        {location, q, Q}
    ],
    Sources = [
        #{
            title => {LC, Name},
            device => Device,
            settings => Settings,
            verilog => Verilog
        }
        ||
        {Name, Verilog} <- Verilogs
    ],
    {Sources ++ Sources0, Pins}.

%%--------------------------------------------------------------------

pins({[], Tail = [A, B, C, D, E | Head]}) ->
    {A, B, C, D, E, {Head, Tail}};
pins({[A], Tail = [B, C, D, E | Head]}) ->
    {A, B, C, D, E, {Head, Tail}};
pins({[A, B], Tail = [C, D, E | Head]}) ->
    {A, B, C, D, E, {Head, Tail}};
pins({[A, B, C], Tail = [D, E | Head]}) ->
    {A, B, C, D, E, {Head, Tail}};
pins({[A, B, C, D], Tail = [E | Head]}) ->
    {A, B, C, D, E, {Head, Tail}};
pins({[A, B, C, D, E | Head], Tail}) ->
    {A, B, C, D, E, {Head, Tail}}.

%%--------------------------------------------------------------------

lcs(_, [], _, _, _) ->
    ok;
lcs(LAB, [I | Is], Device, Experiments0, Rows) ->
    LC = lab:lc(LAB, I),
    {Experiments, Rest} = lists:split(5, Experiments0),
    lc(LC, Device, Experiments, Rows),
    lcs(LAB, Is, Device, Rest, Rows).

%%--------------------------------------------------------------------

lc(LC, Device, Experiments, Rows) ->
    %io:format(" => ~s ~p~n", [Device, LC]),
    Matrix = matrix:build(Device, Experiments),
    %matrix:print(Matrix),
    [Order] = lists:usort([ rcf(RCF) || {_, _, RCF} <- Experiments ]),
    [ fuse(Matrix, LC, Row, Order) || Row <- Rows ],
    ok.

%%--------------------------------------------------------------------

rcf(#{signals := #{a := A, b := B, c := C, d := D}}) ->
    #{dests := [#{route_port := DataA}]} = A,
    #{dests := [#{route_port := DataB}]} = B,
    #{dests := [#{route_port := DataC}]} = C,
    #{dests := [#{route_port := DataD}]} = D,
    [{data_a, PortA}, {data_b, PortB}, {data_c, PortC}, {data_d, PortD}]
        = lists:sort([{DataA, a}, {DataB, b}, {DataC, c}, {DataD, d}]),
    {PortA, PortB, PortC, PortD}.

%%--------------------------------------------------------------------

fuse(Matrix, LC, {Row, Pattern}, Order) ->
    [{_, Fuse}] = matrix:pattern_is(Matrix, Pattern),
    case {LC, lut, order(Order, Row)} of
        Fuse ->
            ok;

        Expect ->
            io:format("Expecting: ~w~n", [Expect]),
            io:format("Location:  ~w~n", [Fuse]),
            throw(stop)
    end.

%%--------------------------------------------------------------------

order({OrderA, OrderB, OrderC, OrderD}, Row) ->
    DataA = bit(OrderA, Row),
    DataB = bit(OrderB, Row),
    DataC = bit(OrderC, Row),
    DataD = bit(OrderD, Row),
    case {DataA, DataB, DataC, DataD} of
        {0, 0, 0, 0} -> a0b0c0d0;
        {0, 0, 0, 1} -> a0b0c0d1;
        {0, 0, 1, 0} -> a0b0c1d0;
        {0, 0, 1, 1} -> a0b0c1d1;
        {0, 1, 0, 0} -> a0b1c0d0;
        {0, 1, 0, 1} -> a0b1c0d1;
        {0, 1, 1, 0} -> a0b1c1d0;
        {0, 1, 1, 1} -> a0b1c1d1;
        {1, 0, 0, 0} -> a1b0c0d0;
        {1, 0, 0, 1} -> a1b0c0d1;
        {1, 0, 1, 0} -> a1b0c1d0;
        {1, 0, 1, 1} -> a1b0c1d1;
        {1, 1, 0, 0} -> a1b1c0d0;
        {1, 1, 0, 1} -> a1b1c0d1;
        {1, 1, 1, 0} -> a1b1c1d0;
        {1, 1, 1, 1} -> a1b1c1d1
    end.

%%--------------------------------------------------------------------

bit(a, {A, _, _, _}) -> A;
bit(b, {_, B, _, _}) -> B;
bit(c, {_, _, C, _}) -> C;
bit(d, {_, _, _, D}) -> D.

%%--------------------------------------------------------------------

pattern() ->
    % pattern chosen so:
    %  * all rows unique
    %  * column 1 has 8 1s
    %  * column 2 has 7 1s
    %  * column 3 has 6 1s
    %  * column 4 has 5 1s
    %  * column 5 has 4 1s
    %  * not symetric
    %  * expressions cannot be reduced to less than 4 variables
    Rows = [
        {{0,0,0,0}, [1,0,0,0,0]},
        {{0,0,0,1}, [0,1,0,0,0]},
        {{0,0,1,0}, [0,0,1,0,0]},
        {{0,0,1,1}, [0,0,0,1,0]},
        {{0,1,0,0}, [0,0,0,0,1]},
        {{0,1,0,1}, [1,1,0,0,0]},
        {{0,1,1,0}, [1,0,1,0,0]},
        {{0,1,1,1}, [0,1,1,0,0]},
        {{1,0,0,0}, [1,0,0,1,0]},
        {{1,0,0,1}, [0,1,0,1,0]},
        {{1,0,1,0}, [0,0,1,1,0]},
        {{1,0,1,1}, [1,0,0,0,1]},
        {{1,1,0,0}, [0,1,0,0,1]},
        {{1,1,0,1}, [1,1,1,0,0]},
        {{1,1,1,0}, [1,1,0,1,0]},
        {{1,1,1,1}, [1,0,1,0,1]}
    ],
    16 = length(lists:usort([ Pattern || {_, Pattern} <- Rows ])),
    Verilogs = [
        verilog(N, Rows)
        ||
        N <- [
            {experiment_a, 1},
            {experiment_b, 2},
            {experiment_c, 3},
            {experiment_d, 4},
            {experiment_e, 5}
        ]
    ],
    {Rows, Verilogs}.

%%--------------------------------------------------------------------

verilog({Name, N}, Rows) ->
    Nth = [ {I, lists:nth(N, L)} || {I, L} <- Rows ],
    Expr = expr_or(Nth, <<>>),
    {Name, verilog(Expr)}.

%%--------------------------------------------------------------------

verilog(Expr) ->
    <<
        "module experiment (\n"
        "  input wire a,\n"
        "  input wire b,\n"
        "  input wire c,\n"
        "  input wire d,\n"
        "  output wire q\n"
        ");\n"
        "  lcell lut (.in(", Expr/binary, "), .out(q));\n"
        "endmodule\n"
     >>.

%%--------------------------------------------------------------------

expr_or([], Expr) ->
    Expr;
expr_or([{_, 0} | Rows], Expr) ->
    expr_or(Rows, Expr);
expr_or([{I, 1} | Rows], Expr = <<>>) ->
    expr_or(Rows, expr_and(I, Expr));
expr_or([{I, 1} | Rows], Expr) ->
    expr_or(Rows, expr_and(I, <<Expr/binary, " || ">>)).

%%--------------------------------------------------------------------

expr_and({A, B, C, D}, Expr) ->
    Aexpr = case A of 0 -> <<"!a">>; 1 -> <<"a">> end,
    Bexpr = case B of 0 -> <<"!b">>; 1 -> <<"b">> end,
    Cexpr = case C of 0 -> <<"!c">>; 1 -> <<"c">> end,
    Dexpr = case D of 0 -> <<"!d">>; 1 -> <<"d">> end,
    <<
        Expr/binary,
        "(",
        Aexpr/binary,
        " && ",
        Bexpr/binary,
        " && ",
        Cexpr/binary,
        " && ",
        Dexpr/binary,
        ")"
    >>.

