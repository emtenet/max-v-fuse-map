-module(lc_carry_chain_4_experiment).

-export([run/0]).

% Duplicate of lc_carry_chain_3_experiment
% but with a carry-chain length of 4

-include("decompile.hrl").

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    Pins0 = device:pins(Device),
    iterate:labs(Device, Pins0, 13,
        fun (LAB, Pins) ->
            sources(Density, Device, LAB, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Density, Device, LAB, Experiments)
        end,
        {batch, 1}
     ).

%%--------------------------------------------------------------------

sources(Density, Device, LAB, Pins) ->
    case adjacent(Density, LAB) of
        {ok, _} ->
            [
                source(Device, LAB, N, Pins)
                ||
                N <- lists:seq(0, 9)
            ];

        false ->
            ok = experiment:fit_error(
                source(Device, LAB, 6, Pins)
            ),
            [
                source(Device, LAB, N, Pins)
                ||
                N <- lists:seq(0, 5)
            ]
    end.

%%--------------------------------------------------------------------

adjacent(Density, {lab, X, Y}) ->
    Adjacent = {lab, X + 1, Y},
    case density:is_lab(Adjacent, Density) of
        true ->
            {ok, Adjacent};

        false ->
            false
    end.

%%--------------------------------------------------------------------

experiments(Density, Device, LAB, Experiments) ->
    io:format(" ==> ~s ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, data_a3, _}) -> true;
        ({_, data_a6, _}) -> true;
        ({_, data_b3, _}) -> true;
        ({_, data_b6, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, data_d6, _}) -> true;
        ({_, fast_slew_rate}) -> true;
        ({_, lut, _}) -> true;
        ({_, output3, _}) -> true;
        ({_, output6, _}) -> true;
        ({_, output_invert}) -> true;
        ({_, output_left, _}) -> true;
        ({_, output_right, _}) -> true;
        ({_, {interconnect, _}, _, _}) -> true;
        ({_, {mux, _}, _, _}) -> true;
        ({_, {mux, _}, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:control_routing(Experiments),
    %display:routing(Experiments, Density),
    %
    expect(Matrix, LAB, adjacent(Density, LAB)),
    %
    Density = device:density(Device),
    lists:foreach(fun (Experiment) ->
        lut_value(Experiment, Density)
    end, Experiments),
    %
    ok.

%%--------------------------------------------------------------------

expect(Matrix, LAB, {ok, ADJ}) ->
    expect:fuse(Matrix, [0,1,1,1,1,1,1,1,1,1], {lab:lc(LAB, 1), carry_in}),
    expect:fuse(Matrix, [0,0,1,1,1,1,1,1,1,1], {lab:lc(LAB, 2), carry_in}),
    expect:fuse(Matrix, [0,0,0,1,1,1,1,1,1,1], {lab:lc(LAB, 3), carry_in}),
    expect:fuse(Matrix, [0,0,0,0,1,1,1,1,1,1], {lab:lc(LAB, 4), carry_in}),
    expect:fuse(Matrix, [1,0,0,0,0,1,1,1,1,1], {lab:lc(LAB, 5), carry_in}),
    expect:fuse(Matrix, [1,1,0,0,0,0,1,1,1,1], {lab:lc(LAB, 6), carry_in}),
    expect:fuse(Matrix, [1,1,1,0,0,0,0,1,1,1], {lab:lc(LAB, 7), carry_in}),
    expect:fuse(Matrix, [1,1,1,1,0,0,0,0,1,1], {lab:lc(LAB, 8), carry_in}),
    expect:fuse(Matrix, [1,1,1,1,1,0,0,0,0,1], {lab:lc(LAB, 9), carry_in}),
    expect:fuse(Matrix, [1,1,1,1,1,1,0,0,0,0], {lab:lc(ADJ, 0), carry_in}),
    expect:fuse(Matrix, [1,1,1,1,1,1,1,0,0,0], {lab:lc(ADJ, 1), carry_in}),
    expect:fuse(Matrix, [1,1,1,1,1,1,1,1,0,0], {lab:lc(ADJ, 2), carry_in}),
    expect:fuse(Matrix, [1,1,1,1,1,1,1,1,1,0], {lab:lc(ADJ, 3), carry_in});
expect(Matrix, LAB, false) ->
    expect:fuse(Matrix, [0,1,1,1,1,1], {lab:lc(LAB, 1), carry_in}),
    expect:fuse(Matrix, [0,0,1,1,1,1], {lab:lc(LAB, 2), carry_in}),
    expect:fuse(Matrix, [0,0,0,1,1,1], {lab:lc(LAB, 3), carry_in}),
    expect:fuse(Matrix, [0,0,0,0,1,1], {lab:lc(LAB, 4), carry_in}),
    expect:fuse(Matrix, [1,0,0,0,0,1], {lab:lc(LAB, 5), carry_in}),
    expect:fuse(Matrix, [1,1,0,0,0,0], {lab:lc(LAB, 6), carry_in}),
    expect:fuse(Matrix, [1,1,1,0,0,0], {lab:lc(LAB, 7), carry_in}),
    expect:fuse(Matrix, [1,1,1,1,0,0], {lab:lc(LAB, 8), carry_in}),
    expect:fuse(Matrix, [1,1,1,1,1,0], {lab:lc(LAB, 9), carry_in}).

%%--------------------------------------------------------------------

lut_value(Experiment = {Name, _, _}, Density) ->
    Logic = decompile:experiment(Experiment, Density),
    {LAB, adder, N} = Name,
    At0 = lab:lc(LAB, N),
    At1 = lc:carry_to(At0),
    At2 = lc:carry_to(At1),
    At3 = lc:carry_to(At2),
    At4 = lc:carry_to(At3),
    #{At0 := LC0} = Logic,
    #{At1 := LC1} = Logic,
    #{At2 := LC2} = Logic,
    #{At3 := LC3} = Logic,
    #{At4 := LC4} = Logic,
    expect:lut(Name, At0, LC0, a_xor_b_carry),
    expect:lut(Name, At1, LC1, a_xor_b_xor_c_carry),
    expect:lut(Name, At2, LC2, a_xor_b_xor_not_c_carry),
    expect:lut(Name, At3, LC3, a_xor_b_xor_c_carry),
    expect:lut(Name, At4, LC4, not_c).

%%--------------------------------------------------------------------

source(Device, LAB, N, {A0, A1, A2, A3, B0, B1, B2, B3, S0, S1, S2, S3, C4}) ->
    #{
        title => {LAB, adder, N},
        device => Device,
        settings => [
            {location, <<"cs0~0">>, lab:lc(LAB, N)},
            {location, <<"a[0]">>, A0},
            {location, <<"a[1]">>, A1},
            {location, <<"a[2]">>, A2},
            {location, <<"a[3]">>, A3},
            {location, <<"b[0]">>, B0},
            {location, <<"b[1]">>, B1},
            {location, <<"b[2]">>, B2},
            {location, <<"b[3]">>, B3},
            {location, <<"sout[0]">>, S0},
            {location, <<"sout[1]">>, S1},
            {location, <<"sout[2]">>, S2},
            {location, <<"sout[3]">>, S3},
            {location, <<"cout[4]">>, C4}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [3:0] a,\n"
            "  input wire [3:0] b,\n"
            "  output wire [3:0] s,\n"
            "  output wire c\n"
            ");\n"
            "  wire [4:1] cin;\n"
            "  wire [4:0] cout;\n"
            "  wire [3:0] sin;\n"
            "  wire [3:0] sout;\n"
            "  assign cout[0] = 0;\n"
            "  assign sin = a ^ b ^ cout[3:0];\n"
            "  assign cin[4:1] = (a & b) | (a & cout[3:0]) | (b & cout[3:0]);\n"
            "  carry_sum cs0 (\n"
            "    .sin(sin[0]),\n"
            "    .sout(sout[0]),\n"
            "    .cin(cin[1]),\n"
            "    .cout(cout[1])\n"
            "  );\n"
            "  carry_sum cs1 (\n"
            "    .sin(sin[1]),\n"
            "    .sout(sout[1]),\n"
            "    .cin(cin[2]),\n"
            "    .cout(cout[2])\n"
            "  );\n"
            "  carry_sum cs2 (\n"
            "    .sin(sin[2]),\n"
            "    .sout(sout[2]),\n"
            "    .cin(cin[3]),\n"
            "    .cout(cout[3])\n"
            "  );\n"
            "  carry_sum cs3 (\n"
            "    .sin(sin[3]),\n"
            "    .sout(sout[3]),\n"
            "    .cin(cin[4]),\n"
            "    .cout(cout[4])\n"
            "  );\n"
            "  assign s = sout;\n"
            "  assign c = cout[4];\n"
            "endmodule\n"
        >>
    }.

