-module(lc_carry_chain_experiment).

-export([run/0]).

% This experiment looks for the Carry Chain fuses.
%
% The fuse:
%  * puts the previous LC in "arithmetic" mode,
%  * assigns the previous LC's carry-out to the LC's `data_c` input.
%
% NOTE: The LC with the fuse, may not be in "arithmentic" mode
%    if the next LC is not requesting a carry-in
%
% NOTE: The "previous" LC to {lc, X, Y, N} is:
%  * {lc, X, Y, N - 1} when N is 1..9
%  * {lc, X - 1, Y, 9} when N is 0.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    Pins0 = device:pins(Device),
    iterate:labs(Device, Pins0, 10,
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
    Adjacent0 = source(Device, LAB, 7, 8, 9, Pins),
    Adjacent = case adjacent(Density, LAB) of
        {ok, _} ->
            [Adjacent0];

        false ->
            ok = experiment:fit_error(Adjacent0),
            []
    end,
    [
        source(Device, LAB, 0, 1, 2, Pins),
        source(Device, LAB, 1, 2, 3, Pins),
        source(Device, LAB, 2, 3, 4, Pins),
        source(Device, LAB, 3, 4, 5, Pins),
        source(Device, LAB, 4, 5, 6, Pins),
        source(Device, LAB, 5, 6, 7, Pins),
        source(Device, LAB, 6, 7, 8, Pins)
        |
        Adjacent
    ].

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
    %
    case adjacent(Density, LAB) of
        {ok, ADJ} ->
            expect:fuse(Matrix, [0,1,1,1,1,1,1,1], {lab:lc(LAB, 1), carry_in}),
            expect:fuse(Matrix, [0,0,1,1,1,1,1,1], {lab:lc(LAB, 2), carry_in}),
            expect:fuse(Matrix, [0,0,0,1,1,1,1,1], {lab:lc(LAB, 3), carry_in}),
            expect:fuse(Matrix, [1,0,0,0,1,1,1,1], {lab:lc(LAB, 4), carry_in}),
            expect:fuse(Matrix, [1,1,0,0,0,1,1,1], {lab:lc(LAB, 5), carry_in}),
            expect:fuse(Matrix, [1,1,1,0,0,0,1,1], {lab:lc(LAB, 6), carry_in}),
            expect:fuse(Matrix, [1,1,1,1,0,0,0,1], {lab:lc(LAB, 7), carry_in}),
            expect:fuse(Matrix, [1,1,1,1,1,0,0,0], {lab:lc(LAB, 8), carry_in}),
            expect:fuse(Matrix, [1,1,1,1,1,1,0,0], {lab:lc(LAB, 9), carry_in}),
            expect:fuse(Matrix, [1,1,1,1,1,1,1,0], {lab:lc(ADJ, 0), carry_in});

        false ->
            expect:fuse(Matrix, [0,1,1,1,1,1,1], {lab:lc(LAB, 1), carry_in}),
            expect:fuse(Matrix, [0,0,1,1,1,1,1], {lab:lc(LAB, 2), carry_in}),
            expect:fuse(Matrix, [0,0,0,1,1,1,1], {lab:lc(LAB, 3), carry_in}),
            expect:fuse(Matrix, [1,0,0,0,1,1,1], {lab:lc(LAB, 4), carry_in}),
            expect:fuse(Matrix, [1,1,0,0,0,1,1], {lab:lc(LAB, 5), carry_in}),
            expect:fuse(Matrix, [1,1,1,0,0,0,1], {lab:lc(LAB, 6), carry_in}),
            expect:fuse(Matrix, [1,1,1,1,0,0,0], {lab:lc(LAB, 7), carry_in}),
            expect:fuse(Matrix, [1,1,1,1,1,0,0], {lab:lc(LAB, 8), carry_in}),
            expect:fuse(Matrix, [1,1,1,1,1,1,0], {lab:lc(LAB, 9), carry_in})
    end,
    %
    ok.

%%--------------------------------------------------------------------

source(Device, LAB, X, Y, Z, {A0, A1, A2, B0, B1, B2, S0, S1, S2, C3}) ->
    #{
        title => {LAB, adder, X, Y, Z},
        device => Device,
        settings => [
            {location, <<"cs0~0">>, lab:lc(LAB, X)},
            {location, <<"cs0~5">>, lab:lc(LAB, Y)},
            {location, <<"cs0~10">>, lab:lc(LAB, Z)},
            {location, <<"a[0]">>, A0},
            {location, <<"a[1]">>, A1},
            {location, <<"a[2]">>, A2},
            {location, <<"b[0]">>, B0},
            {location, <<"b[1]">>, B1},
            {location, <<"b[2]">>, B2},
            {location, <<"sout[0]">>, S0},
            {location, <<"sout[1]">>, S1},
            {location, <<"sout[2]">>, S2},
            {location, <<"cout[3]">>, C3}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [2:0] a,\n"
            "  input wire [2:0] b,\n"
            "  output wire [2:0] s,\n"
            "  output wire c\n"
            ");\n"
            "  wire [3:1] cin;\n"
            "  wire [3:0] cout;\n"
            "  wire [2:0] sin;\n"
            "  wire [2:0] sout;\n"
            "  assign cout[0] = 0;\n"
            "  assign sin = a ^ b ^ cout[2:0];\n"
            "  assign cin[3:1] = (a & b) | (a & cout[2:0]) | (b & cout[2:0]);\n"
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
            "  assign s = sout;\n"
            "  assign c = cout[3];\n"
            "endmodule\n"
        >>
    }.

