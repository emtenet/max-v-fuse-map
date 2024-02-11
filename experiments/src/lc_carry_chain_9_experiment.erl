-module(lc_carry_chain_9_experiment).

-export([run/0]).

% Duplicate of lc_carry_chain_3_experiment
% but with a carry-chain length of 9

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    Pins0 = device:pins(Device),
    iterate:labs(Device, Pins0, 28,
        fun (LAB, Pins) ->
            sources(Device, LAB, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Density, Device, LAB, Experiments)
        end,
        {batch, 1}
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, Pins) ->
    [source(Device, LAB, Pins)].

%%--------------------------------------------------------------------

experiments(Density, Device, LAB, Experiments) ->
    io:format(" ==> ~s ~p~n", [Device, LAB]),
    Minimal = density:minimal_fuses(Density),
    Matrix0 = matrix:build(Device, [{minimal, Minimal} | Experiments]),
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
        %({_, lut, _}) -> true;
        ({_, input_off}) -> true;
        ({_, schmitt_trigger}) -> true;
        ({_, output3, _}) -> true;
        ({_, output6, _}) -> true;
        ({_, output_invert}) -> true;
        ({_, enable_invert}) -> true;
        ({_, input_delay}) -> true;
        ({_, output_left, _}) -> true;
        ({_, output_right, _}) -> true;
        ({_, {interconnect, _}, _, _}) -> true;
        ({_, {mux, _}, _, _}) -> true;
        ({_, {mux, _}, _}) -> true;
        (_) -> false
    end),
    %
    _ = Matrix,
    %matrix:print(Matrix),
    %display:control_routing(Experiments),
    %display:routing(Experiments, Density),
    %
    ok.

%%--------------------------------------------------------------------

source(Device, LAB, Pins) ->
    {A0, A1, A2, A3, A4, A5, A6, A7, A8,
     B0, B1, B2, B3, B4, B5, B6, B7, B8,
     S0, S1, S2, S3, S4, S5, S6, S7, S8, C9} = Pins,
    #{
        title => {LAB, adder},
        device => Device,
        settings => [
            {seed, 3},
            {location, <<"cs0~0">>, lab:lc(LAB, 0)},
            {location, <<"a[0]">>, A0},
            {location, <<"a[1]">>, A1},
            {location, <<"a[2]">>, A2},
            {location, <<"a[3]">>, A3},
            {location, <<"a[4]">>, A4},
            {location, <<"a[5]">>, A5},
            {location, <<"a[6]">>, A6},
            {location, <<"a[7]">>, A7},
            {location, <<"a[8]">>, A8},
            {location, <<"b[0]">>, B0},
            {location, <<"b[1]">>, B1},
            {location, <<"b[2]">>, B2},
            {location, <<"b[3]">>, B3},
            {location, <<"b[4]">>, B4},
            {location, <<"b[5]">>, B5},
            {location, <<"b[6]">>, B6},
            {location, <<"b[7]">>, B7},
            {location, <<"b[8]">>, B8},
            {location, <<"sout[0]">>, S0},
            {location, <<"sout[1]">>, S1},
            {location, <<"sout[2]">>, S2},
            {location, <<"sout[3]">>, S3},
            {location, <<"sout[4]">>, S4},
            {location, <<"sout[5]">>, S5},
            {location, <<"sout[6]">>, S6},
            {location, <<"sout[7]">>, S7},
            {location, <<"sout[8]">>, S8},
            {location, <<"cout[9]">>, C9}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [8:0] a,\n"
            "  input wire [8:0] b,\n"
            "  output wire [8:0] s,\n"
            "  output wire c\n"
            ");\n"
            "  wire [9:1] cin;\n"
            "  wire [9:0] cout;\n"
            "  wire [8:0] sin;\n"
            "  wire [8:0] sout;\n"
            "  assign cout[0] = 0;\n"
            "  assign sin = a ^ b ^ cout[8:0];\n"
            "  assign cin[9:1] = (a & b) | (a & cout[8:0]) | (b & cout[8:0]);\n"
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
            "  carry_sum cs4 (\n"
            "    .sin(sin[4]),\n"
            "    .sout(sout[4]),\n"
            "    .cin(cin[5]),\n"
            "    .cout(cout[5])\n"
            "  );\n"
            "  carry_sum cs5 (\n"
            "    .sin(sin[5]),\n"
            "    .sout(sout[5]),\n"
            "    .cin(cin[6]),\n"
            "    .cout(cout[6])\n"
            "  );\n"
            "  carry_sum cs6 (\n"
            "    .sin(sin[6]),\n"
            "    .sout(sout[6]),\n"
            "    .cin(cin[7]),\n"
            "    .cout(cout[7])\n"
            "  );\n"
            "  carry_sum cs7 (\n"
            "    .sin(sin[7]),\n"
            "    .sout(sout[7]),\n"
            "    .cin(cin[8]),\n"
            "    .cout(cout[8])\n"
            "  );\n"
            "  carry_sum cs8 (\n"
            "    .sin(sin[8]),\n"
            "    .sout(sout[8]),\n"
            "    .cin(cin[9]),\n"
            "    .cout(cout[9])\n"
            "  );\n"
            "  assign s = sout;\n"
            "  assign c = cout[9];\n"
            "endmodule\n"
        >>
    }.

