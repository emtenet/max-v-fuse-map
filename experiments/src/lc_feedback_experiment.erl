-module(lc_feedback_experiment).

-export([run/0]).

% This experiment looks at the LC's register feedback into the LUT.
%
% This also depends on the ability to build a source with a stacked
% LC from independant LUT & REG parts.
%
% The feedback path (data_c) is invered, check that with decompile.

-include("decompile.hrl").

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    [Clk | _] = device:gclk_pins(Device),
    Pins0 = lists:delete(Clk, device:pins(Device)),
    iterate:labs(Device, Pins0, 5,
        fun (LAB, Pins) ->
            sources(Device, LAB, Clk, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Device, LAB, Experiments)
        end,
        {batch, 1}
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, Clk, Pins) ->
    Ns = lists:seq(0, 4),
    [
        source(Device, LAB, N, Clk, Pins)
        ||
        N <- Ns
    ].

%%--------------------------------------------------------------------

experiments(Device, LAB, Experiments) ->
    io:format(" ==> ~s ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{c4, _, _}, _, _}) -> true;
        ({{c4, _, _}, _, _, _}) -> true;
        ({{global, _}, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{iob, _, _}, _, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{lc, _, _, _}, a_clr1}) -> true;
        ({{lc, _, _, _}, clk2}) -> true;
        ({{lc, _, _, _}, data_a3, _}) -> true;
        ({{lc, _, _, _}, data_a6, _}) -> true;
        ({{lc, _, _, _}, data_b3, _}) -> true;
        ({{lc, _, _, _}, data_b6, _}) -> true;
        ({{lc, _, _, _}, data_c3, _}) -> true;
        ({{lc, _, _, _}, data_c6, _}) -> true;
        ({{lc, _, _, _}, data_d3, _}) -> true;
        ({{lc, _, _, _}, data_d6, _}) -> true;
        ({{lc, _, _, _}, lut, _}) -> true;
        ({{lc, _, _, _}, output_left, lut}) -> true;
        ({{lc, _, _, _}, output_right, lut}) -> true;
        ({{lc, _, _, _}, s_clr_load}) -> true;
        ({{r4, _, _}, _, _}) -> true;
        ({{r4, _, _}, _, _, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:routing(Experiments, device:density(Device)),
    %
    expect:fuse(Matrix, [0,1,1,1,1], {lab:lc(LAB, 0), feedback},
                                     {lab:lc(LAB, 5), feedback}),
    expect:fuse(Matrix, [1,0,1,1,1], {lab:lc(LAB, 1), feedback},
                                     {lab:lc(LAB, 6), feedback}),
    expect:fuse(Matrix, [1,1,0,1,1], {lab:lc(LAB, 2), feedback},
                                     {lab:lc(LAB, 7), feedback}),
    expect:fuse(Matrix, [1,1,1,0,1], {lab:lc(LAB, 3), feedback},
                                     {lab:lc(LAB, 8), feedback}),
    expect:fuse(Matrix, [1,1,1,1,0], {lab:lc(LAB, 4), feedback},
                                     {lab:lc(LAB, 9), feedback}),
    %
    Density = device:density(Device),
    lists:foreach(fun (Experiment) ->
        lut_experiment(Experiment, Density)
    end, Experiments),
    ok.

%%--------------------------------------------------------------------

lut_experiment(Experiment = {Name, _, _}, Density) ->
    Logic = decompile:experiment(Experiment, Density),
    {LAB, N} = Name,
    At1 = lab:lc(LAB, N),
    At2 = lab:lc(LAB, N + 5),
    #{At1 := LC1} = Logic,
    #{At2 := LC2} = Logic,
    lut_lc(Name, At1, LC1),
    lut_lc(Name, At2, LC2).

%%--------------------------------------------------------------------

lut_lc(Name, At, LC = #lc{feedback = true, lut_ports = #{data_a := _}}) ->
    expect:lut(Name, At, LC, a_xor_c);
lut_lc(Name, At, LC = #lc{feedback = true, lut_ports = #{data_b := _}}) ->
    expect:lut(Name, At, LC, b_xor_c);
lut_lc(Name, At, LC = #lc{feedback = true, lut_ports = #{data_d := _}}) ->
    expect:lut(Name, At, LC, c_xor_d).

%%--------------------------------------------------------------------

source(Device, LAB, N, Clk, {LUT1In, LUT1Out, FF1In, LUT2In, LUT2Out}) ->
    #{
        title => {LAB, N},
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, lut1, lab:lc(LAB, N)},
            {location, lut1_d, LUT1In},
            {location, lut1_q, LUT1Out},
            {location, ff1, lab:lc(LAB, N)},
            {location, ff1_d, FF1In},
            {location, lut2, lab:lc(LAB, 5 + N)},
            {location, lut2_d, LUT2In},
            {location, lut2_q, LUT2Out},
            {location, ff2, lab:lc(LAB, 5 + N)}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire lut1_d,\n"
            "  output wire lut1_q,\n"
            "  input wire ff1_d,\n"
            "  input wire lut2_d,\n"
            "  output wire lut2_q\n"
            ");\n"
            "  wire feedback1;\n"
            "  lcell lut1 (\n"
            "    .in(lut1_d ^ feedback1),\n"
            "    .out(lut1_q)\n"
            "  );\n"
            "  dff ff1 (\n"
            "    .d(ff1_d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(feedback1)\n"
            "  );\n"
            "  wire feedback2;\n"
            "  lcell lut2 (\n"
            "    .in(lut2_d ^ feedback2),\n"
            "    .out(lut2_q)\n"
            "  );\n"
            "  dff ff2 (\n"
            "    .d(lut2_q),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(feedback2)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

