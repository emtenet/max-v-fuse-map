-module(lab_clr1_experiment).

-export([run/0]).

% This experiment was designed to look at how the LAB's clr1
% source was selected between the four (4) global networks
% or the two (2) LAB control lines.
%
% The two control lines are specifically selected by routing
% the clock through LC's 7 and 8. LC 7 can only be selected into
% even control lines, and LC 8 into odd control lines.
%
% The clr1 line is turned off for the whole LAB with fuse:
%
%  * {{lab, X, Y}, clr1, off}
%
% The following fuse is for sourceing from global lines (or off):
%
%  * {{lab, X, Y}, clr1, global}
%
% And then the particular global is selected with:
%
%  * {{lab, X, Y}, clr1, global0}
%  * {{lab, X, Y}, clr1, global1}
%  * {{lab, X, Y}, clr1, global2}
%  * {{lab, X, Y}, clr1, global3}
%
% Otherwise a selection between control 4 or control 5 is made with:
%
%  * {{lab, X, Y}, clr1, control_5_not_4}
%
% The clr1 line can be inverted with:
%
%  * {{lab, X, Y}, clr1, invert}
%
% NOTE: Not sure why the clr1 lines is always inverted when not selecting
% from a global clock network?

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    device(Device).

%%--------------------------------------------------------------------

device(Device) ->
    Gclks = device:gclk_pins(Device),
    Pins0 = lists:subtract(device:pins(Device), Gclks),
    iterate:labs(Device, 3, Pins0,
        fun (LAB, Pins) ->
            sources(Device, LAB, Gclks, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Device, LAB, Experiments)
        end
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, Gclks, Pins) ->
    [Gclk0, Gclk1, Gclk2, Gclk3] = Gclks,
    {Clk, D, Q} = Pins,
    [
        source_never(Device, LAB, Clk, D, Q, never),
        source_global(Device, LAB, Clk, D, Q, gclk0_not, Gclk0, <<"!">>),
        source_global(Device, LAB, Clk, D, Q, gclk0, Gclk0, <<>>),
        source_global(Device, LAB, Clk, D, Q, gclk1, Gclk1, <<>>),
        source_global(Device, LAB, Clk, D, Q, gclk2, Gclk2, <<>>),
        source_global(Device, LAB, Clk, D, Q, gclk3, Gclk3, <<>>),
        source_local(Device, LAB, Clk, D, Q, local7, Gclk0, 7),
        source_local(Device, LAB, Clk, D, Q, local8, Gclk0, 8)
    ].

%%--------------------------------------------------------------------

experiments(Device, LAB, Experiments) ->
    io:format(" ==> ~p ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{iob, _, _}, _}) -> true;
        ({{iob, _, _}, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{ioc, _, _, _}, _, _, _}) -> true;
        ({{lab, _, _}, clk1, _}) -> true;
        ({{lab, _, _}, s_load, _}) -> true;
        ({{lc, _, _, _}, s_load}) -> true;
        ({_, lut, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %lab_clk1_experiment:control_routing(Experiments),
    %
    expect:fuse(Matrix, [0,1,1,1,1,1,1,1], {LAB, clr1, off}),
    expect:fuse(Matrix, [1,0,1,1,1,1,0,0], {LAB, clr1, invert}),
    expect:fuse(Matrix, [1,0,0,1,1,1,1,1], {LAB, clr1, global0}),
    expect:fuse(Matrix, [1,1,1,0,1,1,1,1], {LAB, clr1, global1}),
    expect:fuse(Matrix, [1,1,1,1,0,1,1,1], {LAB, clr1, global2}),
    expect:fuse(Matrix, [1,1,1,1,1,0,1,1], {LAB, clr1, global3}),
    expect:fuse(Matrix, [0,0,0,0,0,0,1,1], {LAB, clr1, global}),
    %
    [_, _, _, _, _, _, Local7, Local8] = Experiments,
    expect:control(Local7, cc, 4, a_clr),
    expect:control(Local8, cc, 5, a_clr),
    expect:fuse(Matrix, [1,1,1,1,1,1,1,0], {LAB, clr1, control_5_not_4}),
    ok.

%%--------------------------------------------------------------------

source_never(Device, LAB, Clk, D, Q, Name) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  dff ff (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_global(Device, LAB, Clk, D, Q, Name, Clr, Not) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clr, Clr},
            {global_clock, clr, true},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire clr,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  dff ff (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(", Not/binary, "clr),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_local(Device, LAB, Clk, D, Q, Name, Clr, N) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {not_gate_push_back, true},
            {location, clr, Clr},
            {global_clock, clr, false},
            {location, cc, lab:lc(LAB, N)},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire clr,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire c;\n"
            "  lcell cc (\n"
            "    .in(clr),\n"
            "    .out(c)\n"
            "  );\n"
            "  dff ff (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(c),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

