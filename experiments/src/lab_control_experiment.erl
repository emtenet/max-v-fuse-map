-module(lab_control_experiment).

-export([run/0]).

% This experiment aims to populate the route_cache with enough
% information to understand the LAB control MUX.
%
% Therefore multiple experiments will be run per LAB that use all size
% control lines.

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
    iterate:labs(Device, 12, Pins0,
        fun (LAB, Pins) ->
            sources(Device, LAB, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Device, LAB, Experiments)
        end,
        {batch, 4}
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, Pins) ->
    [
        external(Device, Pins, lab:lc(LAB, 0), lab:lc(LAB, 9)),
        external(Device, Pins, lab:lc(LAB, 1), lab:lc(LAB, 8)),
        external(Device, Pins, lab:lc(LAB, 2), lab:lc(LAB, 7)),
        external(Device, Pins, lab:lc(LAB, 3), lab:lc(LAB, 6)),
        external(Device, Pins, lab:lc(LAB, 4), lab:lc(LAB, 5)),
        internal(Device, Pins, LAB)
    ].

%%--------------------------------------------------------------------

experiments(Device, LAB, _Experiments) ->
    io:format(" ==> ~p ~p~n", [Device, LAB]),
    ok.

%%--------------------------------------------------------------------

external(Device, Pins, LC1, LC2) ->
    {Clk1, Clr1, Ena1, Clk2, Clr2, Ena2, A, B, C, D, X, Y} = Pins,
    #{
        title => {LC1, LC2},
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, clr1, Clr1},
            {location, ena1, Ena1},
            {location, clk2, Clk2},
            {location, clr2, Clr2},
            {location, ena2, Ena2},
            {location, ff1, LC1},
            {location, ff2, LC2},
            {location, a, A},
            {location, b, B},
            {location, c, C},
            {location, d, D},
            {location, x, X},
            {location, y, Y}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire clr1,\n"
            "  input wire ena1,\n"
            "  input wire clk2,\n"
            "  input wire clr2,\n"
            "  input wire ena2,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  input wire c,\n"
            "  input wire d,\n"
            "  output wire x,\n"
            "  output wire y\n"
            ");\n"
            "  dffe ff1 (\n"
            "    .d(a && b),\n"
            "    .clk(clk1),\n"
            "    .clrn(clr1),\n"
            "    .prn(1),\n"
            "    .ena(ena1),\n"
            "    .q(x)\n"
            "  );\n"
            "  dffe ff2 (\n"
            "    .d(c && d),\n"
            "    .clk(clk2),\n"
            "    .clrn(clr2),\n"
            "    .prn(1),\n"
            "    .ena(ena2),\n"
            "    .q(y)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

internal(Device, Pins, LAB) ->
    {Clk1, Clr1, Ena1, Clk2, Clr2, Ena2, A, B, C, D, X, Y} = Pins,
    #{
        title => LAB,
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, clr1, Clr1},
            {location, ena1, Ena1},
            {location, clk2, Clk2},
            {location, clr2, Clr2},
            {location, ena2, Ena2},
            {location, ff1, lab:lc(LAB, 0)},
            {location, cc1, lab:lc(LAB, 1)},
            {location, nn1, lab:lc(LAB, 2)},
            {location, ee1, lab:lc(LAB, 3)},
            {location, ff2, lab:lc(LAB, 5)},
            {location, cc2, lab:lc(LAB, 6)},
            {location, nn2, lab:lc(LAB, 7)},
            {location, ee2, lab:lc(LAB, 8)},
            {location, a, A},
            {location, b, B},
            {location, c, C},
            {location, d, D},
            {location, x, X},
            {location, y, Y}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire clr1,\n"
            "  input wire ena1,\n"
            "  input wire clk2,\n"
            "  input wire clr2,\n"
            "  input wire ena2,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  input wire c,\n"
            "  input wire d,\n"
            "  output wire x,\n"
            "  output wire y\n"
            ");\n"
            "  wire c1;\n"
            "  wire n1;\n"
            "  wire e1;\n"
            "  wire c2;\n"
            "  wire n2;\n"
            "  wire e2;\n"
            "  lcell cc1 (\n"
            "    .in(clk1),\n"
            "    .out(c1)\n"
            "  );\n"
            "  lcell nn1 (\n"
            "    .in(clr1),\n"
            "    .out(n1)\n"
            "  );\n"
            "  lcell ee1 (\n"
            "    .in(ena1),\n"
            "    .out(e1)\n"
            "  );\n"
            "  lcell cc2 (\n"
            "    .in(clk2),\n"
            "    .out(c2)\n"
            "  );\n"
            "  lcell nn2 (\n"
            "    .in(clr2),\n"
            "    .out(n2)\n"
            "  );\n"
            "  lcell ee2 (\n"
            "    .in(ena2),\n"
            "    .out(e2)\n"
            "  );\n"
            "  dffe ff1 (\n"
            "    .d(a && b),\n"
            "    .clk(c1),\n"
            "    .clrn(n1),\n"
            "    .prn(1),\n"
            "    .ena(e1),\n"
            "    .q(x)\n"
            "  );\n"
            "  dffe ff2 (\n"
            "    .d(c && d),\n"
            "    .clk(c2),\n"
            "    .clrn(n2),\n"
            "    .prn(1),\n"
            "    .ena(e2),\n"
            "    .q(y)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

