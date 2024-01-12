-module(lab_global_network_playground).

-export([run/0]).

% This experiment explores the global network backwards from a LAB.
%
% Based on the _MAX V Device Handbook_ we are looking for:
%
%  * LAB Column clock enables / disables
%  * Row clock enables / disables
%  * Global Clock selection

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
    iterate:labs(Device, Pins0, 5,
        fun (LAB, Pins) ->
            sources(Device, LAB, Gclks, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Device, LAB, Experiments)
        end,
        {batch, 1}
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, Gclks, Pins) ->
    [G0, G1, G2, G3] = Gclks,
    {P0, D0, Q0, D1, Q1} = Pins,
    Settings = [
        {location, d0, D0},
        {location, d1, D1},
        {location, q0, Q0},
        {location, q1, Q1},
        {location, ff0, lab:lc(LAB, 0)},
        {location, ff1, lab:lc(LAB, 1)}
    ],
    [
        source0(Device, G0, Settings),
        source1(Device, G0, Settings),
        source1(Device, G1, Settings),
        source1(Device, G2, Settings),
        source1(Device, G3, Settings),
        source1(Device, P0, Settings),
        source2(Device, G0, G1, Settings),
        source2(Device, G0, G2, Settings),
        source2(Device, G0, G3, Settings),
        source2(Device, G1, G2, Settings),
        source2(Device, G1, G3, Settings),
        source2(Device, G2, G3, Settings),
        source2(Device, G0, P0, Settings),
        source2(Device, G1, P0, Settings),
        source2(Device, G2, P0, Settings),
        source2(Device, G3, P0, Settings),
        source3(Device, G0, G1, G2, Settings),
        source3(Device, G0, G1, G3, Settings),
        source3(Device, G0, G2, G3, Settings),
        source3(Device, G1, G2, G3, Settings),
        source3(Device, G0, G1, P0, Settings),
        source3(Device, G0, G2, P0, Settings),
        source3(Device, G0, G3, P0, Settings),
        source3(Device, G1, G2, P0, Settings),
        source3(Device, G1, G3, P0, Settings),
        source3(Device, G2, G3, P0, Settings),
        source4(Device, G0, G1, G2, G3, Settings),
        source4(Device, P0, G1, G2, G3, Settings),
        source4(Device, G0, P0, G2, G3, Settings),
        source4(Device, G0, G1, P0, G3, Settings),
        source4(Device, G0, G1, G2, P0, Settings)
    ].

%%--------------------------------------------------------------------

experiments(Device, LAB = {lab, X, _}, Experiments) ->
    io:format(" ==> ~p ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{lab, _, _}, _, _}) -> true;
        ({{lab, _, _}, _, _, _}) -> true;
        ({{lc, _, _, _}, _}) -> true;
        ({{lc, _, _, _}, _, _}) -> true;
        ({_, _, _, _, cell, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %
    Patterns = lists:foldr(fun patterns/2, #{
        {disable, 0} => [],
        {disable, 1} => [],
        {disable, 2} => [],
        {disable, 3} => [],
        {dedicated, 0} => [],
        {dedicated, 1} => [],
        {dedicated, 2} => [],
        {dedicated, 3} => []
    }, Experiments),
    maps:foreach(fun (Name, Pattern) ->
        fuse(Name, Pattern, Matrix, X)
    end, Patterns),
    ok.

%%--------------------------------------------------------------------

source0(Device, Clk, Settings) ->
    #{
        title => {local, Clk},
        device => Device,
        settings => [
            {location, clk, Clk}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire d0,\n"
            "  input wire d1,\n"
            "  output wire q0,\n"
            "  output wire q1\n"
            ");\n"
            "  dff ff0 (\n"
            "    .d(d0),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q0)\n"
            "  );\n"
            "  dff ff1 (\n"
            "    .d(d1),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q1)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source1(Device, Clk, Settings) ->
    #{
        title => {global, Clk},
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire d0,\n"
            "  input wire d1,\n"
            "  output wire q0,\n"
            "  output wire q1\n"
            ");\n"
            "  dff ff0 (\n"
            "    .d(d0),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q0)\n"
            "  );\n"
            "  dff ff1 (\n"
            "    .d(d1),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q1)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source2(Device, Clk, Clr, Settings) ->
    #{
        title => {global, Clk, Clr},
        device => Device,
        settings => [
            {location, clk, Clk},
            {location, clr, Clr},
            {global_clock, clk, true},
            {global_clock, clr, true}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire clr,\n"
            "  input wire d0,\n"
            "  input wire d1,\n"
            "  output wire q0,\n"
            "  output wire q1\n"
            ");\n"
            "  dff ff0 (\n"
            "    .d(d0),\n"
            "    .clk(clk),\n"
            "    .clrn(clr),\n"
            "    .prn(1),\n"
            "    .q(q0)\n"
            "  );\n"
            "  dff ff1 (\n"
            "    .d(d1),\n"
            "    .clk(clk),\n"
            "    .clrn(clr),\n"
            "    .prn(1),\n"
            "    .q(q1)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source3(Device, Clk0, Clk1, Clr, Settings) ->
    #{
        title => {global, Clk0, Clk1, Clr},
        device => Device,
        settings => [
            {location, clk0, Clk0},
            {location, clk1, Clk1},
            {location, clr, Clr},
            {global_clock, clk0, true},
            {global_clock, clk1, true},
            {global_clock, clr, true}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk0,\n"
            "  input wire clk1,\n"
            "  input wire clr,\n"
            "  input wire d0,\n"
            "  input wire d1,\n"
            "  output wire q0,\n"
            "  output wire q1\n"
            ");\n"
            "  dff ff0 (\n"
            "    .d(d0),\n"
            "    .clk(clk0),\n"
            "    .clrn(clr),\n"
            "    .prn(1),\n"
            "    .q(q0)\n"
            "  );\n"
            "  dff ff1 (\n"
            "    .d(d1),\n"
            "    .clk(clk1),\n"
            "    .clrn(clr),\n"
            "    .prn(1),\n"
            "    .q(q1)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source4(Device, Clk0, Clk1, Clr0, Clr1, Settings) ->
    #{
        title => {global, Clk0, Clk1, Clr0, Clr1},
        device => Device,
        settings => [
            {location, clk0, Clk0},
            {location, clk1, Clk1},
            {location, clr0, Clr0},
            {location, clr1, Clr1},
            {global_clock, clk0, true},
            {global_clock, clk1, true},
            {global_clock, clr0, true},
            {global_clock, clr1, true}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk0,\n"
            "  input wire clk1,\n"
            "  input wire clr0,\n"
            "  input wire clr1,\n"
            "  input wire d0,\n"
            "  input wire d1,\n"
            "  output wire q0,\n"
            "  output wire q1\n"
            ");\n"
            "  dff ff0 (\n"
            "    .d(d0),\n"
            "    .clk(clk0),\n"
            "    .clrn(clr0),\n"
            "    .prn(1),\n"
            "    .q(q0)\n"
            "  );\n"
            "  dff ff1 (\n"
            "    .d(d1),\n"
            "    .clk(clk1),\n"
            "    .clrn(clr1),\n"
            "    .prn(1),\n"
            "    .q(q1)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

patterns({_Name, _, #{signals := Signals}}, Patterns) ->
    Bits = maps:fold(fun patterns_signal/3, #{
        {disable, 0} => 0,
        {disable, 1} => 0,
        {disable, 2} => 0,
        {disable, 3} => 0,
        {dedicated, 0} => 1,
        {dedicated, 1} => 1,
        {dedicated, 2} => 1,
        {dedicated, 3} => 1
    }, Signals),
    maps:map(fun (Key, Pattern) ->
        #{Key := Bit} = Bits,
        [Bit | Pattern]
    end, Patterns).

%%--------------------------------------------------------------------

patterns_signal(_, #{dests := Dests}, Bits) ->
    lists:foldl(fun patterns_dest/2, Bits, Dests).

%%--------------------------------------------------------------------

patterns_dest(#{route := Route}, Bits) ->
    pattern_route(Route, Bits).

%%--------------------------------------------------------------------

pattern_route([], Bits) ->
    Bits;
pattern_route([{global_clk_h,_,_,_,G}, {clk_buffer,_,_,_,_} | _], Bits) ->
    Bits#{{disable, G} => 1};
pattern_route([{global_clk_h,_,_,_,G}, {global_clk_mux,_,_,_,G} | _], Bits) ->
    Bits#{{disable, G} => 1, {dedicated, G} => 0};
pattern_route([_ | Route], Bits) ->
    pattern_route(Route, Bits).

%%--------------------------------------------------------------------

fuse({dedicated, N}, Pattern, Matrix, _) ->
    Fuse = {{global, N}, dedicated},
    expect:fuse(Matrix, Pattern, Fuse);
fuse({disable, N}, Pattern, Matrix, X) ->
    Row = {{global,N}, row, off},
    Col = {{global,N}, {column, X}, off},
    expect:fuse(Matrix, Pattern, Row, Col).

