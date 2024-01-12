-module(iob_global_network_playground).

-export([run/0]).

% This experiment explores the global network backwards from an IOB.
%
% Based on the _MAX V Device Handbook_ we are looking for:
%
%  * LAB Column clock enables / disables
%  * Row clock enables / disables
%  * Global Clock selection
%
% NOTE: It appears that only 2 global output enable signale lines
% can be sent to an IOB.

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
    iterate:iobs(Device, Pins0,
        fun (IOB, _) ->
            pin_count(Device, IOB, Gclks)
        end,
        fun (IOB, _, Pins) ->
            sources(Device, IOB, Gclks, Pins)
        end,
        fun (IOB, _, _, Experiments) ->
            experiments(Device, IOB, Experiments)
        end,
        {batch, 2}
     ).

%%--------------------------------------------------------------------

pin_count(Device, IOB, Gclks) ->
    {3, except, pins(Device, IOB, Gclks)}.

%%--------------------------------------------------------------------

pins(Device, IOB, Gclks) ->
    [P0, P1 | _] = [
        Pin
        ||
        {_, Pin} <- Device:iocs(IOB),
        not lists:member(Pin, Gclks)
    ],
    [P0, P1].

%%--------------------------------------------------------------------

sources(Device, IOB, Gclks, {P0, D0, D1}) ->
    [G0, G1, G2, G3] = Gclks,
    [IO0, IO1] = pins(Device, IOB, Gclks),
    Settings = [
        {location, io0, IO0}, {location, d0, D0},
        {location, io1, IO1}, {location, d1, D1}
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
        source2(Device, G3, P0, Settings)
    ].

%%--------------------------------------------------------------------

experiments(Device, IOB = {iob, X, _}, Experiments) ->
    io:format(" ==> ~p ~p~n", [Device, IOB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
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
        {internal, 0} => [],
        {internal, 1} => [],
        {internal, 2} => [],
        {internal, 3} => []
    }, Experiments),
    maps:foreach(fun (Name, Pattern) ->
        fuse(Name, Pattern, Matrix, X)
    end, Patterns),
    ok.

%%--------------------------------------------------------------------

source0(Device, OE, Settings) ->
    #{
        title => {local, OE},
        device => Device,
        settings => [
            {location, oe, OE}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire oe,\n"
            "  input wire d0,\n"
            "  input wire d1,\n"
            "  output wire q0,\n"
            "  output wire q1\n"
            ");\n"
            "  alt_outbuf_tri io0 (\n"
            "    .i(d0),\n"
            "    .o(q0),\n"
            "    .oe(oe)\n"
            "  );\n"
            "  alt_outbuf_tri io1 (\n"
            "    .i(d1),\n"
            "    .o(q1),\n"
            "    .oe(oe)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source1(Device, OE, Settings) ->
    #{
        title => {global, OE},
        device => Device,
        settings => [
            {location, oe, OE}, {global_clock, oe, true}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire oe,\n"
            "  input wire d0,\n"
            "  input wire d1,\n"
            "  output wire q0,\n"
            "  output wire q1\n"
            ");\n"
            "  alt_outbuf_tri io0 (\n"
            "    .i(d0),\n"
            "    .o(q0),\n"
            "    .oe(oe)\n"
            "  );\n"
            "  alt_outbuf_tri io1 (\n"
            "    .i(d1),\n"
            "    .o(q1),\n"
            "    .oe(oe)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source2(Device, OE0, OE1, Settings) ->
    #{
        title => {global, OE0, OE1},
        device => Device,
        settings => [
            {location, oe0, OE0}, {global_clock, oe0, true},
            {location, oe1, OE1}, {global_clock, oe1, true}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire oe0,\n"
            "  input wire oe1,\n"
            "  input wire d0,\n"
            "  input wire d1,\n"
            "  output wire q0,\n"
            "  output wire q1\n"
            ");\n"
            "  alt_outbuf_tri io0 (\n"
            "    .i(d0),\n"
            "    .o(q0),\n"
            "    .oe(oe0)\n"
            "  );\n"
            "  alt_outbuf_tri io1 (\n"
            "    .i(d1),\n"
            "    .o(q1),\n"
            "    .oe(oe1)\n"
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
        {internal, 0} => 1,
        {internal, 1} => 1,
        {internal, 2} => 1,
        {internal, 3} => 1
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
    Bits#{{disable, G} => 1, {internal, G} => 0};
pattern_route([_ | Route], Bits) ->
    pattern_route(Route, Bits).

%%--------------------------------------------------------------------

fuse({internal, N}, Pattern, Matrix, _) ->
    Fuse = {{global, N}, internal},
    expect:fuse(Matrix, Pattern, Fuse);
fuse({disable, N}, Pattern, Matrix, X) ->
    Row = {{global,N}, row, off},
    Col = {{global,N}, {column, X}, off},
    expect:fuse(Matrix, Pattern, Row, Col).

