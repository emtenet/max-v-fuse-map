-module(lab_ena2_experiment).

-export([run/0]).

% This experiment was designed to look at how the LAB's ena2
% source was selected between the two (2) LAB control lines.
%
% The two control lines are specifically selected by:
%
% * routing the global clocks 2 and 3 into local interconnects.
%   Global 2 can only be selected into interconnect 12 and then control 3.
%   Global 3 can only be selected into interconnect 25 and then control 2.
%
% * routing the clock through LC's 7 and 8. LC 7 can only be selected
%   into even control lines, and LC 8 into odd control lines.
%
% The enable line is off by default:
%
%  * {{lab, X, Y}, ena2, off}
%
% The enable line is selected between {control, 2} and {control, 3}:
%
%  * {{lab, X, Y}, ena2_s_load, control_2_not_3}
%
% The emable line can be inverted with:
%
%  * {{lab, X, Y}, ena2_s_load, invert}
%
% NOTE: Some of the fuses are shared with the s-load line.

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
        {batch, 4}
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, Gclks, Pins) ->
    [Gclk1, Gclk2, Ena2, Ena3] = Gclks,
    {Clk1, Ena1, Clk2, D, Q} = Pins,
    Common = {LAB, Clk1, Ena1, Clk2, D, Q},
    [
        source_always(Device, Common, always),
        source_global(Device, Common, gclk2_not, Ena2, <<"!">>, <<>>),
        source_global(Device, Common, gclk2_not, Ena2, <<>>, <<"!">>),
        source_global(Device, Common, gclk2, Ena2, <<>>, <<>>),
        source_global(Device, Common, gclk3, Ena3, <<>>, <<>>),
        source_local(Device, Common, local_even, Gclk1, 6, Gclk2, 7),
        source_local(Device, Common, local_odd, Gclk1, 8, Gclk2, 9)
    ].

%%--------------------------------------------------------------------

experiments(Device, LAB, Experiments) ->
    io:format(" ==> ~p ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{ioc, _, _, _}, _}) -> true;
        ({_, lut, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, data_d6, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:port_routing(ena, Experiments),
    %
    expect:fuse(Matrix, [0,1,1,1,1,1,1], {LAB, ena2, off}),
    expect:fuse(Matrix, [1,0,1,1,1,1,1], {LAB, ena2_s_load, invert}),
    expect:fuse(Matrix, [1,1,0,1,1,1,1], {LAB, ena1, invert}),
    %
    Control = control_pattern(Experiments),
    expect:fuse(Matrix, Control, {LAB, ena2_s_load, control_0_not_1}),
    ok.

%%--------------------------------------------------------------------

control_pattern(Experiments) ->
    lists:map(fun ({_, _, #{signals := Signals}}) ->
        maps:fold(fun control_pattern_bit/3, 1, Signals)
    end, Experiments).

%%--------------------------------------------------------------------

control_pattern_bit(_, #{dests := [#{port := ena, route := Route}]}, Bit) ->
    case Route of
        [{lab_control_mux, _, _, 0, 0} | _] ->
            0;

        [{lab_control_mux, _, _, 0, 1} | _] ->
            1;

        _ ->
            Bit
    end;
control_pattern_bit(_, _, Bit) ->
    Bit.

%%--------------------------------------------------------------------

source_always(Device, {LAB, Clk1, Ena1, Clk2, D, Q}, Name) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, clk2, Clk2},
            {location, ena1, Ena1},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff2, lab:lc(LAB, 1)},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire clk2,\n"
            "  input wire ena1,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire chain;\n"
            "  dffe ff1 (\n"
            "    .d(d),\n"
            "    .clk(clk1),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(ena1),\n"
            "    .q(chain)\n"
            "  );\n"
            "  dffe ff2 (\n"
            "    .d(chain),\n"
            "    .clk(clk2),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_global(Device, {LAB, Clk1, Ena1, Clk2, D, Q}, Name, Ena2, Not1, Not2) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, clk2, Clk2},
            {location, ena1, Ena1},
            {location, ena2, Ena2},
            {global_clock, ena2, true},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff2, lab:lc(LAB, 1)},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire clk2,\n"
            "  input wire ena1,\n"
            "  input wire ena2,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire chain;\n"
            "  dffe ff1 (\n"
            "    .d(d),\n"
            "    .clk(clk1),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(", Not1/binary, "ena1),\n"
            "    .q(chain)\n"
            "  );\n"
            "  dffe ff2 (\n"
            "    .d(chain),\n"
            "    .clk(clk2),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(", Not2/binary, "ena2),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_local(Device, {LAB, Clk1, _, Clk2, D, Q}, Name, Ena1, N1, Ena2, N2) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, clk2, Clk2},
            {location, ena1, Ena1},
            {location, ena2, Ena2},
            {global_clock, clk2, true},
            {global_clock, ena1, false},
            {global_clock, ena2, false},
            {location, ee1, lab:lc(LAB, N1)},
            {location, ee2, lab:lc(LAB, N2)},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff2, lab:lc(LAB, 1)},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire clk2,\n"
            "  input wire ena1,\n"
            "  input wire ena2,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire chain;\n"
            "  wire e1;\n"
            "  wire e2;\n"
            "  lcell ee1 (\n"
            "    .in(ena1),\n"
            "    .out(e1)\n"
            "  );\n"
            "  dffe ff1 (\n"
            "    .d(d),\n"
            "    .clk(clk1),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(e1),\n"
            "    .q(chain)\n"
            "  );\n"
            "  lcell ee2 (\n"
            "    .in(ena2),\n"
            "    .out(e2)\n"
            "  );\n"
            "  dffe ff2 (\n"
            "    .d(chain),\n"
            "    .clk(clk2),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(e2),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

