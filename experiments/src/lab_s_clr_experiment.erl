-module(lab_s_clr_experiment).

-export([run/0]).

% This experiment was designed to look at how the LAB's s-clr
% source was selected between the two (2) LAB control lines.
%
% The two control lines are specifically selected by routing
% the clock through LC's 7 and 8. LC 7 can only be selected into
% even control lines, and LC 8 into odd control lines.
%
% It appears that the s-clr line can only be enabled for all LCs in
% a LAB, not one at a time.
%
% The LAB's s-clr line is enabled with:
%
%  * {{lab, X, Y}, s_clr}
%
% The s-clr line is selected between {control, 4} and {control, 5}:
%
%  * {{lab, X, Y}, s_clr, control_5_not_4}
%
% The s-clr line can be inverted with:
%
%  * {{lab, X, Y}, s_clr, invert}

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
    iterate:labs(Device, 2, Pins0,
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
    [Clk, Clr, _, _] = Gclks,
    {D, Q} = Pins,
    Common = {LAB, Clk, D, Q, Clr},
    [
        source_never(Device, Common, never),
        source_always(Device, Common, always),
        source_global(Device, Common, global, <<"!">>),
        source_global(Device, Common, global_not, <<>>),
        source_local(Device, Common, local7, 7),
        source_local(Device, Common, local8, 8)
    ].

%%--------------------------------------------------------------------

experiments(Device, LAB, Experiments) ->
    io:format(" ==> ~p ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({_, lut, _}) -> true;
        ({_, data_b3, _}) -> true;
        ({_, data_b6, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, data_d6, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:control_routing(Experiments),
    %
    % "always" is compiled to an LCELL outputting a constant 1
    {always, _, #{signals := #{<<"~VCC">> := VCC}}}
      = lists:keyfind(always, 1, Experiments),
    #{dests := [#{port := s_clr}]} = VCC,
    %
    expect:fuse(Matrix, [1,0,0,0,0,0], {LAB, s_clr, control}),
    expect:fuse(Matrix, [1,1,0,1,1,1], {LAB, s_clr, invert}),
    LC = lab:lc(LAB, 0),
    expect:fuse(Matrix, [1,0,0,0,0,0], {LC, s_clr_load}),
    %
    [_, _, _, _, Local7, Local8] = Experiments,
    expect:control(Local7, cc, 4, s_clr),
    expect:control(Local8, cc, 5, s_clr),
    %
    Control = control_pattern(Experiments),
    expect:fuse(Matrix, Control, {LAB, s_clr, control_5_not_4}),
    ok.

%%--------------------------------------------------------------------

control_pattern(Experiments) ->
    lists:map(fun ({_, _, #{signals := Signals}}) ->
        maps:fold(fun control_pattern_bit/3, 1, Signals)
    end, Experiments).

%%--------------------------------------------------------------------

control_pattern_bit(_, #{dests := [#{port := s_clr, route := Route}]}, _) ->
    case Route of
        [{lab_control_mux, _, _, 0, 4} | _] ->
            1;

        [{lab_control_mux, _, _, 0, 5} | _] ->
            0
    end;
control_pattern_bit(_, _, Bit) ->
    Bit.

%%--------------------------------------------------------------------

source_never(Device, {LAB, Clk, D, Q, Clr}, Name) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clr, Clr},
            {global_clock, clr, false},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {global_clock, clk, true},
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
            "  dffeas ff (\n"
            "    .d(d && clr),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .asdata(1),\n"
            "    .aload(0),\n"
            "    .sclr(0),\n"
            "    .sload(0),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_always(Device, {LAB, Clk, D, Q, Clr}, Name) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clr, Clr},
            {global_clock, clr, false},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {global_clock, clk, true},
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
            "  dffeas ff (\n"
            "    .d(d && clr),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .asdata(1),\n"
            "    .aload(0),\n"
            "    .sclr(1),\n"
            "    .sload(0),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_global(Device, {LAB, Clk, D, Q, Clr}, Name, Not) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clr, Clr},
            {global_clock, clr, true},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {global_clock, clk, true},
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
            "  dffeas ff (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .asdata(1),\n"
            "    .aload(0),\n"
            "    .sclr(", Not/binary, "clr),\n"
            "    .sload(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_local(Device, {LAB, Clk, D, Q, Clr}, Name, N) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clr, Clr},
            {global_clock, clr, false},
            {location, cc, lab:lc(LAB, N)},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {global_clock, clk, true},
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
            "  dffeas ff (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .asdata(1),\n"
            "    .aload(0),\n"
            "    .sclr(c),\n"
            "    .sload(0),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

