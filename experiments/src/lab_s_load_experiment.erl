-module(lab_s_load_experiment).

-export([run/0]).

% This experiment was designed to look at how the LAB's s-load
% source was selected between the two (2) LAB control lines.
%
% The two control lines are specifically selected by:
%
% * routing the global clocks 2 and 3 into local interconnects.
%   Global 2 can only be selected into interconnect 12 and then control 1.
%   Global 3 can only be selected into interconnect 25 and then control 0.
%
% * routing the clock through LC's 7 and 8. LC 7 can only be selected
%   into even control lines, and LC 8 into odd control lines.
%
% The s-load line is sourced from a control line when:
%
%  * {{lab, X, Y}, s_load, control}
%
% The s-load line is selected between {control, 0} and {control, 1}:
%
%  * {{lab, X, Y}, s_load, control_0_not_1}
%
% The s-load line can be inverted with:
%
%  * {{lab, X, Y}, s_load, invert}
%
% The LC s-load is enabled with:
%
%  * {{lc, X, Y, N}, s_load}

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
    iterate:labs(Device, 4, Pins0,
        fun (LAB, Pins) ->
            sources(Device, LAB, Gclks, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Device, LAB, Experiments)
        end
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, Gclks, Pins) ->
    [SLoad, _, SLoad2, SLoad3] = Gclks,
    {Clk, SData, D, Q} = Pins,
    [
        source_never(Device, LAB, Clk, D, Q, never, SData),
        source_always(Device, LAB, Clk, D, Q, always, SData),
        source_global(Device, LAB, Clk, D, Q, gclk2, SData, SLoad2, <<"!">>),
        source_global(Device, LAB, Clk, D, Q, gclk2, SData, SLoad2, <<>>),
        source_global(Device, LAB, Clk, D, Q, gclk3, SData, SLoad3, <<>>),
        source_local(Device, LAB, Clk, D, Q, local7, SData, SLoad, 7),
        source_local(Device, LAB, Clk, D, Q, local8, SData, SLoad, 8)
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
        ({_, lut, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:control_routing(Experiments),
    %
    expect:fuse(Matrix, [1,1,0,0,0,0,0], {LAB, s_load, control}),
    expect:fuse(Matrix, [0,1,0,0,0,0,0], {LAB, s_load, unknown}),
    expect:fuse(Matrix, [1,1,0,1,1,1,1], {LAB, s_load, invert}), % ena2_s_load
    expect:fuse(Matrix, [1,0,0,0,0,0,0], {lab:lc(LAB, 0), s_load}),
    %
    Control = control_pattern(Experiments),
    expect:fuse(Matrix, Control, {LAB, s_load, control_0_not_1}), % ena2_s_load
    ok.

%%--------------------------------------------------------------------

control_pattern(Experiments) ->
    lists:map(fun ({_, _, #{signals := Signals}}) ->
        control_pattern_bit(Signals)
    end, Experiments).

%%--------------------------------------------------------------------

control_pattern_bit(#{ss := #{dests := [#{port := s_load, route := Route}]}}) ->
    case  Route of
        [{lab_control_mux, _, _, 0, 1} | _] ->
            1;

        [{lab_control_mux, _, _, 0, 0} | _] ->
            0
    end;
control_pattern_bit(#{sload := #{dests := [#{port := s_load, route := Route}]}}) ->
    case  Route of
        [{lab_control_mux, _, _, 0, 1} | _] ->
            1;

        [{lab_control_mux, _, _, 0, 0} | _] ->
            0
    end;
control_pattern_bit(_) ->
    1.

%%--------------------------------------------------------------------

source_never(Device, LAB, Clk, D, Q, Name, SData) ->
    % Ensure that s-load is not automatically compiled with:
    %   d => d AND sdata
    #{
        title => Name,
        device => Device,
        settings => [
            {location, sdata, SData},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire sdata,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  dff ff (\n"
            "    .d(d && sdata),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_always(Device, LAB, Clk, D, Q, Name, SData) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, sdata, SData},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire sdata,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  dffeas ff (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .asdata(sdata),\n"
            "    .aload(0),\n"
            "    .sclr(0),\n"
            "    .sload(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_global(Device, LAB, Clk, D, Q, Name, SData, SLoad, Not) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, sdata, SData},
            {location, sload, SLoad},
            {global_clock, sload, true},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire sload,\n"
            "  input wire sdata,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  dffeas ff (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .asdata(sdata),\n"
            "    .aload(0),\n"
            "    .sclr(0),\n"
            "    .sload(", Not/binary, "sload),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_local(Device, LAB, Clk, D, Q, Name, SData, SLoad, N) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, sdata, SData},
            {location, sload, SLoad},
            {global_clock, sload, false},
            {location, ss, lab:lc(LAB, N)},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire sload,\n"
            "  input wire sdata,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire s;\n"
            "  lcell ss (\n"
            "    .in(sload),\n"
            "    .out(s)\n"
            "  );\n"
            "  dffeas ff (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .asdata(sdata),\n"
            "    .aload(0),\n"
            "    .sclr(0),\n"
            "    .sload(s),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

