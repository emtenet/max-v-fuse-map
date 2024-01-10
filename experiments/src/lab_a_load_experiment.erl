-module(lab_a_load_experiment).

-export([run/0]).

% This experiment was designed to look at how the LAB's a-load
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
%  * {{lab, X, Y}, a_load, off}
%
% The enable line is selected between {control, 2} and {control, 3}:
%
%  * {{lab, X, Y}, clk2_a_load, control_2_not_3}
%
% The emable line can be inverted with:
%
%  * {{lab, X, Y}, a_load, invert}
%
% NOTE: Some of the fuses are shared with the clk2 line.

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
    [ALoad, _, ALoad2, ALoad3] = Gclks,
    {Clk, AData, D, Q}= Pins,
    Common = {LAB, Clk, D, Q, AData},
    [
        source_never(Device, Common, never),
        source_always(Device, Common, always),
        source_global(Device, Common, gclk2_not, ALoad2, <<"!">>),
        source_global(Device, Common, gclk2, ALoad2, <<>>),
        source_global(Device, Common, gclk3, ALoad3, <<>>),
        source_local(Device, Common, local7, ALoad, 7),
        source_local(Device, Common, local8, ALoad, 8)
    ].

%%--------------------------------------------------------------------

experiments(Device, LAB, Experiments) ->
    io:format(" ==> ~p ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({_, lut, _}) -> true;
        ({_, data_a3, _}) -> true;
        ({_, data_a6, _}) -> true;
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
    expect:fuse(Matrix, [1,1,0,0,0,0,0], {LAB, a_load, control}),
    expect:fuse(Matrix, [1,1,0,1,1,1,1], {LAB, a_load, invert}),
    %
    Control = control_pattern(Experiments),
    expect:fuse(Matrix, Control, {LAB, clk2_a_load, control_3_not_2}),
    ok.

%%--------------------------------------------------------------------

control_pattern(Experiments) ->
    lists:map(fun ({_, _, #{signals := Signals}}) ->
        control_pattern_bit(Signals)
    end, Experiments).

%%--------------------------------------------------------------------

control_pattern_bit(#{aa := #{dests := [#{port := a_load, route := Route}]}}) ->
    case  Route of
        [{lab_control_mux, _, _, 0, 3} | _] ->
            0;

        [{lab_control_mux, _, _, 0, 2} | _] ->
            1
    end;
control_pattern_bit(#{aload := #{dests := [#{port := a_load, route := Route}]}}) ->
    case  Route of
        [{lab_control_mux, _, _, 0, 3} | _] ->
            0;

        [{lab_control_mux, _, _, 0, 2} | _] ->
            1
    end;
control_pattern_bit(_) ->
    1.

%%--------------------------------------------------------------------

source_never(Device, {LAB, Clk, D, Q, AData}, Name) ->
    % Ensure that s-load is not automatically compiled with:
    %   d => d AND adata
    #{
        title => Name,
        device => Device,
        settings => [
            {location, adata, AData},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire adata,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  dffe ff (\n"
            "    .d(d && adata),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_always(Device, {LAB, Clk, D, Q, AData}, Name) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, adata, AData},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire adata,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  dffea ff (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .adata(adata),\n"
            "    .aload(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_global(Device, {LAB, Clk, D, Q, AData}, Name, ALoad, Not) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, adata, AData},
            {location, aload, ALoad},
            {global_clock, aload, true},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire aload,\n"
            "  input wire adata,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  dffea ff (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .adata(adata),\n"
            "    .aload(", Not/binary, "aload),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_local(Device, {LAB, Clk, D, Q, AData}, Name, ALoad, N) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, adata, AData},
            {location, aload, ALoad},
            {global_clock, aload, false},
            {location, aa, lab:lc(LAB, N)},
            {location, ff, lab:lc(LAB, 0)},
            {location, clk, Clk},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire aload,\n"
            "  input wire adata,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire a;\n"
            "  lcell aa (\n"
            "    .in(aload),\n"
            "    .out(a)\n"
            "  );\n"
            "  dffea ff (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .adata(adata),\n"
            "    .aload(a),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

