-module(lab_clk1_experiment).

-export([run/0]).

% This experiment was designed to look at how the LAB's clk1
% source was selected between the four (4) global networks
% or the two (2) LAB control lines.
%
% The two control lines are specifically selected by routing
% the clock through LC's 7 and 8. LC 7 can only be selected into
% even control lines, and LC 8 into odd control lines.
%
% The first layer is a one-hot selection of:
%
%  * {{lab, X, Y}, clk1, global0}
%  * {{lab, X, Y}, clk1, global1}
%  * {{lab, X, Y}, clk1, global2}
%  * {{lab, X, Y}, clk1, global3}
%  * {{lab, X, Y}, clk1, control}
%
% Then in control is selected above, a further fuse:
%
%  * {{lab, X, Y}, clk1, control_0_not_1}
%
% selects between {control, 0} and {control, 1}.
%
% The clock line can be inverted with:
%
%  * {{lab, X, Y}, clk1, invert}
%
% Send (a || b) into LUT to avoid it being converted into an S-LOAD.

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
    {A, B, Q} = Pins,
    [
        source_global(Device, LAB, A, B, Q, gclk0, Gclk0, <<"!">>),
        source_global(Device, LAB, A, B, Q, gclk0, Gclk0, <<>>),
        source_global(Device, LAB, A, B, Q, gclk1, Gclk1, <<>>),
        source_global(Device, LAB, A, B, Q, gclk2, Gclk2, <<>>),
        source_global(Device, LAB, A, B, Q, gclk3, Gclk3, <<>>),
        source_local(Device, LAB, A, B, Q, local7, Gclk0, 7),
        source_local(Device, LAB, A, B, Q, local8, Gclk0, 8)
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
        ({_, lut, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:control_routing(Experiments),
    %
    expect:fuse(Matrix, [0,1,1,1,1,1,1], {LAB, clk1, invert}),
    expect:fuse(Matrix, [0,0,1,1,1,1,1], {LAB, clk1, global0}),
    expect:fuse(Matrix, [1,1,0,1,1,1,1], {LAB, clk1, global1}),
    expect:fuse(Matrix, [1,1,1,0,1,1,1], {LAB, clk1, global2}),
    expect:fuse(Matrix, [1,1,1,1,0,1,1], {LAB, clk1, global3}),
    %
    lists:foreach(fun not_s_data/1, Experiments),
    %
    expect:fuse(Matrix, [1,1,1,1,1,0,0], {LAB, clk1, control}),
    expect:fuse(Matrix, [1,1,1,1,1,0,1], {LAB, clk1, control_0_not_1}),
    ok.

%%--------------------------------------------------------------------

not_s_data({_, _, #{signals := Signals}}) ->
    #{a := #{dests := [#{port := data_a}]}} = Signals,
    #{b := #{dests := [#{port := data_b}]}} = Signals,
    #{clk := #{dests := [#{port := Port}]}} = Signals,
    case Port of
        clk ->
            ok;

        data_a ->
            ok
    end.

%%--------------------------------------------------------------------

source_global(Device, LAB, A, B, Q, Name, Clk, Not) ->
    #{
        title => {LAB, Name},
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, ff, lab:lc(LAB, 0)},
            {location, a, A},
            {location, b, B},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  output wire q\n"
            ");\n"
            "  dff ff (\n"
            "    .d(a || b),\n"
            "    .clk(", Not/binary, "clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_local(Device, LAB, A, B, Q, Name, Clk, N) ->
    #{
        title => {LAB, Name},
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, false},
            {location, cc, lab:lc(LAB, N)},
            {location, ff, lab:lc(LAB, 0)},
            {location, a, A},
            {location, b, B},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  output wire q\n"
            ");\n"
            "  wire c;\n"
            "  lcell cc (\n"
            "    .in(clk),\n"
            "    .out(c)\n"
            "  );\n"
            "  dff ff (\n"
            "    .d(a || b),\n"
            "    .clk(c),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

