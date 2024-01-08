-module(lab_clk1_experiment).

-export([run/0]).

-export([control_routing/1]).

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
    device(Density, Device).

%%--------------------------------------------------------------------

device(Density, Device) ->
    Gclks = device:gclk_pins(Device),
    Pins = lists:subtract(device:pins(Device), Gclks),
    [
        block(Density, Device, LAB, Gclks, Pins)
        ||
        LAB <- device:labs(Device)
    ],
    ok.

%%--------------------------------------------------------------------

block(Density, Device, LAB, Gclks, Pins) ->
    io:format(" ==> ~p ~p~n", [Density, LAB]),
    [Gclk0, Gclk1, Gclk2, Gclk3] = Gclks,
    [A, B, Q | _] = Pins,
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source_global(Device, LAB, A, B, Q, gclk0, Gclk0, <<"!">>),
        source_global(Device, LAB, A, B, Q, gclk0, Gclk0, <<>>),
        source_global(Device, LAB, A, B, Q, gclk1, Gclk1, <<>>),
        source_global(Device, LAB, A, B, Q, gclk2, Gclk2, <<>>),
        source_global(Device, LAB, A, B, Q, gclk3, Gclk3, <<>>),
        source_local(Device, LAB, A, B, Q, local7, Gclk0, 7),
        source_local(Device, LAB, A, B, Q, local8, Gclk0, 8)
    ]),
    Matrix0 = matrix:build(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{iob, _, _}, _}) -> true;
        ({{iob, _, _}, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{ioc, _, _, _}, _, _, _}) -> true;
        %({{c4, _, _}, _, _}) -> true;
        %({{c4, _, _}, _, _, _}) -> true;
        %({{r4, _, _}, _, _}) -> true;
        %({{r4, _, _}, _, _, _}) -> true;
        %({{lab, _, _}, {control, _}, _, _}) -> true;
        %({{lab, _, _}, {interconnect, _}, _}) -> true;
        %({{lab, _, _}, {interconnect, _}, _, _}) -> true;
        %({{lc, _, _, _}, local_line}) -> true;
        ({_, lut, _}) -> true;
        %({_, data_b3, _}) -> true;
        %({_, data_b6, _}) -> true;
        %({_, data_c3, _}) -> true;
        %({_, data_c6, _}) -> true;
        %({_, data_d3, _}) -> true;
        %({_, data_d6, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %control_routing(Experiments),
    %
    expect(Matrix, [0,1,1,1,1,1,1], {LAB, clk1, invert}),
    expect(Matrix, [0,0,1,1,1,1,1], {LAB, clk1, global0}),
    expect(Matrix, [1,1,0,1,1,1,1], {LAB, clk1, global1}),
    expect(Matrix, [1,1,1,0,1,1,1], {LAB, clk1, global2}),
    expect(Matrix, [1,1,1,1,0,1,1], {LAB, clk1, global3}),
    %
    lists:foreach(fun not_s_data/1, Experiments),
    %
    %Control = {LAB, clk1, control},
    %Control0 = {LAB, clk1, control_0_not_1},
    %expect(Matrix, [1,1,1,1,1,0,0], Control),
    %expect(Matrix, [1,1,1,1,1,0,1], Control0),
    ok.

%%--------------------------------------------------------------------

expect(Matrix, Pattern, Fuse) ->
    [{_, Fuse}] = matrix:pattern_is(Matrix, Pattern),
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

control_routing(Experiments) ->
    lists:foreach(fun control_routing_experiment/1, Experiments).

%%--------------------------------------------------------------------

control_routing_experiment({Name, _, #{signals := Signals}}) ->
    io:format(" --> ~p~n", [Name]),
    Routing = maps:fold(fun control_routing_signal/3, [], Signals),
    [
        io:format("  ~12w ~6w <- ~p~n", [LC, Port, Route])
        ||
        {LC, Port, Route} <- lists:sort(Routing)
    ].

%%--------------------------------------------------------------------

control_routing_signal(_, #{dests := Dests}, Routing) ->
    lists:foldl(fun control_routing_dest/2, Routing, Dests).

%%--------------------------------------------------------------------

control_routing_dest(#{lc := LC, port := Port, route := Route}, Routing) ->
    case Route of
        [{lab_clk, _, _, 0, N} | _] ->
            [{LC, Port, {global, N}} | Routing];

        [{lab_control_mux, _, _, 0, N}, From | _] ->
            [{LC, Port, {control, N, From}} | Routing];

        [From | _] when Port =:= s_data ->
            [{LC, Port, From} | Routing];

        _ ->
            Routing
    end;
control_routing_dest(_, Routing) ->
    Routing.

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

