-module(lab_ena2_s_load_experiment).

-export([run/0]).

% If ena2 and s-load share their control selection and they share
% that selection pool with clk1 (control0 & control1).
%
% Can we run out of control lines.
%
% The relief maybe to allow clk1 to use a global line leaving the two
% control lines for ena2 and s-load.
%
% No, the only relief is to remove one or the other ena line.
%
% ERROR: LAB legality constraint that was not satisfied:
%        Too many distinct clock/clock enable pairs and synchronous load
%        signals must be brought into a LAB.
%        Resources used: 3.
%        Resources available: 2.

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
    [LAB | _] = device:labs(Device),
    Gclks = device:gclk_pins(Device),
    Pins = lists:subtract(device:pins(Device), Gclks),
    io:format(" ==> ~p ~p~n", [Device, LAB]),
    [Gclk1, Gclk2 | _] = Gclks,
    [Clk1, Ena1, Clk2, Ena2, Sload, D, Q | _] = Pins,
    Common = {LAB, Ena1, Ena2, Sload, D, Q},
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        % These four fail to compile
        %source(Device, Common, {Clk1, false}, {Clk2, false}, both, clk_clk),
        %source(Device, Common, {Gclk1, true}, {Clk2, false}, both, gclk_clk),
        %source(Device, Common, {Clk1, false}, {Gclk2, true}, both, clk_gclk),
        %source(Device, Common, {Gclk1, true}, {Gclk2, true}, both, gclk_gclk),
        %
        source(Device, Common, {Clk1, false}, {Clk2, false}, ena1, clk_clk),
        source(Device, Common, {Gclk1, true}, {Clk2, false}, ena1, gclk_clk),
        source(Device, Common, {Clk1, false}, {Gclk2, true}, ena1, clk_gclk),
        source(Device, Common, {Gclk1, true}, {Gclk2, true}, ena1, gclk_gclk),
        source(Device, Common, {Clk1, false}, {Clk2, false}, ena2, clk_clk),
        source(Device, Common, {Gclk1, true}, {Clk2, false}, ena2, gclk_clk),
        source(Device, Common, {Clk1, false}, {Gclk2, true}, ena2, clk_gclk),
        source(Device, Common, {Gclk1, true}, {Gclk2, true}, ena2, gclk_gclk)
    ]),
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
    matrix:print(Matrix),
    display:control_routing(Experiments),
    %
    ok.

%%--------------------------------------------------------------------

source(Device, {LAB, Ena1, Ena2, Sload, D, Q}, Clock1, Clock2, Ena, Name) ->
    {Clk1, Global1} = Clock1,
    {Clk2, Global2} = Clock2,
    {Enable1, Enable2} = case Ena of
        both -> {<<"ena1">>, <<"ena2">>};
        ena1 -> {<<"ena1">>, <<"1">>};
        ena2 -> {<<"">>, <<"ena2">>}
    end,
    #{
        title => {Ena, Name},
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {global_clock, clk1, Global1},
            {location, clk2, Clk2},
            {global_clock, clk2, Global2},
            {location, ena1, Ena1},
            {location, ena2, Ena2},
            {location, sload, Sload},
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
            "  input wire sload,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire chain;\n"
            "  dffeas ff1 (\n"
            "    .d(d),\n"
            "    .clk(clk1),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(", Enable1/binary, "),\n"
            "    .asdata(0),\n"
            "    .aload(0),\n"
            "    .sclr(0),\n"
            "    .sload(sload),\n"
            "    .q(chain)\n"
            "  );\n"
            "  dffeas ff2 (\n"
            "    .d(chain),\n"
            "    .clk(clk2),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(", Enable2/binary, "),\n"
            "    .asdata(1),\n"
            "    .aload(0),\n"
            "    .sclr(0),\n"
            "    .sload(sload),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

