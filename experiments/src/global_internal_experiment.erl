-module(global_internal_experiment).

-export([run/0]).

% This experiment aims to populate the route_cache with enough
% information to understand the Global MUX for internal sources
% not just the dedicated sources.
%
% To force sources to a specifig global line, the other three global
% lines will be source from their dedicated source.

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
    Pins = lists:subtract(device:pins(Device), Gclks),
    [LAB | _] = device:labs(Device),
    experiment(Device, LAB, Pins, Gclks).

experiment(Device, LAB, [A, B, C | Pins], Gclks = [G0, G1, G2, G3]) ->
    io:format(" ==> ~p ~s ~s ~s~n", [Device, A, B, C]),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source(Device, LAB, G0, G1, G2, gclk3, A, B, C),
        source(Device, LAB, G0, G1, G2, gclk3, B, C, A),
        source(Device, LAB, G0, G1, G2, gclk3, C, A, B),
        source(Device, LAB, G0, G1, G3, gclk2, A, B, C),
        source(Device, LAB, G0, G1, G3, gclk2, B, C, A),
        source(Device, LAB, G0, G1, G3, gclk2, C, A, B),
        source(Device, LAB, G0, G2, G3, gclk1, A, B, C),
        source(Device, LAB, G0, G2, G3, gclk1, B, C, A),
        source(Device, LAB, G0, G2, G3, gclk1, C, A, B),
        source(Device, LAB, G1, G2, G3, gclk0, A, B, C),
        source(Device, LAB, G1, G2, G3, gclk0, B, C, A),
        source(Device, LAB, G1, G2, G3, gclk0, C, A, B)
    ]),
    _ = Experiments,
    %
    %Matrix0 = matrix:build(Device, Experiments),
    %Matrix = matrix:remove_fuses(Matrix0, fun
    %    ({{c4, _, _}, _, _}) -> true;
    %    ({{c4, _, _}, _, _, _}) -> true;
    %    ({{global, _}, _, _}) -> true;
    %    ({{iob, _, _}, _, _, _}) -> true;
    %    ({{iob, _, _}, _, _}) -> true;
    %    ({{ioc, _, _, _}, _}) -> true;
    %    ({{ioc, _, _, _}, _, _}) -> true;
    %    ({{lab, _, _}, _}) -> true;
    %    ({{lab, _, _}, _, _}) -> true;
    %    ({{lab, _, _}, _, _, _}) -> true;
    %    ({{lc, _, _, _}, _}) -> true;
    %    ({{lc, _, _, _}, _, _}) -> true;
    %    ({{r4, _, _}, _, _, _}) -> true;
    %    ({{r4, _, _}, _, _}) -> true;
    %    (_) -> false
    %end),
    %matrix:print(Matrix),
    %display:routing(Experiments, device:density(Device)),
    %
    experiment(Device, LAB, Pins, Gclks);
experiment(_, _, _, _) ->
    ok.

%%--------------------------------------------------------------------

source(Device, LAB, Clk1, Clr1, Clk2, Name, Clr2, D, Q) ->
    #{
        title => {Name, Clr2},
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, clr1, Clr1},
            {location, clk2, Clk2},
            {location, clr2, Clr2},
            {global_clock, clk1, true},
            {global_clock, clk2, true},
            {global_clock, clr1, true},
            {global_clock, clr2, true},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff2, lab:lc(LAB, 1)},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire clr1,\n"
            "  input wire clk2,\n"
            "  input wire clr2,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire chain;\n"
            "  dffe ff1 (\n"
            "    .d(d),\n"
            "    .clk(clk1),\n"
            "    .clrn(clr1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .q(chain)\n"
            "  );\n"
            "  dffe ff2 (\n"
            "    .d(chain),\n"
            "    .clk(clk2),\n"
            "    .clrn(clr2),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

