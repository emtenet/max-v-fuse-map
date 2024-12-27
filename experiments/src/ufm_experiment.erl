-module(ufm_experiment).

-export([run/0]).

% This experiment looks at the UFM block (User Flash Memory)

%%====================================================================
%% run
%%====================================================================

run() ->
    %density(max_v_240z),
    %density(max_v_570z),
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    io:format(" ==> ~s~n", [Device]),
    Pins0 = device:pins(Device),
    {Settings0, Pins1} = pin_settings(Pins0),
    {Settings1, Pins2} = pin_settings(Pins1),
    {Settings2, Pins3} = pin_settings(Pins2),
    {Settings3, _} = pin_settings(Pins3),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source(Device, []),
        source(Device, []),
        source(Device, Settings0),
        source(Device, Settings1),
        source(Device, Settings2),
        source(Device, Settings3)
    ]),
    _ = Experiments,
    %
    %Matrix0 = matrix:build(Device, Experiments),
    %Matrix = matrix:remove_fuses(Matrix0, fun
    %    ({{c4, _, _}, _, _}) -> true;
    %    ({{c4, _, _}, _, _, _}) -> true;
    %    ({{global, _}, _, _}) -> true;
    %    ({{iob, X, Y}, _, _, _}) when X =/= 9 orelse Y > 3 -> true;
    %    ({{iob, X, Y}, _, _}) when X =/= 9 orelse Y > 3 -> true;
    %    ({{ioc, 1, 4, 6}, _}) -> false;
    %    ({{ioc, 1, 4, 6}, _, _}) -> false;
    %    ({{ioc, X, Y, _}, _}) when X =/= 9 orelse Y > 3 -> true;
    %    ({{ioc, X, Y, _}, _, _}) when X =/= 9 orelse Y > 3 -> true;
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
    %display:routing(Experiments, Density),
    %
    ok.

%%--------------------------------------------------------------------

pin_settings([En, Osc, Clk, Shift, In, Out | Pins]) ->
    Settings = [
        {location, oscena, En},
        {location, osc, Osc},
        {location, drclk, Clk},
        {location, drshft, Shift},
        {location, drdin, In},
        {location, drdout, Out}
    ],
    {Settings, Pins}.

%%--------------------------------------------------------------------

source(Device, Settings) ->
    #{
        title => ufm,
        device => Device,
        settings => Settings,
        verilog => <<
            "module experiment (\n"
            "  input wire program,\n"
            "  input wire erase,\n"
            "  input wire oscena,\n"
            "  input wire arclk,\n"
            "  input wire arshft,\n"
            "  input wire ardin,\n"
            "  input wire drclk,\n"
            "  input wire drshft,\n"
            "  input wire drdin,\n"
            "  output wire busy,\n"
            "  output wire osc,\n"
            "  output wire drdout,\n"
            "  output wire bgpbusy\n"
            ");\n"
            "  maxv_ufm ufm (\n"
            "    .program(program),\n"
            "    .erase(erase),\n"
            "    .oscena(oscena),\n"
            "    .arclk(arclk),\n"
            "    .arshft(arshift),\n"
            "    .ardin(ardin),\n"
            "    .drclk(drclk),\n"
            "    .drshft(drshft),\n"
            "    .drdin(drdin),\n"
            "    .busy(busy),\n"
            "    .osc(osc),\n"
            "    .drdout(drdout),\n"
            "    .bgpbusy(bgpbusy)\n"
            "  );\n"
            "  defparam\n"
            "    ufm.address_width = 9,\n"
            "    ufm.lpm_type = \"maxv_ufm\";\n"
            "endmodule\n"
        >>
    }.

