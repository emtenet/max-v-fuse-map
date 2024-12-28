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
    [Clk | _] = device:gclk_pins(Device),
    Pins0 = lists:delete(Clk, device:pins(Device)),
    {Settings0, Pins1} = pin_settings(Pins0),
    {Settings1, Pins2} = pin_settings(Pins1),
    {Settings2, Pins3} = pin_settings(Pins2),
    {Settings3, _} = pin_settings(Pins3),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source(Device, plain, []),
        source(Device, {pins, 0}, Settings0),
        source(Device, {pins, 1}, Settings1),
        source(Device, {pins, 2}, Settings2),
        source(Device, {pins, 3}, Settings3),
        source(Device, Clk)
    ]),
    _ = Experiments,
    %
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{c4, _, _}, _, _}) -> true;
        ({{c4, _, _}, _, _, _}) -> true;
        ({{global, _}, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{iob, _, _}, _, _}) -> true;
        ({{ioc, 1, 1, N}, _}) when N > 3 -> false;
        ({{ioc, 1, 1, N}, _, _}) when N > 3 -> false;
        ({{ioc, 1, 2, N}, _}) when N > 3 -> false;
        ({{ioc, 1, 2, N}, _, _}) when N > 3 -> false;
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{lab, _, _}, _}) -> true;
        ({{lab, _, _}, _, _}) -> true;
        ({{lab, _, _}, _, _, _}) -> true;
        ({{lc, _, _, _}, _}) -> true;
        ({{lc, _, _, _}, _, _}) -> true;
        ({{r4, _, _}, _, _, _}) -> true;
        ({{r4, _, _}, _, _}) -> true;
        (_) -> false
    end),
    matrix:print(Matrix),
    display:routing(Experiments, Density),
    %
    ok.

%%--------------------------------------------------------------------

pin_settings([A, B, C, D, E, F, G, H, I, J, K, L, M | Pins]) ->
    Settings = [
        {location, program, A},
        {location, erase, B},
        {location, osc_ena, C},
        {location, ar_clk, D},
        {location, ar_shift, E},
        {location, ar_in, F},
        {location, dr_clk, G},
        {location, dr_shift, H},
        {location, dr_in, I},
        {location, busy, J},
        {location, osc, K},
        {location, dr_out, L},
        {location, isp_busy, M}
    ],
    {Settings, Pins}.

%%--------------------------------------------------------------------

source(Device, Title, Settings) ->
    #{
        title => Title,
        device => Device,
        settings => Settings,
        verilog => <<
            "module experiment (\n"
            "  input wire program,\n"
            "  input wire erase,\n"
            "  input wire osc_ena,\n"
            "  input wire ar_clk,\n"
            "  input wire ar_shift,\n"
            "  input wire ar_in,\n"
            "  input wire dr_clk,\n"
            "  input wire dr_shift,\n"
            "  input wire dr_in,\n"
            "  output wire busy,\n"
            "  output wire osc,\n"
            "  output wire dr_out,\n"
            "  output wire isp_busy\n"
            ");\n"
            "  maxv_ufm ufm (\n"
            "    .program(program),\n"
            "    .erase(erase),\n"
            "    .oscena(osc_ena),\n"
            "    .arclk(ar_clk),\n"
            "    .arshft(ar_shift),\n"
            "    .ardin(ar_din),\n"
            "    .drclk(dr_clk),\n"
            "    .drshft(dr_shift),\n"
            "    .drdin(dr_in),\n"
            "    .busy(busy),\n"
            "    .osc(osc),\n"
            "    .drdout(dr_out),\n"
            "    .bgpbusy(isp_busy)\n"
            "  );\n"
            "  defparam\n"
            "    ufm.address_width = 9,\n"
            "    ufm.lpm_type = \"maxv_ufm\";\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source(Device, Clk) ->
    #{
        title => via,
        device => Device,
        settings => [
            {location, clk, Clk}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire program,\n"
            "  input wire erase,\n"
            "  input wire osc_ena,\n"
            "  input wire ar_clk,\n"
            "  input wire ar_shift,\n"
            "  input wire ar_in,\n"
            "  input wire dr_clk,\n"
            "  input wire dr_shift,\n"
            "  input wire dr_in,\n"
            "  output wire busy,\n"
            "  output wire osc,\n"
            "  output wire dr_out,\n"
            "  output wire isp_busy\n"
            ");\n"
            "  wire program_wire;\n"
            "  wire erase_wire;\n"
            "  wire osc_ena_wire;\n"
            "  wire ar_clk_wire;\n"
            "  wire ar_shift_wire;\n"
            "  wire ar_in_wire;\n"
            "  wire dr_clk_wire;\n"
            "  wire dr_shift_wire;\n"
            "  wire dr_in_wire;\n"
            "  dff program_via (\n"
            "    .d(program),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(program_wire)\n"
            "  );\n"
            "  dff erase_via (\n"
            "    .d(erase),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(erase_wire)\n"
            "  );\n"
            "  dff osc_ena_via (\n"
            "    .d(osc_ena),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(osc_ena_wire)\n"
            "  );\n"
            "  dff ar_clk_via (\n"
            "    .d(ar_clk),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(ar_clk_wire)\n"
            "  );\n"
            "  dff ar_shift_via (\n"
            "    .d(ar_shift),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(ar_shift_wire)\n"
            "  );\n"
            "  dff ar_in_via (\n"
            "    .d(ar_in),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(ar_in_wire)\n"
            "  );\n"
            "  dff dr_clk_via (\n"
            "    .d(dr_clk),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(dr_clk_wire)\n"
            "  );\n"
            "  dff dr_shift_via (\n"
            "    .d(dr_shift),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(dr_shift_wire)\n"
            "  );\n"
            "  dff dr_in_via (\n"
            "    .d(dr_in),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(dr_in_wire)\n"
            "  );\n"
            "  maxv_ufm ufm (\n"
            "    .program(program_wire),\n"
            "    .erase(erase_wire),\n"
            "    .oscena(osc_ena_wire),\n"
            "    .arclk(ar_clk_wire),\n"
            "    .arshft(ar_shift_wire),\n"
            "    .ardin(ar_din_wire),\n"
            "    .drclk(dr_clk_wire),\n"
            "    .drshft(dr_shift_wire),\n"
            "    .drdin(dr_in_wire),\n"
            "    .busy(busy_wire),\n"
            "    .osc(osc_wire),\n"
            "    .drdout(dr_out_wire),\n"
            "    .bgpbusy(isp_busy_wire)\n"
            "  );\n"
            "  defparam\n"
            "    ufm.address_width = 9,\n"
            "    ufm.lpm_type = \"maxv_ufm\";\n"
            "  wire busy_wire;\n"
            "  wire osc_wire;\n"
            "  wire dr_out_wire;\n"
            "  wire isp_busy_wire;\n"
            "  dff busy_via (\n"
            "    .d(busy_wire),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(busy)\n"
            "  );\n"
            "  dff osc_via (\n"
            "    .d(osc_wire),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(osc)\n"
            "  );\n"
            "  dff dr_out_via (\n"
            "    .d(dr_out_wire),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(dr_out)\n"
            "  );\n"
            "  dff isp_busy_via (\n"
            "    .d(isp_busy_wire),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(isp_busy)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

