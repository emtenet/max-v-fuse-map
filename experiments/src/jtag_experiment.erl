-module(jtag_experiment).

-export([run/0]).

% This experiment looks at the internal JTAG port

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
        source(Device, 0, <<>>, []),
        source(Device, 1, <<>>, []),
        source(Device, 0, <<>>, Settings0),
        source(Device, 1, <<>>, Settings1),
        source(Device, 2, <<>>, Settings2),
        source(Device, 3, <<>>, Settings3),
        source(Device, 3, <<"!">>, Settings3)
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
    %    ({{ioc, 1, _, N}, _}) when N > 3 -> false;
    %    ({{ioc, 1, _, N}, _, _}) when N > 3 -> false;
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
    %display:routing(Experiments, Density),
    %
    ok.

%%--------------------------------------------------------------------

pin_settings([Tck, Tdi, Tdo, Tms | Pins]) ->
    Settings = [
        {location, tck, Tck},
        {location, tdi, Tdi},
        {location, tdo, Tdo},
        {location, tms, Tms}
    ],
    {Settings, Pins}.

%%--------------------------------------------------------------------

source(Device, Index, Not, Settings) ->
    IndexBin = integer_to_binary(Index),
    #{
        title => {sld_virtual_jtag, Index},
        device => Device,
        settings => Settings,
        verilog => <<
            "module experiment (\n"
            "  output wire tck,\n"
            "  output wire tdi,\n"
            "  input wire tdo,\n"
            "  output wire tms,\n"
            "  input wire [3:0] ir_out,\n"
            "  output wire [3:0] ir_in,\n"
            "  output wire virtual_state_cdr,\n"
            "  output wire virtual_state_sdr,\n"
            "  output wire virtual_state_e1dr,\n"
            "  output wire virtual_state_pdr,\n"
            "  output wire virtual_state_e2dr,\n"
            "  output wire virtual_state_udr,\n"
            "  output wire virtual_state_cir,\n"
            "  output wire virtual_state_uir,\n"
            "  output wire jtag_state_tlr,\n"
            "  output wire jtag_state_rti,\n"
            "  output wire jtag_state_sdrs,\n"
            "  output wire jtag_state_cdr,\n"
            "  output wire jtag_state_sdr,\n"
            "  output wire jtag_state_e1dr,\n"
            "  output wire jtag_state_pdr,\n"
            "  output wire jtag_state_e2dr,\n"
            "  output wire jtag_state_udr,\n"
            "  output wire jtag_state_sirs,\n"
            "  output wire jtag_state_cir,\n"
            "  output wire jtag_state_sir,\n"
            "  output wire jtag_state_e1ir,\n"
            "  output wire jtag_state_pir,\n"
            "  output wire jtag_state_e2ir,\n"
            "  output wire jtag_state_uir\n"
            ");\n"
            "  sld_virtual_jtag mega (\n"
            "    .tck(tck),\n"
            "    .tdi(tdi),\n"
            "    .tdo(", Not/binary, "tdo),\n"
            "    .tms(tms),\n"
            "    .ir_out(ir_out),\n"
            "    .ir_in(ir_in),\n"
            "    .virtual_state_cdr(virtual_state_cdr),\n"
            "    .virtual_state_sdr(virtual_state_sdr),\n"
            "    .virtual_state_e1dr(virtual_state_e1dr),\n"
            "    .virtual_state_pdr(virtual_state_pdr),\n"
            "    .virtual_state_e2dr(virtual_state_e2dr),\n"
            "    .virtual_state_udr(virtual_state_udr),\n"
            "    .virtual_state_cir(virtual_state_cir),\n"
            "    .virtual_state_uir(virtual_state_uir),\n"
            "    .jtag_state_tlr(jtag_state_tlr),\n"
            "    .jtag_state_rti(jtag_state_rti),\n"
            "    .jtag_state_sdrs(jtag_state_sdrs),\n"
            "    .jtag_state_cdr(jtag_state_cdr),\n"
            "    .jtag_state_sdr(jtag_state_sdr),\n"
            "    .jtag_state_e1dr(jtag_state_e1dr),\n"
            "    .jtag_state_pdr(jtag_state_pdr),\n"
            "    .jtag_state_e2dr(jtag_state_e2dr),\n"
            "    .jtag_state_udr(jtag_state_udr),\n"
            "    .jtag_state_sirs(jtag_state_sirs),\n"
            "    .jtag_state_cir(jtag_state_cir),\n"
            "    .jtag_state_sir(jtag_state_sir),\n"
            "    .jtag_state_e1ir(jtag_state_e1ir),\n"
            "    .jtag_state_pir(jtag_state_pir),\n"
            "    .jtag_state_e2ir(jtag_state_e2ir),\n"
            "    .jtag_state_uir(jtag_state_uir)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_type = \"SLD_VIRTUAL_JTAG\",\n"
            "    mega.sld_auto_instance_index = \"NO\",\n"
            "    mega.sld_instance_index = ", IndexBin/binary, ",\n"
            "    mega.sld_ir_width = 4;\n"
            "endmodule\n"
        >>
    }.

