-module(device_pins_experiment).

-export([run/0]).

% This experiment looks at enabling the two device wide pins:
%
%  * DEV_CLRn
%  * DEV_OE

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    io:format(" => ~s~n", [Density]),
    Device = density:largest_device(Density),
    {ok, Experiments} = experiment:compile_to_fuses([
        source(Device, normal, false, false),
        source(Device, dev_clr, true, false),
        source(Device, dev_oe, false, true),
        source(Device, both, true, true)
    ]),
    Matrix0 = matrix:build(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{c4, _, _}, _, _}) -> true;
        ({{c4, _, _}, _, _, _}) -> true;
        ({{global, _}, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{lab, _, _}, _}) -> true;
        ({{lab, _, _}, _, _}) -> true;
        ({{lab, _, _}, _, _, _}) -> true;
        ({{lc, _, _, _}, _}) -> true;
        ({{lc, _, _, _}, _, _}) -> true;
        ({{r4, _, _}, _, _}) -> true;
        ({{r4, _, _}, _, _, _}) -> true;
        (_) -> false
    end),
    %
    matrix:print(Matrix),
    %
    expect:fuse(Matrix, [1,0,1,0], {device, reset}),
    expect:fuse(Matrix, [1,1,0,0], {device, output_enable}),
    ok.

%%--------------------------------------------------------------------

source(Device, Title, Clr, OE) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {device_clrn, Clr},
            {device_oe, OE},
            {global_clock, clk, true}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire i,\n"
            "  output wire o\n"
            ");\n"
            "  dffeas ff (\n"
            "    .d(i),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(o)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

