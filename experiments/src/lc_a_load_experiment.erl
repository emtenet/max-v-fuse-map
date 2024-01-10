-module(lc_a_load_experiment).

-export([run/0]).

% What if only some FFs have an a-load line?
%
% Then those that do not have the {lc(), a_clr1} fuse.

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
    iterate:labs(Device, 5, Pins0,
        fun (LAB, Pins) ->
            sources(Device, LAB, Gclks, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Device, LAB, Experiments)
        end,
        {batch, 10}
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, Gclks, Pins) ->
    [Clk | _] = Gclks,
    {Adata, Aload, D, X, Q} = Pins,
    Common = {LAB, Clk, Adata, Aload, D, X, Q},
    [
        source(Device, Common, all),
        source(Device, Common, ff1),
        source(Device, Common, ff2),
        source(Device, Common, ff3)
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
    expect:fuse(Matrix, [1,0,1,1], {lab:lc(LAB, 0), a_clr1}),
    expect:fuse(Matrix, [1,1,0,1], {lab:lc(LAB, 1), a_clr1}),
    expect:fuse(Matrix, [1,1,1,0], {lab:lc(LAB, 2), a_clr1}),
    ok.

%%--------------------------------------------------------------------

source(Device, {LAB, Clk, Adata, Aload, D, X, Q}, Name) ->
    Aload1 = a_load(ff1, Name),
    Aload2 = a_load(ff2, Name),
    Aload3 = a_load(ff3, Name),
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, adata, Adata},
            {location, aload, Aload},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff2, lab:lc(LAB, 1)},
            {location, ff3, lab:lc(LAB, 2)},
            {location, d, D},
            {location, x, X},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire adata,\n"
            "  input wire aload,\n"
            "  input wire d,\n"
            "  input wire x,\n"
            "  output wire q\n"
            ");\n"
            "  wire chain1;\n"
            "  wire chain2;\n"
            "  dffea ff1 (\n"
            "    .d(d ^ x),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .adata(adata),\n"
            "    .aload(", Aload1/binary, "),\n"
            "    .q(chain1)\n"
            "  );\n"
            "  dffea ff2 (\n"
            "    .d(chain1 ^ x),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .adata(adata),\n"
            "    .aload(", Aload2/binary, "),\n"
            "    .q(chain2)\n"
            "  );\n"
            "  dffea ff3 (\n"
            "    .d(chain2 ^ x),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .adata(adata),\n"
            "    .aload(", Aload3/binary, "),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

a_load(Name, Name) -> <<"0">>;
a_load(_FF, _Name) -> <<"aload">>.

