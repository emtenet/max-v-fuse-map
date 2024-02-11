-module(lc_register_chain_experiment).

-export([run/0]).

% This experiment looks for the register chain fuse.
%
% There seems to be a fuse for a register chain into LC #0 but not
% sure if it is possible or what source might show it.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    [Clk | _] = device:gclk_pins(Device),
    Pins0 = lists:delete(Clk, device:pins(Device)),
    iterate:labs(Device, Pins0, 13,
        fun (LAB, Pins) ->
            sources(Device, LAB, Clk, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Density, Device, LAB, Experiments)
        end,
        {batch, 1}
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, Clk, Pins) ->
    [
        source(Device, LAB, 0, 1, 2, 3, Clk, Pins),
        source(Device, LAB, 1, 2, 3, 4, Clk, Pins),
        source(Device, LAB, 2, 3, 4, 5, Clk, Pins),
        source(Device, LAB, 3, 4, 5, 6, Clk, Pins),
        source(Device, LAB, 4, 5, 6, 7, Clk, Pins),
        source(Device, LAB, 5, 6, 7, 8, Clk, Pins),
        source(Device, LAB, 6, 7, 8, 9, Clk, Pins)
    ].

%%--------------------------------------------------------------------

experiments(_Density, Device, LAB, Experiments) ->
    io:format(" ==> ~s ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, a_clr1}) -> true;
        ({_, clk2}) -> true;
        ({_, data_a3, _}) -> true;
        ({_, data_a6, _}) -> true;
        ({_, data_b3, _}) -> true;
        ({_, data_b6, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, data_d6, _}) -> true;
        ({_, lut, _}) -> true;
        ({_, output3, _}) -> true;
        ({_, output4, _}) -> true;
        ({_, output6, _}) -> true;
        ({_, output_left, _}) -> true;
        ({_, output_local, _}) -> true;
        ({_, output_right, _}) -> true;
        ({_, s_clr_load}) -> true;
        ({_, {interconnect, _}, _, _}) -> true;
        ({_, {interconnect, _}, _}) -> true;
        ({_, {mux, _}, _, _}) -> true;
        ({_, {mux, _}, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:control_routing(Experiments),
    %display:routing(Experiments, _Density),
    %
    expect:fuse(Matrix, [1,0,0,0,0,0,0], {lab:lc(LAB, 1), register_chain, off}),
    expect:fuse(Matrix, [1,1,0,0,0,0,0], {lab:lc(LAB, 2), register_chain, off}),
    expect:fuse(Matrix, [1,1,1,0,0,0,0], {lab:lc(LAB, 3), register_chain, off}),
    expect:fuse(Matrix, [0,1,1,1,0,0,0], {lab:lc(LAB, 4), register_chain, off}),
    expect:fuse(Matrix, [0,0,1,1,1,0,0], {lab:lc(LAB, 5), register_chain, off}),
    expect:fuse(Matrix, [0,0,0,1,1,1,0], {lab:lc(LAB, 6), register_chain, off}),
    expect:fuse(Matrix, [0,0,0,0,1,1,1], {lab:lc(LAB, 7), register_chain, off}),
    expect:fuse(Matrix, [0,0,0,0,0,1,1], {lab:lc(LAB, 8), register_chain, off}),
    expect:fuse(Matrix, [0,0,0,0,0,0,1], {lab:lc(LAB, 9), register_chain, off}),
    %
    ok.

%%--------------------------------------------------------------------

source(Device, LAB, W, X, Y, Z, Clk, {A, B, C, D, E, F, G, H, I, J, K, L, M}) ->
    #{
        title => {LAB, X, Y, Z},
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, lut1, lab:lc(LAB, X)},
            {location, lut2, lab:lc(LAB, Y)},
            {location, lut3, lab:lc(LAB, Z)},
            {location, a, A},
            {location, b, B},
            {location, c, C},
            {location, d, D},
            {location, e, E},
            {location, f, F},
            {location, g, G},
            {location, h, H},
            {location, i, I},
            {location, j, J},
            {location, k, K},
            {location, ff0, lab:lc(LAB, W)},
            {location, ff1, lab:lc(LAB, X)},
            {location, ff2, lab:lc(LAB, Y)},
            {location, ff3, lab:lc(LAB, Z)},
            {location, l, L},
            {location, m, M}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  input wire c,\n"
            "  input wire d,\n"
            "  input wire e,\n"
            "  input wire f,\n"
            "  input wire g,\n"
            "  input wire h,\n"
            "  input wire i,\n"
            "  input wire j,\n"
            "  output wire k,\n"
            "  input wire l,\n"
            "  output wire m\n"
            ");\n"
            "  wire chain1;\n"
            "  wire chain2;\n"
            "  lcell lut1 (\n"
            "    .in(a && b && c && d),\n"
            "    .out(chain1)\n"
            "  );\n"
            "  lcell lut2 (\n"
            "    .in(chain1 && e && f && g),\n"
            "    .out(chain2)\n"
            "  );\n"
            "  lcell lut3 (\n"
            "    .in(chain2 && h && i && j),\n"
            "    .out(k)\n"
            "  );\n"
            "  wire chain3;\n"
            "  wire chain4;\n"
            "  wire chain5;\n"
            "  dff ff0 (\n"
            "    .d(l),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(chain3)\n"
            "  );\n"
            "  dff ff1 (\n"
            "    .d(chain3),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(chain4)\n"
            "  );\n"
            "  dff ff2 (\n"
            "    .d(chain4),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(chain5)\n"
            "  );\n"
            "  dff ff3 (\n"
            "    .d(chain5),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(m)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

