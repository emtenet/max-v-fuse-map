-module(lc_lut_chain_experiment).

-export([run/0]).

% This experiment looks for the LUT Chain fuse.
%
% There seems to be a fuse for a LUT Chain into LC #0 but not
% sure if it is possible or what source might show it.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    Pins0 = device:pins(Device),
    iterate:labs(Device, Pins0, 11,
        fun (LAB, Pins) ->
            sources(Device, LAB, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Device, LAB, Experiments)
        end,
        {batch, 1}
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, Pins) ->
    [
        source(Device, LAB, 0, 1, 2, Pins),
        source(Device, LAB, 1, 2, 3, Pins),
        source(Device, LAB, 2, 3, 4, Pins),
        source(Device, LAB, 3, 4, 5, Pins),
        source(Device, LAB, 4, 5, 6, Pins),
        source(Device, LAB, 5, 6, 7, Pins),
        source(Device, LAB, 6, 7, 8, Pins),
        source(Device, LAB, 7, 8, 9, Pins)
    ].

%%--------------------------------------------------------------------

experiments(Device, LAB = {lab, X, Y}, Experiments) ->
    io:format(" ==> ~s ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, {mux, _}, _}) -> true;
        ({_, {mux, _}, _, _}) -> true;
        ({_, {interconnect, _}, _, _}) -> true;
        ({_, output3, _}) -> true;
        ({_, output6, _}) -> true;
        ({_, output_left, _}) -> true;
        ({_, output_right, _}) -> true;
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
    lut_chain_fuse(Matrix, Experiments, X, Y, 1),
    lut_chain_fuse(Matrix, Experiments, X, Y, 2),
    lut_chain_fuse(Matrix, Experiments, X, Y, 3),
    lut_chain_fuse(Matrix, Experiments, X, Y, 4),
    lut_chain_fuse(Matrix, Experiments, X, Y, 5),
    lut_chain_fuse(Matrix, Experiments, X, Y, 6),
    lut_chain_fuse(Matrix, Experiments, X, Y, 7),
    lut_chain_fuse(Matrix, Experiments, X, Y, 8),
    lut_chain_fuse(Matrix, Experiments, X, Y, 9),
    %
    ok.

%%--------------------------------------------------------------------

lut_chain_fuse(Matrix, Experiments, X, Y, N) ->
    Pattern = lut_chain_pattern(Experiments, N),
    expect:fuse(Matrix, Pattern, {{lc, X, Y, N}, lut_chain, off}).

%%--------------------------------------------------------------------

lut_chain_pattern(Experiments, N) ->
    lists:map(fun ({_, _, #{signals := Signals}}) ->
        maps:fold(fun (_Name, Signal, Acc) ->
            lut_chain_pattern_bit(Signal, N, Acc)
        end, 0, Signals)
    end, Experiments).

%%--------------------------------------------------------------------

lut_chain_pattern_bit(#{dests := [Dest]}, N, Acc) ->
    case Dest of
        #{lc := {lc, _, _, N},
          route_port := data_d,
          route := [{lut_chain, _, _, 0, NN}]} ->
            NN = N - 1,
            1;

        _ ->
            Acc
    end;
lut_chain_pattern_bit(_, _, Acc) ->
    Acc.

%%--------------------------------------------------------------------

source(Device, LAB, X, Y, Z, {A, B, C, D, E, F, G, H, I, J, Q}) ->
    #{
        title => {LAB, X, Y, Z},
        device => Device,
        settings => [
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
            {location, lut1, lab:lc(LAB, X)},
            {location, lut2, lab:lc(LAB, Y)},
            {location, lut3, lab:lc(LAB, Z)},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
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
            "  output wire q\n"
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
            "    .out(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

