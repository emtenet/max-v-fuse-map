-module(lc_local_line_experiment).

-export([run/0]).

% This experiment looks at the LC's local-line routing from either:
%
%   * the LUT output, or
%   * the register output.

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
    iterate:labs(Device, Pins0, 2,
        fun (LAB, Pins) ->
            sources(Device, LAB, Clk, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Device, LAB, Experiments)
        end,
        {batch, 1}
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, Clk, Pins) ->
    Ns = lists:seq(0, 9),
    [
        source(Device, LAB, Clk, X, x_to_y(X), Pins, Type)
        ||
        Type <- [lut, reg],
        X <- Ns
    ].

%%--------------------------------------------------------------------

x_to_y(0) -> 9;
x_to_y(_) -> 0.

%%--------------------------------------------------------------------

experiments(Device, LAB = {lab, X, Y}, Experiments) ->
    io:format(" ==> ~s ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{lc, _, _, _}, clk2}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:control_routing(Experiments),
    %
    lists:foreach(fun always_local_line/1, Experiments),
    %
    [
        expect:fuse(Matrix, pattern(N), {{lc, X, Y, N}, local_line, lut})
        ||
        N <- lists:seq(0, 9)
    ],
    %
    ok.

%%--------------------------------------------------------------------

always_local_line({_, _, #{signals := #{x := X}}}) ->
    #{dests := [#{route := [{local_line, _, _, 0, _}]}]} = X,
    ok.

%%--------------------------------------------------------------------

pattern(N) ->
    % looking for LUT output to local-line
    % all LUT experiments where the first 10 in order,
    % all REG experiments where the second 10.
    % Pattern is:
    %   0 for the indexed LUT on the left,
    %   1 otherwise.
    lists:map(
        fun (X) -> pattern_bit(X, N) end,
        [0,1,2,3,4,5,6,7,8,9,x,x,x,x,x,x,x,x,x,x]
    ).

%%--------------------------------------------------------------------

pattern_bit(X, X) -> 0;
pattern_bit(_, _) -> 1.

%%--------------------------------------------------------------------

source(Device, LAB, Clk, X, Y, {D, Q}, lut) ->
    #{
        title => {LAB, X, lut, Y},
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, d, D},
            {location, x, lab:lc(LAB, X)},
            {location, y, lab:lc(LAB, Y)},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire local;\n"
            "  lcell x (\n"
            "    .in(d),\n"
            "    .out(local)\n"
            "  );\n"
            "  dff y (\n"
            "    .d(local),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    };
source(Device, LAB, Clk, X, Y, {D, Q}, reg) ->
    #{
        title => {LAB, X, reg, Y},
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, d, D},
            {location, x, lab:lc(LAB, X)},
            {location, y, lab:lc(LAB, Y)},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire local;\n"
            "  dff x (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(local)\n"
            "  );\n"
            "  dff y (\n"
            "    .d(local),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

