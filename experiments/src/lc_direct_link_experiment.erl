-module(lc_direct_link_experiment).

-export([run/0]).

% This experiment looks at the LC's direct-link routing to:
%
%   * a left LAB / IOB via direct-link
%   * a right LAB / IOB via direct-link
%
% from either:
%
%   * the LUT output, or
%   * the register output.
%
% NOTE: the IOB is sometimes up or down, but goes out the "right" direct-link.

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
    iterate:iobs(Device, Pins0, 0,
        fun (IOB, _, {}) ->
            iob_sources(Device, IOB, Clk)
        end,
        fun (IOB, _, {}, Experiments) ->
            iob_experiments(Device, IOB, Clk, Experiments)
        end,
        {batch, 3}
    ),
    iterate:labs(Device, Pins0, 2,
        fun (LAB, Pins) ->
            lab_sources(Density, Device, LAB, Clk, Pins)
        end,
        fun (LAB, _, Experiments) ->
            lab_experiments(Density, Device, LAB, Experiments)
        end,
        {batch, 5}
    ).

%%====================================================================
%% IOB
%%====================================================================

iob_ways(Device, IOB, Clk) ->
    % [{ioc(), lc(), left | right}]
    FastOuts = density:fast_outs(Device),
    % [{ioc(), pin()}]
    Pins = device:iocs(Device, IOB),
    %
    LUTs = lists:filtermap(fun (Pin) ->
        iob_way(lut, Pin, Pins, FastOuts, Clk)
    end, Pins),
    REGs = lists:filtermap(fun (Pin) ->
        iob_way(reg, Pin, Pins, FastOuts, Clk)
    end, Pins),
    LUTs ++ REGs.

%%--------------------------------------------------------------------

iob_way(_, {_, Clk}, _, _, Clk) ->
    false;
iob_way(LUT, {IOC, Out}, Pins, FastOuts, Clk) ->
    case lists:keyfind(IOC, 1, FastOuts) of
        {IOC, LC, Side} ->
            case iob_input(Pins, Out, Clk) of
                false ->
                    false;

                In ->
                    {true, {LUT, Side, In, LC, Out}}
            end;

        false ->
            false
    end.

%%--------------------------------------------------------------------

iob_input([], _, _) ->
    false;
iob_input([{_, Out} | Pins], Out, Clk) ->
    iob_input(Pins, Out, Clk);
iob_input([{_, Clk} | Pins], Out, Clk) ->
    iob_input(Pins, Out, Clk);
iob_input([{_, Pin} | _], _, _) ->
    Pin.

%%--------------------------------------------------------------------

iob_sources(Device, IOB, Clk) ->
    Ways = iob_ways(Device, IOB, Clk),
    [
        case LUT of
            lut ->
                source:in_lut_out(Device, {lut, LC}, I, LC, O);

            reg ->
                source:in_reg_out(Device, {reg, LC}, Clk, I, LC, O)
        end
        ||
        {LUT, _Side, I, LC, O} <- Ways
    ].

%%--------------------------------------------------------------------

iob_experiments(Device, IOB, Clk, Experiments) ->
    io:format(" ==> ~s ~p~n", [Device, IOB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, fast_slew_rate}) -> true;
        ({_, data_a6, _}) -> true;
        ({_, data_a3, _}) -> true;
        ({_, data_b6, _}) -> true;
        ({_, data_b3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_d6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, lut, _}) -> true;
        ({_, a_clr1}) -> true;
        ({_, clk2}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:control_routing(Experiments),
    %
    Ways = iob_ways(Device, IOB, Clk),
    lists:foreach(fun iob_is_direct_link/1, lists:zip(Experiments, Ways)),
    %
    [
        expect:fuse(Matrix, Pattern, Fuse)
        ||
        {Pattern, Fuse} <- iob_patterns(Ways)
    ],
    %
    ok.

%%--------------------------------------------------------------------

iob_is_direct_link({{_, _, #{signals := #{via := X}}}, {_, Side, _, LC, _}}) ->
    {lc, _, _, N} = LC,
    % left & right buffers are numbered differently
    I = case Side of
        left -> (2 * N) + 0;
        right -> (2 * N) + 1
    end,
    #{dests := [#{route := Route}]} = X,
    [{io_bypass_out, _, _, _, _}, {le_buffer, _, _, 0, I}] = Route.

%%--------------------------------------------------------------------

iob_patterns(Ways) ->
    lists:filtermap(fun (Way) ->
        iob_pattern(Way, Ways)
    end, Ways).

%%--------------------------------------------------------------------

iob_pattern(Way = {lut, left, _, LC, _}, Ways) ->
    Fuse = {LC, output_left, lut},
    Pattern = lists:map(fun (W) -> iob_pattern_bit(W, Way) end, Ways),
    {true, {Pattern, Fuse}};
iob_pattern(Way = {lut, right, _, LC, _}, Ways) ->
    Fuse = {LC, output_right, lut},
    Pattern = lists:map(fun (W) -> iob_pattern_bit(W, Way) end, Ways),
    {true, {Pattern, Fuse}};
iob_pattern(_, _) ->
    false.

%%--------------------------------------------------------------------

iob_pattern_bit(Way, Way) -> 0;
iob_pattern_bit(_, _) -> 1.

%%====================================================================
%% LAB
%%====================================================================

lab_ways(Density, {lab, X, Y}) ->
    % Random choice of LC to test
    LC = {lc, X, Y, (X + Y) rem 10},
    % Can we route left and/or right?
    case {density:is_lab(X - 1, Y, Density),
          density:is_lab(X + 1, Y, Density)} of
        {true, true} ->
            [
                {lut, left, LC, {lc, X - 1, Y, 0}},
                {lut, right, LC, {lc, X + 1, Y, 0}},
                {reg, left, LC, {lc, X - 1, Y, 0}},
                {reg, right, LC, {lc, X + 1, Y, 0}}
            ];

        {true, false} ->
            [
                {lut, left, LC, {lc, X - 1, Y, 0}},
                {reg, left, LC, {lc, X - 1, Y, 0}}
            ];

        {false, true} ->
            [
                {lut, right, LC, {lc, X + 1, Y, 0}},
                {reg, right, LC, {lc, X + 1, Y, 0}}
            ]
    end.

%%--------------------------------------------------------------------

lab_sources(Density, Device, LAB, Clk, Pins) ->
    [
        lab_source(Device, X, Clk, Y, Pins, Type)
        ||
        {Type, _Side, X, Y} <- lab_ways(Density, LAB)
    ].

%%--------------------------------------------------------------------

lab_experiments(Density, Device, LAB, Experiments) ->
    io:format(" ==> ~s ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, fast_slew_rate}) -> true;
        ({_, data_a6, _}) -> true;
        ({_, data_a3, _}) -> true;
        ({_, data_b6, _}) -> true;
        ({_, data_b3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_d6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, lut, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:control_routing(Experiments),
    %
    Ways = lab_ways(Density, LAB),
    lists:foreach(fun lab_is_direct_link/1, lists:zip(Experiments, Ways)),
    %
    [{_, _, LC, _} | _] = Ways,
    expect:fuse(Matrix, lab_pattern(Ways, left), {LC,  output_left, lut}),
    expect:fuse(Matrix, lab_pattern(Ways, right), {LC,  output_right, lut}),
    %
    ok.

%%--------------------------------------------------------------------

lab_is_direct_link({{_, _, #{signals := #{x := X}}}, {_, Side, LC, _}}) ->
    {lc, _, _, N} = LC,
    % left & right buffers are numbered differently
    I = case Side of
        left -> (2 * N) + 0;
        right -> (2 * N) + 1
    end,
    #{dests := [#{route := Route}]} = X,
    [{local_interconnect, _, _, _, _}, {le_buffer, _, _, 0, I}] = Route.

%%--------------------------------------------------------------------

lab_pattern(Ways, Side) ->
    lists:map(fun (Way) ->
        lab_pattern_bit(Way, Side)
    end, Ways).

%%--------------------------------------------------------------------

lab_pattern_bit({lut, Side, _, _}, Side) -> 0;
lab_pattern_bit(_, _) -> 1.

%%--------------------------------------------------------------------

lab_source(Device, X, Clk, Y, {D, Q}, lut) ->
    #{
        title => {X, lut, Y},
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, d, D},
            {location, x, X},
            {location, y, Y},
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
lab_source(Device, X, Clk, Y, {D, Q}, reg) ->
    #{
        title => {X, reg, Y},
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, d, D},
            {location, x, X},
            {location, y, Y},
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

