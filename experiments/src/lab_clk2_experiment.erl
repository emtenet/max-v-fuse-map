-module(lab_clk2_experiment).

-export([run/0]).

% This experiment was designed to look at how the LAB's clk2
% source was selected between the four (4) global networks
% or the two (2) LAB control lines.
%
% The two control lines are specifically selected by routing
% the clock through LC's 7 and 8. LC 7 can only be selected into
% even control lines, and LC 8 into odd control lines.
%
% The first layer is a one-hot selection of:
%
%  * {{lab, X, Y}, clk2, global0}
%  * {{lab, X, Y}, clk2, global1}
%  * {{lab, X, Y}, clk2, global2}
%  * {{lab, X, Y}, clk2, global3}
%  * {{lab, X, Y}, clk2, control}
%
% Then in control is selected above, a further fuse:
%
%  * {{lab, X, Y}, clk2, control_0_not_1}
%
% selects between {control, 0} and {control, 1}.
%
% The clock line can be inverted with:
%
%  * {{lab, X, Y}, clk2, invert}

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
    iterate:labs(Device, 6, Pins0,
        fun (LAB, Pins) ->
            sources(Device, LAB, Gclks, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Device, LAB, Experiments)
        end
    ).

%%--------------------------------------------------------------------

sources(Device, LAB, Gclks, Pins) ->
    [Gclk0, Gclk1, Gclk2, Gclk3] = Gclks,
    {Clk1, D1, Q1, A, B, Q} = Pins,
    Common = {LAB, Clk1, D1, Q1, A, B, Q},
    [
        source_lut(Device, Common, lut),
        source_global(Device, Common, gclk0, Gclk0, <<"!">>),
        source_global(Device, Common, gclk0, Gclk0, <<>>),
        source_global(Device, Common, gclk1, Gclk1, <<>>),
        source_global(Device, Common, gclk2, Gclk2, <<>>),
        source_global(Device, Common, gclk3, Gclk3, <<>>),
        source_local(Device, Common, even_local7, Gclk0, 7, even),
        source_local(Device, Common, even_local8, Gclk0, 8, even),
        source_local(Device, Common, odd_local7, Gclk0, 7, odd),
        source_local(Device, Common, odd_local8, Gclk0, 8, odd)
    ].

%%--------------------------------------------------------------------

experiments(Device, LAB, Experiments) ->
    io:format(" ==> ~p ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{iob, _, _}, _}) -> true;
        ({{iob, _, _}, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{ioc, _, _, _}, _, _, _}) -> true;
        ({{lab, _, _}, s_load, _}) -> true;
        ({{lc, _, _, _}, s_load}) -> true;
        ({_, lut, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:control_routing(Experiments),
    %
    lists:foreach(fun not_s_data/1, Experiments),
    %
    {LC0Clk, LC1Clk} = clk2_pattern(Experiments),
    io:format("LC 0 clk2    = ~w~n", [LC0Clk]),
    io:format("LC 1 clk2    = ~w~n", [LC1Clk]),
    Clk1Control = clk1_control_pattern(Experiments),
    Clk1Select = clk1_select_pattern(Experiments),
    Clk2Control = clk2_control_pattern(Experiments),
    Clk2Select = clk2_select_pattern(Experiments),
    io:format("clk1 control = ~w~n", [Clk1Control]),
    io:format("clk1 is 0/1  = ~w~n", [Clk1Select]),
    io:format("clk2 control = ~w~n", [Clk2Control]),
    io:format("clk2 is 3/2  = ~w~n", [Clk2Select]),
    %
    % we would like the clk2 control fuse to be on at least once
    case lists:usort(Clk2Control) of
        [0, 1] ->
            expect:fuse(Matrix, LC0Clk, {lab:lc(LAB, 0), clk2}),
            expect:fuse(Matrix, LC1Clk, {lab:lc(LAB, 1), clk2}),
            %
            expect:fuse(Matrix, [1,0,1,1,1,1,1,1,1,1], {LAB, clk2, invert}),
            expect:fuse(Matrix, [1,0,0,1,1,1,1,1,1,1], {LAB, clk2, global0}),
            expect:fuse(Matrix, [1,1,1,0,1,1,1,1,1,1], {LAB, clk2, global1}),
            expect:fuse(Matrix, [1,1,1,1,0,1,1,1,1,1], {LAB, clk2, global2}),
            expect:fuse(Matrix, [1,1,1,1,1,0,1,1,1,1], {LAB, clk2, global3}),
            %
            expect:fuse(Matrix, Clk1Control, {LAB, clk1, control}),
            expect:fuse(Matrix, Clk1Select,  {LAB, clk1, control_0_not_1}),
            expect:fuse(Matrix, Clk2Control, {LAB, clk2, control}),
            expect:fuse(Matrix, Clk2Select,  {LAB, clk2, control_3_not_2});

        _ ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------

not_s_data({_, _, #{signals := Signals}}) ->
    #{a := #{dests := [#{port := data_a}]}} = Signals,
    #{b := #{dests := [#{port := data_b}]}} = Signals,
    case Signals of
        #{clk := #{dests := [#{port := Port}]}} ->
            case Port of
                clk ->
                    ok;

                data_a ->
                    ok
            end;

        _ ->
            ok
    end.

%%--------------------------------------------------------------------

clk2_pattern(Experiments) ->
    lists:unzip(lists:map(fun ({_, _, #{signals := Signals}}) ->
        clk2_pattern_signals(Signals)
    end, Experiments)).

%%--------------------------------------------------------------------

clk2_pattern_signals(Signals) ->
    case Signals of
        #{cc   := #{dests := [#{port := clk, lc := A, route := RouteA}]},
          clk  := #{dests := [#{port := clk, lc := _, route := RouteB}]}} ->
            clk2_pattern_routes(A, RouteA, RouteB);

        #{cc   := #{dests := [#{port := clk, lc := A, route := RouteA}]},
          clk1 := #{dests := [#{port := clk, lc := _, route := RouteB}]}} ->
            clk2_pattern_routes(A, RouteA, RouteB);

        #{clk  := #{dests := [#{port := clk, lc := A, route := RouteA}]},
          clk1 := #{dests := [#{port := clk, lc := _, route := RouteB}]}} ->
            clk2_pattern_routes(A, RouteA, RouteB);

        #{clk1 := #{dests := [#{port := clk, lc := A, route := RouteA}]}} ->
            {lc, _, _, 0} = A,
            clk2_pattern_bit(clk2_pattern_route(RouteA))
    end.

%%--------------------------------------------------------------------

clk2_pattern_routes({lc, _, _, 0}, RouteA, RouteB) ->
    clk2_pattern_bit(clk2_pattern_route(RouteA), clk2_pattern_route(RouteB));
clk2_pattern_routes({lc, _, _, 1}, RouteA, RouteB) ->
    clk2_pattern_bit(clk2_pattern_route(RouteB), clk2_pattern_route(RouteA)).

%%--------------------------------------------------------------------

clk2_pattern_route([{lab_control_mux, _, _, 0, 0} | _]) -> clk1;
clk2_pattern_route([{lab_control_mux, _, _, 0, 1} | _]) -> clk1;
clk2_pattern_route([{lab_control_mux, _, _, 0, 2} | _]) -> clk2;
clk2_pattern_route([{lab_control_mux, _, _, 0, 3} | _]) -> clk2;
clk2_pattern_route([{lab_clk, _, _, _, _} | _]) -> global.

%%--------------------------------------------------------------------

clk2_pattern_bit(clk1, _) -> {1, 0};
clk2_pattern_bit(_, clk2) -> {1, 0};
clk2_pattern_bit(clk2, _) -> {0, 1};
clk2_pattern_bit(_, clk1) -> {0, 1}.

%%--------------------------------------------------------------------

clk2_pattern_bit(clk1) -> {1, 0};
clk2_pattern_bit(clk2) -> {0, 1}.

%%--------------------------------------------------------------------

clk1_control_pattern(Experiments) ->
    lists:map(fun ({_, _, #{signals := Signals}}) ->
        maps:fold(fun clk1_control_pattern_bit/3, 1, Signals)
    end, Experiments).

%%--------------------------------------------------------------------

clk1_control_pattern_bit(_, #{dests := [#{port := clk, route := Route}]}, Bit) ->
    case Route of
        [{lab_control_mux, _, _, 0, 0} | _] ->
            0;

        [{lab_control_mux, _, _, 0, 1} | _] ->
            0;

        _ ->
            Bit
    end;
clk1_control_pattern_bit(_, _, Bit) ->
    Bit.

%%--------------------------------------------------------------------

clk1_select_pattern(Experiments) ->
    lists:map(fun ({_, _, #{signals := Signals}}) ->
        maps:fold(fun clk1_select_pattern_bit/3, 1, Signals)
    end, Experiments).

%%--------------------------------------------------------------------

clk1_select_pattern_bit(_, #{dests := [#{port := clk, route := Route}]}, Bit) ->
    case Route of
        [{lab_control_mux, _, _, 0, 0} | _] ->
            0;

        [{lab_control_mux, _, _, 0, 1} | _] ->
            1;

        _ ->
            Bit
    end;
clk1_select_pattern_bit(_, _, Bit) ->
    Bit.

%%--------------------------------------------------------------------

clk2_control_pattern(Experiments) ->
    lists:map(fun ({_, _, #{signals := Signals}}) ->
        maps:fold(fun clk2_control_pattern_bit/3, 1, Signals)
    end, Experiments).

%%--------------------------------------------------------------------

clk2_control_pattern_bit(_, #{dests := [#{port := clk, route := Route}]}, Bit) ->
    case Route of
        [{lab_control_mux, _, _, 0, 3} | _] ->
            0;

        [{lab_control_mux, _, _, 0, 2} | _] ->
            0;

        _ ->
            Bit
    end;
clk2_control_pattern_bit(_, _, Bit) ->
    Bit.

%%--------------------------------------------------------------------

clk2_select_pattern(Experiments) ->
    lists:map(fun ({_, _, #{signals := Signals}}) ->
        maps:fold(fun clk2_select_pattern_bit/3, 1, Signals)
    end, Experiments).

%%--------------------------------------------------------------------

clk2_select_pattern_bit(_, #{dests := [#{port := clk, route := Route}]}, Bit) ->
    case Route of
        [{lab_control_mux, _, _, 0, 3} | _] ->
            0;

        [{lab_control_mux, _, _, 0, 2} | _] ->
            1;

        _ ->
            Bit
    end;
clk2_select_pattern_bit(_, _, Bit) ->
    Bit.

%%--------------------------------------------------------------------

source_lut(Device, {LAB, Clk1, D1, Q1, A, B, Q}, Name) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, d1, D1},
            {location, q1, Q1},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff, lab:lc(LAB, 1)},
            {location, a, A},
            {location, b, B},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire d1,\n"
            "  output wire q1,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  output wire q\n"
            ");\n"
            "  dff ff1 (\n"
            "    .d(d1),\n"
            "    .clk(clk1),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q1)\n"
            "  );\n"
            "  lcell ff (\n"
            "    .in(a || b),\n"
            "    .out(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_global(Device, {LAB, Clk1, D1, Q1, A, B, Q}, Name, Clk, Not) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, d1, D1},
            {location, q1, Q1},
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff, lab:lc(LAB, 1)},
            {location, a, A},
            {location, b, B},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire d1,\n"
            "  output wire q1,\n"
            "  input wire clk,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  output wire q\n"
            ");\n"
            "  dff ff1 (\n"
            "    .d(d1),\n"
            "    .clk(clk1),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q1)\n"
            "  );\n"
            "  dff ff (\n"
            "    .d(a || b),\n"
            "    .clk(", Not/binary, "clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_local(Device, {LAB, Clk1, D1, Q1, A, B, Q}, Name, Clk, N, even) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, d1, D1},
            {location, q1, Q1},
            {location, clk, Clk},
            {global_clock, clk, false},
            {location, cc, lab:lc(LAB, N)},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff, lab:lc(LAB, 1)},
            {location, a, A},
            {location, b, B},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire d1,\n"
            "  output wire q1,\n"
            "  input wire clk,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  output wire q\n"
            ");\n"
            "  dff ff1 (\n"
            "    .d(d1),\n"
            "    .clk(clk1),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q1)\n"
            "  );\n"
            "  wire c;\n"
            "  lcell cc (\n"
            "    .in(clk),\n"
            "    .out(c)\n"
            "  );\n"
            "  dff ff (\n"
            "    .d(a || b),\n"
            "    .clk(c),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    };
source_local(Device, {LAB, Clk1, D1, Q1, A, B, Q}, Name, Clk, N, odd) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, d1, D1},
            {location, q1, Q1},
            {location, clk, Clk},
            {global_clock, clk, false},
            {location, cc, lab:lc(LAB, N)},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff, lab:lc(LAB, 1)},
            {location, a, A},
            {location, b, B},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire d1,\n"
            "  output wire q1,\n"
            "  input wire clk,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  output wire q\n"
            ");\n"
            "  dff ff1 (\n"
            "    .d(d1),\n"
            "    .clk(c),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q1)\n"
            "  );\n"
            "  wire c;\n"
            "  lcell cc (\n"
            "    .in(clk1),\n"
            "    .out(c)\n"
            "  );\n"
            "  dff ff (\n"
            "    .d(a || b),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

