-module(lab_a_clr2_experiment).

-export([run/0]).

% New fuse mapping:
%
% clr x GLOBAL3	        {{lab,2,4},clr1,global3}
%
%clr1 OFF	        {2,4,line,25,cell,21}
%clr1 GLOBAL	        {2,4,line,25,cell,15}
%clr1 CONTROL 5_not_4	{2,4,line,20,cell,5}
%clr1 INVERT	        {2,4,line,24,cell,21}
%
%clr2 OFF	        {{lab,2,4},clr1,off}
%clr2 GLOBAL	        {{lab,2,4},clr1,global}
%clr2 CONTROL 5_not_4	{{lab,2,4},clr1,control_5_not_4}
%clr2 INVERT	        {{lab,2,4},clr1,invert}

-define(FUSE(Map, Name),
    expect:fuse(Matrix, lists:map(fun Map/1, Controls), Name)
).

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
    iterate:labs(Device, Pins0, 6,
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
    {Clk1, Clk2, D, Q, Clr1, Clr2} = Pins,
    Common = {LAB, Clk1, Clk2, D, Q},
    [
        source_never(Device, Common, never, Gclk3),
        source_global(Device, Common, g3not_g0, Gclk3, Gclk0, <<"!">>, <<>>),
        source_global(Device, Common, g3_g0not, Gclk3, Gclk0, <<>>, <<"!">>),
        source_global(Device, Common, g3_g1, Gclk3, Gclk1, <<>>, <<>>),
        source_global(Device, Common, g3_g2, Gclk3, Gclk2, <<>>, <<>>),
        source_global(Device, Common, g2_g0, Gclk2, Gclk0, <<>>, <<>>),
        source_global(Device, Common, g2_g1, Gclk2, Gclk1, <<>>, <<>>),
        source_global(Device, Common, g2_g3, Gclk2, Gclk3, <<>>, <<>>),
        source_global(Device, Common, g1_g0, Gclk1, Gclk0, <<>>, <<>>),
        source_global(Device, Common, g1_g2, Gclk1, Gclk2, <<>>, <<>>),
        source_global(Device, Common, g1_g3, Gclk1, Gclk3, <<>>, <<>>),
        source_global(Device, Common, g0_g1, Gclk0, Gclk1, <<>>, <<>>),
        source_global(Device, Common, g0_g2, Gclk0, Gclk2, <<>>, <<>>),
        source_global(Device, Common, g0_g3, Gclk0, Gclk3, <<>>, <<>>),
        source_local(Device, Common, l7_l6, Clr1, 7, Clr2, 6),
        source_local(Device, Common, l8_l9, Clr1, 8, Clr2, 9)
    ].

%%--------------------------------------------------------------------

experiments(Device, LAB, Experiments0) ->
    io:format(" ==> ~p ~p~n", [Device, LAB]),
    Minimal = {minimal, density:minimal_fuses(Device), #{signals => #{}}},
    Experiments = [Minimal | Experiments0],
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{iob, _, _}, _}) -> true;
        ({{iob, _, _}, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{ioc, _, _, _}, _, _, _}) -> true;
        ({{lab, _, _}, s_load, _}) -> true;
        ({{lc, _, _, _}, clk}) -> true;
        ({_, lut, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:port_routing(a_clr, Experiments),
    %
    {ok, Sels0} = matrix:find_fuse(Matrix0, {lab:lc(LAB, 0), a_clr1}),
    {ok, Sels1} = matrix:find_fuse(Matrix0, {lab:lc(LAB, 1), a_clr1}),
    {ok, Sels2} = matrix:find_fuse(Matrix0, {lab:lc(LAB, 2), a_clr1}),
    %io:format("LC 0 a-clr1 = ~w~n", [Sels0]),
    %io:format("LC 1 a-clr1 = ~w~n", [Sels1]),
    %io:format("LC 2 a-clr1 = ~w~n", [Sels2]),
    expect:fuse(Matrix, Sels0, {lab:lc(LAB, 0), a_clr1}),
    expect:fuse(Matrix, Sels1, {lab:lc(LAB, 1), a_clr1}),
    expect:fuse(Matrix, Sels2, {lab:lc(LAB, 2), a_clr1}),
    Controls = clr_controls(Experiments, Sels0, Sels1, Sels2),
    %
    ?FUSE(clr_global0, {LAB, a_clr, global0}),
    ?FUSE(clr_global1, {LAB, a_clr, global1}),
    ?FUSE(clr_global2, {LAB, a_clr, global2}),
    ?FUSE(clr_global3, {LAB, a_clr, global3}),
    %
    ?FUSE(clr1_off,     {LAB, a_clr1, off}),
    ?FUSE(clr1_global,  {LAB, a_clr1, global}),
    ?FUSE(clr1_control, {LAB, a_clr1, control_5_not_4}),
    ?FUSE(clr1_invert,  {LAB, a_clr1, invert}),
    %
    ?FUSE(clr2_off,     {LAB, a_clr2, off}),
    ?FUSE(clr2_global,  {LAB, a_clr2, global}),
    ?FUSE(clr2_control, {LAB, a_clr2, control_5_not_4}),
    ?FUSE(clr2_invert,  {LAB, a_clr2, invert}),
    ok.

%%--------------------------------------------------------------------

clr_controls(Experiments, Sels0, Sels1, Sels2) ->
    Sels = lists:zip3(Sels0, Sels1, Sels2),
    lists:map(fun clr_controls_experiment/1, lists:zip(Experiments, Sels)).

%%--------------------------------------------------------------------

clr_controls_experiment({{Name, _, #{signals := Signals}}, Sels}) ->
    {Invert1, Invert2} = clr_controls_invert(Name, Sels),
    Zero = {undefined, undefined, Sels},
    case maps:fold(fun clr_controls_signal/3, Zero, Signals) of
        {undefined, undefined, _} ->
            {undefined,
             undefined};

        {undefined, Source2, _} ->
            {undefined,
             {Source2, Invert2}};

        {Source1, undefined, _} ->
            {{Source1, Invert1},
             undefined};

        {Source1, Source2, _} ->
            {{Source1, Invert1},
             {Source2, Invert2}}
    end.

%%--------------------------------------------------------------------

clr_controls_invert(g3not_g0, {0, 1, 1}) -> {invert, normal};
clr_controls_invert(g3not_g0, {1, 0, 0}) -> {normal, invert};
clr_controls_invert(g3_g0not, {0, 1, 1}) -> {normal, invert};
clr_controls_invert(g3_g0not, {1, 0, 0}) -> {invert, normal};
clr_controls_invert(l7_l6, _) -> {invert, invert};
clr_controls_invert(l8_l9, _) -> {invert, invert};
clr_controls_invert(_, _) -> {normal, normal}.

%%--------------------------------------------------------------------

clr_controls_signal(_, #{dests := Dests}, Acc) ->
    lists:foldl(fun clr_controls_dest/2, Acc, Dests).

%%--------------------------------------------------------------------

clr_controls_dest(#{lc := LC, port := a_clr, route := Route}, Acc0) ->
    {lc, _, _, N} = LC,
    Control = clr_controls_route(Route),
    {Control1, Control2, Sels} = Acc0,
    case clr_controls_sel(N, Sels) of
        clr1 when Control1 =:= undefined ->
            {Control, Control2, Sels};

        clr2 when Control2 =:= undefined ->
            {Control1, Control, Sels};

        clr1 ->
            Control1 = Control,
            Acc0;

        clr2 ->
            Control2 = Control,
            Acc0
    end;
clr_controls_dest(_, Acc) ->
    Acc.

%%--------------------------------------------------------------------

clr_controls_route([{lab_clk, _, _, 0, N} | _]) ->
    {global, N};
clr_controls_route([{lab_control_mux, _, _, 0, N} | _]) ->
    {local, N};
clr_controls_route(_) ->
    undefined.

%%--------------------------------------------------------------------

clr_controls_sel(0, {Sel, _, _}) -> clr_controls_sel(Sel);
clr_controls_sel(1, {_, Sel, _}) -> clr_controls_sel(Sel);
clr_controls_sel(2, {_, _, Sel}) -> clr_controls_sel(Sel).

%%--------------------------------------------------------------------

clr_controls_sel(0) -> clr1;
clr_controls_sel(1) -> clr2.

%%--------------------------------------------------------------------

clr_global0({{{global, 0},_}, _}) -> 0;
clr_global0({_, {{global, 0},_}}) -> 0;
clr_global0(_) -> 1.

%%--------------------------------------------------------------------

clr_global1({{{global, 1},_}, _}) -> 0;
clr_global1({_, {{global, 1},_}}) -> 0;
clr_global1(_) -> 1.

%%--------------------------------------------------------------------

clr_global2({{{global, 2},_}, _}) -> 0;
clr_global2({_, {{global, 2},_}}) -> 0;
clr_global2(_) -> 1.

%%--------------------------------------------------------------------

clr_global3({{{global, 3},_}, _}) -> 0;
clr_global3({_, {{global, 3},_}}) -> 0;
clr_global3(_) -> 1.

%%--------------------------------------------------------------------

clr1_off({undefined, _}) -> 0;
clr1_off(_) -> 1.

%%--------------------------------------------------------------------

clr2_off({_, undefined}) -> 0;
clr2_off(_) -> 1.

%%--------------------------------------------------------------------

clr1_control({{{local, 5}, _}, _}) -> 0;
clr1_control(_) -> 1.

%%--------------------------------------------------------------------

clr2_control({_, {{local, 5}, _}}) -> 0;
clr2_control(_) -> 1.

%%--------------------------------------------------------------------

clr1_global({undefined, _}) -> 0;
clr1_global({{{global, _},_}, _}) -> 0;
clr1_global(_) -> 1.

%%--------------------------------------------------------------------

clr2_global({_, undefined}) -> 0;
clr2_global({_, {{global, _},_}}) -> 0;
clr2_global(_) -> 1.

%%--------------------------------------------------------------------

clr1_invert({{_, invert}, _}) -> 0;
clr1_invert(_) -> 1.

%%--------------------------------------------------------------------

clr2_invert({_, {_, invert}}) -> 0;
clr2_invert(_) -> 1.

%%--------------------------------------------------------------------

source_never(Device, {LAB, Clk1, Clk2, D, Q}, Name, Clr1) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, clk2, Clk2},
            {global_clock, clk1, false},
            {global_clock, clk2, false},
            {location, clr1, Clr1},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff2, lab:lc(LAB, 1)},
            {location, ff3, lab:lc(LAB, 2)},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire clk2,\n"
            "  input wire clr1,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire chain1;\n"
            "  wire chain2;\n"
            "  dff ff1 (\n"
            "    .d(d),\n"
            "    .clk(clk1),\n"
            "    .clrn(clr1),\n"
            "    .prn(1),\n"
            "    .q(chain1)\n"
            "  );\n"
            "  dff ff2 (\n"
            "    .d(chain1),\n"
            "    .clk(clk2),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(chain2)\n"
            "  );\n"
            "  dff ff3 (\n"
            "    .d(chain2),\n"
            "    .clk(clk1),\n"
            "    .clrn(clr1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_global(Device, {LAB, Clk1, Clk2, D, Q}, Name, Clr1, Clr2, Not1, Not2) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, clk2, Clk2},
            {global_clock, clk1, false},
            {global_clock, clk2, false},
            {location, clr1, Clr1},
            {location, clr2, Clr2},
            {global_clock, clr1, true},
            {global_clock, clr2, true},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff2, lab:lc(LAB, 1)},
            {location, ff3, lab:lc(LAB, 2)},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire clk2,\n"
            "  input wire clr1,\n"
            "  input wire clr2,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire chain1;\n"
            "  wire chain2;\n"
            "  dff ff1 (\n"
            "    .d(d),\n"
            "    .clk(clk1),\n"
            "    .clrn(", Not1/binary, "clr1),\n"
            "    .prn(1),\n"
            "    .q(chain1)\n"
            "  );\n"
            "  dff ff2 (\n"
            "    .d(chain1),\n"
            "    .clk(clk2),\n"
            "    .clrn(", Not2/binary, "clr2),\n"
            "    .prn(1),\n"
            "    .q(chain2)\n"
            "  );\n"
            "  dff ff3 (\n"
            "    .d(chain2),\n"
            "    .clk(clk2),\n"
            "    .clrn(", Not2/binary, "clr2),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_local(Device, {LAB, Clk1, Clk2, D, Q}, Name, Clr1, N1, Clr2, N2) ->
    #{
        title => Name,
        device => Device,
        settings => [
            {location, clk1, Clk1},
            {location, clk2, Clk2},
            {global_clock, clk1, false},
            {global_clock, clk2, false},
            {location, clr1, Clr1},
            {location, clr2, Clr2},
            {global_clock, clr1, false},
            {global_clock, clr2, false},
            {location, cc1, lab:lc(LAB, N1)},
            {location, cc2, lab:lc(LAB, N2)},
            {location, ff1, lab:lc(LAB, 0)},
            {location, ff2, lab:lc(LAB, 1)},
            {location, ff3, lab:lc(LAB, 2)},
            {location, d, D},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk1,\n"
            "  input wire clk2,\n"
            "  input wire clr1,\n"
            "  input wire clr2,\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  wire chain1;\n"
            "  wire chain2;\n"
            "  wire c1;\n"
            "  wire c2;\n"
            "  dff ff1 (\n"
            "    .d(d),\n"
            "    .clk(clk1),\n"
            "    .clrn(c1),\n"
            "    .prn(1),\n"
            "    .q(chain1)\n"
            "  );\n"
            "  dff ff2 (\n"
            "    .d(chain1),\n"
            "    .clk(clk2),\n"
            "    .clrn(c2),\n"
            "    .prn(1),\n"
            "    .q(chain2)\n"
            "  );\n"
            "  dff ff3 (\n"
            "    .d(chain2),\n"
            "    .clk(clk2),\n"
            "    .clrn(c1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "  lcell cc1 (\n"
            "    .in(clr1),\n"
            "    .out(c1)\n"
            "  );\n"
            "  lcell cc2 (\n"
            "    .in(clr2),\n"
            "    .out(c2)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

