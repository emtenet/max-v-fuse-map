-module(display).

-export([control_routing/1]).
-export([port_routing/2]).
-export([routing/2]).

%%====================================================================
%% control_routing
%%====================================================================

control_routing(Experiments) ->
    lists:foreach(fun control_routing_experiment/1, Experiments).

%%--------------------------------------------------------------------

control_routing_experiment({Name, _, #{signals := Signals}}) ->
    io:format(" --> ~p~n", [Name]),
    Routing = maps:fold(fun control_routing_signal/3, [], Signals),
    [
        io:format("  ~12w ~6w <- ~p~n", [LC, Port, Route])
        ||
        {LC, Port, Route} <- lists:sort(Routing)
    ].

%%--------------------------------------------------------------------

control_routing_signal(_, #{dests := Dests}, Routing) ->
    lists:foldl(fun control_routing_dest/2, Routing, Dests).

%%--------------------------------------------------------------------

control_routing_dest(D = #{lc := LC, port := Port, route := Route}, Routing) ->
    case Route of
        [{lab_clk, _, _, 0, N} | _] ->
            [{LC, Port, {global, N}} | Routing];

        [{lab_control_mux, _, _, 0, N}, From | _] ->
            [{LC, Port, {control, N, From}} | Routing];

        [From | _] when Port =:= s_data ->
            #{route_port := data_c} = D,
            [{LC, Port, From} | Routing];

        [From | _] ->
            #{route_port := Data} = D,
            [{LC, Data, From} | Routing]
    end;
control_routing_dest(#{ioc := IOC, route := Route}, Routing) ->
    case Route of
        [{io_data_out, _, _, _, 0}, From | _] ->
            [{IOC, output, From} | Routing];

        [{io_oe, _, _, _, 0}, From | _] ->
            [{IOC, enable, From} | Routing]
    end.

%%====================================================================
%% port_routing
%%====================================================================

port_routing(Port, Experiments) ->
    lists:foreach(fun (Experiment) ->
        port_routing_experiment(Port, Experiment)
    end, Experiments).

%%--------------------------------------------------------------------

port_routing_experiment(Port, {Name, _, #{signals := Signals}}) ->
    io:format(" --> ~p~n", [Name]),
    Routing = maps:fold(fun (K, V, Acc) ->
        port_routing_signal(Port, K, V, Acc)
    end, [], Signals),
    [
        io:format("  ~12w ~6w <- ~p~n", [LC, Port, Route])
        ||
        {LC, Route} <- lists:sort(Routing)
    ].

%%--------------------------------------------------------------------

port_routing_signal(Port, _, #{dests := Dests}, Routing) ->
    lists:foldl(fun (V, Acc) ->
        port_routing_dest(Port, V, Acc)
    end, Routing, Dests).

%%--------------------------------------------------------------------

port_routing_dest(Port, #{lc := LC, port := Port, route := Route}, Routing) ->
    case Route of
        [{lab_clk, _, _, 0, N} | _] ->
            [{LC, {global, N}} | Routing];

        [{lab_control_mux, _, _, 0, N}, From | _] ->
            [{LC, {control, N, From}} | Routing];

        [From | _] ->
            [{LC, From} | Routing]
    end;
port_routing_dest(_, _, Routing) ->
    Routing.

%%====================================================================
%% routing
%%====================================================================

routing(Experiments, Density) ->
    lists:foreach(fun (Experiment) ->
        routing_experiment(Experiment, Density)
    end, Experiments).

%%--------------------------------------------------------------------

routing_experiment({Name, Fuses, #{signals := Signals}}, Density) ->
    io:format(" --> ~p~n", [Name]),
    Routing0 = {#{}, #{}},
    Routing = maps:fold(fun routing_signal/3, Routing0, Signals),
    lists:foreach(fun (Cell) ->
        routing_cell(<<"IO">>, Cell, Fuses, Density)
    end, lists:sort(maps:to_list(element(2, Routing)))),
    lists:foreach(fun (Cell) ->
        routing_cell(<<"LC">>, Cell, Fuses, Density)
    end, lists:sort(maps:to_list(element(1, Routing)))),
    ok.

%%--------------------------------------------------------------------

routing_signal(From, #{dests := Dests}, Routing) ->
    lists:foldl(fun (Dest, Acc) ->
        routing_dest(From, Dest, Acc)
    end, Routing, Dests).

%%--------------------------------------------------------------------

routing_dest(From, Dest, Routing) ->
    case Dest of
        #{name := Name, lc := LC, route := Route, route_port := Port} ->
            routing_add_lc(Name, LC, Route, Port, From, Routing);

        #{name := Name, lc := LC, route := Route, port := Port} ->
            routing_add_lc(Name, LC, Route, Port, From, Routing);

        #{name := Name, ioc := IO, route := Route, port := Port} ->
            routing_add_io(Name, IO, Route, Port, From, Routing)
    end.

%%--------------------------------------------------------------------

routing_add_lc(Name, LC, Route, Port, From, {LCs, IOs}) ->
    case LCs of
        #{Name := Existing} ->
            {LC, Ports} = Existing,
            {LCs#{Name => {LC, Ports#{Port => {From, Route}}}}, IOs};

        _ ->
            {LCs#{Name => {LC, #{Port => {From, Route}}}}, IOs}
    end.

%%--------------------------------------------------------------------

routing_add_io(Name, IO, Route, Port, From, {LCs, IOs}) ->
    case IOs of
        #{Name := Existing} ->
            {IO, Ports} = Existing,
            {LCs, IOs#{Name => {IO, Ports#{Port => {From, Route}}}}};

        _ ->
            {LCs, IOs#{Name => {IO, #{Port => {From, Route}}}}}
    end.

%%--------------------------------------------------------------------

routing_cell(Type, {Name, {Cell, Ports}}, Fuses, Density) ->
    io:format("~s ~w ~s~n", [Type, Cell, Name]),
    lists:foreach(fun routing_port/1, lists:sort(maps:to_list(Ports))),
    routing_lut(Cell, Fuses, Density),
    io:format("~n", []).

%%--------------------------------------------------------------------

routing_port({Port, {From, Route}}) when byte_size(From) > 8 ->
    io:format("  ~8s <- ~s~n", [Port, From]),
    io:format("              ~w~n", [Route]);
routing_port({Port, {From, Route}}) ->
    io:format("  ~8s <- ~s ~w~n", [Port, From, Route]).

%%--------------------------------------------------------------------

routing_lut(LC = {lc, _, _, _}, Fuses, Density) ->
    LUT0 = routing_lut_fuses(LC, Density),
    LUT = lists:foldl(fun routing_lut_reduce/2, LUT0, Fuses),
    io:format("  ~p~n", [maps:values(LUT)]),
    ok;
routing_lut(_, _, _) ->
    ok.

%%--------------------------------------------------------------------

routing_lut_fuses(LC, Density) ->
    maps:from_list(lists:map(fun (LUT) ->
        routing_lut_fuse(LC, LUT, Density)
    end, [
        a0b0c0d0,
        a0b0c0d1,
        a0b0c1d0,
        a0b0c1d1,
        a0b1c0d0,
        a0b1c0d1,
        a0b1c1d0,
        a0b1c1d1,
        a1b0c0d0,
        a1b0c0d1,
        a1b0c1d0,
        a1b0c1d1,
        a1b1c0d0,
        a1b1c0d1,
        a1b1c1d0,
        a1b1c1d1
    ])).

%%--------------------------------------------------------------------

routing_lut_fuse(LC, LUT, Density) ->
    {ok, Fuse} = fuse_map:from_name({LC, lut, LUT}, Density),
    {Fuse, LUT}.

%%--------------------------------------------------------------------

routing_lut_reduce(Fuse, LUT) ->
    maps:remove(Fuse, LUT).

