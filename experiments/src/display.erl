-module(display).

-export([control_routing/1]).
-export([port_routing/2]).

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
control_routing_dest(_, Routing) ->
    Routing.

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

