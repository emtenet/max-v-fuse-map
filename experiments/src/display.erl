-module(display).

-export([control_routing/0]).

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

control_routing_dest(#{lc := LC, port := Port, route := Route}, Routing) ->
    case Route of
        [{lab_clk, _, _, 0, N} | _] ->
            [{LC, Port, {global, N}} | Routing];

        [{lab_control_mux, _, _, 0, N}, From | _] ->
            [{LC, Port, {control, N, From}} | Routing];

        [From | _] when Port =:= s_data ->
            [{LC, Port, From} | Routing];

        _ ->
            Routing
    end;
control_routing_dest(_, Routing) ->
    Routing.

