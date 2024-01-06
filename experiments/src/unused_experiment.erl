-module(unused_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    [{IOB, _} | _] = lists:sort(device:iobs(Device)),
    [In, Out | _] = lists:sort([
        {IOC, Pin}
        ||
        {Pin, IOC} <- Device:iocs(),
        ioc:iob(IOC) =:= IOB
    ]),
    io:format(" => ~s ~w -> ~w~n", [Device, source:ioc(In), source:ioc(Out)]),
    {ok, Experiments} = experiment:compile_to_fuses([
        source:in_out(Device, Unused, [{unused_pins, Unused}], In, Out)
        ||
        Unused <- setting:unused_pins()
    ]),
    Matrix = matrix:build_with_map(Device, Experiments),
    %matrix:print(Matrix),
    %
    Inputs = matrix:pattern_is(Matrix, [0,0,0,1,1]),
    BusHolds = matrix:pattern_is(Matrix, [1,0,1,1,1]),
    WeakPullUps = matrix:pattern_is(Matrix, [1,1,0,1,1]),
    %
    io:format("~b pins~n", [length(Device:pins())]),
    io:format("~b inputs~n", [length(Inputs)]),
    io:format("~b bus holds~n", [length(BusHolds)]),
    io:format("~b weak pull-ups~n", [length(WeakPullUps)]),
    Total = matrix:fuse_count(Matrix),
    io:format("~b fuses~n", [Total]),
    Total = length(Inputs) + length(BusHolds) + length(WeakPullUps),
    %
    lists:foreach(fun check_input/1, Inputs),
    lists:foreach(fun check_bus_hold/1, BusHolds),
    lists:foreach(fun check_weak_pull_up/1, WeakPullUps),
    %
    [ missing_bus_hold(IOC, BusHolds) || {_, IOC} <- Device:iocs() ],
    ok.

%%--------------------------------------------------------------------

check_input({_, {{ioc, _, _, _}, input}}) -> ok.

%%--------------------------------------------------------------------

check_bus_hold({_, {{ioc, _, _, _}, bus_hold}}) -> ok.

%%--------------------------------------------------------------------

check_weak_pull_up({_, {{ioc, _, _, _}, weak_pull_up}}) -> ok.

%%--------------------------------------------------------------------

missing_bus_hold(IOC, []) ->
    io:format("missing ~p bus hold~n", [IOC]);
missing_bus_hold(IOC, [{_, {IOC, bus_hold}} | _]) ->
    ok;
missing_bus_hold(IOC, [_ | BusHolds]) ->
    missing_bus_hold(IOC, BusHolds).

