-module(unused_experiment).

-export([run/0]).

% This experiment uses the `unused_pins` setting to change all
% unused pins:
%
%  * input / output
%  * bus holl - on / off
%  * weak pull up - on / off
%
% Three sets fuses can then be identified but not which fuse
% is connected to which IOC.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    [{IOB, _} | _] = device:iobs(Device),
    [In, Out | _] = device:iocs(Device, IOB),
    io:format(" => ~s ~w -> ~w~n", [Device, source:ioc(In), source:ioc(Out)]),
    {ok, Experiments} = experiment:compile_to_fuses([
        source:in_out(Device, Unused, [{unused_pins, Unused}], In, Out)
        ||
        Unused <- setting:unused_pins()
    ]),
    Matrix = matrix:build(Device, Experiments),
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
    ok.

%%--------------------------------------------------------------------

check_input({_, {{ioc, _, _, _}, input}}) -> ok.

%%--------------------------------------------------------------------

check_bus_hold({_, {{ioc, _, _, _}, bus_hold}}) -> ok.

%%--------------------------------------------------------------------

check_weak_pull_up({_, {{ioc, _, _, _}, weak_pull_up}}) -> ok.

