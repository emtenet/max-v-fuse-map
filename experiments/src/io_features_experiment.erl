-module(io_features_experiment).

-export([run/0]).

% This experiment is looking for the fuses for each of the I/O features:
%
%  * bus-hold
%  * weak pull-up
%  * schmitt trigger
%  * input delay
%  * fast slew rate
%  * open drain
%  * low current strength
%
% So far there are too many fuses that could represent the I/O mode
% of input or output...
%
% There appear to be two current strength fuses but thet always are
% set together. i.e. only 2 combinations of 4.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    io:format(" => ~s~n", [Density]),
    Device = density:largest_device(Density),
    IOBs = device:iobs(Device),
    lists:foreach(fun (IOB) -> block(Density, Device, IOB) end, IOBs).

%%--------------------------------------------------------------------

block(Density, Device, {IOB, _}) ->
    Pins = device:iocs(Device, IOB),
    Last = lists:last(Pins),
    pins(Density, Device, Pins, Last).

%%--------------------------------------------------------------------

pins(_, _, [], _) ->
    ok;
pins(Density, Device, [Pin | Pins], Other) ->
    pin(Density, Device, Pin, Other),
    pins(Density, Device, Pins, Pin).

%%--------------------------------------------------------------------

pin(_Density, Device, Pin, Other) ->
    IOC = source:ioc(Pin),
    io:format(" => ~s ~w~n", [Device, IOC]),
    {ok, Experiments} = experiment:compile_to_fuses([
        source:in_out(Device, input, [], Pin, Other),
        source:in_out(Device, bus_hold, [
            {bus_hold, i, true}
        ], Pin, Other),
        source:in_out(Device, weak_pull_up, [
            {weak_pull_up, i, true}
        ], Pin, Other),
        source:in_out(Device, schmitt_trigger, [
            {io_standard, i, v3_3_schmitt_trigger},
            {io_standard, o, v3_3_ttl}
        ], Pin, Other),
        source:in_out(Device, no_delay, [
            {input_delay, i, false}
        ], Pin, Other),
        source:in_out(Device, output, [], Other, Pin),
        source:in_out(Device, fast_slew_rate, [
            {slow_slew_rate, o, true}
        ], Other, Pin),
        source:open_drain(Device, open_drain, Other, Pin),
        source:in_out(Device, current_strength, [
            {current_strength, o, minimum}
        ], Other, Pin)
    ]),
    Matrix = matrix:build_with_map(Device, Experiments),
    %matrix:print(Matrix),
    %
    % in/out:     i i i i i o o o o
    fuse(Matrix, [1,0,1,1,1,1,1,1,1], IOC, bus_hold),
    fuse(Matrix, [1,1,0,1,1,1,1,1,1], IOC, weak_pull_up),
    fuse(Matrix, [1,1,1,0,1,0,0,0,0], IOC, schmitt_trigger),
    fuse(Matrix, [0,0,0,0,1,1,1,1,1], IOC, input_delay),
    fuse(Matrix, [0,0,0,0,0,0,1,0,0], IOC, fast_slew_rate),
    fuse(Matrix, [1,1,1,1,1,1,1,0,1], IOC, open_drain),
    fuse(Matrix, [1,1,1,1,1,1,1,1,0], IOC, low_current_0, low_current_1),
    ok.

%%--------------------------------------------------------------------

fuse(Matrix, Pattern, IOC, Name) ->
    Fuses = matrix:pattern_is(Matrix, Pattern),
    case Fuses of
        [{_, {IOC, Name}}] ->
            ok;

        _ ->
            io:format("Expecting:~n  ~w~n", [{IOC, Name}]),
            io:format("Candidates:~n  ~p~n", [Fuses]),
            throw(stop)
    end.

%%--------------------------------------------------------------------

fuse(Matrix, Pattern, IOC, Name1, Name2) ->
    Fuses = matrix:pattern_is(Matrix, Pattern),
    case Fuses of
        [{_, {IOC, Name1}}, {_, {IOC, Name2}}] ->
            ok;

        [{_, {IOC, Name2}}, {_, {IOC, Name1}}] ->
            ok;

        _ ->
            io:format("Expecting:~n  ~w~n  ~w~n", [{IOC, Name1}, {IOC, Name2}]),
            io:format("Candidates:~n  ~p~n", [Fuses]),
            throw(stop)
    end.

