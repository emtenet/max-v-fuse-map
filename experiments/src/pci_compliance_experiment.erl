-module(pci_compliance_experiment).

-export([run/0]).

% This experiment is looking for the fuses to enable PCI complaince.
%
% PCI compliance is only available on select devices
% and only in I/O Bank 3 (the right bank).

%%====================================================================
%% run
%%====================================================================

run() ->
    density(max_v_1270z),
    density(max_v_2210z),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    io:format(" => ~s~n", [Density]),
    Device = density:largest_device(Density),
    IOBs = lists:sort(device:iobs(Device)),
    lists:foreach(fun (IOB) -> block(Density, Device, IOB) end, IOBs).

%%--------------------------------------------------------------------

block(Density, Device, {IOB, _}) ->
    case density:pci_compliant(IOB, Density) of
        true ->
            Pins = lists:sort(fun source:sort_by_ioc/2, [
                Pin
                ||
                Pin <- Device:iocs(),
                ioc:iob(source:ioc(Pin)) =:= IOB
            ]),
            Last = lists:last(Pins),
            pins(Density, Device, Pins, Last);

        false ->
            ok
    end.

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
        source:in_out(Device, pci_input, [
            {pci_compliance, i, true}
        ], Pin, Other),
        source:in_out(Device, pci_output, [
            {pci_compliance, o, true}
        ], Other, Pin),
        source:in_out(Device, output, [], Other, Pin)
    ]),
    Matrix = matrix:build_with_map(Device, Experiments),
    %matrix:print(Matrix),
    %
    fuse(Matrix, [1,0,0,1], IOC, pci_compliance),
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

