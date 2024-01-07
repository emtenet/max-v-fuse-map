-module(io_modes_experiment).

-export([run/0]).

% This experiment is looking at the different I/O modes:
%
%  * input
%  * output
%  * tri-state
%  * bidirectional
%
% Found fuses:
%  * input_off - when in output (only) mode
%  * output_invert - when the output is inverted
%  * enable_invert - when the output enable is inverted

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
    case lists:reverse(Pins) of
        [Other, Extra, _ | _] ->
            pins(Density, Device, Pins, Other, Extra);

        _ ->
            ok
    end.

%%--------------------------------------------------------------------

pins(_, _, [], _, _) ->
    ok;
pins(Density, Device, [Pin | Pins], Other, Extra) ->
    pin(Density, Device, Pin, Other, Extra),
    pins(Density, Device, Pins, Pin, Other).

%%--------------------------------------------------------------------

pin(_Density, Device, O, I, OE) ->
    io:format(" => ~s ~w (oe ~w)~n", [Device, source:ioc(O), source:ioc(OE)]),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source:in_out(Device, out, I, O),
        source:in_oe_out(Device, out_en, I, OE, O),
        source:in_oe_out(Device, out_en_inv, I, {inv, OE}, O),
        source:in_oe_out(Device, out_en_0, I, 0, O),
        source:in_oe_out(Device, out_en_1, I, 1, O),
        source:in_out(Device, out_1, 1, O),
        source:in_out(Device, out_0, 0, O),
        source:in_out(Device, out_inv, {inv, I}, O),
        source:in_out(Device, in, [], O, I),
        bidir(Device, bidir, [], OE, O, I)
    ]),
    %io:format("~p~n", [[{Title, RCF} || {Title, _, RCF} <- Experiments]]),
    %throw(stop),
    Matrix0 = matrix:build_with_map(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{ioc, _, _, _}, input_delay}) -> true;
        ({{ioc, _, _, _}, schmitt_trigger}) -> true;
        ({{ioc, _, _, _}, slow_slew_rate}) -> true;
        (_) -> false
    end),
    %matrix:print(Matrix),
    %
    % think of a 1 when an _input_
    fuse(Matrix, [0,0,0,0,0,0,0,0,1,1], O, input_off),
    fuse(Matrix, [0,1,1,0,0,0,0,0,0,1], OE,input_off),
    fuse(Matrix, [1,1,1,1,1,0,0,1,0,0], I, input_off),
    % think of a 1 when _NOT_ inverted
    fuse(Matrix, [1,1,1,1,1,1,0,0,0,1], O, output_invert),
    fuse(Matrix, [0,0,0,0,0,0,0,0,1,1], I, output_invert),
    % think of a 1 when _NOT_ inverted
    fuse(Matrix, [1,1,0,0,1,1,1,1,0,1], O, enable_invert),
    fuse(Matrix, [1,0,0,1,1,1,1,1,1,0], OE,enable_invert),
    fuse(Matrix, [0,0,0,0,0,1,1,0,1,1], I, enable_invert),
    ok.

%%--------------------------------------------------------------------

fuse(Matrix, Pattern, Pin, Name) ->
    IOC = source:ioc(Pin),
    Fuses = matrix:pattern_is(Matrix, Pattern),
    case Fuses of
        [{_, {IOC, Name}}] ->
            ok;

        _ ->
            case lists:keyfind({IOC, Name}, 2, Fuses) of
                {_, _} ->
                    ok;

                false ->
                    io:format("Expecting:~n  ~w~n", [{IOC, Name}]),
                    io:format("Candidates:~n  ~p~n", [Fuses]),
                    throw(stop)
            end
    end.

%%--------------------------------------------------------------------

bidir(Device, Title, Settings, OE, IO, O) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, oe, source:pin(OE)},
            {location, io, source:pin(IO)},
            {location, o, source:pin(O)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire oe,\n"
            "  inout wire io,\n"
            "  output wire o\n"
            ");\n"
            "  alt_iobuf pad (.i(1), .oe(oe), .io(io), .o(o));\n"
            "endmodule\n"
        >>
    }.

