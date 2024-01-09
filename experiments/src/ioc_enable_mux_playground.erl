-module(ioc_enable_mux_playground).

-export([run/0]).

% This playground is the supporting detail for `ioc_enable_mux_theory`.
%
% How are IOB local_interconnects muxed into an IOC output enables?
%
% Although this is was a playground, it has been run for all IOCs
% to generate data for `ioc_enable_mux_theory` to process.

%-define(PLAYGROUND, true).

%%====================================================================
%% run
%%====================================================================

-ifdef(PLAYGROUND).
run() ->
    %block(max_v_240z, max_v_240z_t100, {iob, 8, 2}, {lab, 7, 2}),
    block(max_v_240z, max_v_240z_t100, {iob, 1, 3}, {lab, 2, 3}),
    %block(max_v_240z, max_v_240z_t100, {iob, 4, 0}, {lab, 4, 1}),
    %block(max_v_240z, max_v_240z_t100, {iob, 3, 5}, {lab, 3, 4}),
    %block(max_v_570z, max_v_570z_f256, {iob, 13, 2}, {lab, 12, 2}),
    %block(max_v_570z, max_v_570z_f256, {iob, 0, 5}, {lab, 1, 5}),
    %block(max_v_570z, max_v_570z_f256, {iob, 11, 0}, {lab, 11, 1}),
    %block(max_v_570z, max_v_570z_f256, {iob, 5, 3}, {lab, 5, 4}),
    %block(max_v_570z, max_v_570z_f256, {iob, 6, 8}, {lab, 6, 7}),
    ok.
-else.
run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    lists:foreach(fun ({IOB, LAB}) ->
        block(Density, Device, IOB, LAB)
    end, density:iobs(Density)).
-endif.

%%--------------------------------------------------------------------

block(Density, Device, IOB, LAB) ->
    lists:foreach(fun (Pin) ->
        pin(Density, Device, Pin, LAB)
    end, device:iocs(Device, IOB)).

%%--------------------------------------------------------------------

pin(Density, Device, {IOC, Pin}, LAB) ->
    io:format(" => ~s ~p ~p~n", [Device, IOC, LAB]),
    Globals = device:gclk_pins(Device),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source_global(Device, Pin, Global)
        ||
        Global <- Globals,
        Global =/= Pin
    ] ++ [
        source(Device, Pin, lab:lc(LAB, N))
        ||
        N <- lists:seq(0, 9)
    ]),
    playground(Density, IOC, Experiments).

%%--------------------------------------------------------------------

-ifdef(PLAYGROUND).
playground(Density, IOC, Experiments) ->
    Matrix = matrix:build(Density, Experiments),
    %matrix:print(Matrix),
    {Keys, Routes} = routes(Experiments),
    print_keys(Keys),
    ioc_fuses(IOC, Keys, Routes, Matrix),
    ok.
-else.
playground(_, _, _) ->
    ok.
-endif.

%%====================================================================
%% source
%%====================================================================

source(Device, Pin, LC) ->
    #{
        title => {Pin, LC},
        device => Device,
        settings => [
            {location, lut, LC},
            {location, o, Pin}
        ],
        verilog => <<
            "module experiment (\n"
            "  output wire o\n"
            ");\n"
            "  wire oe;\n"
            "  lcell lut (\n"
            "    .in(1),\n"
            "    .out(oe)\n"
            "  );\n",
            "  alt_outbuf_tri ioc (\n"
            "    .i(1),\n"
            "    .o(o),\n"
            "    .oe(oe)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_global(Device, Pin, OE) ->
    #{
        title => {Pin, OE},
        device => Device,
        settings => [
            {global_clock, oe, true},
            {location, oe, OE},
            {location, o, Pin}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire oe,\n"
            "  output wire o\n"
            ");\n"
            "  alt_outbuf_tri ioc (\n"
            "    .i(1),\n"
            "    .o(o),\n"
            "    .oe(oe)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%====================================================================
%% routes
%%====================================================================

-ifdef(PLAYGROUND).
routes(Experiments) ->
    Init = {#{}, #{}, $A},
    {RouteToKey, KeyToRoute, _} =
        lists:foldl(fun route_keys_experiment/2, Init, Experiments),
    Keys = lists:sort(maps:to_list(KeyToRoute)),
    Routes = lists:map(
        fun (E) -> route_experiment(E, RouteToKey) end,
        Experiments
    ),
    {Keys, Routes}.

%%--------------------------------------------------------------------

route_keys_experiment({_, _, #{signals := Signals}}, Acc) ->
    maps:fold(fun route_keys_signal/3, Acc, Signals).

%%--------------------------------------------------------------------

route_keys_signal(_, #{dests := Dests}, Acc0) ->
    lists:foldl(
        fun (Dest, Acc) -> route_keys_dest(Dest, Acc) end,
        Acc0,
        Dests
    ).

%%--------------------------------------------------------------------

route_keys_dest(#{port := oe, route := Route}, Acc) ->
    [OE = {io_oe, _, _, _, _},
     Interconnect = {local_interconnect, _, _, _, _}
     |
     _
    ] = Route,
    route_keys_add([OE, Interconnect], Acc).

%%--------------------------------------------------------------------

route_keys_add(Route, Acc = {ToKey, ToRoute, Key0}) ->
    case ToKey of
        #{Route := _} ->
            Acc;

        _ ->
            Key = <<Key0>>,
            {ToKey#{Route => Key}, ToRoute#{Key => Route}, Key0 + 1}
    end.

%%--------------------------------------------------------------------

route_experiment({_, _, #{signals := Signals}}, RouteToKey) ->
    maps:fold(fun (_, Signal, Acc) ->
         route_signal(Signal, RouteToKey, Acc)
    end, #{}, Signals).

%%--------------------------------------------------------------------

route_signal(#{dests := Dests}, RouteToKey, Init) ->
    lists:foldl(fun (Dest, Acc) ->
        route_dest(Dest, RouteToKey, Acc)
    end, Init, Dests).

%%--------------------------------------------------------------------

route_dest(#{port := oe, route := Route}, RouteToKey, Acc) ->
    [OE = {io_oe, X, Y, N, 0},
     Interconnect = {local_interconnect, _, _, _, _}
     |
     _
    ] = Route,
    route_add({ioc, X, Y, N}, [OE, Interconnect], RouteToKey, Acc).

%%--------------------------------------------------------------------

route_add(IOC, Route, RouteToKey, Acc) ->
    #{Route := Key} = RouteToKey,
    Acc#{IOC => {Key, Route}}.

%%--------------------------------------------------------------------

print_keys(Keys) ->
    [
        io:format("~s: ~w~n", [Key, Route])
        ||
        {Key, Route} <- Keys
    ],
    ok.

%%====================================================================
%% fuses
%%====================================================================

ioc_fuses(IOC, Keys, Routes, Matrix = {matrix, _, AllFuses}) ->
    InitialFuses = [
        {Fuse, #{}, Name}
        ||
        {Fuse, _, Name} <- AllFuses
    ],
    Fuses = lists:foldl(fun ({Key, _}, Acc) ->
        ioc_fuses(IOC, Key, Routes, Matrix, Acc)
    end, InitialFuses, Keys),
    io:format("~n~w:~n        ~s~n", [
        IOC,
        lists:join(<<" ">>, [ Key || {Key, _} <- Keys ])
    ]),
    [
        io:format("~6b |~s| ~w~n", [
            Fuse,
            lists:join(<<"|">>, [
                ioc_fuse_bit(Key, Bits)
                ||
                {Key, _} <- Keys
            ]),
            Name
        ])
        ||
        {Fuse, Bits, Name} <- Fuses
    ],
    ok.

%%--------------------------------------------------------------------

ioc_fuses(IOC, Key, Routes, Matrix, Fuses) ->
    IsKey = [ ioc_key_is(Route, IOC, Key) || Route <- Routes ],
    case lists:member(true, IsKey) of
        true ->
            Pattern0 = [ boolean_to(Is, 0, x) || Is <- IsKey ],
            Pattern1 = [ boolean_to(Is, 1, x) || Is <- IsKey ],
            Fuses0 = matrix:pattern_match(Matrix, Pattern0),
            Fuses1 = matrix:pattern_match(Matrix, Pattern1),
            lists:filtermap(fun ({Fuse, Bits, Name}) ->
                ioc_fuse_bits(Fuse, Key, Fuses0, Fuses1, Bits, Name)
            end, Fuses);

        false ->
            Fuses
    end.

%%--------------------------------------------------------------------

ioc_fuse_bits(Fuse, Key, Fuses0, Fuses1, Bits, Name) ->
    case {lists:keyfind(Fuse, 1, Fuses0), lists:keyfind(Fuse, 1, Fuses1)} of
        {{_, _}, false} ->
            {true, {Fuse, Bits#{Key => 0}, Name}};

        {false, {_, _}} ->
            {true, {Fuse, Bits#{Key => 1}, Name}};

        {false, false} ->
            false
    end.

%%--------------------------------------------------------------------

ioc_fuse_bit(Key, Bits) ->
    case Bits of
        #{Key := 0} -> <<"-">>;
        #{Key := 1} -> <<"#">>;
        _ -> <<" ">>
    end.

%%====================================================================
%% utilities
%%====================================================================

boolean_to(true, T, _) -> T;
boolean_to(false, _, F) -> F.

%%--------------------------------------------------------------------

ioc_key_is(Route, IOC, Key) ->
    case Route of
        #{IOC := {Key, _}} ->
            true;

        _ ->
            false
    end.
-endif.

