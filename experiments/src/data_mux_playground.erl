-module(data_mux_playground).

-export([run/0]).

-record(route, {
    data_a :: {binary(), [tuple()]},
    data_b :: {binary(), [tuple()]},
    data_c :: {binary(), [tuple()]},
    data_d :: {binary(), [tuple()]}
}).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    [{{ioc, X, T, _}, Pin} | _] = Device:top_iocs(2),
    Y = T - 1,
    LAB = {lab, X, Y},
    Ns = lists:seq(0, 9),
    I = 0,
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf(lists:flatten([
        [
            source_4(Device, LAB, A, B, C, D, I, Pin)
            ||
            A <- Ns, A =/= I,
            B <- Ns, B =/= I, B > A,
            C <- Ns, C =/= I, C > B,
            D <- Ns, D =/= I, D > C
        ]
    ])),
    Matrix0 = matrix:build(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, local_line}) -> true;
        ({_, lut, _}) -> true;
        ({XX, YY, _, _, cell, _}) when XX =/= X; YY =/= Y -> true;
        ({XX, YY, II, _, cell, _}) when XX =:= X, YY =:= Y, II =/= I -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %
    {Keys, Routes} = routes(Experiments, X, Y),
    print_keys(Keys),
    %
    port_fuses(data_a, Keys, Routes, Matrix),
    port_fuses(data_b, Keys, Routes, Matrix),
    port_fuses(data_c, Keys, Routes, Matrix),
    port_fuses(data_d, Keys, Routes, Matrix),
    ok.

%%--------------------------------------------------------------------

source_4(Device, LAB, A, B, C, D, I, Pin) ->
    #{
        title => {A, B, C, D, thru, I, to, Pin},
        device => Device,
        settings => [
            {location, a, lab:lc(LAB, A)},
            {location, b, lab:lc(LAB, B)},
            {location, c, lab:lc(LAB, C)},
            {location, d, lab:lc(LAB, D)},
            {location, thru, lab:lc(LAB, I)},
            {location, q, Pin}
        ],
        verilog => <<
            "module experiment (\n"
            "  output wire q\n"
            ");\n"
            "  wire a_q;\n"
            "  wire b_q;\n"
            "  wire c_q;\n"
            "  wire d_q;\n"
            "  lcell a (\n"
            "    .in(1),\n"
            "    .out(a_q)\n"
            "  );\n"
            "  lcell b (\n"
            "    .in(1),\n"
            "    .out(b_q)\n"
            "  );\n"
            "  lcell c (\n"
            "    .in(1),\n"
            "    .out(c_q)\n"
            "  );\n"
            "  lcell d (\n"
            "    .in(1),\n"
            "    .out(d_q)\n"
            "  );\n"
            "  lcell thru (\n"
            "    .in(a_q && b_q && c_q && d_q),\n"
            "    .out(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%====================================================================
%% generate & print routes
%%====================================================================

routes(Experiments, X, Y) ->
    Init = {
        maps:from_list([
            {[{lc, X, Y, N}, {local_line, X, Y, 0, N}], <<($0 + N)>>}
            ||
            N <- lists:seq(1, 9)
        ]),
        maps:from_list([
            {<<($0 + N)>>, [{lc, X, Y, N}, {local_line, X, Y, 0, N}]}
            ||
            N <- lists:seq(1, 9)
        ]),
        $A
    },
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

route_keys_signal(_, #{lc := LC, dests := Dests}, Acc0) ->
    lists:foldl(
        fun (Dest, Acc) -> route_keys_dest(LC, Dest, Acc) end,
        Acc0,
        Dests
    ).

%%--------------------------------------------------------------------

route_keys_dest(_, #{port := data_in}, Acc) ->
    Acc;
route_keys_dest(
            LC,
            #{route_port := _, route := Route0},
            Acc = {ToKey, ToRoute, Key0}
        ) ->
    Route = [LC | lists:reverse(Route0)],
    case ToKey of
        #{Route := _} ->
            Acc;

        _ ->
            Key = <<Key0>>,
            {ToKey#{Route => Key}, ToRoute#{Key => Route}, Key0 + 1}
    end.

%%--------------------------------------------------------------------

route_experiment({_, _, #{signals := Signals}}, RouteToKey) ->
    #route{
        data_a = route_signals(Signals, data_a, RouteToKey),
        data_b = route_signals(Signals, data_b, RouteToKey),
        data_c = route_signals(Signals, data_c, RouteToKey),
        data_d = route_signals(Signals, data_d, RouteToKey)
    }.

%%--------------------------------------------------------------------

route_signals(Signals, Port, RouteToKey) ->
    None = {<<" ">>, []},
    maps:fold(fun (_, Signal, Acc) ->
         route_signal(Signal, Port, RouteToKey, Acc)
    end, None, Signals).

%%--------------------------------------------------------------------

route_signal(#{lc := LC, dests := Dests}, Port, RouteToKey, Init) ->
    lists:foldl(fun (Dest, Acc) ->
        route_dest(LC, Dest, Port, RouteToKey, Acc)
    end, Init, Dests).

%%--------------------------------------------------------------------

route_dest(_, #{port := data_in}, _, _, Acc) ->
    Acc;
route_dest(LC, #{route_port := Port, route := Route0}, Port, RouteToKey, _) ->
    Route = [LC | lists:reverse(Route0)],
    #{Route := Key} = RouteToKey,
    {Key, Route};
route_dest(_, _, _, _, Acc) ->
    Acc.

%%--------------------------------------------------------------------

print_keys(Keys) ->
    [
        io:format("~s: ~w~n", [Key, Route])
        ||
        {Key, Route} <- Keys
    ],
    ok.

%%====================================================================
%% route utilities
%%====================================================================

port_fuses(Port, Keys, Routes, Matrix) ->
    Pattern = [
        boolean_to(port_is_empty(Route, Port), 1, x)
        ||
        Route <- Routes
    ],
    InitialFuses = [
        {Fuse, #{}, Name}
        ||
        {Fuse, Name} <- matrix:pattern_match(Matrix, Pattern)
    ],
    Fuses = lists:foldl(fun ({Key, _}, Acc) ->
        port_fuses(Port, Key, Routes, Matrix, Acc)
    end, InitialFuses, Keys),
    io:format("~n~s: ~s~n", [
        Port,
        lists:join(<<" ">>, [ Key || {Key, _} <- Keys ])
    ]),
    [
        io:format("~6b |~s| ~w~n", [
            Fuse,
            lists:join(<<"|">>, [
                port_fuse_bit(Key, Bits)
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

port_fuses(Port, Key, Routes, Matrix, Fuses) ->
    IsKey = [ port_key_is(Route, Port, Key) || Route <- Routes ],
    case lists:member(true, IsKey) of
        true ->
            Pattern0 = [ boolean_to(Is, 0, x) || Is <- IsKey ],
            Pattern1 = [ boolean_to(Is, 1, x) || Is <- IsKey ],
            Fuses0 = matrix:pattern_match(Matrix, Pattern0),
            Fuses1 = matrix:pattern_match(Matrix, Pattern1),
            lists:filtermap(fun ({Fuse, Bits, Name}) ->
                port_fuse_bits(Fuse, Key, Fuses0, Fuses1, Bits, Name)
            end, Fuses);

        false ->
            Fuses
    end.

%%--------------------------------------------------------------------

port_fuse_bits(Fuse, Key, Fuses0, Fuses1, Bits, Name) ->
    case {lists:keyfind(Fuse, 1, Fuses0), lists:keyfind(Fuse, 1, Fuses1)} of
        {{_, _}, false} ->
            {true, {Fuse, Bits#{Key => 0}, Name}};

        {false, {_, _}} ->
            {true, {Fuse, Bits#{Key => 1}, Name}};

        {false, false} ->
            false
    end.

%%--------------------------------------------------------------------

port_fuse_bit(Key, Bits) ->
    case Bits of
        #{Key := 0} -> <<"-">>;
        #{Key := 1} -> <<"#">>;
        _ -> <<" ">>
    end.

%%====================================================================
%% route utilities
%%====================================================================

port_is_empty(#route{data_a = {_, []}}, data_a) ->
    true;
port_is_empty(#route{data_b = {_, []}}, data_b) ->
    true;
port_is_empty(#route{data_c = {_, []}}, data_c) ->
    true;
port_is_empty(#route{data_d = {_, []}}, data_d) ->
    true;
port_is_empty(_, _) ->
    false.

%%--------------------------------------------------------------------

port_key_is(#route{data_a = {Key, _}}, data_a, Key) ->
    true;
port_key_is(#route{data_b = {Key, _}}, data_b, Key) ->
    true;
port_key_is(#route{data_c = {Key, _}}, data_c, Key) ->
    true;
port_key_is(#route{data_d = {Key, _}}, data_d, Key) ->
    true;
port_key_is(_, _, _) ->
    false.

%%--------------------------------------------------------------------

boolean_to(true, T, _) -> T;
boolean_to(false, _, F) -> F.

