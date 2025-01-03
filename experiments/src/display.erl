-module(display).

-export([control_routing/1]).
-export([port_routing/2]).
-export([routing/2]).

-export([expression/1]).

-include("decompile.hrl").

%%====================================================================
%% sum / carry expressions
%%====================================================================

carry_expression(LUT) ->
    C = LUT band 2#11111111,
    case C of
        16#17 ->
            <<"!(a.b | a.c | b.c)">>;

        16#8e ->
            <<"a.b | a.(!c) | b.(!c)">>;

        _ ->
            expression((C bsl 8) bor C)
    end.

%%--------------------------------------------------------------------

sum_expression(LUT) ->
    S = LUT bsr 8,
    case S of
        16#69 ->
            <<"a^b^(!c)">>;

        _ ->
            expression((S bsl 8) bor S)
    end.

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
        io:format("  ~12w ~8w <- ~p~n", [LC, Port, Route])
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
control_routing_dest(#{ioc := IOC, port := Port, route := Route}, Routing) ->
    case Route of
        [{io_bypass_out, _, _, _, 0}, From | _] ->
            [{IOC, Port, From} | Routing];

        [{io_data_out, _, _, _, 0}, From | _] ->
            [{IOC, Port, From} | Routing];

        [{io_oe, _, _, _, 0}, From | _] ->
            [{IOC, Port, From} | Routing];

        [] ->
            [{IOC, Port, internal} | Routing]
    end;
control_routing_dest(#{jtag := JTAG, port := Port, route := Route}, Routing) ->
    case Route of
        [{jtag_tdo_user, _, _, _, _}, From | _] ->
            [{JTAG, Port, From} | Routing];

        [] ->
            [{JTAG, Port, internal} | Routing]
    end.

%%====================================================================
%% expression
%%====================================================================

expression(2#0000000000000000) -> <<"0">>;
expression(2#1111111111111111) -> <<"1">>;
expression(2#0110100110010110) -> <<"a^b^c^d">>;
expression(2#1001011001101001) -> <<"!a^b^c^d">>;
expression(LUT) ->
    <<" | ", Expr/binary>> = expression(LUT, LUT, [
        % 8-bits
        {2#1010101010101010, <<"a">>},
        {2#0101010101010101, <<"!a">>},
        {2#1100110011001100, <<"b">>},
        {2#0011001100110011, <<"!b">>},
        {2#1111000011110000, <<"c">>},
        {2#0000111100001111, <<"!c">>},
        {2#1111111100000000, <<"d">>},
        {2#0000000011111111, <<"!d">>},
        % 8-bits
        {2#0110011001100110, <<"a^b">>},
        {2#1001100110011001, <<"!a^b">>},
        {2#0101101001011010, <<"a^c">>},
        {2#1010010110100101, <<"!a^c">>},
        {2#0101010110101010, <<"a^d">>},
        {2#1010101001010101, <<"!a^d">>},
        {2#0011110000111100, <<"b^c">>},
        {2#1100001111000011, <<"!b^c">>},
        {2#0011001111001100, <<"b^d">>},
        {2#1100110000110011, <<"!b^d">>},
        {2#0000111111110000, <<"c^d">>},
        {2#1111000000001111, <<"!c^d">>},
        % 8-bits
        {2#1001011010010110, <<"a^b^c">>},
        {2#0110100101101001, <<"!a^b^c">>},
        {2#0101101010100101, <<"a^c^d">>},
        {2#1010010101011010, <<"!a^c^d">>},
        {2#1100001100111100, <<"b^c^d">>},
        {2#0011110011000011, <<"!b^c^d">>},
        % 4-bits
        {2#1000100010001000, <<"a.b">>},
        {2#0100010001000100, <<"!a.b">>},
        {2#0010001000100010, <<"a.!b">>},
        {2#0001000100010001, <<"!a.!b">>},
        {2#1010000010100000, <<"a.c">>},
        {2#0101000001010000, <<"!a.c">>},
        {2#0000101000001010, <<"a.!c">>},
        {2#0000010100000101, <<"!a.!c">>},
        {2#1010101000000000, <<"a.d">>},
        {2#0101010100000000, <<"!a.d">>},
        {2#0000000010101010, <<"a.!d">>},
        {2#0000000001010101, <<"!a.!d">>},
        {2#1100000011000000, <<"b.c">>},
        {2#0011000000110000, <<"!b.c">>},
        {2#0000110000001100, <<"b.!c">>},
        {2#0000001100000011, <<"!b.!c">>},
        {2#1100110000000000, <<"b.d">>},
        {2#0011001100000000, <<"!b.d">>},
        {2#0000000011001100, <<"b.!d">>},
        {2#0000000000110011, <<"!b.!d">>},
        {2#1111000000000000, <<"c.d">>},
        {2#0000111100000000, <<"!c.d">>},
        {2#0000000011110000, <<"c.!d">>},
        {2#0000000000001111, <<"!c.!d">>},
        % 2-bits
        {2#1000000010000000, <<"a.b.c">>},
        {2#0100000001000000, <<"!a.b.c">>},
        {2#0010000000100000, <<"a.!b.c">>},
        {2#0001000000010000, <<"!a.!b.c">>},
        {2#0000100000001000, <<"a.b.!c">>},
        {2#0000010000000100, <<"!a.b.!c">>},
        {2#0000001000000010, <<"a.!b.!c">>},
        {2#0000000100000001, <<"!a.!b.!c">>},
        {2#1000100000000000, <<"a.b.d">>},
        {2#0100010000000000, <<"!a.b.d">>},
        {2#0010001000000000, <<"a.!b.d">>},
        {2#0001000100000000, <<"!a.!b.d">>},
        {2#0000000010001000, <<"a.b.!d">>},
        {2#0000000001000100, <<"!a.b.!d">>},
        {2#0000000000100010, <<"a.!b.!d">>},
        {2#0000000000010001, <<"!a.!b.!d">>},
        {2#1010000000000000, <<"a.c.d">>},
        {2#0101000000000000, <<"!a.c.d">>},
        {2#0000101000000000, <<"a.!c.d">>},
        {2#0000010100000000, <<"!a.!c.d">>},
        {2#0000000010100000, <<"a.c.!d">>},
        {2#0000000001010000, <<"!a.c.!d">>},
        {2#0000000000001010, <<"a.!c.!d">>},
        {2#0000000000000101, <<"!a.!c.!d">>},
        {2#1100000000000000, <<"b.c.d">>},
        {2#0011000000000000, <<"!b.c.d">>},
        {2#0000110000000000, <<"b.!c.d">>},
        {2#0000001100000000, <<"!b.!c.d">>},
        {2#0000000011000000, <<"b.c.!d">>},
        {2#0000000000110000, <<"!b.c.!d">>},
        {2#0000000000001100, <<"b.!c.!d">>},
        {2#0000000000000011, <<"!b.!c.!d">>},
        % 1-bits
        {2#1000000000000000, <<"a.b.c.d">>},
        {2#0100000000000000, <<"!a.b.c.d">>},
        {2#0010000000000000, <<"a.!b.c.d">>},
        {2#0001000000000000, <<"!a.!b.c.d">>},
        {2#0000100000000000, <<"a.b.!c.d">>},
        {2#0000010000000000, <<"!a.b.!c.d">>},
        {2#0000001000000000, <<"a.!b.!c.d">>},
        {2#0000000100000000, <<"!a.!b.!c.d">>},
        {2#0000000010000000, <<"a.b.c.!d">>},
        {2#0000000001000000, <<"!a.b.c.!d">>},
        {2#0000000000100000, <<"a.!b.c.!d">>},
        {2#0000000000010000, <<"!a.!b.c.!d">>},
        {2#0000000000001000, <<"a.b.!c.!d">>},
        {2#0000000000000100, <<"!a.b.!c.!d">>},
        {2#0000000000000010, <<"a.!b.!c.!d">>},
        {2#0000000000000001, <<"!a.!b.!c.!d">>}
    ], <<>>),
    Expr.

%%--------------------------------------------------------------------

expression(_, 0, _, Acc) ->
    Acc;
expression(All, Rem, [{Mask, Term} | Terms], Acc)
        when (All band Mask) =:= Mask andalso
             (Rem band Mask) =/= 0 ->
    expression(All, Rem band (bnot Mask), Terms,
        <<Acc/binary, " | ", Term/binary>>
    );
expression(All, Rem, [_ | Terms], Acc) ->
    expression(All, Rem, Terms, Acc).

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

-spec routing({title(), fuses(), rcf_file:rcf()}, density()) -> ok.

routing(Experiment = {_, _, _}, Density) ->
    routing_experiment(Experiment, Density);
routing(Experiments, Density) ->
    lists:foreach(fun (Experiment) ->
        routing_experiment(Experiment, Density)
    end, Experiments).

%%--------------------------------------------------------------------

routing_experiment(Experiment = {Name, _, _}, Density) ->
    io:format(" --> ~p~n", [Name]),
    Cells = decompile:experiment(Experiment, Density),
    lists:foreach(fun ({Key, Value}) ->
        routing_cell(Key, Value, Cells)
    end, lists:sort(maps:to_list(Cells))),
    ok.

%%--------------------------------------------------------------------

routing_cell(At = {lc, _, _, _}, LC, Cells) ->
    routing_lut(At, LC),
    routing_reg(At, LC, Cells);
routing_cell(At = {ioc, _, _, _}, IOC = #ioc{}, _) ->
    routing_ioc(At, IOC);
routing_cell(At = {jtag, _, _}, JTAG = #jtag{}, _) ->
    routing_jtag(At, JTAG);
routing_cell(At = {ufm, _, _}, UFM = #ufm{}, _) ->
    routing_ufm(At, UFM).

%%--------------------------------------------------------------------

routing_ioc(_, #ioc{input_name = undefined, output_name = undefined}) ->
    ok;
routing_ioc(At, IOC = #ioc{input_name = Input, output_name = Output}) ->
    case {Input, Output} of
        {Name, undefined} ->
            io:format("IOC ~w ~s~n", [At, Name]);

        {undefined, Name} ->
            io:format("IOC ~w ~s~n", [At, Name]);

        {Alternate, Name} ->
            io:format("IOC ~w ~s (~s)~n", [At, Name, Alternate])
    end,
    case IOC of
        #ioc{output_port = undefined, enable_port = undefined} ->
            io:format("    input~n", []);

        #ioc{enable_port = undefined, output = invert} ->
            routing_port_inverted(output, IOC#ioc.output_port);

        #ioc{enable_port = undefined} ->
            routing_port(output, IOC#ioc.output_port);

        #ioc{output = invert, enable = invert} ->
            routing_port_inverted(output, IOC#ioc.output_port),
            routing_port_inverted(enable, IOC#ioc.enable_port);

        #ioc{output = invert} ->
            routing_port_inverted(output, IOC#ioc.output_port),
            routing_port(enable, IOC#ioc.enable_port);

        #ioc{enable = invert} ->
            routing_port(output, IOC#ioc.output_port),
            routing_port_inverted(enable, IOC#ioc.enable_port);

        _ ->
            routing_port(output, IOC#ioc.output_port),
            routing_port(enable, IOC#ioc.enable_port)
    end,
    io:format("~n", []).

%%--------------------------------------------------------------------

routing_jtag(_, #jtag{name = undefined}) ->
    ok;
routing_jtag(At, #jtag{name = Name, ports = Ports}) ->
    io:format("JTAG ~w ~s~n", [At, Name]),
    lists:foreach(fun ({Port, Routing}) ->
        routing_port(Port, Routing)
    end, lists:sort(maps:to_list(Ports))),
    io:format("~n", []).

%%--------------------------------------------------------------------

routing_lut(_, #lc{lut_name = undefined, output_left = reg, output_right = reg, output_local = reg}) ->
    ok;
routing_lut(At, LC = #lc{lut_name = Name}) ->
    io:format("LUT ~w ~s~n", [At, Name]),
    routing_lut_common(At, LC).

%%--------------------------------------------------------------------

routing_lut_common(At, LC = #lc{lut_ports = Ports}) ->
    lists:foreach(fun ({Port, Routing}) ->
        routing_port(Port, Routing)
    end, lists:sort(maps:to_list(Ports))),
    case LC#lc.feedback of
        true ->
            routing_port(feedback, {LC#lc.reg_name, {At,reg}});

        false ->
            ok
    end,
    case LC#lc.carry_in of
        true ->
            io:format("  carry_in <- ~w~n", [{lc:carry_from(At), carry_out}]);

        false ->
            ok
    end,
    case LC#lc.carry_out of
        true ->
            LUT = LC#lc.lut,
            io:format("    (~4.16.0B) sum = ~s, carry = ~s~n", [
                LUT,
                sum_expression(LUT),
                carry_expression(LUT)
            ]);

        false ->
            LUT = LC#lc.lut,
            io:format("    (~4.16.0B) lut = ~s~n", [
                LUT,
                expression(LUT)
            ])
    end,
    io:format("~n", []).

%%--------------------------------------------------------------------

routing_reg(_, #lc{reg_name = undefined}, _) ->
    ok;
routing_reg(At, LC = #lc{reg_name = Name, reg_ports = Ports}, Cells) ->
    io:format("REG ~w ~s~n", [At, Name]),
    lists:foreach(fun ({Port, Routing}) ->
        routing_port(Port, Routing)
    end, lists:sort(maps:to_list(Ports))),
    routing_reg_chain(At, LC, Cells),
    case LC of
        #lc{lut_name = undefined, lut_ports = LUTPorts}
                when map_size(LUTPorts) > 0 ->
            routing_lut_common(At, LC);

        _ ->
            io:format("~n", [])
    end.

%%--------------------------------------------------------------------

routing_reg_chain(_, #lc{reg_chain = false}, _) ->
    ok;
routing_reg_chain({lc, X, Y, N}, #lc{reg_chain = true}, Cells) ->
    LC = {lc, X, Y, N - 1},
    #{LC := #lc{reg_name = Name}} = Cells,
    routing_port(data, {Name, {LC, reg}}).

%%--------------------------------------------------------------------

routing_port(_, undefined) ->
    ok;
routing_port(Port, {From0, Route0}) ->
    From = iolist_to_binary(io_lib:format("~s", [From0])),
    Route = iolist_to_binary(io_lib:format("~w", [Route0])),
    case 14 + byte_size(From) + byte_size(Route) of
        W when W > 79 ->
            io:format(" ~9s <- ~s~n", [Port, From]),
            io:format("              ~s~n", [Route]);

        _ ->
            io:format(" ~9s <- ~s ~s~n", [Port, From, Route])
    end.

%%--------------------------------------------------------------------

routing_port_inverted(_, undefined) ->
    ok;
routing_port_inverted(Port, {From0, Route0}) ->
    From = iolist_to_binary(io_lib:format("~s", [From0])),
    Route = iolist_to_binary(io_lib:format("~w", [Route0])),
    case 14 + byte_size(From) + byte_size(Route) of
        W when W > 77 ->
            io:format(" ~9s <- ! ~s~n", [Port, From]),
            io:format("              ~s~n", [Route]);

        _ ->
            io:format(" ~9s <- ! ~s ~s~n", [Port, From, Route])
    end.

%%--------------------------------------------------------------------

routing_ufm(_, #ufm{name = undefined}) ->
    ok;
routing_ufm(At, #ufm{name = _Name, ports = Ports}) ->
    %io:format("UFM ~w ~s~n", [At, _Name]),
    io:format("UFM ~w~n", [At]),
    lists:foreach(fun ({Port, Routing}) ->
        routing_port(Port, Routing)
    end, lists:sort(maps:to_list(Ports))),
    io:format("~n", []).

