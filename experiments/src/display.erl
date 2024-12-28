-module(display).

-export([control_routing/1]).
-export([port_routing/2]).
-export([routing/2]).

-type density() :: density:density().
-type fuses() :: [fuse:fuse()].
-type ioc() :: ioc:ioc().
-type jtag() :: jtag:jtag().
-type lc() :: lc:lc().
-type title() :: term().

-define(LUT_INIT, 2#1111111111111111).

-record(lc, {
    carry_in = false :: boolean(),
    carry_out = false :: boolean(),
    feedback = false :: boolean(),
    lut = ?LUT_INIT :: non_neg_integer(),
    lut_chain = true :: boolean(),
    lut_name :: binary() | undefined,
    lut_ports = #{} :: #{},
    output_left = reg :: lut | reg,
    output_local = reg :: lut | reg,
    output_right = reg :: lut | reg,
    reg_chain = true :: boolean(),
    reg_name :: binary() | undefined,
    reg_ports = #{} :: #{}
}).

-record(ioc, {
    enable = normal :: normal | invert,
    enable_port :: {binary(), list()} | undefined,
    input_name :: binary() | undefined,
    output = normal :: normal | invert,
    output_name :: binary() | undefined,
    output_port :: {binary(), list()} | undefined
}).

-record(jtag, {
    name :: binary() | undefined,
    ports = #{} :: #{}
}).

-record(ufm, {
    name :: binary() | undefined,
    ports = #{} :: #{}
}).

-type logic() :: #{
    lc() => #lc{},
    ioc() => #ioc{},
    jtag() => #jtag{}
}.

%%====================================================================
%% carry_adjust
%%====================================================================

carry_adjust({lc, _, _, N}, LUT) when N =:= 4 orelse N =:= 9 ->
    (LUT band 16#ff00) bor
    ((LUT band 16#00f0) bsr 4) bor
    ((LUT band 16#000f) bsl 4);
carry_adjust({lc, _, _, _}, LUT) ->
    (LUT band 16#ff00) bor
    (((bnot LUT) band 16#00f0) bsr 4) bor
    (((bnot LUT) band 16#000f) bsl 4).

%%--------------------------------------------------------------------

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
%% collect
%%====================================================================

-spec collect({title(), fuses(), rcf_file:rcf()}, density()) -> logic().

collect({_Name, Fuses, #{signals := Signals}}, Density) ->
    Cells0 = lists:foldl(fun (Fuse, Cells) ->
        collect_fuse(Fuse, Density, Cells)
    end, #{}, Fuses),
    maps:fold(fun collect_signal/3, Cells0, Signals).

%%--------------------------------------------------------------------

carry_out({lc, X, Y, 0}) -> {lc, X - 1, Y, 9};
carry_out({lc, X, Y, N}) -> {lc, X, Y, N - 1}.

%%--------------------------------------------------------------------

collect_fuse(Fuse, Density, Cells0) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {In, carry_in}} ->
            Cells1 = collect_lc(In, fun (LC) ->
                LC#lc{carry_in = true}
            end, Cells0),
            collect_lc(carry_out(In), fun (LC) ->
                LC#lc{carry_out = true}
            end, Cells1);

        {ok, {At, feedback}} ->
            collect_lc(At, fun (LC) -> LC#lc{feedback = true} end, Cells0);

        {ok, {At, lut, LUT}} ->
            collect_lc(At, fun (LC) -> collect_lut(LUT, LC) end, Cells0);

        {ok, {At, lut_chain, off}} ->
            collect_lc(At, fun (LC) -> LC#lc{lut_chain = false} end, Cells0);

        {ok, {At, output_left, lut}} ->
            collect_lc(At, fun (LC) -> LC#lc{output_left = lut} end, Cells0);

        {ok, {At, output_local, lut}} ->
            collect_lc(At, fun (LC) -> LC#lc{output_local = lut} end, Cells0);

        {ok, {At, output_right, lut}} ->
            collect_lc(At, fun (LC) -> LC#lc{output_right = lut} end, Cells0);

        {ok, {At, register_chain, off}} ->
            collect_lc(At, fun (LC) ->
                LC#lc{reg_chain = false}
            end, Cells0);

        {ok, {At, enable_invert}} ->
            collect_ioc(At, fun (IOC) -> IOC#ioc{enable = invert} end, Cells0);

        {ok, {At, output_invert}} ->
            collect_ioc(At, fun (IOC) -> IOC#ioc{output = invert} end, Cells0);

        _ ->
            Cells0
    end.

%%--------------------------------------------------------------------

collect_signal(Signal, #{dests := Dests}, Cells0) ->
    lists:foldl(fun (Dest, Cells) ->
        collect_dest(Signal, Dest, Cells)
    end, Cells0, Dests).

%%--------------------------------------------------------------------

collect_dest(Signal, Dest, Cells0) ->
    case Dest of
        #{name := Name, lc := At, route := Route0, port := P}
                when P =:= data_a; P =:= data_b; P =:= data_c; P =:= data_d ->
            #{route_port := Port} = Dest,
            Route = collect_route(Route0, Cells0),
            collect_lc(At, fun(LC) ->
                collect_lut_port(LC, Name, Port, Route, Signal)
            end, collect_source(Signal, Route0, Cells0));

        #{name := Name, lc := At, route := Route0, port := Port}
                when Port =:= invert_a ->
            Route = collect_route(Route0, Cells0),
            collect_lc(At, fun(LC) ->
                collect_lut_port(LC, Name, Port, Route, Signal)
            end, collect_source(Signal, Route0, Cells0));

        #{name := Name, lc := At, route := Route0, port := Port} ->
            Route = collect_route(Route0, Cells0),
            collect_lc(At, fun(LC) ->
                collect_reg_port(LC, Name, Port, Route, Signal)
            end, collect_source(Signal, Route0, Cells0));

        #{name := Name, ioc := At, route := [], port := data_in} ->
            % JTAG
            collect_ioc(At, fun(IOC0) ->
                IOC = collect_ioc_output_name(IOC0, Name),
                IOC#ioc{output_port = {Signal, {}}}
            end, Cells0);

        #{name := Name, ioc := At, route := Route0, port := data_in} ->
            Route = case Route0 of
                [{io_bypass_out, _, _, _, 0} | Route1] ->
                    collect_route(Route1, Cells0);

                [{io_data_out, _, _, _, 0} | Route1] ->
                    collect_route(Route1, Cells0)
            end,
            collect_ioc(At, fun(IOC0) ->
                IOC = collect_ioc_output_name(IOC0, Name),
                IOC#ioc{output_port = {Signal, Route}}
            end, collect_source(Signal, Route0, Cells0));

        #{name := Name, ioc := At, route := Route0, port := oe} ->
            [{io_oe, _, _, _, 0} | Route1] = Route0,
            Route = collect_route(Route1, Cells0),
            collect_ioc(At, fun(IOC0) ->
                IOC = collect_ioc_output_name(IOC0, Name),
                IOC#ioc{enable_port = {Signal, Route}}
            end, collect_source(Signal, Route0, Cells0));

        #{name := Name, jtag := At, route := Route0, port := Port} ->
            Route = collect_route(Route0, Cells0),
            collect_jtag(At, fun(JTAG) ->
                collect_jtag_port(JTAG, Name, Port, Route, Signal)
            end, collect_source(Signal, Route0, Cells0));

        #{name := Name, ufm := At, route := Route0, port := Port} ->
            Route = collect_route(Route0, Cells0),
            collect_ufm(At, fun(UFM) ->
                collect_ufm_port(UFM, Name, Port, Route, Signal)
            end, collect_source(Signal, Route0, Cells0))
    end.

%%--------------------------------------------------------------------

collect_route([], _) ->
    {};
collect_route([{jtag, X, Y, tdo} | Route], Cells) ->
    collect_route({jtag, X, Y, tdo}, Route, Cells);
collect_route([{lab_clk, X, Y, 0, I} | Route], Cells) ->
    collect_route({clk, X, Y, I}, Route, Cells);
collect_route([{lab_control_mux, _, _, 0, I} | Route], Cells) ->
    collect_route({control, I}, Route, Cells);
collect_route([{le_buffer, X, Y, 0, I}], _) ->
    {lc, X, Y, I div 2};
collect_route([{local_interconnect, _, _, 0, I} | Route], Cells) ->
    collect_route({interconnect, I}, Route, Cells);
collect_route([{local_line, X, Y, 0, N}], Cells) ->
    LC = {lc, X, Y, N},
    {LC, collect_route_local(LC, Cells)};
collect_route([{lut_chain, X, Y, 0, N}], _) ->
    LC = {lc, X, Y, N},
    {LC, lut_chain}.

%%--------------------------------------------------------------------

collect_route(Local, [Direct], Cells) ->
    case Direct of
        {io_data_in, X, Y, N, 0} ->
            {Local, {ioc, X, Y, N}};

        {jtag, X, Y, Port} ->
            {Local, {jtag, X, Y}, Port};

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 0 ->
            LC = {lc, X, Y, I div 2},
            {Local, LC, collect_route_left(LC, Cells)};

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 1 ->
            LC = {lc, X, Y, I div 2},
            {Local, LC, collect_route_right(LC, Cells)};

        {local_line, X, Y, 0, N} ->
            LC = {lc, X, Y, N},
            {Local, LC, collect_route_local(LC, Cells)};

        {ufm, X, Y, Port} ->
            {Local, {ufm, X, Y}, Port}
    end;
collect_route(Local, [_ | Route], Cells) ->
    collect_route_inner(Local, Route, Cells).

%%--------------------------------------------------------------------

collect_route_inner(Local, [Source], Cells) ->
    case Source of
        {clk_buffer, _, _, N, 0} ->
            {Local, '...', {global, N}};

        {io_data_in, X, Y, N, 0} ->
            {Local, '...', {ioc, X, Y, N}};

        {jtag, X, Y, Port} ->
            {Local, '...', {jtag, X, Y}, Port};

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 0 ->
            LC = {lc, X, Y, I div 2},
            {Local, '...', LC, collect_route_left(LC, Cells)};

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 1 ->
            LC = {lc, X, Y, I div 2},
            {Local, '...', LC, collect_route_right(LC, Cells)};

        {ufm, X, Y, Port} ->
            {Local, '...', {ufm, X, Y}, Port}
    end;
collect_route_inner(Local, [_ | Route], Cells) ->
    collect_route_inner(Local, Route, Cells).

%%--------------------------------------------------------------------

collect_route_left(At, Cells) ->
    #{At := #lc{output_left = Output}} = Cells,
    Output.

%%--------------------------------------------------------------------

collect_route_local(At, Cells) ->
    #{At := #lc{output_local = Output}} = Cells,
    Output.

%%--------------------------------------------------------------------

collect_route_right(At, Cells) ->
    #{At := #lc{output_right = Output}} = Cells,
    Output.

%%--------------------------------------------------------------------

collect_source(_, [], Cells) ->
    Cells;
collect_source(Signal, [Source], Cells) ->
    case Source of
        {clk_buffer, _, _, _, 0} ->
            Cells;

        {io_data_in, X, Y, N, 0} ->
            At = {ioc, X, Y, N},
            collect_ioc(At, fun (IOC) ->
                collect_ioc_input_name(IOC, Signal)
            end, Cells);

        {jtag, X, Y, _} ->
            At = {jtag, X, Y},
            collect_jtag(At, fun (JTAG) ->
                collect_jtag_name(JTAG, Signal)
            end, Cells);

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 0 ->
            At = {lc, X, Y, I div 2},
            collect_lc(At, fun (LC) ->
                collect_lc_left(LC, Signal)
            end, Cells);

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 1 ->
            At = {lc, X, Y, I div 2},
            collect_lc(At, fun (LC) ->
                collect_lc_right(LC, Signal)
            end, Cells);

        {local_line, X, Y, 0, N} ->
            At = {lc, X, Y, N},
            collect_lc(At, fun (LC) ->
                collect_lc_local(LC, Signal)
            end, Cells);

        {lut_chain, X, Y, 0, N} ->
            At = {lc, X, Y, N},
            collect_lc(At, fun (LC) ->
                collect_lut_name(LC, Signal)
            end, Cells);

        {ufm, X, Y, _} ->
            At = {ufm, X, Y},
            collect_ufm(At, fun (UFM) ->
                collect_ufm_name(UFM, Signal)
            end, Cells)
    end;
collect_source(Signal, [_ | Route], Cells) ->
    collect_source(Signal, Route, Cells).

%%--------------------------------------------------------------------

collect_jtag(At = {jtag, _, _}, Collect, Cells) ->
    case Cells of
        #{At := LC} ->
            Cells#{At => Collect(LC)};

        _ ->
            Cells#{At => Collect(#jtag{})}
    end.

%%--------------------------------------------------------------------

collect_jtag_name(JTAG = #jtag{name = Name}, Name) ->
    JTAG;
collect_jtag_name(JTAG = #jtag{name = undefined}, Name) ->
    JTAG#jtag{name = Name};
collect_jtag_name(JTAG, _Name) ->
    %io:format("JTAG NAME ~p ~s~n", [JTAG, _Name]),
    JTAG.

%%--------------------------------------------------------------------

collect_jtag_port(JTAG0 = #jtag{ports = Ports0}, Name, Port, Route, Signal) ->
    JTAG = collect_jtag_name(JTAG0, Name),
    Ports = Ports0#{Port => {Signal, Route}},
    JTAG#jtag{ports = Ports}.

%%--------------------------------------------------------------------

collect_lc(At = {lc, _, _, _}, Collect, Cells) ->
    case Cells of
        #{At := LC} ->
            Cells#{At => Collect(LC)};

        _ ->
            Cells#{At => Collect(#lc{})}
    end.

%%--------------------------------------------------------------------

collect_lc_left(LC = #lc{}, Name) ->
    case LC#lc.output_left of
        lut ->
            collect_lut_name(LC, Name);

        reg ->
            collect_reg_name(LC, Name)
    end.

%%--------------------------------------------------------------------

collect_lc_local(LC = #lc{}, Name) ->
    case LC#lc.output_local of
        lut ->
            collect_lut_name(LC, Name);

        reg ->
            collect_reg_name(LC, Name)
    end.

%%--------------------------------------------------------------------

collect_lc_right(LC = #lc{}, Name) ->
    case LC#lc.output_right of
        lut ->
            collect_lut_name(LC, Name);

        reg ->
            collect_reg_name(LC, Name)
    end.

%%--------------------------------------------------------------------

collect_ioc(At = {ioc, _, _, _}, Collect, Cells) ->
    case Cells of
        #{At := IOC} ->
            Cells#{At => Collect(IOC)};

        _ ->
            Cells#{At => Collect(#ioc{})}
    end.

%%--------------------------------------------------------------------

collect_ioc_input_name(IOC = #ioc{input_name = Name}, Name) ->
    IOC;
collect_ioc_input_name(IOC = #ioc{input_name = undefined}, Name) ->
    IOC#ioc{input_name = Name}.

%%--------------------------------------------------------------------

collect_ioc_output_name(IOC = #ioc{output_name = Name}, Name) ->
    IOC;
collect_ioc_output_name(IOC = #ioc{output_name = undefined}, Name) ->
    IOC#ioc{output_name = Name}.

%%--------------------------------------------------------------------

collect_lut(a0b0c0d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111111110};
collect_lut(a1b0c0d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111111101};
collect_lut(a0b1c0d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111111011};
collect_lut(a1b1c0d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111110111};
collect_lut(a0b0c1d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111101111};
collect_lut(a1b0c1d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111011111};
collect_lut(a0b1c1d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111110111111};
collect_lut(a1b1c1d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111101111111};
collect_lut(a0b0c0d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111011111111};
collect_lut(a1b0c0d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111110111111111};
collect_lut(a0b1c0d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111101111111111};
collect_lut(a1b1c0d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111011111111111};
collect_lut(a0b0c1d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1110111111111111};
collect_lut(a1b0c1d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1101111111111111};
collect_lut(a0b1c1d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1011111111111111};
collect_lut(a1b1c1d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#0111111111111111}.

%%--------------------------------------------------------------------

collect_lut_name(LC = #lc{lut_name = Name}, Name) ->
    LC;
collect_lut_name(LC = #lc{lut_name = undefined}, Name) ->
    LC#lc{lut_name = Name}.

%%--------------------------------------------------------------------

collect_lut_port(LC0 = #lc{lut_ports = Ports0}, _Name, Port, Route, Signal) ->
    % not sure if this is a LUT or REG name
    %LC = collect_lut_name(LC0, Name),
    Ports = Ports0#{Port => {Signal, Route}},
    LC0#lc{lut_ports = Ports}.

%%--------------------------------------------------------------------

collect_reg_name(LC = #lc{reg_name = Name}, Name) ->
    LC;
collect_reg_name(LC = #lc{reg_name = undefined}, Name) ->
    LC#lc{reg_name = Name}.

%%--------------------------------------------------------------------

collect_reg_port(LC0 = #lc{reg_ports = Ports0}, Name, Port, Route, Signal) ->
    LC = collect_reg_name(LC0, Name),
    Ports = Ports0#{Port => {Signal, Route}},
    LC#lc{reg_ports = Ports}.

%%--------------------------------------------------------------------

collect_ufm(At = {ufm, _, _}, Collect, Cells) ->
    case Cells of
        #{At := LC} ->
            Cells#{At => Collect(LC)};

        _ ->
            Cells#{At => Collect(#ufm{})}
    end.

%%--------------------------------------------------------------------

collect_ufm_name(UFM = #ufm{name = Name}, Name) ->
    UFM;
collect_ufm_name(UFM = #ufm{name = undefined}, Name) ->
    UFM#ufm{name = Name};
collect_ufm_name(UFM, _Name) ->
    %io:format("UFM NAME ~p ~s~n", [UFM, _Name]),
    UFM.

%%--------------------------------------------------------------------

collect_ufm_port(UFM0 = #ufm{ports = Ports0}, Name, Port, Route, Signal) ->
    UFM = collect_ufm_name(UFM0, Name),
    Ports = Ports0#{Port => {Signal, Route}},
    UFM#ufm{ports = Ports}.

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

routing(Experiments, Density) ->
    lists:foreach(fun (Experiment) ->
        routing_experiment(Experiment, Density)
    end, Experiments).

%%--------------------------------------------------------------------

routing_experiment(Experiment = {Name, _, _}, Density) ->
    io:format(" --> ~p~n", [Name]),
    Cells = collect(Experiment, Density),
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

routing_lut(_, #lc{lut_name = undefined, carry_out = false, lut_ports = Ports})
        when map_size(Ports) =:= 0 ->
    ok;
routing_lut(At, LC = #lc{lut_name = Name}) ->
    io:format("LUT ~w ~s~n", [At, Name]),
    routing_lut_common(At, LC).

%%--------------------------------------------------------------------

routing_lut_common(At, LC = #lc{lut_ports = Ports}) ->
    lists:foreach(fun ({Port, Routing}) ->
        routing_port(Port, Routing)
    end, lists:sort(maps:to_list(Ports))),
    case LC#lc.carry_out of
        true ->
            LUT = carry_adjust(At, LC#lc.lut),
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

