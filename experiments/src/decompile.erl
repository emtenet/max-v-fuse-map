-module(decompile).

-export([experiment/2]).

-include("decompile.hrl").

%%====================================================================
%% experiment
%%====================================================================

-spec experiment({title(), fuses(), rcf_file:rcf()}, density()) -> logic().

experiment({_Name, Fuses, #{signals := Signals}}, Density) ->
    Cells0 = lists:foldl(fun (Fuse, Cells) ->
        fuse(Fuse, Density, Cells)
    end, #{}, Fuses),
    Cells1 = maps:fold(fun signal/3, Cells0, Signals),
    maps:map(fun fixup_lut/2, Cells1).

%%--------------------------------------------------------------------

fuse(Fuse, Density, Cells0) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {In, carry_in}} ->
            Cells1 = lc(In, fun (LC) ->
                LC#lc{carry_in = true}
            end, Cells0),
            {ok, Out} = lc:carry_from(In, Density),
            lc(Out, fun (LC) ->
                LC#lc{carry_out = true}
            end, Cells1);

        {ok, {At, feedback}} ->
            lc(At, fun (LC) -> LC#lc{feedback = true} end, Cells0);

        {ok, {At, lut, LUT}} ->
            lc(At, fun (LC) -> lut(LUT, LC) end, Cells0);

        {ok, {At, lut_chain, off}} ->
            lc(At, fun (LC) -> LC#lc{lut_chain = false} end, Cells0);

        {ok, {At, output_left, lut}} ->
            lc(At, fun (LC) -> LC#lc{output_left = lut} end, Cells0);

        {ok, {At, output_local, lut}} ->
            lc(At, fun (LC) -> LC#lc{output_local = lut} end, Cells0);

        {ok, {At, output_right, lut}} ->
            lc(At, fun (LC) -> LC#lc{output_right = lut} end, Cells0);

        {ok, {At, register_chain, off}} ->
            lc(At, fun (LC) ->
                LC#lc{reg_chain = false}
            end, Cells0);

        {ok, {At, enable_invert}} ->
            ioc(At, fun (IOC) -> IOC#ioc{enable = invert} end, Cells0);

        {ok, {At, output_invert}} ->
            ioc(At, fun (IOC) -> IOC#ioc{output = invert} end, Cells0);

        _ ->
            Cells0
    end.

%%--------------------------------------------------------------------

signal(Signal, #{dests := Dests}, Cells0) ->
    lists:foldl(fun (Dest, Cells) ->
        dest(Signal, Dest, Cells)
    end, Cells0, Dests).

%%--------------------------------------------------------------------

dest(Signal, Dest, Cells0) ->
    case Dest of
        #{name := Name, lc := At, route := Route0, port := P}
                when P =:= data_a; P =:= data_b; P =:= data_c; P =:= data_d ->
            #{route_port := Port} = Dest,
            Route = route(Route0, Cells0),
            lc(At, fun(LC) ->
                lut_port(LC, Name, Port, Route, Signal)
            end, source(Signal, Route0, Cells0));

        #{name := Name, lc := At, route := Route0, port := Port}
                when Port =:= invert_a ->
            Route = route(Route0, Cells0),
            lc(At, fun(LC) ->
                lut_port(LC, Name, Port, Route, Signal)
            end, source(Signal, Route0, Cells0));

        #{name := Name, lc := At, route := Route0, port := Port} ->
            Route = route(Route0, Cells0),
            lc(At, fun(LC) ->
                reg_port(LC, Name, Port, Route, Signal)
            end, source(Signal, Route0, Cells0));

        #{name := Name, ioc := At, route := [], port := data_in} ->
            % JTAG
            ioc(At, fun(IOC0) ->
                IOC = ioc_output_name(IOC0, Name),
                IOC#ioc{output_port = {Signal, {}}}
            end, Cells0);

        #{name := Name, ioc := At, route := Route0, port := data_in} ->
            Route = case Route0 of
                [{io_bypass_out, _, _, _, 0} | Route1] ->
                    route(Route1, Cells0);

                [{io_data_out, _, _, _, 0} | Route1] ->
                    route(Route1, Cells0)
            end,
            ioc(At, fun(IOC0) ->
                IOC = ioc_output_name(IOC0, Name),
                IOC#ioc{output_port = {Signal, Route}}
            end, source(Signal, Route0, Cells0));

        #{name := Name, ioc := At, route := Route0, port := oe} ->
            [{io_oe, _, _, _, 0} | Route1] = Route0,
            Route = route(Route1, Cells0),
            ioc(At, fun(IOC0) ->
                IOC = ioc_output_name(IOC0, Name),
                IOC#ioc{enable_port = {Signal, Route}}
            end, source(Signal, Route0, Cells0));

        #{name := Name, jtag := At, route := Route0, port := Port} ->
            Route = route(Route0, Cells0),
            jtag(At, fun(JTAG) ->
                jtag_port(JTAG, Name, Port, Route, Signal)
            end, source(Signal, Route0, Cells0));

        #{name := Name, ufm := At, route := Route0, port := Port} ->
            Route = route(Route0, Cells0),
            ufm(At, fun(UFM) ->
                ufm_port(UFM, Name, Port, Route, Signal)
            end, source(Signal, Route0, Cells0))
    end.

%%--------------------------------------------------------------------

route([], _) ->
    {};
route([{jtag, X, Y, tdo} | Route], Cells) ->
    route({jtag, X, Y, tdo}, Route, Cells);
route([{lab_clk, X, Y, 0, I} | Route], Cells) ->
    route({clk, X, Y, I}, Route, Cells);
route([{lab_control_mux, _, _, 0, I} | Route], Cells) ->
    route({control, I}, Route, Cells);
route([{le_buffer, X, Y, 0, I}], _) ->
    {lc, X, Y, I div 2};
route([{local_interconnect, _, _, 0, I} | Route], Cells) ->
    route({interconnect, I}, Route, Cells);
route([{local_line, X, Y, 0, N}], Cells) ->
    LC = {lc, X, Y, N},
    {LC, route_local(LC, Cells)};
route([{lut_chain, X, Y, 0, N}], _) ->
    LC = {lc, X, Y, N},
    {LC, lut_chain}.

%%--------------------------------------------------------------------

route(Local, [Direct], Cells) ->
    case Direct of
        {io_data_in, X, Y, N, 0} ->
            {Local, {ioc, X, Y, N}};

        {jtag, X, Y, Port} ->
            {Local, {jtag, X, Y}, Port};

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 0 ->
            LC = {lc, X, Y, I div 2},
            {Local, LC, route_left(LC, Cells)};

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 1 ->
            LC = {lc, X, Y, I div 2},
            {Local, LC, route_right(LC, Cells)};

        {local_line, X, Y, 0, N} ->
            LC = {lc, X, Y, N},
            {Local, LC, route_local(LC, Cells)};

        {ufm, X, Y, Port} ->
            {Local, {ufm, X, Y}, Port}
    end;
route(Local, [_ | Route], Cells) ->
    route_inner(Local, Route, Cells).

%%--------------------------------------------------------------------

route_inner(Local, [Source], Cells) ->
    case Source of
        {clk_buffer, _, _, N, 0} ->
            {Local, '...', {global, N}};

        {io_data_in, X, Y, N, 0} ->
            {Local, '...', {ioc, X, Y, N}};

        {jtag, X, Y, Port} ->
            {Local, '...', {jtag, X, Y}, Port};

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 0 ->
            LC = {lc, X, Y, I div 2},
            {Local, '...', LC, route_left(LC, Cells)};

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 1 ->
            LC = {lc, X, Y, I div 2},
            {Local, '...', LC, route_right(LC, Cells)};

        {ufm, X, Y, Port} ->
            {Local, '...', {ufm, X, Y}, Port}
    end;
route_inner(Local, [_ | Route], Cells) ->
    route_inner(Local, Route, Cells).

%%--------------------------------------------------------------------

route_left(At, Cells) ->
    #{At := #lc{output_left = Output}} = Cells,
    Output.

%%--------------------------------------------------------------------

route_local(At, Cells) ->
    #{At := #lc{output_local = Output}} = Cells,
    Output.

%%--------------------------------------------------------------------

route_right(At, Cells) ->
    #{At := #lc{output_right = Output}} = Cells,
    Output.

%%--------------------------------------------------------------------

source(_, [], Cells) ->
    Cells;
source(Signal, [Source], Cells) ->
    case Source of
        {clk_buffer, _, _, _, 0} ->
            Cells;

        {io_data_in, X, Y, N, 0} ->
            At = {ioc, X, Y, N},
            ioc(At, fun (IOC) ->
                ioc_input_name(IOC, Signal)
            end, Cells);

        {jtag, X, Y, _} ->
            At = {jtag, X, Y},
            jtag(At, fun (JTAG) ->
                jtag_name(JTAG, Signal)
            end, Cells);

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 0 ->
            At = {lc, X, Y, I div 2},
            lc(At, fun (LC) ->
                lc_left(LC, Signal)
            end, Cells);

        {le_buffer, X, Y, 0, I} when I rem 2 =:= 1 ->
            At = {lc, X, Y, I div 2},
            lc(At, fun (LC) ->
                lc_right(LC, Signal)
            end, Cells);

        {local_line, X, Y, 0, N} ->
            At = {lc, X, Y, N},
            lc(At, fun (LC) ->
                lc_local(LC, Signal)
            end, Cells);

        {lut_chain, X, Y, 0, N} ->
            At = {lc, X, Y, N},
            lc(At, fun (LC) ->
                lut_name(LC, Signal)
            end, Cells);

        {ufm, X, Y, _} ->
            At = {ufm, X, Y},
            ufm(At, fun (UFM) ->
                ufm_name(UFM, Signal)
            end, Cells)
    end;
source(Signal, [_ | Route], Cells) ->
    source(Signal, Route, Cells).

%%--------------------------------------------------------------------

jtag(At = {jtag, _, _}, Collect, Cells) ->
    case Cells of
        #{At := LC} ->
            Cells#{At => Collect(LC)};

        _ ->
            Cells#{At => Collect(#jtag{})}
    end.

%%--------------------------------------------------------------------

jtag_name(JTAG = #jtag{name = Name}, Name) ->
    JTAG;
jtag_name(JTAG = #jtag{name = undefined}, Name) ->
    JTAG#jtag{name = Name};
jtag_name(JTAG, _Name) ->
    %io:format("JTAG NAME ~p ~s~n", [JTAG, _Name]),
    JTAG.

%%--------------------------------------------------------------------

jtag_port(JTAG0 = #jtag{ports = Ports0}, Name, Port, Route, Signal) ->
    JTAG = jtag_name(JTAG0, Name),
    Ports = Ports0#{Port => {Signal, Route}},
    JTAG#jtag{ports = Ports}.

%%--------------------------------------------------------------------

lc(At = {lc, _, _, _}, Collect, Cells) ->
    case Cells of
        #{At := LC} ->
            Cells#{At => Collect(LC)};

        _ ->
            Cells#{At => Collect(#lc{})}
    end.

%%--------------------------------------------------------------------

lc_left(LC = #lc{}, Name) ->
    case LC#lc.output_left of
        lut ->
            lut_name(LC, Name);

        reg ->
            reg_name(LC, Name)
    end.

%%--------------------------------------------------------------------

lc_local(LC = #lc{}, Name) ->
    case LC#lc.output_local of
        lut ->
            lut_name(LC, Name);

        reg ->
            reg_name(LC, Name)
    end.

%%--------------------------------------------------------------------

lc_right(LC = #lc{}, Name) ->
    case LC#lc.output_right of
        lut ->
            lut_name(LC, Name);

        reg ->
            reg_name(LC, Name)
    end.

%%--------------------------------------------------------------------

ioc(At = {ioc, _, _, _}, Collect, Cells) ->
    case Cells of
        #{At := IOC} ->
            Cells#{At => Collect(IOC)};

        _ ->
            Cells#{At => Collect(#ioc{})}
    end.

%%--------------------------------------------------------------------

ioc_input_name(IOC = #ioc{input_name = Name}, Name) ->
    IOC;
ioc_input_name(IOC = #ioc{input_name = undefined}, Name) ->
    IOC#ioc{input_name = Name}.

%%--------------------------------------------------------------------

ioc_output_name(IOC = #ioc{output_name = Name}, Name) ->
    IOC;
ioc_output_name(IOC = #ioc{output_name = undefined}, Name) ->
    IOC#ioc{output_name = Name}.

%%--------------------------------------------------------------------

lut(a0b0c0d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111111110};
lut(a1b0c0d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111111101};
lut(a0b1c0d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111111011};
lut(a1b1c0d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111110111};
lut(a0b0c1d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111101111};
lut(a1b0c1d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111111011111};
lut(a0b1c1d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111110111111};
lut(a1b1c1d0, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111101111111};
lut(a0b0c0d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111111011111111};
lut(a1b0c0d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111110111111111};
lut(a0b1c0d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111101111111111};
lut(a1b1c0d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1111011111111111};
lut(a0b0c1d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1110111111111111};
lut(a1b0c1d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1101111111111111};
lut(a0b1c1d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#1011111111111111};
lut(a1b1c1d1, LC = #lc{lut = LUT}) ->
    LC#lc{lut = LUT band 2#0111111111111111}.

%%--------------------------------------------------------------------

lut_name(LC = #lc{lut_name = Name}, Name) ->
    LC;
lut_name(LC = #lc{lut_name = undefined}, Name) ->
    LC#lc{lut_name = Name}.

%%--------------------------------------------------------------------

lut_port(LC0 = #lc{lut_ports = Ports0}, _Name, Port, Route, Signal) ->
    % not sure if this is a LUT or REG name
    %LC = lut_name(LC0, Name),
    Ports = Ports0#{Port => {Signal, Route}},
    LC0#lc{lut_ports = Ports}.

%%--------------------------------------------------------------------

reg_name(LC = #lc{reg_name = Name}, Name) ->
    LC;
reg_name(LC = #lc{reg_name = undefined}, Name) ->
    LC#lc{reg_name = Name}.

%%--------------------------------------------------------------------

reg_port(LC0 = #lc{reg_ports = Ports0}, Name, Port, Route, Signal) ->
    LC = reg_name(LC0, Name),
    Ports = Ports0#{Port => {Signal, Route}},
    LC#lc{reg_ports = Ports}.

%%--------------------------------------------------------------------

ufm(At = {ufm, _, _}, Collect, Cells) ->
    case Cells of
        #{At := LC} ->
            Cells#{At => Collect(LC)};

        _ ->
            Cells#{At => Collect(#ufm{})}
    end.

%%--------------------------------------------------------------------

ufm_name(UFM = #ufm{name = Name}, Name) ->
    UFM;
ufm_name(UFM = #ufm{name = undefined}, Name) ->
    UFM#ufm{name = Name};
ufm_name(UFM, _Name) ->
    %io:format("UFM NAME ~p ~s~n", [UFM, _Name]),
    UFM.

%%--------------------------------------------------------------------

ufm_port(UFM0 = #ufm{ports = Ports0}, Name, Port, Route, Signal) ->
    UFM = ufm_name(UFM0, Name),
    Ports = Ports0#{Port => {Signal, Route}},
    UFM#ufm{ports = Ports}.

%%--------------------------------------------------------------------

fixup_lut(_, LC = #lc{feedback = true, lut = LUT}) ->
    % inverted C input
    LC#lc{
        lut = ((LUT band 16#F0F0) bsr 4) bor ((LUT band 16#0F0F) bsl 4)
    };
fixup_lut(_, Cell) ->
    Cell.

