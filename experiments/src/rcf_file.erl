-module(rcf_file).

-export([read/1]).
-export([decode/1]).

-export_type([rcf/0]).
-export_type([rcf_binary/0]).

-type rcf_binary() :: binary().

-type rcf() :: #{
    device => binary(),
    signals := #{name() => signal()}
}.

-type name() :: atom() | binary().

-type signal() :: #{
    name := name(),
    dests := [dest()],
    ioc => ioc(),
    lc => lc()
}.

-type dest() :: #{
    name := name(),
    port => dest_port(),
    route_port => route_port(),
    ioc => ioc(),
    lc => lc()
}.

-type dest_port() :: clk | data_a | data_b | data_c | data_d | data_in | s_clk.

-type route_port() :: data_a | data_b | data_c | data_d.

-type ioc() :: ioc:ioc().
-type lc() :: lc:lc().

-define(IS_DIGIT(C), ((C) >= $0 andalso (C) =< $9)).

%%====================================================================
%% read
%%====================================================================

-spec read(file:name_all()) -> {ok, rcf()}.

read(File) ->
    {ok, Data} = file:read_file(File),
    decode(Data).

%%====================================================================
%% decode
%%====================================================================

-spec decode(rcf_binary()) -> {ok, rcf()}.

decode(Data) when is_binary(Data) ->
    Lines = binary:split(Data, <<"\r\n">>, [global]),
    decode_lines(Lines, #{
        signals => #{},
        iocs => #{},
        lcs => #{}
    }).

%%--------------------------------------------------------------------

decode_lines([], RCF) ->
    {ok, RCF};
decode_lines([Line | Lines], RCF0) ->
    case decode(Line, RCF0) of
        skip ->
            decode_lines(Lines, RCF0);

        global ->
            decode_global_lines(Lines, RCF0, #{});

        {signal, Signal} ->
            decode_signal_lines(Lines, RCF0, Signal, [], #{})
    end.

%%--------------------------------------------------------------------

decode(<<>>, _) ->
    skip;
decode(<<"#", _/binary>>, _) ->
    skip;
decode(<<"section global_data {">>, _) ->
    global;
decode(<<"signal_name = ", Line/binary>>, _) ->
    [Signal, Comment] = binary:split(Line, <<" {\t#">>),
    case Comment of
        <<"LC_", _/binary>> ->
            {ok, LC, <<>>} = lc:parse(Comment),
            {signal, #{
                name => decode_name(Signal),
                lc => LC,
                dests => []
            }};

        <<"IOC_", _/binary>> ->
            {ok, IOC, <<>>} = ioc:parse(Comment),
            {signal, #{
                name => decode_name(Signal),
                ioc => IOC,
                dests => []
            }};

        <<"JTAG_", _/binary>> ->
            {ok, JTAG, <<>>} = jtag:parse(Comment),
            {signal, #{
                name => decode_name(Signal),
                jtag => JTAG,
                dests => []
            }};

        <<"UFM_", _/binary>> ->
            {ok, UFM, <<>>} = ufm:parse(Comment),
            {signal, #{
                name => decode_name(Signal),
                ufm => UFM,
                dests => []
            }}
    end.

%%--------------------------------------------------------------------

decode_global_lines([<<"}">> | Lines], RCF, Global) ->
    decode_lines(Lines, RCF#{global => Global});
decode_global_lines([Line | Lines], RCF, Global0) ->
    case decode_global(Line, Global0) of
        skip ->
            decode_global_lines(Lines, RCF, Global0);

        {ok, Global} ->
            decode_global_lines(Lines, RCF, Global)
    end.

%%--------------------------------------------------------------------

decode_global(<<"\trcf_written_by = \"", _/binary>>, _) ->
    skip;
decode_global(<<"\tdevice = ", Line/binary>>, Global) ->
    Size = byte_size(Line) - 1,
    <<Device:Size/binary, ";">> = Line,
    {ok, Global#{device => Device}}.

%%--------------------------------------------------------------------

decode_name(Name) ->
    try
        binary_to_existing_atom(Name)
    catch
        error:badarg ->
            Name
    end.

%%--------------------------------------------------------------------

decode_coord(<<"X", X10, X1, "Y", Rest/binary>>)
        when ?IS_DIGIT(X10) andalso ?IS_DIGIT(X1) ->
    decode_coord(Rest, (10 * (X10 - $0)) + (X1 - $0));
decode_coord(<<"X", X, "Y", Rest/binary>>)
        when ?IS_DIGIT(X) ->
    decode_coord(Rest, X - $0).

%%--------------------------------------------------------------------

decode_coord(<<Y10, Y1, "S", Rest/binary>>, X)
        when ?IS_DIGIT(Y10) andalso ?IS_DIGIT(Y1) ->
    decode_coord(Rest, X, (10 * (Y10 - $0)) + (Y1 - $0));
decode_coord(<<Y, "S", Rest/binary>>, X)
        when ?IS_DIGIT(Y) ->
    decode_coord(Rest, X, Y - $0).

%%--------------------------------------------------------------------

decode_coord(<<S10, S1, "I", Rest/binary>>, X, Y)
        when ?IS_DIGIT(S10) andalso ?IS_DIGIT(S1) ->
    decode_coord(Rest, X, Y, (10 * (S10 - $0)) + (S1 - $0));
decode_coord(<<S, "I", Rest/binary>>, X, Y)
        when ?IS_DIGIT(S) ->
    decode_coord(Rest, X, Y, S - $0).

%%--------------------------------------------------------------------

decode_coord(<<I10, I1, ";">>, X, Y, S)
        when ?IS_DIGIT(I10) andalso ?IS_DIGIT(I1) ->
    {X, Y, S, (10 * (I10 - $0)) + (I1 - $0)};
decode_coord(<<I, ";">>, X, Y, S)
        when ?IS_DIGIT(I) ->
    {X, Y, S, I - $0}.

%%--------------------------------------------------------------------

decode_signal_lines([<<"}">> | Lines], RCF, Signal, Stack, _) ->
    [] = Stack,
    case Signal of
        #{jtag := _, dests := []} ->
            % altera_internal_jtag
            decode_lines(Lines, RCF);

        #{name := Name} ->
            #{signals := Signals} = RCF,
            decode_lines(Lines, RCF#{signals => Signals#{Name => Signal}})
    end;
decode_signal_lines([<<>> | Lines], RCF, Signal, [], Labels) ->
    decode_signal_lines(Lines, RCF, Signal, [], Labels);
decode_signal_lines([Line0 | Lines], RCF, Signal0, Stack0, Labels0) ->
    <<"\t", Line/binary>> = Line0,
    case decode_signal(Line) of
        {branch_point, Label} ->
            [] = Stack0,
            #{Label := Stack} = Labels0,
            decode_signal_lines(Lines, RCF, Signal0, Stack, Labels0);

        {push, Top} ->
            Signal = rename_signal(Signal0, Top),
            Stack = [Top | Stack0],
            decode_signal_lines(Lines, RCF, Signal, Stack, Labels0);

        {push, Label, Top} ->
            Signal = rename_signal(Signal0, Top),
            Stack = [Top | Stack0],
            Labels = Labels0#{Label => Stack},
            decode_signal_lines(Lines, RCF, Signal, Stack, Labels);

        {dest, #{jtag := _}} when Stack0 =:= [] ->
            % altera_internal_jtag
            decode_signal_lines(Lines, RCF, Signal0, [], Labels0);

        {dest, Dest0} ->
            {Dest, Stack} = rename_dest(Dest0, Stack0),
            #{dests := Dests} = Signal0,
            Signal = Signal0#{
                dests => [Dest#{route => Stack} | Dests]
            },
            decode_signal_lines(Lines, RCF, Signal, [], Labels0)
    end.

%%--------------------------------------------------------------------

rename_dest(Dest = #{jtag := _}, Stack0) ->
    #{port := tdo_user} = Dest,
    [{jtag, X, Y, tdo} | Stack] = Stack0,
    {Dest#{jtag => {jtag, X, Y}, port => tdo}, Stack};
rename_dest(Dest = #{ufm := _}, Stack) ->
    [{local_interconnect, X, Y, 0, _} | _] = Stack,
    {Dest#{ufm => {ufm, X, Y}}, Stack};
rename_dest(Dest, Stack) ->
    {Dest, Stack}.

%%--------------------------------------------------------------------

rename_signal(Signal = #{jtag := _}, {jtag, X, Y, _}) ->
    Signal#{jtag => {jtag, X, Y}};
rename_signal(Signal = #{ufm := _}, {ufm, X, Y, _}) ->
    Signal#{ufm => {ufm, X, Y}};
rename_signal(Signal, _) ->
    Signal.

%%--------------------------------------------------------------------

decode_signal(<<"branch_point = ", Line/binary>>) ->
    Size = byte_size(Line) - 1,
    <<Label:Size/binary, ";">> = Line,
    {branch_point, Label};
decode_signal(<<"label = ", Line/binary>>) ->
    [Label, Rest] = binary:split(Line, <<", ">>),
    {push, Route} = decode_signal(Rest),
    {push, Label, Route};
decode_signal(<<"C4:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {c4, X, Y, S, I}};
decode_signal(<<"CLK_BUFFER:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {clk_buffer, X, Y, S, I}};
decode_signal(<<"GLOBAL_CLK_H:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {global_clk_h, X, Y, S, I}};
decode_signal(<<"GLOBAL_CLK_MUX:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {global_clk_mux, X, Y, S, I}};
decode_signal(<<"IO_BYPASS_OUT:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {io_bypass_out, X, Y, S, I}};
decode_signal(<<"IO_DATAIN:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {io_data_in, X, Y, S, I}};
decode_signal(<<"IO_DATAOUT:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {io_data_out, X, Y, S, I}};
decode_signal(<<"IO_OE_PIN:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {io_oe, X, Y, S, I}};
decode_signal(<<"JTAG_TCKUTAP_PIN:", Line/binary>>) ->
    {X, Y, _, _} = decode_coord(Line),
    {push, {jtag, X, Y, tck}};
decode_signal(<<"JTAG_TDIUTAP_PIN:", Line/binary>>) ->
    {X, Y, _, _} = decode_coord(Line),
    {push, {jtag, X, Y, tdi}};
decode_signal(<<"JTAG_TDOUSER_PIN:", Line/binary>>) ->
    {X, Y, _, _} = decode_coord(Line),
    {push, {jtag, X, Y, tdo}};
decode_signal(<<"JTAG_TMSUTAP_PIN:", Line/binary>>) ->
    {X, Y, _, _} = decode_coord(Line),
    {push, {jtag, X, Y, tms}};
decode_signal(<<"LAB_CLK:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {lab_clk, X, Y, S, I}};
decode_signal(<<"LAB_CONTROL_MUX:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {lab_control_mux, X, Y, S, I}};
decode_signal(<<"LE_BUFFER:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {le_buffer, X, Y, S, I}};
decode_signal(<<"LOCAL_INTERCONNECT:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {local_interconnect, X, Y, S, I}};
decode_signal(<<"LOCAL_LINE:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {local_line, X, Y, S, I}};
decode_signal(<<"LUT_CHAIN:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {lut_chain, X, Y, S, I}};
decode_signal(<<"R4:", Line/binary>>) ->
    {X, Y, S, I} = decode_coord(Line),
    {push, {r4, X, Y, S, I}};
decode_signal(<<"UFM_OUT:", Line/binary>>) ->
    case decode_coord(Line) of
        {X, Y, _, 0} ->
            {push, {ufm, X, Y, dr_out}};

        {X, Y, _, 1} ->
            {push, {ufm, X, Y, busy}};

        {X, Y, _, 2} ->
            {push, {ufm, X, Y, osc}};

        {X, Y, _, 3} ->
            {push, {ufm, X, Y, isp_busy}}
    end;
decode_signal(<<"dest = ( ", Line/binary>>) ->
    [Name, Rest] = binary:split(Line, <<", ">>),
    decode_dest(decode_name(Name), Rest).

%%--------------------------------------------------------------------

decode_dest(Name, <<"ACLR )", Line/binary>>) ->
    decode_dest_lc(Name, a_clr, Line);
decode_dest(Name, <<"ALOAD )", Line/binary>>) ->
    decode_dest_lc(Name, a_load, Line);
decode_dest(Name, <<"ARCLK )", Line/binary>>) ->
    decode_dest_ufm(Name, ar_clk, Line);
decode_dest(Name, <<"ARDIN )", Line/binary>>) ->
    decode_dest_ufm(Name, ar_in, Line);
decode_dest(Name, <<"ARSHFT )", Line/binary>>) ->
    decode_dest_ufm(Name, ar_shift, Line);
decode_dest(Name, <<"CLK )", Line/binary>>) ->
    decode_dest_lc(Name, clk, Line);
decode_dest(Name, <<"DATAA ), route_port = ", Line/binary>>) ->
    decode_dest_route(Name, data_a, Line);
decode_dest(Name, <<"DATAB ), route_port = ", Line/binary>>) ->
    decode_dest_route(Name, data_b, Line);
decode_dest(Name, <<"DATAC ), route_port = ", Line/binary>>) ->
    decode_dest_route(Name, data_c, Line);
decode_dest(Name, <<"DATAD ), route_port = ", Line/binary>>) ->
    decode_dest_route(Name, data_d, Line);
decode_dest(Name, <<"DATAIN )", Line/binary>>) ->
    decode_dest_ioc(Name, data_in, Line);
decode_dest(Name, <<"DRCLK )", Line/binary>>) ->
    decode_dest_ufm(Name, dr_clk, Line);
decode_dest(Name, <<"DRDIN )", Line/binary>>) ->
    decode_dest_ufm(Name, dr_in, Line);
decode_dest(Name, <<"DRSHFT )", Line/binary>>) ->
    decode_dest_ufm(Name, dr_shift, Line);
decode_dest(Name, <<"ENA )", Line/binary>>) ->
    decode_dest_lc(Name, ena, Line);
decode_dest(Name, <<"ERASE )", Line/binary>>) ->
    decode_dest_ufm(Name, erase, Line);
decode_dest(Name, <<"INVERTA )", Line/binary>>) ->
    decode_dest_lc(Name, invert_a, Line);
decode_dest(Name, <<"OE )", Line/binary>>) ->
    decode_dest_ioc(Name, oe, Line);
decode_dest(Name, <<"OSCENA )", Line/binary>>) ->
    decode_dest_ufm(Name, osc_ena, Line);
decode_dest(Name, <<"PROGRAM )", Line/binary>>) ->
    decode_dest_ufm(Name, program, Line);
decode_dest(Name, <<"SCLR )", Line/binary>>) ->
    decode_dest_lc(Name, s_clr, Line);
decode_dest(Name, <<"SLOAD )", Line/binary>>) ->
    decode_dest_lc(Name, s_load, Line);
decode_dest(Name, <<"SYNCH_DATA ), route_port = ", Line/binary>>) ->
    decode_dest_route(Name, s_data, Line);
decode_dest(Name, <<"TCK )", Line/binary>>) ->
    decode_dest_jtag(Name, tck, Line);
decode_dest(Name, <<"TDI )", Line/binary>>) ->
    decode_dest_jtag(Name, tdi, Line);
decode_dest(Name, <<"TDO )", Line/binary>>) ->
    decode_dest_jtag(Name, tdo, Line);
decode_dest(Name, <<"TDOUSER )", Line/binary>>) ->
    decode_dest_jtag(Name, tdo_user, Line);
decode_dest(Name, <<"TMS )", Line/binary>>) ->
    decode_dest_jtag(Name, tms, Line).

%%--------------------------------------------------------------------

decode_dest_ioc(Name, Port, <<";\t#", IOC0/binary>>) ->
    {ok, IOC, <<>>} = ioc:parse(IOC0),
    {dest, #{
        name => Name,
        port => Port,
        ioc => IOC
    }}.

%%--------------------------------------------------------------------

decode_dest_jtag(Name, Port, <<";\t#", JTAG0/binary>>) ->
    {ok, JTAG, <<>>} = jtag:parse(JTAG0),
    {dest, #{
        name => Name,
        port => Port,
        jtag => JTAG
    }}.

%%--------------------------------------------------------------------

decode_dest_lc(Name, Port, <<";\t#", LC0/binary>>) ->
    {ok, LC, <<>>} = lc:parse(LC0),
    {dest, #{
        name => Name,
        port => Port,
        lc => LC
    }}.

%%--------------------------------------------------------------------

decode_dest_ufm(Name, Port, <<";\t#", UFM0/binary>>) ->
    {ok, UFM, <<>>} = ufm:parse(UFM0),
    {dest, #{
        name => Name,
        port => Port,
        ufm => UFM
    }}.

%%--------------------------------------------------------------------

decode_dest_route(Name, Port, <<"DATAA;\t#", LC0/binary>>) ->
    {ok, LC, <<>>} = lc:parse(LC0),
    {dest, #{
        name => Name,
        port => Port,
        route_port => data_a,
        lc => LC
    }};
decode_dest_route(Name, Port, <<"DATAB;\t#", LC0/binary>>) ->
    {ok, LC, <<>>} = lc:parse(LC0),
    {dest, #{
        name => Name,
        port => Port,
        route_port => data_b,
        lc => LC
    }};
decode_dest_route(Name, Port, <<"DATAC;\t#", LC0/binary>>) ->
    {ok, LC, <<>>} = lc:parse(LC0),
    {dest, #{
        name => Name,
        port => Port,
        route_port => data_c,
        lc => LC
    }};
decode_dest_route(Name, Port, <<"DATAD;\t#", LC0/binary>>) ->
    {ok, LC, <<>>} = lc:parse(LC0),
    {dest, #{
        name => Name,
        port => Port,
        route_port => data_d,
        lc => LC
    }}.

%%====================================================================
%% test
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

led_test() ->
    {ok, _} = read("tests/led.rcf").

-endif.

