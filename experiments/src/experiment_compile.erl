-module(experiment_compile).

-export([pre/1]).
-export([connect/0]).
-export([post/2]).

-export_type([files/0]).
-export_type([job_ref/0]).
-export_type([result/0]).
-export_type([source/0]).

-type compile() :: experiment:compile().
-type device() :: device:device().

-type files() :: #{
    pof := binary(),
    rcf := binary()
}.

-type job_ref() :: reference().

-type result() :: {ok, files()} | {error, term()}.

-type source() :: #{
    title := experiment:title(),
    device := binary(),
    settings := binary(),
    verilog := binary()
}.

%%====================================================================
%% pre
%%====================================================================

-spec pre(compile()) -> source().

pre(#{title := Title, device := Device, settings := Settings, verilog := Verilog})
        when is_atom(Device) andalso
             is_list(Settings) andalso
             is_binary(Verilog) ->
    #{
        title => Title,
        family => <<"MAX V">>,
        device => device:name(Device),
        settings => setting:encode(Settings),
        verilog => Verilog
    }.

%%====================================================================
%% connect
%%====================================================================

-spec connect() -> ok.

connect() ->
    case nodes() of
        [] ->
            pong = net_adm:ping('quartus@SILENT-PC'),
            ok = global:sync(),
            case global:whereis_name(quartus) of
                undefined ->
                    not_connected;

                Pid when is_pid(Pid) ->
                    ok
            end;

        [_] ->
            ok
    end.

%%====================================================================
%% post
%%====================================================================

-spec post(device(), result()) -> {ok, experiment:result()} | error.

post(Device, {ok, #{pof := POF, rcf := RCF}}) ->
    {ok, {Device, POF, RCF}};
post(_, {error, {quartus_map, Exit, Out}}) ->
    post_out("MAP", Exit, Out);
post(_, {error, {quartus_fit, Exit, Out}}) ->
    post_out("FIT", Exit, Out);
post(_, {error, {quartus_asm, Exit, Out}}) ->
    post_out("ASM", Exit, Out);
post(_, {error, {quartus_cdb, Exit, Out}}) ->
    post_out("CDB", Exit, Out);
post(_, {error, Error}) ->
    io:format("QUARTUS: ~p~n", [Error]),
    error.

%%--------------------------------------------------------------------

post_out(Who, Exit, Out) ->
    io:format("========================================~n", []),
    io:format("~s~n", [Out]),
    io:format("========================================~n", []),
    io:format("QUARTUS ~s: exit # ~p~n", [Who, Exit]),
    error.

