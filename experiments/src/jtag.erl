-module(jtag).

-export([name/1]).
-export([from/3]).
-export([parse/1]).
-export([write/2]).

-export_type([jtag/0]).

-type x() :: max_v:x().
-type y() :: max_v:y().
-type n() :: max_v:n().

-type jtag() :: {jtag, x(), y(), n()}.

%%====================================================================
%% name
%%====================================================================

-spec name(jtag()) -> binary().

name(LC) ->
    write(<<>>, LC).

%%====================================================================
%% from
%%====================================================================

-spec from(1..20, 1..13, 0..9) -> jtag().

from(X, Y, N)
        when (X >= 1 andalso X =< 20) andalso
             (Y >= 1 andalso Y =< 13) andalso
             (N >= 0 andalso N =< 9) ->
    {jtag, X, Y, N}.

%%====================================================================
%% parse
%%====================================================================

-spec parse(binary()) -> {ok, jtag(), binary()}.

parse(Name) ->
    {ok, [X, Y, N], Rest} = parse:format(Name, [
        <<"JTAG_X">>,
        number,
        <<"_Y">>,
        number,
        <<"_N">>,
        number
    ]),
    {ok, from(X, Y, N), Rest}.

%%====================================================================
%% write
%%====================================================================

-spec write(binary(), jtag()) -> binary().

write(To, {jtag, X, Y, N}) ->
    write:format(To, [
        <<"JTAG_X">>,
        X,
        <<"_Y">>,
        Y,
        <<"_N">>,
        N
    ]).

