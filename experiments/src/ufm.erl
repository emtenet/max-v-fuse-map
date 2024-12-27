-module(ufm).

-export([name/1]).
-export([from/3]).
-export([parse/1]).
-export([write/2]).

-export_type([ufm/0]).

-type x() :: max_v:x().
-type y() :: max_v:y().
-type n() :: max_v:n().

-type ufm() :: {ufm, x(), y(), n()}.

%%====================================================================
%% name
%%====================================================================

-spec name(ufm()) -> binary().

name(LC) ->
    write(<<>>, LC).

%%====================================================================
%% from
%%====================================================================

-spec from(0..20, 1..13, 0..9) -> ufm().

from(X, Y, N)
        when (X >= 0 andalso X =< 20) andalso
             (Y >= 1 andalso Y =< 13) andalso
             (N >= 0 andalso N =< 9) ->
    {ufm, X, Y, N}.

%%====================================================================
%% parse
%%====================================================================

-spec parse(binary()) -> {ok, ufm(), binary()}.

parse(Name) ->
    {ok, [X, Y, N], Rest} = parse:format(Name, [
        <<"UFM_X">>,
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

-spec write(binary(), ufm()) -> binary().

write(To, {ufm, X, Y, N}) ->
    write:format(To, [
        <<"UFM_X">>,
        X,
        <<"_Y">>,
        Y,
        <<"_N">>,
        N
    ]).

