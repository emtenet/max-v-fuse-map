-module(lc).

-export([name/1]).
-export([from/3]).
-export([parse/1]).
-export([write/2]).
-export([lab/1]).
-export([inputs/0]).

-export_type([lc/0]).
-export_type([input/0]).

-type lab() :: lab:lab().
-type x() :: max_v:x().
-type y() :: max_v:y().
-type n() :: max_v:n().

-type lc() :: {lc, x(), y(), n()}.

-type input() :: data_a | data_b | data_c | data_d.

-define(IS_DIGIT(C), ((C) >= $0 andalso (C) =< $9)).

%%====================================================================
%% name
%%====================================================================

-spec name(lc()) -> binary().

name(LC) ->
    write(<<>>, LC).

%%====================================================================
%% from
%%====================================================================

-spec from(1..20, 1..13, 0..9) -> lc().

from(X, Y, N)
        when (X >= 1 andalso X =< 20) andalso
             (Y >= 1 andalso Y =< 13) andalso
             (N >= 0 andalso N =< 9) ->
    {lc, X, Y, N}.

%%====================================================================
%% parse
%%====================================================================

-spec parse(binary()) -> {ok, lc(), binary()}.

parse(Name) ->
    {ok, [X, Y, N], Rest} = parse:format(Name, [
        <<"LC_X">>,
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

-spec write(binary(), lc()) -> binary().

write(To, {lc, X, Y, N}) ->
    write:format(To, [
        <<"LC_X">>,
        X,
        <<"_Y">>,
        Y,
        <<"_N">>,
        N
    ]).

%%====================================================================
%% lab
%%====================================================================

-spec lab(lc()) -> lab().

lab({lc, X, Y, _}) ->
    {lab, X, Y}.

%%====================================================================
%% inputs
%%====================================================================

-spec inputs() -> [input()].

inputs() ->
    [data_a, data_b, data_c, data_d].

