-module(lc).

-export([name/1]).
-export([from/3]).
-export([parse/1]).
-export([write/2]).
-export([lab/1]).
-export([inputs/0]).
-export([carry_from/1]).
-export([carry_from/2]).
-export([carry_to/1]).
-export([carry_to/2]).
-export([chain_from/1]).
-export([chain_to/1]).

-export_type([lc/0]).
-export_type([input/0]).

-type density() :: density:density().
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

%%====================================================================
%% carry_from
%%====================================================================

-spec carry_from(lc()) -> lc().

carry_from({lc, X, Y, 0}) ->
    {lc, X - 1, Y, 9};
carry_from({lc, X, Y, N}) ->
    {lc, X, Y, N - 1}.

%%--------------------------------------------------------------------

-spec carry_from(lc(), density()) -> {ok, lc()} | false.

carry_from({lc, X, Y, 0}, Density) ->
    case density:is_lab(X - 1, Y, Density) of
        true ->
            {ok, {lc, X - 1, Y, 9}};

        false ->
            false
    end;
carry_from({lc, X, Y, N}, _) ->
    {ok, {lc, X, Y, N - 1}}.

%%====================================================================
%% carry_to
%%====================================================================

-spec carry_to(lc()) -> lc().

carry_to({lc, X, Y, 9}) ->
    {lc, X + 1, Y, 0};
carry_to({lc, X, Y, N}) ->
    {lc, X, Y, N + 1}.

%%--------------------------------------------------------------------

-spec carry_to(lc(), density()) -> {ok, lc()} | false.

carry_to({lc, X, Y, 9}, Density) ->
    case density:is_lab(X + 1, Y, Density) of
        true ->
            {ok, {lc, X + 1, Y, 9}};

        false ->
            false
    end;
carry_to({lc, X, Y, N}, _) ->
    {ok, {lc, X, Y, N + 1}}.

%%====================================================================
%% chain_from
%%====================================================================

-spec chain_from(lc()) -> {ok, lc()} | false.

chain_from({lc, _, _, 0}) ->
    false;
chain_from({lc, X, Y, N}) ->
    {ok, {lc, X, Y, N - 1}}.

%%====================================================================
%% chain_to
%%====================================================================

-spec chain_to(lc()) -> {ok, lc()} | false.

chain_to({lc, _, _, 9}) ->
    false;
chain_to({lc, X, Y, N}) ->
    {ok, {lc, X, Y, N + 1}}.

