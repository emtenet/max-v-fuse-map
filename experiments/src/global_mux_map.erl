-module(global_mux_map).

-export([to_small_interconnect/2]).
-export([from_small_interconnect/1]).

-export([to_large_interconnect/2]).
-export([from_large_interconnect/1]).

-export([mux6s/0]).
-export([mux4s/0]).
-export([mux3s/0]).

-export_type([mux6/0]).
-export_type([mux4/0]).
-export_type([mux3/0]).

% MAX V 5M240Z
-type small_interconnect() :: {interconnect, 0..17}.

% MAX V 5M570Z and above
-type large_interconnect() :: {interconnect, 0..9} | default.

-type mux6() :: mux0 | mux1 | mux2 | mux3 | mux4 | mux5.
-type mux4() :: mux0 | mux1 | mux2 | mux3.
-type mux3() :: mux0 | mux1 | mux2.

%%====================================================================
%% small
%%====================================================================

-define(SMALL_MAPPINGS(),
    ?INTERCONNECT(mux0, mux0, 0);
    ?INTERCONNECT(mux0, mux1, 1);
    ?INTERCONNECT(mux0, mux2, 2);
    ?INTERCONNECT(mux1, mux0, 3);
    ?INTERCONNECT(mux1, mux1, 4);
    ?INTERCONNECT(mux1, mux2, 5);
    ?INTERCONNECT(mux2, mux0, 6);
    ?INTERCONNECT(mux2, mux1, 7);
    ?INTERCONNECT(mux2, mux2, 8);
    ?INTERCONNECT(mux3, mux0, 9);
    ?INTERCONNECT(mux3, mux1, 10);
    ?INTERCONNECT(mux3, mux2, 11);
    ?INTERCONNECT(mux4, mux0, 12);
    ?INTERCONNECT(mux4, mux1, 13);
    ?INTERCONNECT(mux4, mux2, 14);
    ?INTERCONNECT(mux5, mux0, 15);
    ?INTERCONNECT(mux5, mux1, 16);
    ?INTERCONNECT(mux5, mux2, 17)
).

%%====================================================================
%% to_small_interconnect
%%====================================================================

-spec to_small_interconnect(mux6(), mux3()) -> small_interconnect().

-define(INTERCONNECT(Mux6, Mux3, N),
    to_small_interconnect(Mux6, Mux3) ->
        {interconnect, N}
).

?SMALL_MAPPINGS().

-undef(INTERCONNECT).

%%====================================================================
%% from_small_interconnect
%%====================================================================

-spec from_small_interconnect(small_interconnect()) -> {ok, mux6(), mux3()}.

-define(INTERCONNECT(Mux6, Mux3, N),
    from_small_interconnect({interconnect, N}) ->
        {ok, Mux6, Mux3}
).

?SMALL_MAPPINGS().

-undef(INTERCONNECT).

%%====================================================================
%% large
%%====================================================================

-define(LARGE_MAPPINTS(),
    ?INTERCONNECT(mux0, mux0, 0);
    ?INTERCONNECT(mux0, mux1, 1);
    ?INTERCONNECT(mux0, mux2, 2);
    ?INTERCONNECT(mux1, mux0, 3);
    ?INTERCONNECT(mux1, mux1, 4);
    ?INTERCONNECT(mux1, mux2, 5);
    ?INTERCONNECT(mux2, mux0, 6);
    ?INTERCONNECT(mux2, mux1, 7);
    ?INTERCONNECT(mux2, mux2, 8);
    ?INTERCONNECT(mux3, mux0, 9);
    ?DEFAULT     (mux3, mux1)
    ?UNDEFINED   (mux3, mux2)
).

%%====================================================================
%% to_large_interconnect
%%====================================================================

-spec to_large_interconnect(mux4(), mux3()) -> large_interconnect() | undefined.

-define(INTERCONNECT(Mux4, Mux3, N),
    to_large_interconnect(Mux4, Mux3) ->
        {interconnect, N}
).
-define(DEFAULT(Mux4, Mux3),
    to_large_interconnect(Mux4, Mux3) ->
        default;
).
-define(UNDEFINED(Mux4, Mux3),
    to_large_interconnect(Mux4, Mux3) ->
        undefined
).

?LARGE_MAPPINTS().

-undef(INTERCONNECT).
-undef(DEFAULT).
-undef(UNDEFINED).

%%====================================================================
%% from_large_interconnect
%%====================================================================

-spec from_large_interconnect(large_interconnect()) -> {ok, mux4(), mux3()}.

-define(INTERCONNECT(Mux4, Mux3, N),
    from_large_interconnect({interconnect, N}) ->
        {ok, Mux4, Mux3}
).
-define(DEFAULT(Mux4, Mux3),
    from_large_interconnect(default) ->
        {ok, Mux4, Mux3}
).
-define(UNDEFINED(Mux4, Mux3), ).

?LARGE_MAPPINTS().

-undef(INTERCONNECT).
-undef(DEFAULT).
-undef(UNDEFINED).

%%====================================================================
%% mux6s
%%====================================================================

-spec mux6s() -> [mux6()].

mux6s() ->
    [mux0, mux1, mux2, mux3, mux4, mux5].

%%====================================================================
%% mux4s
%%====================================================================

-spec mux4s() -> [mux4()].

mux4s() ->
    [mux0, mux1, mux2, mux3].

%%====================================================================
%% mux3s
%%====================================================================

-spec mux3s() -> [mux3()].

mux3s() ->
    [mux0, mux1, mux2].

%%====================================================================
%% tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

large_coverage_test() ->
    Interconnects = lists:usort([
        to_large_interconnect(Mux4, Mux3)
        ||
        Mux4 <- mux4s(),
        Mux3 <- mux3s()
    ]),
    ?assertEqual(1 + 10 + 1, length(Interconnects)).

%%--------------------------------------------------------------------

large_inverse_test() ->
    [
        large_inverse_test(Mux4, Mux3)
        ||
        Mux4 <- mux4s(),
        Mux3 <- mux3s()
    ],
    ok.

%%--------------------------------------------------------------------

large_inverse_test(Mux4, Mux3) ->
    case to_large_interconnect(Mux4, Mux3) of
        undefined ->
            ok;

        Interconnect ->
            ?assertEqual({ok, Mux4, Mux3}, from_large_interconnect(Interconnect))
    end.

%%--------------------------------------------------------------------

small_coverage_test() ->
    Interconnects = lists:usort([
        to_small_interconnect(Mux6, Mux3)
        ||
        Mux6 <- mux6s(),
        Mux3 <- mux3s()
    ]),
    ?assertEqual(18, length(Interconnects)).

%%--------------------------------------------------------------------

small_inverse_test() ->
    [
        small_inverse_test(Mux6, Mux3)
        ||
        Mux6 <- mux6s(),
        Mux3 <- mux3s()
    ],
    ok.

%%--------------------------------------------------------------------

small_inverse_test(Mux6, Mux3) ->
    Interconnect = to_small_interconnect(Mux6, Mux3),
    ?assertEqual({ok, Mux6, Mux3}, from_small_interconnect(Interconnect)).

-endif.

