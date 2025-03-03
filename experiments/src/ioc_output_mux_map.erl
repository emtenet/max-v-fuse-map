-module(ioc_output_mux_map).

-export([to_col_interconnect/2]).
-export([from_col_interconnect/1]).

-export([to_row_interconnect/2]).
-export([from_row_interconnect/1]).

-export([fast_out/2]).
-export([fast_out_column/1]).
-export([fast_out_row/1]).

-export([mux6s/0]).
-export([mux4s/0]).
-export([mux3s/0]).

-export_type([mux6/0]).
-export_type([mux4/0]).
-export_type([mux3/0]).

-type col_interconnect() :: iob:col_interconnect().
-type row_interconnect() :: iob:row_interconnect().

-type mux6() :: mux0 | mux1 | mux2 | mux3 | mux4 | mux5.
-type mux4() :: mux0 | mux1 | mux2 | mux3.
-type mux3() :: mux0 | mux1 | mux2.

%%====================================================================
%% table4
%%====================================================================

-define(COL_MAPPINTS(),
    ?UNDEFINED   (mux0, mux0)
    ?INTERCONNECT(mux0, mux1, 0);
    ?INTERCONNECT(mux0, mux2, 1);
    ?INTERCONNECT(mux1, mux0, 2);
    ?INTERCONNECT(mux1, mux1, 3);
    ?INTERCONNECT(mux1, mux2, 4);
    ?FAST_OUT    (mux2, mux0);
    ?INTERCONNECT(mux2, mux1, 5);
    ?INTERCONNECT(mux2, mux2, 6);
    ?INTERCONNECT(mux3, mux0, 7);
    ?INTERCONNECT(mux3, mux1, 8);
    ?INTERCONNECT(mux3, mux2, 9)
).

%%====================================================================
%% to_col_interconnect
%%====================================================================

-spec to_col_interconnect(mux4(), mux3()) -> col_interconnect() | undefined.

-define(UNDEFINED(Mux4, Mux3),
    to_col_interconnect(Mux4, Mux3) ->
        undefined;
).
-define(INTERCONNECT(Mux4, Mux3, N),
    to_col_interconnect(Mux4, Mux3) ->
        {interconnect, N}
).
-define(FAST_OUT(Mux4, Mux3),
    to_col_interconnect(Mux4, Mux3) ->
        fast_out
).

?COL_MAPPINTS().

-undef(UNDEFINED).
-undef(INTERCONNECT).
-undef(FAST_OUT).

%%====================================================================
%% from_col_interconnect
%%====================================================================

-spec from_col_interconnect(col_interconnect()) -> {ok, mux6(), mux3()}.

-define(UNDEFINED(Mux4, Mux3), ).
-define(INTERCONNECT(Mux4, Mux3, N),
    from_col_interconnect({interconnect, N}) ->
        {ok, Mux4, Mux3}
).
-define(FAST_OUT(Mux4, Mux3),
    from_col_interconnect(fast_out) ->
        {ok, Mux4, Mux3}
).

?COL_MAPPINTS().

-undef(UNDEFINED).
-undef(INTERCONNECT).
-undef(FAST_OUT).

%%====================================================================
%% table7
%%====================================================================

-define(ROW_MAPPINGS(),
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
%% to_row_interconnect
%%====================================================================

-spec to_row_interconnect(mux6(), mux3()) -> row_interconnect().

-define(INTERCONNECT(Mux6, Mux3, N),
    to_row_interconnect(Mux6, Mux3) ->
        {interconnect, N}
).

?ROW_MAPPINGS().

-undef(INTERCONNECT).

%%====================================================================
%% from_row_interconnect
%%====================================================================

-spec from_row_interconnect(row_interconnect()) -> {ok, mux6(), mux3()}.

-define(INTERCONNECT(Mux6, Mux3, N),
    from_row_interconnect({interconnect, N}) ->
        {ok, Mux6, Mux3}
).

?ROW_MAPPINGS().

-undef(INTERCONNECT).

%%====================================================================
%% fast_out
%%====================================================================

%-spec fast_out(ioc(), density() | #metric{}) -> {le_buffer, x(), y(), _, n()}.

fast_out({ioc, X, Y, N}, Density) ->
    fast_out(X, Y, N, density:block_type(X, Y, Density)).

%%--------------------------------------------------------------------

fast_out_column({ioc, X, Y, N}) when Y > 3 ->
    fast_out_top(X, Y, N);
fast_out_column({ioc, X, Y, N}) ->
    fast_out_bottom(X, Y, N).

%%--------------------------------------------------------------------

fast_out_row({ioc, X, Y, N}) when X < 2 ->
    fast_out_left(X, Y, N);
fast_out_row({ioc, X, Y, N}) ->
    fast_out_right(X, Y, N).

%%--------------------------------------------------------------------

fast_out(X, Y, N, row) when X < 2 ->
    fast_out_left(X, Y, N);
fast_out(X, Y, N, row) ->
    fast_out_right(X, Y, N);
fast_out(X, Y, N, column) when Y > 3 ->
    fast_out_top(X, Y, N);
fast_out(X, Y, N, column) ->
    fast_out_bottom(X, Y, N).

%%--------------------------------------------------------------------

fast_out_left(X, Y, 0) -> {le_buffer, X + 1, Y, 0, 4};
fast_out_left(X, Y, 1) -> {le_buffer, X + 1, Y, 0, 6};
fast_out_left(X, Y, 2) -> {le_buffer, X + 1, Y, 0, 8};
fast_out_left(X, Y, 3) -> {le_buffer, X + 1, Y, 0, 18};
fast_out_left(X, Y, 4) -> {le_buffer, X + 1, Y, 0, 16};
fast_out_left(X, Y, 5) -> {le_buffer, X + 1, Y, 0, 14};
fast_out_left(X, Y, 6) -> {le_buffer, X + 1, Y, 0, 12}.

%%--------------------------------------------------------------------

fast_out_right(X, Y, 0) -> {le_buffer, X - 1, Y, 0, 5};
fast_out_right(X, Y, 1) -> {le_buffer, X - 1, Y, 0, 7};
fast_out_right(X, Y, 2) -> {le_buffer, X - 1, Y, 0, 9};
fast_out_right(X, Y, 3) -> {le_buffer, X - 1, Y, 0, 19};
fast_out_right(X, Y, 4) -> {le_buffer, X - 1, Y, 0, 17};
fast_out_right(X, Y, 5) -> {le_buffer, X - 1, Y, 0, 15}.

%%--------------------------------------------------------------------

fast_out_top(X, Y, 0) -> {le_buffer, X, Y - 1, 0, 7};
fast_out_top(X, Y, 1) -> {le_buffer, X, Y - 1, 0, 5};
fast_out_top(X, Y, 2) -> {le_buffer, X, Y - 1, 0, 3};
fast_out_top(X, Y, 3) -> {le_buffer, X, Y - 1, 0, 1}.

%%--------------------------------------------------------------------

fast_out_bottom(X, Y, 0) -> {le_buffer, X, Y + 1, 0, 11};
fast_out_bottom(X, Y, 1) -> {le_buffer, X, Y + 1, 0, 13};
fast_out_bottom(X, Y, 2) -> {le_buffer, X, Y + 1, 0, 15};
fast_out_bottom(X, Y, 3) -> {le_buffer, X, Y + 1, 0, 17}.

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

col_coverage_test() ->
    Interconnects = lists:usort([
        to_col_interconnect(Mux4, Mux3)
        ||
        Mux4 <- mux4s(),
        Mux3 <- mux3s()
    ]),
    ?assertEqual(1 + 10 + 1, length(Interconnects)).

%%--------------------------------------------------------------------

col_inverse_test() ->
    [
        col_inverse_test(Mux4, Mux3)
        ||
        Mux4 <- mux4s(),
        Mux3 <- mux3s()
    ],
    ok.

%%--------------------------------------------------------------------

col_inverse_test(Mux4, Mux3) ->
    case to_col_interconnect(Mux4, Mux3) of
        undefined ->
            ok;

        Interconnect ->
            ?assertEqual({ok, Mux4, Mux3}, from_col_interconnect(Interconnect))
    end.

%%--------------------------------------------------------------------

row_coverage_test() ->
    Interconnects = lists:usort([
        to_row_interconnect(Mux6, Mux3)
        ||
        Mux6 <- mux6s(),
        Mux3 <- mux3s()
    ]),
    ?assertEqual(18, length(Interconnects)).

%%--------------------------------------------------------------------

row_inverse_test() ->
    [
        row_inverse_test(Mux6, Mux3)
        ||
        Mux6 <- mux6s(),
        Mux3 <- mux3s()
    ],
    ok.

%%--------------------------------------------------------------------

row_inverse_test(Mux6, Mux3) ->
    Interconnect = to_row_interconnect(Mux6, Mux3),
    ?assertEqual({ok, Mux6, Mux3}, from_row_interconnect(Interconnect)).

-endif.

