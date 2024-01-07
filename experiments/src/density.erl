-module(density).

-export([list/0]).
-export([devices/1]).
-export([largest_device/1]).
-export([fuse_count/1]).
-export([or_device/1]).
-export([minimal_fuses/1]).
-export([fast_outs/1]).
-export([iobs/1]).
-export([labs/1]).
-export([metric/1]).
-export([top_io/1]).
-export([top_lab/1]).
-export([left_io/2]).
-export([left_lab/2]).
-export([right_io/1]).
-export([right_lab/1]).
-export([bottom_io/2]).
-export([bottom_lab/2]).
-export([columns/1]).
-export([rows/1]).
-export([left_rows/1]).
-export([right_rows/1]).
-export([global_block/1]).
-export([block_type/3]).
-export([is_lab/2]).
-export([is_lab/3]).
-export([is_iob/2]).
-export([is_iob/3]).
-export([is_column_iob/2]).
-export([is_column_iob/3]).
-export([is_row_iob/2]).
-export([is_row_iob/3]).
-export([is_left_iob/2]).
-export([is_left_iob/3]).
-export([is_top_iob/2]).
-export([is_top_iob/3]).
-export([is_right_iob/2]).
-export([is_right_iob/3]).
-export([is_bottom_iob/2]).
-export([is_bottom_iob/3]).
-export([is_pci_iob/2]).
-export([is_pci_iob/3]).

-export_type([density/0]).

-include("max_v.hrl").

-type density() ::
    max_v_240z |
    max_v_570z |
    max_v_1270z |
    max_v_2210z.

-type device() :: device:device().
-type fuse() :: fuse:fuse().
-type iob() :: iob:iob().
-type ioc() :: ioc:ioc().
-type lab() :: lab:lab().
-type lc() :: lc:lc().
-type x() :: max_v:x().
-type y() :: max_v:y().

%%====================================================================
%% list
%%====================================================================

-spec list() -> [density()].

list() ->
    [max_v_240z,
     max_v_570z,
     max_v_1270z,
     max_v_2210z
    ].

%%====================================================================
%% devices
%%====================================================================

-spec devices(density()) -> [device()].

devices(max_v_40z) ->
    [max_v_40z_e64,
     max_v_40z_m64,
     max_v_80z_e64,
     max_v_80z_m64,
     max_v_80z_m68,
     max_v_80z_t100,
     max_v_160z_e64,
     max_v_160z_m68,
     max_v_160z_m100,
     max_v_160z_t100,
     max_v_240z_m68,
     max_v_240z_m100,
     max_v_240z_t100
    ];
devices(max_v_570z) ->
    [max_v_240z_t144,
     max_v_570z_m100,
     max_v_570z_t100,
     max_v_570z_t144,
     max_v_570z_f256
    ];
devices(max_v_1270z) ->
    [max_v_1270z_t144,
     max_v_1270z_f256
    ];
devices(max_v_2210z) ->
    [max_v_1270z_f324,
     max_v_2210z_f256,
     max_v_2210z_f324
    ].

%%====================================================================
%% largest_device
%%====================================================================

-spec largest_device(density()) -> device().

largest_device(max_v_240z) -> max_v_240z_t100;
largest_device(max_v_570z) -> max_v_570z_f256;
largest_device(max_v_1270z) -> max_v_1270z_f256;
largest_device(max_v_2210z) -> max_v_2210z_f324.

%%====================================================================
%% fuse_count
%%====================================================================

-spec fuse_count(density() | device()) -> 53248 | 110592 | 208896 | 348160.

fuse_count(max_v_240z) -> 53248;
fuse_count(max_v_570z) -> 110592;
fuse_count(max_v_1270z) -> 208896;
fuse_count(max_v_2210z) -> 348160.

%%====================================================================
%% or_device
%%====================================================================

-spec or_device(density() | device()) -> density().

or_device(Density = max_v_240z) -> Density;
or_device(Density = max_v_570z) -> Density;
or_device(Density = max_v_1270z) -> Density;
or_device(Density = max_v_2210z) -> Density;
or_device(Device) ->
    device:density(Device).

%%====================================================================
%% minimal_fuses
%%====================================================================

-spec minimal_fuses(density() | device()) -> [fuse()].

minimal_fuses(max_v_240z) -> max_v_240z_minimal:fuses();
minimal_fuses(max_v_570z) -> max_v_570z_minimal:fuses();
minimal_fuses(max_v_1270z) -> max_v_1270z_minimal:fuses();
minimal_fuses(max_v_2210z) -> max_v_2210z_minimal:fuses();
minimal_fuses(Device) ->
    minimal_fuses(device:density(Device)).

%%====================================================================
%% fast_outs
%%====================================================================

-spec fast_outs(density() | device()) -> [{ioc(), lc(), left | right}].

fast_outs(max_v_240z) -> max_v_240z_fast_out:iocs();
fast_outs(max_v_570z) -> max_v_570z_fast_out:iocs();
fast_outs(max_v_1270z) -> max_v_1270z_fast_out:iocs();
fast_outs(max_v_2210z) -> max_v_2210z_fast_out:iocs();
fast_outs(Device) ->
    fast_outs(device:density(Device)).

%%====================================================================
%% iobs
%%====================================================================

-spec iobs(density()) -> [{iob(), lab()}].

-define(IOB_LEFT(X, Y), {{iob, X, Y}, {lab, X + 1, Y}}).
-define(IOB_BOTTOM(X, Y), {{iob, X, Y}, {lab, X, Y + 1}}).
-define(IOB_RIGHT(X, Y), {{iob, X, Y}, {lab, X - 1, Y}}).
-define(IOB_TOP(X, Y), {{iob, X, Y}, {lab, X, Y - 1}}).

iobs(max_v_240z) ->
    [?IOB_LEFT(1, 4),
     ?IOB_LEFT(1, 3),
     ?IOB_LEFT(1, 2),
     ?IOB_LEFT(1, 1),
     ?IOB_TOP(2, 5),
     ?IOB_TOP(3, 5),
     ?IOB_TOP(4, 5),
     ?IOB_TOP(5, 5),
     ?IOB_TOP(6, 5),
     ?IOB_TOP(7, 5),
     ?IOB_RIGHT(8, 4),
     ?IOB_RIGHT(8, 3),
     ?IOB_RIGHT(8, 2),
     ?IOB_RIGHT(8, 1),
     ?IOB_BOTTOM(7, 0),
     ?IOB_BOTTOM(6, 0),
     ?IOB_BOTTOM(5, 0),
     ?IOB_BOTTOM(4, 0),
     ?IOB_BOTTOM(3, 0),
     ?IOB_BOTTOM(2, 0)];
iobs(max_v_570z) ->
    [?IOB_LEFT(0, 4),
     ?IOB_LEFT(0, 5),
     ?IOB_LEFT(0, 6),
     ?IOB_LEFT(0, 7),
     ?IOB_TOP(1, 8),
     ?IOB_TOP(2, 8),
     ?IOB_TOP(3, 8),
     ?IOB_TOP(4, 8),
     ?IOB_TOP(5, 8),
     ?IOB_TOP(6, 8),
     ?IOB_TOP(7, 8),
     ?IOB_TOP(8, 8),
     ?IOB_TOP(9, 8),
     ?IOB_TOP(10, 8),
     ?IOB_TOP(11, 8),
     ?IOB_TOP(12, 8),
     ?IOB_RIGHT(13, 7),
     ?IOB_RIGHT(13, 6),
     ?IOB_RIGHT(13, 5),
     ?IOB_RIGHT(13, 4),
     ?IOB_RIGHT(13, 3),
     ?IOB_RIGHT(13, 2),
     ?IOB_RIGHT(13, 1),
     ?IOB_BOTTOM(12, 0),
     ?IOB_BOTTOM(11, 0),
     ?IOB_BOTTOM(10, 0),
     %?IOB_BOTTOM(9, 3),
     ?IOB_BOTTOM(8, 3),
     ?IOB_BOTTOM(7, 3),
     ?IOB_BOTTOM(6, 3),
     ?IOB_BOTTOM(5, 3),
     ?IOB_BOTTOM(4, 3),
     ?IOB_BOTTOM(3, 3),
     ?IOB_BOTTOM(2, 3),
     ?IOB_BOTTOM(1, 3)];
iobs(max_v_1270z) ->
    [?IOB_LEFT(0, 4),
     ?IOB_LEFT(0, 5),
     ?IOB_LEFT(0, 6),
     ?IOB_LEFT(0, 7),
     ?IOB_LEFT(0, 8),
     ?IOB_LEFT(0, 9),
     ?IOB_LEFT(0, 10),
     ?IOB_TOP(1, 11),
     ?IOB_TOP(2, 11),
     ?IOB_TOP(3, 11),
     ?IOB_TOP(4, 11),
     ?IOB_TOP(5, 11),
     ?IOB_TOP(6, 11),
     ?IOB_TOP(7, 11),
     ?IOB_TOP(8, 11),
     ?IOB_TOP(9, 11),
     ?IOB_TOP(10, 11),
     ?IOB_TOP(11, 11),
     ?IOB_TOP(12, 11),
     ?IOB_TOP(13, 11),
     ?IOB_TOP(14, 11),
     ?IOB_TOP(15, 11),
     ?IOB_TOP(16, 11),
     ?IOB_RIGHT(17, 10),
     ?IOB_RIGHT(17, 9),
     ?IOB_RIGHT(17, 8),
     ?IOB_RIGHT(17, 7),
     ?IOB_RIGHT(17, 6),
     ?IOB_RIGHT(17, 5),
     ?IOB_RIGHT(17, 4),
     ?IOB_RIGHT(17, 3),
     ?IOB_RIGHT(17, 2),
     ?IOB_RIGHT(17, 1),
     ?IOB_BOTTOM(16, 0),
     ?IOB_BOTTOM(15, 0),
     ?IOB_BOTTOM(14, 0),
     ?IOB_BOTTOM(13, 0),
     ?IOB_BOTTOM(12, 0),
     %?IOB_BOTTOM(11, 3),
     ?IOB_BOTTOM(10, 3),
     ?IOB_BOTTOM(9, 3),
     ?IOB_BOTTOM(8, 3),
     ?IOB_BOTTOM(7, 3),
     ?IOB_BOTTOM(6, 3),
     ?IOB_BOTTOM(5, 3),
     ?IOB_BOTTOM(4, 3),
     ?IOB_BOTTOM(3, 3),
     ?IOB_BOTTOM(2, 3),
     ?IOB_BOTTOM(1, 3)];
iobs(max_v_2210z) ->
    [?IOB_LEFT(0, 4),
     ?IOB_LEFT(0, 5),
     ?IOB_LEFT(0, 6),
     ?IOB_LEFT(0, 7),
     ?IOB_LEFT(0, 8),
     ?IOB_LEFT(0, 9),
     ?IOB_LEFT(0, 10),
     ?IOB_LEFT(0, 11),
     ?IOB_LEFT(0, 12),
     ?IOB_LEFT(0, 13),
     ?IOB_BOTTOM(1, 14),
     ?IOB_BOTTOM(2, 14),
     ?IOB_BOTTOM(3, 14),
     ?IOB_BOTTOM(4, 14),
     ?IOB_BOTTOM(5, 14),
     ?IOB_BOTTOM(6, 14),
     ?IOB_BOTTOM(7, 14),
     ?IOB_BOTTOM(8, 14),
     ?IOB_BOTTOM(9, 14),
     ?IOB_BOTTOM(10, 14),
     ?IOB_BOTTOM(11, 14),
     ?IOB_BOTTOM(12, 14),
     ?IOB_BOTTOM(13, 14),
     ?IOB_BOTTOM(14, 14),
     ?IOB_BOTTOM(15, 14),
     ?IOB_BOTTOM(16, 14),
     ?IOB_BOTTOM(17, 14),
     ?IOB_BOTTOM(18, 14),
     ?IOB_BOTTOM(19, 14),
     ?IOB_BOTTOM(20, 14),
     ?IOB_RIGHT(21, 13),
     ?IOB_RIGHT(21, 12),
     ?IOB_RIGHT(21, 11),
     ?IOB_RIGHT(21, 10),
     ?IOB_RIGHT(21, 9),
     ?IOB_RIGHT(21, 8),
     ?IOB_RIGHT(21, 7),
     ?IOB_RIGHT(21, 6),
     ?IOB_RIGHT(21, 5),
     ?IOB_RIGHT(21, 4),
     ?IOB_RIGHT(21, 3),
     ?IOB_RIGHT(21, 2),
     ?IOB_RIGHT(21, 1),
     ?IOB_BOTTOM(20, 0),
     ?IOB_BOTTOM(19, 0),
     ?IOB_BOTTOM(18, 0),
     ?IOB_BOTTOM(17, 0),
     ?IOB_BOTTOM(16, 0),
     ?IOB_BOTTOM(15, 0),
     ?IOB_BOTTOM(14, 0),
     %?IOB_BOTTOM(13, 3),
     ?IOB_BOTTOM(12, 3),
     ?IOB_BOTTOM(11, 3),
     ?IOB_BOTTOM(10, 3),
     ?IOB_BOTTOM(9, 3),
     ?IOB_BOTTOM(8, 3),
     ?IOB_BOTTOM(7, 3),
     ?IOB_BOTTOM(6, 3),
     ?IOB_BOTTOM(5, 3),
     ?IOB_BOTTOM(4, 3),
     ?IOB_BOTTOM(3, 3),
     ?IOB_BOTTOM(2, 3),
     ?IOB_BOTTOM(1, 3)].

%%====================================================================
%% labs
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fuses_test() ->
    ?assertEqual(24, length(labs(max_v_240z))),
    ?assertEqual(57, length(labs(max_v_570z))),
    ?assertEqual(127, length(labs(max_v_1270z))),
    ?assertEqual(221, length(labs(max_v_2210z))).

-endif.

-spec labs(density()) -> [lab()].

-define(COLUMN4(X, Y),
    {lab, X, Y - 0},
    {lab, X, Y - 1},
    {lab, X, Y - 2},
    {lab, X, Y - 3}
).
-define(COLUMN7(X, Y),
    {lab, X, Y - 0},
    {lab, X, Y - 1},
    {lab, X, Y - 2},
    {lab, X, Y - 3},
    {lab, X, Y - 4},
    {lab, X, Y - 5},
    {lab, X, Y - 6}
).
-define(COLUMN10(X, Y),
    {lab, X, Y - 0},
    {lab, X, Y - 1},
    {lab, X, Y - 2},
    {lab, X, Y - 3},
    {lab, X, Y - 4},
    {lab, X, Y - 5},
    {lab, X, Y - 6},
    {lab, X, Y - 7},
    {lab, X, Y - 8},
    {lab, X, Y - 9}
).
-define(COLUMN13(X, Y),
    {lab, X, Y - 0},
    {lab, X, Y - 1},
    {lab, X, Y - 2},
    {lab, X, Y - 3},
    {lab, X, Y - 4},
    {lab, X, Y - 5},
    {lab, X, Y - 6},
    {lab, X, Y - 7},
    {lab, X, Y - 8},
    {lab, X, Y - 9},
    {lab, X, Y - 10},
    {lab, X, Y - 11},
    {lab, X, Y - 12}
).

labs(max_v_240z) ->
    [?COLUMN4(2, 4),
     ?COLUMN4(3, 4),
     ?COLUMN4(4, 4),
     ?COLUMN4(5, 4),
     ?COLUMN4(6, 4),
     ?COLUMN4(7, 4)
    ];
labs(max_v_570z) ->
    [?COLUMN4(1, 7),
     ?COLUMN4(2, 7),
     ?COLUMN4(3, 7),
     ?COLUMN4(4, 7),
     ?COLUMN4(5, 7),
     ?COLUMN4(6, 7),
     ?COLUMN4(7, 7),
     ?COLUMN4(8, 7),
     ?COLUMN4(9, 7),
     ?COLUMN7(10, 7),
     ?COLUMN7(11, 7),
     ?COLUMN7(12, 7)
    ];
labs(max_v_1270z) ->
    [?COLUMN7(1, 10),
     ?COLUMN7(2, 10),
     ?COLUMN7(3, 10),
     ?COLUMN7(4, 10),
     ?COLUMN7(5, 10),
     ?COLUMN7(6, 10),
     ?COLUMN7(7, 10),
     ?COLUMN7(8, 10),
     ?COLUMN7(9, 10),
     ?COLUMN7(10, 10),
     ?COLUMN7(11, 10),
     ?COLUMN10(12, 10),
     ?COLUMN10(13, 10),
     ?COLUMN10(14, 10),
     ?COLUMN10(15, 10),
     ?COLUMN10(16, 10)
    ];
labs(max_v_2210z) ->
    [?COLUMN10(1, 13),
     ?COLUMN10(2, 13),
     ?COLUMN10(3, 13),
     ?COLUMN10(4, 13),
     ?COLUMN10(5, 13),
     ?COLUMN10(6, 13),
     ?COLUMN10(7, 13),
     ?COLUMN10(8, 13),
     ?COLUMN10(9, 13),
     ?COLUMN10(10, 13),
     ?COLUMN10(11, 13),
     ?COLUMN10(12, 13),
     ?COLUMN10(13, 13),
     ?COLUMN13(14, 13),
     ?COLUMN13(15, 13),
     ?COLUMN13(16, 13),
     ?COLUMN13(17, 13),
     ?COLUMN13(18, 13),
     ?COLUMN13(19, 13),
     ?COLUMN13(20, 13)
    ].

%%====================================================================
%% metric
%%====================================================================

-spec metric(density()) -> #metric{}.

-define(METRIC(L, LL, R, B, BB, T), #metric{
    density = Density,
    top_io = T,
    top_lab = T - 1,
    left_io = L,
    left_lab = L + 1,
    right_io = R,
    right_lab = R - 1,
    bottom_io = B,
    bottom_lab = B + 1,
    indent_left_io = LL,
    indent_left_lab = LL + 1,
    indent_right_lab = LL - 1,
    indent_bottom_io = BB,
    indent_bottom_lab = BB + 1,
    pattern_x = L,
    pattern_y = 3 - BB
}).

metric(Density = max_v_240z) -> ?METRIC(1, 1, 8, 0, 0, 5);
metric(Density = max_v_570z) -> ?METRIC(0, 9, 13, 0, 3, 8);
metric(Density = max_v_1270z) -> ?METRIC(0, 11, 17, 0, 3, 11);
metric(Density = max_v_2210z) -> ?METRIC(0, 13, 21, 0, 3, 14).

%%--------------------------------------------------------------------

-spec top_io(density()) -> y().

top_io(Density) ->
    Metric = metric(Density),
    Metric#metric.top_io.

%%--------------------------------------------------------------------

-spec top_lab(density()) -> y().

top_lab(Density) ->
    Metric = metric(Density),
    Metric#metric.top_lab.

%%--------------------------------------------------------------------

-spec left_io(y(), density()) -> x().

left_io(Y, Density) ->
    case metric(Density) of
        Metric when Y =< Metric#metric.indent_bottom_io ->
            Metric#metric.indent_left_io;

        Metric ->
            Metric#metric.left_io
    end.

%%--------------------------------------------------------------------

-spec left_lab(y(), density()) -> x().

left_lab(Y, Density) ->
    case metric(Density) of
        Metric when Y =< Metric#metric.indent_bottom_io ->
            Metric#metric.indent_left_lab;

        Metric ->
            Metric#metric.left_lab
    end.

%%--------------------------------------------------------------------

-spec right_io(density()) -> x().

right_io(Density) ->
    Metric = metric(Density),
    Metric#metric.right_io.

%%--------------------------------------------------------------------

-spec right_lab(density()) -> x().

right_lab(Density) ->
    Metric = metric(Density),
    Metric#metric.right_lab.

%%--------------------------------------------------------------------

-spec bottom_io(x(), density()) -> y().

bottom_io(X, Density) ->
    case metric(Density) of
        Metric when X =< Metric#metric.indent_left_io ->
            Metric#metric.indent_bottom_io;

        Metric ->
            Metric#metric.bottom_io
    end.

%%--------------------------------------------------------------------

-spec bottom_lab(x(), density()) -> y().

bottom_lab(X, Density) ->
    case metric(Density) of
        Metric when X =< Metric#metric.indent_left_io ->
            Metric#metric.indent_bottom_lab;

        Metric ->
            Metric#metric.bottom_lab
    end.

%%====================================================================
%% columns
%%====================================================================

-spec columns(density()) -> [max_ii:x()].

columns(max_v_240z) -> [2,3,4,5,6,7];
columns(max_v_570z) -> [1,2,3,4,5,6,7,8,9,10,11,12];
columns(max_v_1270z) -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16];
columns(max_v_2210z) -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20].

%%====================================================================
%% rows
%%====================================================================

-spec rows(density()) -> [max_ii:y()].

rows(max_v_240z) -> [1,2,3,4];
rows(max_v_570z) -> [1,2,3,4,5,6,7];
rows(max_v_1270z) -> [1,2,3,4,5,6,7,8,9,10];
rows(max_v_2210z) -> [1,2,3,4,5,6,7,8,9,10,11,12,13].

%%====================================================================
%% left_rows
%%====================================================================

-spec left_rows(density()) -> [max_ii:y()].

left_rows(Density = max_v_240z) ->
    rows(Density);
left_rows(Density) ->
    [_, _, _ | Rows] = rows(Density),
    Rows.

%%====================================================================
%% right_rows
%%====================================================================

-spec right_rows(density()) -> [max_ii:y()].

right_rows(Density) ->
    rows(Density).

%%====================================================================
%% global_block
%%====================================================================

-spec global_block(density()) -> {max_ii:x(), max_ii:y()} | false.

global_block(max_v_240z) -> false;
global_block(max_v_570z) -> {9, 3};
global_block(max_v_1270z) -> {11, 3};
global_block(max_v_2210z) -> {13, 3}.

%%====================================================================
%% block_type
%%====================================================================

-ifdef(TEST).

block_type_test() ->
    block_type_test(max_v_240z),
    block_type_test(max_v_570z),
    block_type_test(max_v_1270z),
    block_type_test(max_v_2210z),
    ok.

%%--------------------------------------------------------------------

block_type_test(Density) ->
    IOBs = [IOB || {IOB, _} <- iobs(Density)],
    LABs = labs(Density),
    Global = global_block(Density),
    Expect0 = length(IOBs) + length(LABs),
    Expect = case Global of
        {_, _} -> Expect0 + 1;
        false -> Expect0
    end,
    Grid = [
        {X, Y}
        ||
        X <- lists:seq(-5, 25),
        Y <- lists:seq(-5, 20)
    ],
    Count = lists:foldl(fun ({X, Y}, Tally) ->
        block_type_test(X, Y, Density, IOBs, LABs, Global, Tally)
    end, 0, Grid),
    ?assertEqual(Expect, Count),
    ok.

%%--------------------------------------------------------------------

block_type_test(X, Y, Density, IOBs, LABs, Global, Tally) ->
    Type = block_type(X, Y, Density),
    ?assertEqual(Type =:= logic, is_lab(X, Y, Density)),
    ?assertEqual(Type =:= column, is_column_iob(X, Y, Density)),
    ?assertEqual(Type =:= row, is_row_iob(X, Y, Density)),
    ?assertEqual(Type =:= row orelse Type =:= column, is_iob(X, Y, Density)),
    case Type of
        logic ->
            ?assertMatch({true, _, _}, {lists:member({lab, X, Y}, LABs), X, Y}),
            Tally + 1;

        row ->
            ?assert(lists:member({iob, X, Y}, IOBs)),
            Tally + 1;

        column ->
            ?assert(lists:member({iob, X, Y}, IOBs)),
            Tally + 1;

        global ->
            ?assertEqual(Global, {X, Y}),
            Tally + 1;

        false ->
            Tally
    end.

-endif.

%%--------------------------------------------------------------------

-spec block_type(max_ii:x(), max_ii:y(), density())
    -> logic | row | column | global | false.

block_type(X, Y, Density) ->
    block(X, Y, metric(Density)).

%%--------------------------------------------------------------------

block(X, Y, #metric{top_io = Y, left_lab = L, right_lab = R}) ->
    if
        X < L -> false;
        X > R -> false;
        true -> column
    end;
block(_, Y, #metric{top_lab = T}) when Y > T ->
    false;
block(X, Y, #metric{left_io = X, indent_bottom_lab = B}) ->
    if
        Y < B -> false;
        true -> row
    end;
block(X, _, #metric{left_lab = L}) when X < L ->
    false;
block(X, Y, #metric{right_io = X, bottom_lab = B}) ->
    if
        Y < B -> false;
        true -> row
    end;
block(X, _, #metric{right_lab = R}) when X > R ->
    false;
block(X, Y, #metric{bottom_io = Y, indent_left_lab = L, right_lab = R}) ->
    if
        X < L -> false;
        X > R -> false;
        true -> column
    end;
block(_, Y, #metric{bottom_lab = B}) when Y < B ->
    false;
block(X, _, #metric{indent_left_io = G}) when X > G ->
    logic;
block(X, Y, #metric{indent_left_io = X, indent_bottom_io = B}) ->
    if
        Y < B -> false;
        Y =:= B -> global;
        true -> logic
    end;
block(X, Y, #metric{indent_bottom_io = Y, left_lab = L}) ->
    if
        X < L -> false;
        true -> column
    end;
block(_, Y, #metric{indent_bottom_lab = B}) when Y < B ->
    false;
block(_, _, _) ->
    logic.

%%====================================================================
%% is_lab
%%====================================================================

-spec is_lab(lab(), density()) -> boolean().

is_lab({lab, X, Y}, Density) ->
    is_lab(X, Y, Density).

%%--------------------------------------------------------------------

-spec is_lab(max_ii:x(), max_ii:y(), density()) -> boolean().

is_lab(X, Y, Density) ->
    case metric(Density) of
        #metric{top_lab = T} when Y > T ->
            false;

        #metric{left_lab = L} when X < L ->
            false;

        #metric{right_lab = R} when X > R ->
            false;

        #metric{bottom_lab = B} when Y < B ->
            false;

        #metric{indent_bottom_lab = B, indent_left_lab = L}
                when Y < B andalso X < L ->
            false;

        _ ->
            true
    end.

%%====================================================================
%% is_iob
%%====================================================================

-spec is_iob(iob(), density()) -> boolean().

is_iob({iob, X, Y}, Density) ->
    is_iob(X, Y, Density).

%%--------------------------------------------------------------------

-spec is_iob(max_ii:x(), max_ii:y(), density()) -> boolean().

is_iob(X, Y, Density) ->
    case metric(Density) of
        #metric{left_io = X, top_lab = T, indent_bottom_lab = B} ->
            Y >= B andalso Y =< T;

        #metric{right_io = X, top_lab = T, bottom_lab = B} ->
            Y >= B andalso Y =< T;

        #metric{top_io = Y, left_lab = L, right_lab = R} ->
            X >= L andalso X =< R;

        #metric{bottom_io = Y, indent_left_lab = L, right_lab = R} ->
            X >= L andalso X =< R;

        #metric{indent_bottom_io = Y, left_lab = L, indent_right_lab = R} ->
            X >= L andalso X =< R;

        _ ->
            false
    end.

%%====================================================================
%% is_column_iob
%%====================================================================

-spec is_column_iob(iob(), density()) -> boolean().

is_column_iob({iob, X, Y}, Density) ->
    is_column_iob(X, Y, Density).

%%--------------------------------------------------------------------

-spec is_column_iob(max_ii:x(), max_ii:y(), density()) -> boolean().

is_column_iob(X, Y, Density) ->
    case metric(Density) of
        #metric{top_io = Y, left_lab = L, right_lab = R} ->
            X >= L andalso X =< R;

        #metric{bottom_io = Y, indent_left_lab = L, right_lab = R} ->
            X >= L andalso X =< R;

        #metric{indent_bottom_io = Y, left_lab = L, indent_right_lab = R} ->
            X >= L andalso X =< R;

        _ ->
            false
    end.

%%====================================================================
%% is_row_iob
%%====================================================================

-spec is_row_iob(iob(), density()) -> boolean().

is_row_iob({iob, X, Y}, Density) ->
    is_row_iob(X, Y, Density).

%%--------------------------------------------------------------------

-spec is_row_iob(max_ii:x(), max_ii:y(), density()) -> boolean().

is_row_iob(X, Y, Density) ->
    case metric(Density) of
        #metric{left_io = X, top_lab = T, indent_bottom_lab = B} ->
            Y >= B andalso Y =< T;

        #metric{right_io = X, top_lab = T, bottom_lab = B} ->
            Y >= B andalso Y =< T;

        _ ->
            false
    end.

%%====================================================================
%% is_left_iob
%%====================================================================

-spec is_left_iob(iob(), density()) -> boolean().

is_left_iob({iob, X, Y}, Density) ->
    is_left_iob(X, Y, Density).

%%--------------------------------------------------------------------

-spec is_left_iob(max_ii:x(), max_ii:y(), density()) -> boolean().

is_left_iob(X, Y, Density) ->
    case metric(Density) of
        #metric{left_io = X, top_lab = T, indent_bottom_lab = B} ->
            Y >= B andalso Y =< T;

        _ ->
            false
    end.

%%====================================================================
%% is_top_iob
%%====================================================================

-spec is_top_iob(iob(), density()) -> boolean().

is_top_iob({iob, X, Y}, Density) ->
    is_top_iob(X, Y, Density).

%%--------------------------------------------------------------------

-spec is_top_iob(max_ii:x(), max_ii:y(), density()) -> boolean().

is_top_iob(X, Y, Density) ->
    case metric(Density) of
        #metric{top_io = Y, left_lab = L, right_lab = R} ->
            X >= L andalso X =< R;

        _ ->
            false
    end.

%%====================================================================
%% is_right_iob
%%====================================================================

-spec is_right_iob(iob(), density()) -> boolean().

is_right_iob({iob, X, Y}, Density) ->
    is_right_iob(X, Y, Density).

%%--------------------------------------------------------------------

-spec is_right_iob(max_ii:x(), max_ii:y(), density()) -> boolean().

is_right_iob(X, Y, Density) ->
    case metric(Density) of
        #metric{right_io = X, top_lab = T, bottom_lab = B} ->
            Y >= B andalso Y =< T;

        _ ->
            false
    end.

%%====================================================================
%% is_bottom_iob
%%====================================================================

-spec is_bottom_iob(iob(), density()) -> boolean().

is_bottom_iob({iob, X, Y}, Density) ->
    is_bottom_iob(X, Y, Density).

%%--------------------------------------------------------------------

-spec is_bottom_iob(max_ii:x(), max_ii:y(), density()) -> boolean().

is_bottom_iob(X, Y, Density) ->
    case metric(Density) of
        #metric{bottom_io = Y, indent_left_lab = L, right_lab = R} ->
            X >= L andalso X =< R;

        #metric{indent_bottom_io = Y, left_lab = L, indent_right_lab = R} ->
            X >= L andalso X =< R;

        _ ->
            false
    end.

%%====================================================================
%% is_pci_iob
%%====================================================================

-spec is_pci_iob(iob(), density()) -> boolean().

is_pci_iob({iob, X, Y}, Density) ->
    is_pci_iob(X, Y, Density).

%%--------------------------------------------------------------------

-spec is_pci_iob(max_ii:x(), max_ii:y(), density()) -> boolean().

is_pci_iob(_, _, max_v_240z) ->
    false;
is_pci_iob(_, _, max_v_570z) ->
    false;
is_pci_iob(X, Y, Density) ->
    case metric(Density) of
        #metric{right_io = X, top_lab = T, bottom_lab = B} ->
            Y >= B andalso Y =< T;

        _ ->
            false
    end.

