-module(max_v_570z_m100).

-export([iocs/0]).
-export([iocs/1]).
-export([pins/0]).
-export([top_iocs/1]).
-export([top_pins/1]).
-export([left_iocs/1]).
-export([left_pins/1]).
-export([right_iocs/1]).
-export([right_pins/1]).
-export([bottom_iocs/1]).
-export([bottom_pins/1]).

-type iob() :: iob:iob().
-type ioc() :: ioc:ioc().
-type pin() :: pin:pin().
-type x() :: max_v:x().
-type y() :: max_v:y().

-spec iocs() -> [{ioc(), pin()}].

iocs() ->
    [{{ioc,0,4,5},h1},
     {{ioc,0,5,0},f2},
     {{ioc,0,5,1},e1},
     {{ioc,0,5,2},f1},
     {{ioc,0,5,3},g1},
     {{ioc,0,5,4},g2},
     {{ioc,0,5,5},f3},
     {{ioc,0,7,0},c1},
     {{ioc,0,7,2},d3},
     {{ioc,0,7,3},d2},
     {{ioc,0,7,4},d1},
     {{ioc,1,8,0},b1},
     {{ioc,1,8,2},c2},
     {{ioc,3,8,0},b2},
     {{ioc,3,8,3},a1},
     {{ioc,4,8,1},b3},
     {{ioc,4,8,2},a2},
     {{ioc,5,8,1},b4},
     {{ioc,5,8,2},a3},
     {{ioc,6,8,2},a5},
     {{ioc,6,8,3},b5},
     {{ioc,7,8,0},b6},
     {{ioc,7,8,1},a6},
     {{ioc,8,8,0},a8},
     {{ioc,8,8,1},b7},
     {{ioc,8,8,2},a7},
     {{ioc,8,8,3},c6},
     {{ioc,9,8,0},b8},
     {{ioc,10,8,3},a9},
     {{ioc,12,8,1},a11},
     {{ioc,12,8,2},a10},
     {{ioc,12,8,3},b9},
     {{ioc,13,7,5},c10},
     {{ioc,13,7,3},b11},
     {{ioc,13,7,1},b10},
     {{ioc,13,6,5},d11},
     {{ioc,13,6,2},d10},
     {{ioc,13,6,1},c11},
     {{ioc,13,5,0},d9},
     {{ioc,13,4,4},f10},
     {{ioc,13,4,3},g11},
     {{ioc,13,4,2},f11},
     {{ioc,13,4,1},e11},
     {{ioc,13,4,0},e10},
     {{ioc,13,3,1},f9},
     {{ioc,13,2,4},h9},
     {{ioc,13,2,2},h10},
     {{ioc,13,2,1},h11},
     {{ioc,13,2,0},g10},
     {{ioc,13,1,4},k11},
     {{ioc,13,1,3},j10},
     {{ioc,13,1,1},j11},
     {{ioc,12,0,2},l11},
     {{ioc,11,0,2},k10},
     {{ioc,10,0,2},l9},
     {{ioc,10,0,1},k9},
     {{ioc,10,0,0},l10},
     {{ioc,8,3,3},l8},
     {{ioc,8,3,2},k8},
     {{ioc,7,3,3},k6},
     {{ioc,7,3,2},j6},
     {{ioc,7,3,1},l7},
     {{ioc,7,3,0},k7},
     {{ioc,6,3,3},l4},
     {{ioc,6,3,2},k5},
     {{ioc,6,3,1},l5},
     {{ioc,6,3,0},l6},
     {{ioc,4,3,3},k3},
     {{ioc,4,3,2},l3},
     {{ioc,4,3,1},k4},
     {{ioc,3,3,2},l1},
     {{ioc,3,3,1},l2},
     {{ioc,1,3,3},h3},
     {{ioc,1,3,1},h2}
    ].

-spec iocs(iob()) -> [{ioc(), pin()}].

iocs({iob,0,4}) ->
    [{{ioc,0,4,5},h1}];
iocs({iob,0,5}) ->
    [{{ioc,0,5,0},f2},
     {{ioc,0,5,1},e1},
     {{ioc,0,5,2},f1},
     {{ioc,0,5,3},g1},
     {{ioc,0,5,4},g2},
     {{ioc,0,5,5},f3}];
iocs({iob,0,6}) ->
    [];
iocs({iob,0,7}) ->
    [{{ioc,0,7,0},c1},
     {{ioc,0,7,2},d3},
     {{ioc,0,7,3},d2},
     {{ioc,0,7,4},d1}];
iocs({iob,1,8}) ->
    [{{ioc,1,8,0},b1},
     {{ioc,1,8,2},c2}];
iocs({iob,2,8}) ->
    [];
iocs({iob,3,8}) ->
    [{{ioc,3,8,0},b2},
     {{ioc,3,8,3},a1}];
iocs({iob,4,8}) ->
    [{{ioc,4,8,1},b3},
     {{ioc,4,8,2},a2}];
iocs({iob,5,8}) ->
    [{{ioc,5,8,1},b4},
     {{ioc,5,8,2},a3}];
iocs({iob,6,8}) ->
    [{{ioc,6,8,2},a5},
     {{ioc,6,8,3},b5}];
iocs({iob,7,8}) ->
    [{{ioc,7,8,0},b6},
     {{ioc,7,8,1},a6}];
iocs({iob,8,8}) ->
    [{{ioc,8,8,0},a8},
     {{ioc,8,8,1},b7},
     {{ioc,8,8,2},a7},
     {{ioc,8,8,3},c6}];
iocs({iob,9,8}) ->
    [{{ioc,9,8,0},b8}];
iocs({iob,10,8}) ->
    [{{ioc,10,8,3},a9}];
iocs({iob,11,8}) ->
    [];
iocs({iob,12,8}) ->
    [{{ioc,12,8,1},a11},
     {{ioc,12,8,2},a10},
     {{ioc,12,8,3},b9}];
iocs({iob,13,7}) ->
    [{{ioc,13,7,5},c10},
     {{ioc,13,7,3},b11},
     {{ioc,13,7,1},b10}];
iocs({iob,13,6}) ->
    [{{ioc,13,6,5},d11},
     {{ioc,13,6,2},d10},
     {{ioc,13,6,1},c11}];
iocs({iob,13,5}) ->
    [{{ioc,13,5,0},d9}];
iocs({iob,13,4}) ->
    [{{ioc,13,4,4},f10},
     {{ioc,13,4,3},g11},
     {{ioc,13,4,2},f11},
     {{ioc,13,4,1},e11},
     {{ioc,13,4,0},e10}];
iocs({iob,13,3}) ->
    [{{ioc,13,3,1},f9}];
iocs({iob,13,2}) ->
    [{{ioc,13,2,4},h9},
     {{ioc,13,2,2},h10},
     {{ioc,13,2,1},h11},
     {{ioc,13,2,0},g10}];
iocs({iob,13,1}) ->
    [{{ioc,13,1,4},k11},
     {{ioc,13,1,3},j10},
     {{ioc,13,1,1},j11}];
iocs({iob,12,0}) ->
    [{{ioc,12,0,2},l11}];
iocs({iob,11,0}) ->
    [{{ioc,11,0,2},k10}];
iocs({iob,10,0}) ->
    [{{ioc,10,0,2},l9},
     {{ioc,10,0,1},k9},
     {{ioc,10,0,0},l10}];
iocs({iob,8,3}) ->
    [{{ioc,8,3,3},l8},
     {{ioc,8,3,2},k8}];
iocs({iob,7,3}) ->
    [{{ioc,7,3,3},k6},
     {{ioc,7,3,2},j6},
     {{ioc,7,3,1},l7},
     {{ioc,7,3,0},k7}];
iocs({iob,6,3}) ->
    [{{ioc,6,3,3},l4},
     {{ioc,6,3,2},k5},
     {{ioc,6,3,1},l5},
     {{ioc,6,3,0},l6}];
iocs({iob,5,3}) ->
    [];
iocs({iob,4,3}) ->
    [{{ioc,4,3,3},k3},
     {{ioc,4,3,2},l3},
     {{ioc,4,3,1},k4}];
iocs({iob,3,3}) ->
    [{{ioc,3,3,2},l1},
     {{ioc,3,3,1},l2}];
iocs({iob,2,3}) ->
    [];
iocs({iob,1,3}) ->
    [{{ioc,1,3,3},h3},
     {{ioc,1,3,1},h2}].

-spec pins() -> [pin()].

pins() ->
    [h1,
     f2,
     e1,
     f1,
     g1,
     g2,
     f3,
     c1,
     d3,
     d2,
     d1,
     b1,
     c2,
     b2,
     a1,
     b3,
     a2,
     b4,
     a3,
     a5,
     b5,
     b6,
     a6,
     a8,
     b7,
     a7,
     c6,
     b8,
     a9,
     a11,
     a10,
     b9,
     c10,
     b11,
     b10,
     d11,
     d10,
     c11,
     d9,
     f10,
     g11,
     f11,
     e11,
     e10,
     f9,
     h9,
     h10,
     h11,
     g10,
     k11,
     j10,
     j11,
     l11,
     k10,
     l9,
     k9,
     l10,
     l8,
     k8,
     k6,
     j6,
     l7,
     k7,
     l4,
     k5,
     l5,
     l6,
     k3,
     l3,
     k4,
     l1,
     l2,
     h3,
     h2
    ].

-spec top_iocs(x()) -> [{ioc(), pin()}].

top_iocs(1) ->
    [{{ioc,1,8,0},b1},
     {{ioc,1,8,2},c2}];
top_iocs(2) ->
    [];
top_iocs(3) ->
    [{{ioc,3,8,0},b2},
     {{ioc,3,8,3},a1}];
top_iocs(4) ->
    [{{ioc,4,8,1},b3},
     {{ioc,4,8,2},a2}];
top_iocs(5) ->
    [{{ioc,5,8,1},b4},
     {{ioc,5,8,2},a3}];
top_iocs(6) ->
    [{{ioc,6,8,2},a5},
     {{ioc,6,8,3},b5}];
top_iocs(7) ->
    [{{ioc,7,8,0},b6},
     {{ioc,7,8,1},a6}];
top_iocs(8) ->
    [{{ioc,8,8,0},a8},
     {{ioc,8,8,1},b7},
     {{ioc,8,8,2},a7},
     {{ioc,8,8,3},c6}];
top_iocs(9) ->
    [{{ioc,9,8,0},b8}];
top_iocs(10) ->
    [{{ioc,10,8,3},a9}];
top_iocs(11) ->
    [];
top_iocs(12) ->
    [{{ioc,12,8,1},a11},
     {{ioc,12,8,2},a10},
     {{ioc,12,8,3},b9}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [b1, c2];
top_pins(2) ->
    [];
top_pins(3) ->
    [b2, a1];
top_pins(4) ->
    [b3, a2];
top_pins(5) ->
    [b4, a3];
top_pins(6) ->
    [a5, b5];
top_pins(7) ->
    [b6, a6];
top_pins(8) ->
    [a8, b7, a7, c6];
top_pins(9) ->
    [b8];
top_pins(10) ->
    [a9];
top_pins(11) ->
    [];
top_pins(12) ->
    [a11, a10, b9].

-spec left_iocs(y()) -> [{ioc(), pin()}].

left_iocs(1) ->
    [];
left_iocs(2) ->
    [];
left_iocs(3) ->
    [];
left_iocs(4) ->
    [{{ioc,0,4,5},h1}];
left_iocs(5) ->
    [{{ioc,0,5,0},f2},
     {{ioc,0,5,1},e1},
     {{ioc,0,5,2},f1},
     {{ioc,0,5,3},g1},
     {{ioc,0,5,4},g2},
     {{ioc,0,5,5},f3}];
left_iocs(6) ->
    [];
left_iocs(7) ->
    [{{ioc,0,7,0},c1},
     {{ioc,0,7,2},d3},
     {{ioc,0,7,3},d2},
     {{ioc,0,7,4},d1}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [];
left_pins(2) ->
    [];
left_pins(3) ->
    [];
left_pins(4) ->
    [h1];
left_pins(5) ->
    [f2, e1, f1, g1, g2, f3];
left_pins(6) ->
    [];
left_pins(7) ->
    [c1, d3, d2, d1].

-spec right_iocs(y()) -> [{ioc(), pin()}].

right_iocs(1) ->
    [{{ioc,13,1,4},k11},
     {{ioc,13,1,3},j10},
     {{ioc,13,1,1},j11}];
right_iocs(2) ->
    [{{ioc,13,2,4},h9},
     {{ioc,13,2,2},h10},
     {{ioc,13,2,1},h11},
     {{ioc,13,2,0},g10}];
right_iocs(3) ->
    [{{ioc,13,3,1},f9}];
right_iocs(4) ->
    [{{ioc,13,4,4},f10},
     {{ioc,13,4,3},g11},
     {{ioc,13,4,2},f11},
     {{ioc,13,4,1},e11},
     {{ioc,13,4,0},e10}];
right_iocs(5) ->
    [{{ioc,13,5,0},d9}];
right_iocs(6) ->
    [{{ioc,13,6,5},d11},
     {{ioc,13,6,2},d10},
     {{ioc,13,6,1},c11}];
right_iocs(7) ->
    [{{ioc,13,7,5},c10},
     {{ioc,13,7,3},b11},
     {{ioc,13,7,1},b10}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [k11, j10, j11];
right_pins(2) ->
    [h9, h10, h11, g10];
right_pins(3) ->
    [f9];
right_pins(4) ->
    [f10, g11, f11, e11, e10];
right_pins(5) ->
    [d9];
right_pins(6) ->
    [d11, d10, c11];
right_pins(7) ->
    [c10, b11, b10].

-spec bottom_iocs(x()) -> [{ioc(), pin()}].

bottom_iocs(1) ->
    [{{ioc,1,3,3},h3},
     {{ioc,1,3,1},h2}];
bottom_iocs(2) ->
    [];
bottom_iocs(3) ->
    [{{ioc,3,3,2},l1},
     {{ioc,3,3,1},l2}];
bottom_iocs(4) ->
    [{{ioc,4,3,3},k3},
     {{ioc,4,3,2},l3},
     {{ioc,4,3,1},k4}];
bottom_iocs(5) ->
    [];
bottom_iocs(6) ->
    [{{ioc,6,3,3},l4},
     {{ioc,6,3,2},k5},
     {{ioc,6,3,1},l5},
     {{ioc,6,3,0},l6}];
bottom_iocs(7) ->
    [{{ioc,7,3,3},k6},
     {{ioc,7,3,2},j6},
     {{ioc,7,3,1},l7},
     {{ioc,7,3,0},k7}];
bottom_iocs(8) ->
    [{{ioc,8,3,3},l8},
     {{ioc,8,3,2},k8}];
bottom_iocs(9) ->
    [];
bottom_iocs(10) ->
    [{{ioc,10,0,2},l9},
     {{ioc,10,0,1},k9},
     {{ioc,10,0,0},l10}];
bottom_iocs(11) ->
    [{{ioc,11,0,2},k10}];
bottom_iocs(12) ->
    [{{ioc,12,0,2},l11}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [h3, h2];
bottom_pins(2) ->
    [];
bottom_pins(3) ->
    [l1, l2];
bottom_pins(4) ->
    [k3, l3, k4];
bottom_pins(5) ->
    [];
bottom_pins(6) ->
    [l4, k5, l5, l6];
bottom_pins(7) ->
    [k6, j6, l7, k7];
bottom_pins(8) ->
    [l8, k8];
bottom_pins(9) ->
    [];
bottom_pins(10) ->
    [l9, k9, l10];
bottom_pins(11) ->
    [k10];
bottom_pins(12) ->
    [l11].

