-module(max_v_240z_m100).

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
    [{{ioc,1,4,0},b1},
     {{ioc,1,4,1},c2},
     {{ioc,1,4,2},c1},
     {{ioc,1,4,3},d3},
     {{ioc,1,3,0},d2},
     {{ioc,1,3,1},d1},
     {{ioc,1,3,2},e2},
     {{ioc,1,3,3},f2},
     {{ioc,1,2,0},e1},
     {{ioc,1,2,1},f1},
     {{ioc,1,2,2},g1},
     {{ioc,1,2,3},g2},
     {{ioc,1,1,0},f3},
     {{ioc,1,1,1},h1},
     {{ioc,1,1,2},h3},
     {{ioc,1,1,3},h2},
     {{ioc,2,5,0},b3},
     {{ioc,2,5,1},a2},
     {{ioc,2,5,2},b2},
     {{ioc,3,5,0},b5},
     {{ioc,3,5,1},a4},
     {{ioc,3,5,2},b4},
     {{ioc,3,5,3},a3},
     {{ioc,4,5,0},a6},
     {{ioc,4,5,1},c5},
     {{ioc,4,5,2},a5},
     {{ioc,5,5,0},a7},
     {{ioc,5,5,1},c6},
     {{ioc,5,5,2},b6},
     {{ioc,5,5,3},c7},
     {{ioc,6,5,0},a9},
     {{ioc,6,5,1},b8},
     {{ioc,6,5,2},a8},
     {{ioc,6,5,3},b7},
     {{ioc,7,5,0},b10},
     {{ioc,7,5,1},a11},
     {{ioc,7,5,2},a10},
     {{ioc,7,5,3},b9},
     {{ioc,8,4,4},d11},
     {{ioc,8,4,3},d10},
     {{ioc,8,4,2},c11},
     {{ioc,8,4,1},c10},
     {{ioc,8,4,0},b11},
     {{ioc,8,3,4},g11},
     {{ioc,8,3,3},f11},
     {{ioc,8,3,2},e11},
     {{ioc,8,3,1},e10},
     {{ioc,8,3,0},d9},
     {{ioc,8,2,3},h11},
     {{ioc,8,2,2},g10},
     {{ioc,8,2,1},f9},
     {{ioc,8,2,0},f10},
     {{ioc,8,1,4},k11},
     {{ioc,8,1,3},j10},
     {{ioc,8,1,2},j11},
     {{ioc,8,1,1},h9},
     {{ioc,8,1,0},h10},
     {{ioc,7,0,2},l10},
     {{ioc,7,0,1},k10},
     {{ioc,7,0,0},l11},
     {{ioc,6,0,3},l8},
     {{ioc,6,0,2},k8},
     {{ioc,6,0,1},l9},
     {{ioc,6,0,0},k9},
     {{ioc,5,0,3},j7},
     {{ioc,5,0,2},j6},
     {{ioc,5,0,1},l7},
     {{ioc,5,0,0},k7},
     {{ioc,4,0,2},l6},
     {{ioc,4,0,1},j5},
     {{ioc,4,0,0},k6},
     {{ioc,3,0,3},k4},
     {{ioc,3,0,2},l4},
     {{ioc,3,0,1},k5},
     {{ioc,3,0,0},l5},
     {{ioc,2,0,3},l1},
     {{ioc,2,0,2},l2},
     {{ioc,2,0,1},k3},
     {{ioc,2,0,0},l3}
    ].

-spec iocs(iob()) -> [{ioc(), pin()}].

iocs({iob,1,4}) ->
    [{{ioc,1,4,0},b1},
     {{ioc,1,4,1},c2},
     {{ioc,1,4,2},c1},
     {{ioc,1,4,3},d3}];
iocs({iob,1,3}) ->
    [{{ioc,1,3,0},d2},
     {{ioc,1,3,1},d1},
     {{ioc,1,3,2},e2},
     {{ioc,1,3,3},f2}];
iocs({iob,1,2}) ->
    [{{ioc,1,2,0},e1},
     {{ioc,1,2,1},f1},
     {{ioc,1,2,2},g1},
     {{ioc,1,2,3},g2}];
iocs({iob,1,1}) ->
    [{{ioc,1,1,0},f3},
     {{ioc,1,1,1},h1},
     {{ioc,1,1,2},h3},
     {{ioc,1,1,3},h2}];
iocs({iob,2,5}) ->
    [{{ioc,2,5,0},b3},
     {{ioc,2,5,1},a2},
     {{ioc,2,5,2},b2}];
iocs({iob,3,5}) ->
    [{{ioc,3,5,0},b5},
     {{ioc,3,5,1},a4},
     {{ioc,3,5,2},b4},
     {{ioc,3,5,3},a3}];
iocs({iob,4,5}) ->
    [{{ioc,4,5,0},a6},
     {{ioc,4,5,1},c5},
     {{ioc,4,5,2},a5}];
iocs({iob,5,5}) ->
    [{{ioc,5,5,0},a7},
     {{ioc,5,5,1},c6},
     {{ioc,5,5,2},b6},
     {{ioc,5,5,3},c7}];
iocs({iob,6,5}) ->
    [{{ioc,6,5,0},a9},
     {{ioc,6,5,1},b8},
     {{ioc,6,5,2},a8},
     {{ioc,6,5,3},b7}];
iocs({iob,7,5}) ->
    [{{ioc,7,5,0},b10},
     {{ioc,7,5,1},a11},
     {{ioc,7,5,2},a10},
     {{ioc,7,5,3},b9}];
iocs({iob,8,4}) ->
    [{{ioc,8,4,4},d11},
     {{ioc,8,4,3},d10},
     {{ioc,8,4,2},c11},
     {{ioc,8,4,1},c10},
     {{ioc,8,4,0},b11}];
iocs({iob,8,3}) ->
    [{{ioc,8,3,4},g11},
     {{ioc,8,3,3},f11},
     {{ioc,8,3,2},e11},
     {{ioc,8,3,1},e10},
     {{ioc,8,3,0},d9}];
iocs({iob,8,2}) ->
    [{{ioc,8,2,3},h11},
     {{ioc,8,2,2},g10},
     {{ioc,8,2,1},f9},
     {{ioc,8,2,0},f10}];
iocs({iob,8,1}) ->
    [{{ioc,8,1,4},k11},
     {{ioc,8,1,3},j10},
     {{ioc,8,1,2},j11},
     {{ioc,8,1,1},h9},
     {{ioc,8,1,0},h10}];
iocs({iob,7,0}) ->
    [{{ioc,7,0,2},l10},
     {{ioc,7,0,1},k10},
     {{ioc,7,0,0},l11}];
iocs({iob,6,0}) ->
    [{{ioc,6,0,3},l8},
     {{ioc,6,0,2},k8},
     {{ioc,6,0,1},l9},
     {{ioc,6,0,0},k9}];
iocs({iob,5,0}) ->
    [{{ioc,5,0,3},j7},
     {{ioc,5,0,2},j6},
     {{ioc,5,0,1},l7},
     {{ioc,5,0,0},k7}];
iocs({iob,4,0}) ->
    [{{ioc,4,0,2},l6},
     {{ioc,4,0,1},j5},
     {{ioc,4,0,0},k6}];
iocs({iob,3,0}) ->
    [{{ioc,3,0,3},k4},
     {{ioc,3,0,2},l4},
     {{ioc,3,0,1},k5},
     {{ioc,3,0,0},l5}];
iocs({iob,2,0}) ->
    [{{ioc,2,0,3},l1},
     {{ioc,2,0,2},l2},
     {{ioc,2,0,1},k3},
     {{ioc,2,0,0},l3}].

-spec pins() -> [pin()].

pins() ->
    [b1,
     c2,
     c1,
     d3,
     d2,
     d1,
     e2,
     f2,
     e1,
     f1,
     g1,
     g2,
     f3,
     h1,
     h3,
     h2,
     b3,
     a2,
     b2,
     b5,
     a4,
     b4,
     a3,
     a6,
     c5,
     a5,
     a7,
     c6,
     b6,
     c7,
     a9,
     b8,
     a8,
     b7,
     b10,
     a11,
     a10,
     b9,
     d11,
     d10,
     c11,
     c10,
     b11,
     g11,
     f11,
     e11,
     e10,
     d9,
     h11,
     g10,
     f9,
     f10,
     k11,
     j10,
     j11,
     h9,
     h10,
     l10,
     k10,
     l11,
     l8,
     k8,
     l9,
     k9,
     j7,
     j6,
     l7,
     k7,
     l6,
     j5,
     k6,
     k4,
     l4,
     k5,
     l5,
     l1,
     l2,
     k3,
     l3
    ].

-spec top_iocs(x()) -> [{ioc(), pin()}].

top_iocs(2) ->
    [{{ioc,2,5,0},b3},
     {{ioc,2,5,1},a2},
     {{ioc,2,5,2},b2}];
top_iocs(3) ->
    [{{ioc,3,5,0},b5},
     {{ioc,3,5,1},a4},
     {{ioc,3,5,2},b4},
     {{ioc,3,5,3},a3}];
top_iocs(4) ->
    [{{ioc,4,5,0},a6},
     {{ioc,4,5,1},c5},
     {{ioc,4,5,2},a5}];
top_iocs(5) ->
    [{{ioc,5,5,0},a7},
     {{ioc,5,5,1},c6},
     {{ioc,5,5,2},b6},
     {{ioc,5,5,3},c7}];
top_iocs(6) ->
    [{{ioc,6,5,0},a9},
     {{ioc,6,5,1},b8},
     {{ioc,6,5,2},a8},
     {{ioc,6,5,3},b7}];
top_iocs(7) ->
    [{{ioc,7,5,0},b10},
     {{ioc,7,5,1},a11},
     {{ioc,7,5,2},a10},
     {{ioc,7,5,3},b9}].

-spec top_pins(x()) -> [pin()].

top_pins(2) ->
    [b3, a2, b2];
top_pins(3) ->
    [b5, a4, b4, a3];
top_pins(4) ->
    [a6, c5, a5];
top_pins(5) ->
    [a7, c6, b6, c7];
top_pins(6) ->
    [a9, b8, a8, b7];
top_pins(7) ->
    [b10, a11, a10, b9].

-spec left_iocs(y()) -> [{ioc(), pin()}].

left_iocs(1) ->
    [{{ioc,1,1,0},f3},
     {{ioc,1,1,1},h1},
     {{ioc,1,1,2},h3},
     {{ioc,1,1,3},h2}];
left_iocs(2) ->
    [{{ioc,1,2,0},e1},
     {{ioc,1,2,1},f1},
     {{ioc,1,2,2},g1},
     {{ioc,1,2,3},g2}];
left_iocs(3) ->
    [{{ioc,1,3,0},d2},
     {{ioc,1,3,1},d1},
     {{ioc,1,3,2},e2},
     {{ioc,1,3,3},f2}];
left_iocs(4) ->
    [{{ioc,1,4,0},b1},
     {{ioc,1,4,1},c2},
     {{ioc,1,4,2},c1},
     {{ioc,1,4,3},d3}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [f3, h1, h3, h2];
left_pins(2) ->
    [e1, f1, g1, g2];
left_pins(3) ->
    [d2, d1, e2, f2];
left_pins(4) ->
    [b1, c2, c1, d3].

-spec right_iocs(y()) -> [{ioc(), pin()}].

right_iocs(1) ->
    [{{ioc,8,1,4},k11},
     {{ioc,8,1,3},j10},
     {{ioc,8,1,2},j11},
     {{ioc,8,1,1},h9},
     {{ioc,8,1,0},h10}];
right_iocs(2) ->
    [{{ioc,8,2,3},h11},
     {{ioc,8,2,2},g10},
     {{ioc,8,2,1},f9},
     {{ioc,8,2,0},f10}];
right_iocs(3) ->
    [{{ioc,8,3,4},g11},
     {{ioc,8,3,3},f11},
     {{ioc,8,3,2},e11},
     {{ioc,8,3,1},e10},
     {{ioc,8,3,0},d9}];
right_iocs(4) ->
    [{{ioc,8,4,4},d11},
     {{ioc,8,4,3},d10},
     {{ioc,8,4,2},c11},
     {{ioc,8,4,1},c10},
     {{ioc,8,4,0},b11}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [k11, j10, j11, h9, h10];
right_pins(2) ->
    [h11, g10, f9, f10];
right_pins(3) ->
    [g11, f11, e11, e10, d9];
right_pins(4) ->
    [d11, d10, c11, c10, b11].

-spec bottom_iocs(x()) -> [{ioc(), pin()}].

bottom_iocs(2) ->
    [{{ioc,2,0,3},l1},
     {{ioc,2,0,2},l2},
     {{ioc,2,0,1},k3},
     {{ioc,2,0,0},l3}];
bottom_iocs(3) ->
    [{{ioc,3,0,3},k4},
     {{ioc,3,0,2},l4},
     {{ioc,3,0,1},k5},
     {{ioc,3,0,0},l5}];
bottom_iocs(4) ->
    [{{ioc,4,0,2},l6},
     {{ioc,4,0,1},j5},
     {{ioc,4,0,0},k6}];
bottom_iocs(5) ->
    [{{ioc,5,0,3},j7},
     {{ioc,5,0,2},j6},
     {{ioc,5,0,1},l7},
     {{ioc,5,0,0},k7}];
bottom_iocs(6) ->
    [{{ioc,6,0,3},l8},
     {{ioc,6,0,2},k8},
     {{ioc,6,0,1},l9},
     {{ioc,6,0,0},k9}];
bottom_iocs(7) ->
    [{{ioc,7,0,2},l10},
     {{ioc,7,0,1},k10},
     {{ioc,7,0,0},l11}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(2) ->
    [l1, l2, k3, l3];
bottom_pins(3) ->
    [k4, l4, k5, l5];
bottom_pins(4) ->
    [l6, j5, k6];
bottom_pins(5) ->
    [j7, j6, l7, k7];
bottom_pins(6) ->
    [l8, k8, l9, k9];
bottom_pins(7) ->
    [l10, k10, l11].

