-module(max_v_80z_m68).

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
    [{{ioc,1,4,1},a1},
     {{ioc,1,4,2},b1},
     {{ioc,1,4,3},c1},
     {{ioc,1,3,0},c2},
     {{ioc,1,3,1},d1},
     {{ioc,1,3,2},d2},
     {{ioc,1,3,3},e2},
     {{ioc,1,2,0},e1},
     {{ioc,1,1,1},f2},
     {{ioc,1,1,2},f3},
     {{ioc,1,1,3},g2},
     {{ioc,2,5,0},a3},
     {{ioc,2,5,1},b2},
     {{ioc,2,5,2},a2},
     {{ioc,3,5,0},a5},
     {{ioc,3,5,1},b4},
     {{ioc,3,5,2},a4},
     {{ioc,3,5,3},b3},
     {{ioc,4,5,0},b6},
     {{ioc,4,5,2},b5},
     {{ioc,5,5,0},a7},
     {{ioc,5,5,1},a6},
     {{ioc,5,5,2},b7},
     {{ioc,6,5,2},a8},
     {{ioc,6,5,3},b8},
     {{ioc,8,4,4},d8},
     {{ioc,8,4,3},c8},
     {{ioc,8,4,2},b9},
     {{ioc,8,4,1},a9},
     {{ioc,8,3,4},e8},
     {{ioc,8,3,1},d9},
     {{ioc,8,3,0},c9},
     {{ioc,8,2,3},f9},
     {{ioc,8,2,0},e9},
     {{ioc,8,1,4},h9},
     {{ioc,8,1,3},g9},
     {{ioc,8,1,2},g8},
     {{ioc,8,1,1},f8},
     {{ioc,7,0,2},j8},
     {{ioc,7,0,1},j9},
     {{ioc,6,0,3},j6},
     {{ioc,6,0,2},h7},
     {{ioc,6,0,1},j7},
     {{ioc,6,0,0},h8},
     {{ioc,5,0,1},g6},
     {{ioc,5,0,0},h6},
     {{ioc,3,0,3},j4},
     {{ioc,3,0,2},h5},
     {{ioc,2,0,3},j2},
     {{ioc,2,0,2},h3},
     {{ioc,2,0,1},j3},
     {{ioc,2,0,0},h4}
    ].

-spec iocs(iob()) -> [{ioc(), pin()}].

iocs({iob,1,4}) ->
    [{{ioc,1,4,1},a1},
     {{ioc,1,4,2},b1},
     {{ioc,1,4,3},c1}];
iocs({iob,1,3}) ->
    [{{ioc,1,3,0},c2},
     {{ioc,1,3,1},d1},
     {{ioc,1,3,2},d2},
     {{ioc,1,3,3},e2}];
iocs({iob,1,2}) ->
    [{{ioc,1,2,0},e1}];
iocs({iob,1,1}) ->
    [{{ioc,1,1,1},f2},
     {{ioc,1,1,2},f3},
     {{ioc,1,1,3},g2}];
iocs({iob,2,5}) ->
    [{{ioc,2,5,0},a3},
     {{ioc,2,5,1},b2},
     {{ioc,2,5,2},a2}];
iocs({iob,3,5}) ->
    [{{ioc,3,5,0},a5},
     {{ioc,3,5,1},b4},
     {{ioc,3,5,2},a4},
     {{ioc,3,5,3},b3}];
iocs({iob,4,5}) ->
    [{{ioc,4,5,0},b6},
     {{ioc,4,5,2},b5}];
iocs({iob,5,5}) ->
    [{{ioc,5,5,0},a7},
     {{ioc,5,5,1},a6},
     {{ioc,5,5,2},b7}];
iocs({iob,6,5}) ->
    [{{ioc,6,5,2},a8},
     {{ioc,6,5,3},b8}];
iocs({iob,7,5}) ->
    [];
iocs({iob,8,4}) ->
    [{{ioc,8,4,4},d8},
     {{ioc,8,4,3},c8},
     {{ioc,8,4,2},b9},
     {{ioc,8,4,1},a9}];
iocs({iob,8,3}) ->
    [{{ioc,8,3,4},e8},
     {{ioc,8,3,1},d9},
     {{ioc,8,3,0},c9}];
iocs({iob,8,2}) ->
    [{{ioc,8,2,3},f9},
     {{ioc,8,2,0},e9}];
iocs({iob,8,1}) ->
    [{{ioc,8,1,4},h9},
     {{ioc,8,1,3},g9},
     {{ioc,8,1,2},g8},
     {{ioc,8,1,1},f8}];
iocs({iob,7,0}) ->
    [{{ioc,7,0,2},j8},
     {{ioc,7,0,1},j9}];
iocs({iob,6,0}) ->
    [{{ioc,6,0,3},j6},
     {{ioc,6,0,2},h7},
     {{ioc,6,0,1},j7},
     {{ioc,6,0,0},h8}];
iocs({iob,5,0}) ->
    [{{ioc,5,0,1},g6},
     {{ioc,5,0,0},h6}];
iocs({iob,4,0}) ->
    [];
iocs({iob,3,0}) ->
    [{{ioc,3,0,3},j4},
     {{ioc,3,0,2},h5}];
iocs({iob,2,0}) ->
    [{{ioc,2,0,3},j2},
     {{ioc,2,0,2},h3},
     {{ioc,2,0,1},j3},
     {{ioc,2,0,0},h4}].

-spec pins() -> [pin()].

pins() ->
    [a1,
     b1,
     c1,
     c2,
     d1,
     d2,
     e2,
     e1,
     f2,
     f3,
     g2,
     a3,
     b2,
     a2,
     a5,
     b4,
     a4,
     b3,
     b6,
     b5,
     a7,
     a6,
     b7,
     a8,
     b8,
     d8,
     c8,
     b9,
     a9,
     e8,
     d9,
     c9,
     f9,
     e9,
     h9,
     g9,
     g8,
     f8,
     j8,
     j9,
     j6,
     h7,
     j7,
     h8,
     g6,
     h6,
     j4,
     h5,
     j2,
     h3,
     j3,
     h4
    ].

-spec top_iocs(x()) -> [{ioc(), pin()}].

top_iocs(2) ->
    [{{ioc,2,5,0},a3},
     {{ioc,2,5,1},b2},
     {{ioc,2,5,2},a2}];
top_iocs(3) ->
    [{{ioc,3,5,0},a5},
     {{ioc,3,5,1},b4},
     {{ioc,3,5,2},a4},
     {{ioc,3,5,3},b3}];
top_iocs(4) ->
    [{{ioc,4,5,0},b6},
     {{ioc,4,5,2},b5}];
top_iocs(5) ->
    [{{ioc,5,5,0},a7},
     {{ioc,5,5,1},a6},
     {{ioc,5,5,2},b7}];
top_iocs(6) ->
    [{{ioc,6,5,2},a8},
     {{ioc,6,5,3},b8}];
top_iocs(7) ->
    [].

-spec top_pins(x()) -> [pin()].

top_pins(2) ->
    [a3, b2, a2];
top_pins(3) ->
    [a5, b4, a4, b3];
top_pins(4) ->
    [b6, b5];
top_pins(5) ->
    [a7, a6, b7];
top_pins(6) ->
    [a8, b8];
top_pins(7) ->
    [].

-spec left_iocs(y()) -> [{ioc(), pin()}].

left_iocs(1) ->
    [{{ioc,1,1,1},f2},
     {{ioc,1,1,2},f3},
     {{ioc,1,1,3},g2}];
left_iocs(2) ->
    [{{ioc,1,2,0},e1}];
left_iocs(3) ->
    [{{ioc,1,3,0},c2},
     {{ioc,1,3,1},d1},
     {{ioc,1,3,2},d2},
     {{ioc,1,3,3},e2}];
left_iocs(4) ->
    [{{ioc,1,4,1},a1},
     {{ioc,1,4,2},b1},
     {{ioc,1,4,3},c1}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [f2, f3, g2];
left_pins(2) ->
    [e1];
left_pins(3) ->
    [c2, d1, d2, e2];
left_pins(4) ->
    [a1, b1, c1].

-spec right_iocs(y()) -> [{ioc(), pin()}].

right_iocs(1) ->
    [{{ioc,8,1,4},h9},
     {{ioc,8,1,3},g9},
     {{ioc,8,1,2},g8},
     {{ioc,8,1,1},f8}];
right_iocs(2) ->
    [{{ioc,8,2,3},f9},
     {{ioc,8,2,0},e9}];
right_iocs(3) ->
    [{{ioc,8,3,4},e8},
     {{ioc,8,3,1},d9},
     {{ioc,8,3,0},c9}];
right_iocs(4) ->
    [{{ioc,8,4,4},d8},
     {{ioc,8,4,3},c8},
     {{ioc,8,4,2},b9},
     {{ioc,8,4,1},a9}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [h9, g9, g8, f8];
right_pins(2) ->
    [f9, e9];
right_pins(3) ->
    [e8, d9, c9];
right_pins(4) ->
    [d8, c8, b9, a9].

-spec bottom_iocs(x()) -> [{ioc(), pin()}].

bottom_iocs(2) ->
    [{{ioc,2,0,3},j2},
     {{ioc,2,0,2},h3},
     {{ioc,2,0,1},j3},
     {{ioc,2,0,0},h4}];
bottom_iocs(3) ->
    [{{ioc,3,0,3},j4},
     {{ioc,3,0,2},h5}];
bottom_iocs(4) ->
    [];
bottom_iocs(5) ->
    [{{ioc,5,0,1},g6},
     {{ioc,5,0,0},h6}];
bottom_iocs(6) ->
    [{{ioc,6,0,3},j6},
     {{ioc,6,0,2},h7},
     {{ioc,6,0,1},j7},
     {{ioc,6,0,0},h8}];
bottom_iocs(7) ->
    [{{ioc,7,0,2},j8},
     {{ioc,7,0,1},j9}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(2) ->
    [j2, h3, j3, h4];
bottom_pins(3) ->
    [j4, h5];
bottom_pins(4) ->
    [];
bottom_pins(5) ->
    [g6, h6];
bottom_pins(6) ->
    [j6, h7, j7, h8];
bottom_pins(7) ->
    [j8, j9].

