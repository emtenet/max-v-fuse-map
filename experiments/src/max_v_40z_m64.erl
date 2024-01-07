-module(max_v_40z_m64).

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
     {{ioc,1,4,3},c2},
     {{ioc,1,3,1},d2},
     {{ioc,1,2,0},e2},
     {{ioc,1,2,3},f1},
     {{ioc,1,1,2},f3},
     {{ioc,2,5,0},a2},
     {{ioc,2,5,2},b2},
     {{ioc,3,5,0},a4},
     {{ioc,3,5,2},a3},
     {{ioc,5,5,0},a6},
     {{ioc,5,5,1},c5},
     {{ioc,6,5,2},c6},
     {{ioc,8,4,3},b8},
     {{ioc,8,4,2},a8},
     {{ioc,8,3,1},b7},
     {{ioc,8,3,0},c8},
     {{ioc,8,2,0},d8},
     {{ioc,8,1,3},f8},
     {{ioc,7,0,2},g8},
     {{ioc,7,0,1},h8},
     {{ioc,6,0,3},f6},
     {{ioc,6,0,2},h7},
     {{ioc,6,0,1},g7},
     {{ioc,5,0,1},f5},
     {{ioc,3,0,3},h5},
     {{ioc,3,0,1},h6},
     {{ioc,2,0,3},h3},
     {{ioc,2,0,1},h4}
    ].

-spec iocs(iob()) -> [{ioc(), pin()}].

iocs({iob,1,4}) ->
    [{{ioc,1,4,1},a1},
     {{ioc,1,4,2},b1},
     {{ioc,1,4,3},c2}];
iocs({iob,1,3}) ->
    [{{ioc,1,3,1},d2}];
iocs({iob,1,2}) ->
    [{{ioc,1,2,0},e2},
     {{ioc,1,2,3},f1}];
iocs({iob,1,1}) ->
    [{{ioc,1,1,2},f3}];
iocs({iob,2,5}) ->
    [{{ioc,2,5,0},a2},
     {{ioc,2,5,2},b2}];
iocs({iob,3,5}) ->
    [{{ioc,3,5,0},a4},
     {{ioc,3,5,2},a3}];
iocs({iob,4,5}) ->
    [];
iocs({iob,5,5}) ->
    [{{ioc,5,5,0},a6},
     {{ioc,5,5,1},c5}];
iocs({iob,6,5}) ->
    [{{ioc,6,5,2},c6}];
iocs({iob,7,5}) ->
    [];
iocs({iob,8,4}) ->
    [{{ioc,8,4,3},b8},
     {{ioc,8,4,2},a8}];
iocs({iob,8,3}) ->
    [{{ioc,8,3,1},b7},
     {{ioc,8,3,0},c8}];
iocs({iob,8,2}) ->
    [{{ioc,8,2,0},d8}];
iocs({iob,8,1}) ->
    [{{ioc,8,1,3},f8}];
iocs({iob,7,0}) ->
    [{{ioc,7,0,2},g8},
     {{ioc,7,0,1},h8}];
iocs({iob,6,0}) ->
    [{{ioc,6,0,3},f6},
     {{ioc,6,0,2},h7},
     {{ioc,6,0,1},g7}];
iocs({iob,5,0}) ->
    [{{ioc,5,0,1},f5}];
iocs({iob,4,0}) ->
    [];
iocs({iob,3,0}) ->
    [{{ioc,3,0,3},h5},
     {{ioc,3,0,1},h6}];
iocs({iob,2,0}) ->
    [{{ioc,2,0,3},h3},
     {{ioc,2,0,1},h4}].

-spec pins() -> [pin()].

pins() ->
    [a1,
     b1,
     c2,
     d2,
     e2,
     f1,
     f3,
     a2,
     b2,
     a4,
     a3,
     a6,
     c5,
     c6,
     b8,
     a8,
     b7,
     c8,
     d8,
     f8,
     g8,
     h8,
     f6,
     h7,
     g7,
     f5,
     h5,
     h6,
     h3,
     h4
    ].

-spec top_iocs(x()) -> [{ioc(), pin()}].

top_iocs(2) ->
    [{{ioc,2,5,0},a2},
     {{ioc,2,5,2},b2}];
top_iocs(3) ->
    [{{ioc,3,5,0},a4},
     {{ioc,3,5,2},a3}];
top_iocs(4) ->
    [];
top_iocs(5) ->
    [{{ioc,5,5,0},a6},
     {{ioc,5,5,1},c5}];
top_iocs(6) ->
    [{{ioc,6,5,2},c6}];
top_iocs(7) ->
    [].

-spec top_pins(x()) -> [pin()].

top_pins(2) ->
    [a2, b2];
top_pins(3) ->
    [a4, a3];
top_pins(4) ->
    [];
top_pins(5) ->
    [a6, c5];
top_pins(6) ->
    [c6];
top_pins(7) ->
    [].

-spec left_iocs(y()) -> [{ioc(), pin()}].

left_iocs(1) ->
    [{{ioc,1,1,2},f3}];
left_iocs(2) ->
    [{{ioc,1,2,0},e2},
     {{ioc,1,2,3},f1}];
left_iocs(3) ->
    [{{ioc,1,3,1},d2}];
left_iocs(4) ->
    [{{ioc,1,4,1},a1},
     {{ioc,1,4,2},b1},
     {{ioc,1,4,3},c2}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [f3];
left_pins(2) ->
    [e2, f1];
left_pins(3) ->
    [d2];
left_pins(4) ->
    [a1, b1, c2].

-spec right_iocs(y()) -> [{ioc(), pin()}].

right_iocs(1) ->
    [{{ioc,8,1,3},f8}];
right_iocs(2) ->
    [{{ioc,8,2,0},d8}];
right_iocs(3) ->
    [{{ioc,8,3,1},b7},
     {{ioc,8,3,0},c8}];
right_iocs(4) ->
    [{{ioc,8,4,3},b8},
     {{ioc,8,4,2},a8}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [f8];
right_pins(2) ->
    [d8];
right_pins(3) ->
    [b7, c8];
right_pins(4) ->
    [b8, a8].

-spec bottom_iocs(x()) -> [{ioc(), pin()}].

bottom_iocs(2) ->
    [{{ioc,2,0,3},h3},
     {{ioc,2,0,1},h4}];
bottom_iocs(3) ->
    [{{ioc,3,0,3},h5},
     {{ioc,3,0,1},h6}];
bottom_iocs(4) ->
    [];
bottom_iocs(5) ->
    [{{ioc,5,0,1},f5}];
bottom_iocs(6) ->
    [{{ioc,6,0,3},f6},
     {{ioc,6,0,2},h7},
     {{ioc,6,0,1},g7}];
bottom_iocs(7) ->
    [{{ioc,7,0,2},g8},
     {{ioc,7,0,1},h8}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(2) ->
    [h3, h4];
bottom_pins(3) ->
    [h5, h6];
bottom_pins(4) ->
    [];
bottom_pins(5) ->
    [f5];
bottom_pins(6) ->
    [f6, h7, g7];
bottom_pins(7) ->
    [g8, h8].

