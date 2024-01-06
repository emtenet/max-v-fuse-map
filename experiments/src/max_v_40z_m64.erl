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

-spec iocs() -> [{pin(), ioc()}].

iocs() ->
    [{a1,{ioc,1,4,1}},
     {a2,{ioc,2,5,0}},
     {a3,{ioc,3,5,2}},
     {a4,{ioc,3,5,0}},
     {a6,{ioc,5,5,0}},
     {a8,{ioc,8,4,2}},
     {b1,{ioc,1,4,2}},
     {b2,{ioc,2,5,2}},
     {b7,{ioc,8,3,1}},
     {b8,{ioc,8,4,3}},
     {c2,{ioc,1,4,3}},
     {c5,{ioc,5,5,1}},
     {c6,{ioc,6,5,2}},
     {c8,{ioc,8,3,0}},
     {d2,{ioc,1,3,1}},
     {d8,{ioc,8,2,0}},
     {e2,{ioc,1,2,0}},
     {f1,{ioc,1,2,3}},
     {f3,{ioc,1,1,2}},
     {f5,{ioc,5,0,1}},
     {f6,{ioc,6,0,3}},
     {f8,{ioc,8,1,3}},
     {g7,{ioc,6,0,1}},
     {g8,{ioc,7,0,2}},
     {h3,{ioc,2,0,3}},
     {h4,{ioc,2,0,1}},
     {h5,{ioc,3,0,3}},
     {h6,{ioc,3,0,1}},
     {h7,{ioc,6,0,2}},
     {h8,{ioc,7,0,1}}
    ].

-spec iocs(iob()) -> [{pin(), ioc()}].

iocs({iob,1,4}) ->
    [{a1,{ioc,1,4,1}},
     {b1,{ioc,1,4,2}},
     {c2,{ioc,1,4,3}}];
iocs({iob,1,3}) ->
    [{d2,{ioc,1,3,1}}];
iocs({iob,1,2}) ->
    [{e2,{ioc,1,2,0}},
     {f1,{ioc,1,2,3}}];
iocs({iob,1,1}) ->
    [{f3,{ioc,1,1,2}}];
iocs({iob,2,5}) ->
    [{a2,{ioc,2,5,0}},
     {b2,{ioc,2,5,2}}];
iocs({iob,3,5}) ->
    [{a4,{ioc,3,5,0}},
     {a3,{ioc,3,5,2}}];
iocs({iob,4,5}) ->
    [];
iocs({iob,5,5}) ->
    [{a6,{ioc,5,5,0}},
     {c5,{ioc,5,5,1}}];
iocs({iob,6,5}) ->
    [{c6,{ioc,6,5,2}}];
iocs({iob,7,5}) ->
    [];
iocs({iob,8,4}) ->
    [{a8,{ioc,8,4,2}},
     {b8,{ioc,8,4,3}}];
iocs({iob,8,3}) ->
    [{c8,{ioc,8,3,0}},
     {b7,{ioc,8,3,1}}];
iocs({iob,8,2}) ->
    [{d8,{ioc,8,2,0}}];
iocs({iob,8,1}) ->
    [{f8,{ioc,8,1,3}}];
iocs({iob,7,0}) ->
    [{h8,{ioc,7,0,1}},
     {g8,{ioc,7,0,2}}];
iocs({iob,6,0}) ->
    [{g7,{ioc,6,0,1}},
     {h7,{ioc,6,0,2}},
     {f6,{ioc,6,0,3}}];
iocs({iob,5,0}) ->
    [{f5,{ioc,5,0,1}}];
iocs({iob,4,0}) ->
    [];
iocs({iob,3,0}) ->
    [{h6,{ioc,3,0,1}},
     {h5,{ioc,3,0,3}}];
iocs({iob,2,0}) ->
    [{h4,{ioc,2,0,1}},
     {h3,{ioc,2,0,3}}].

-spec pins() -> [pin()].

pins() ->
    [a1,
     a2,
     a3,
     a4,
     a6,
     a8,
     b1,
     b2,
     b7,
     b8,
     c2,
     c5,
     c6,
     c8,
     d2,
     d8,
     e2,
     f1,
     f3,
     f5,
     f6,
     f8,
     g7,
     g8,
     h3,
     h4,
     h5,
     h6,
     h7,
     h8
    ].

-spec top_iocs(x()) -> [{pin(), ioc()}].

top_iocs(2) ->
    [{a2,{ioc,2,5,0}},
     {b2,{ioc,2,5,2}}];
top_iocs(3) ->
    [{a4,{ioc,3,5,0}},
     {a3,{ioc,3,5,2}}];
top_iocs(4) ->
    [];
top_iocs(5) ->
    [{a6,{ioc,5,5,0}},
     {c5,{ioc,5,5,1}}];
top_iocs(6) ->
    [{c6,{ioc,6,5,2}}];
top_iocs(7) ->
    [].

-spec top_pins(x()) -> [pin()].

top_pins(2) ->
    [a2, b2];
top_pins(3) ->
    [a3, a4];
top_pins(4) ->
    [];
top_pins(5) ->
    [a6, c5];
top_pins(6) ->
    [c6];
top_pins(7) ->
    [].

-spec left_iocs(y()) -> [{pin(), ioc()}].

left_iocs(1) ->
    [{f3,{ioc,1,1,2}}];
left_iocs(2) ->
    [{e2,{ioc,1,2,0}},
     {f1,{ioc,1,2,3}}];
left_iocs(3) ->
    [{d2,{ioc,1,3,1}}];
left_iocs(4) ->
    [{a1,{ioc,1,4,1}},
     {b1,{ioc,1,4,2}},
     {c2,{ioc,1,4,3}}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [f3];
left_pins(2) ->
    [e2, f1];
left_pins(3) ->
    [d2];
left_pins(4) ->
    [a1, b1, c2].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{f8,{ioc,8,1,3}}];
right_iocs(2) ->
    [{d8,{ioc,8,2,0}}];
right_iocs(3) ->
    [{c8,{ioc,8,3,0}},
     {b7,{ioc,8,3,1}}];
right_iocs(4) ->
    [{a8,{ioc,8,4,2}},
     {b8,{ioc,8,4,3}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [f8];
right_pins(2) ->
    [d8];
right_pins(3) ->
    [b7, c8];
right_pins(4) ->
    [a8, b8].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(2) ->
    [{h4,{ioc,2,0,1}},
     {h3,{ioc,2,0,3}}];
bottom_iocs(3) ->
    [{h6,{ioc,3,0,1}},
     {h5,{ioc,3,0,3}}];
bottom_iocs(4) ->
    [];
bottom_iocs(5) ->
    [{f5,{ioc,5,0,1}}];
bottom_iocs(6) ->
    [{g7,{ioc,6,0,1}},
     {h7,{ioc,6,0,2}},
     {f6,{ioc,6,0,3}}];
bottom_iocs(7) ->
    [{h8,{ioc,7,0,1}},
     {g8,{ioc,7,0,2}}].

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
    [f6, g7, h7];
bottom_pins(7) ->
    [g8, h8].

