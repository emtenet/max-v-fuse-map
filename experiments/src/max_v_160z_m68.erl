-module(max_v_160z_m68).

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
     {a2,{ioc,2,5,2}},
     {a3,{ioc,2,5,0}},
     {a4,{ioc,3,5,2}},
     {a5,{ioc,3,5,0}},
     {a6,{ioc,5,5,1}},
     {a7,{ioc,5,5,0}},
     {a8,{ioc,6,5,2}},
     {a9,{ioc,8,4,1}},
     {b1,{ioc,1,4,2}},
     {b2,{ioc,2,5,1}},
     {b3,{ioc,3,5,3}},
     {b4,{ioc,3,5,1}},
     {b5,{ioc,4,5,2}},
     {b6,{ioc,4,5,0}},
     {b7,{ioc,5,5,2}},
     {b8,{ioc,6,5,3}},
     {b9,{ioc,8,4,2}},
     {c1,{ioc,1,4,3}},
     {c2,{ioc,1,3,0}},
     {c8,{ioc,8,4,3}},
     {c9,{ioc,8,3,0}},
     {d1,{ioc,1,3,1}},
     {d2,{ioc,1,3,2}},
     {d8,{ioc,8,4,4}},
     {d9,{ioc,8,3,1}},
     {e1,{ioc,1,2,0}},
     {e2,{ioc,1,3,3}},
     {e8,{ioc,8,3,4}},
     {e9,{ioc,8,2,0}},
     {f2,{ioc,1,1,1}},
     {f3,{ioc,1,1,2}},
     {f8,{ioc,8,1,1}},
     {f9,{ioc,8,2,3}},
     {g2,{ioc,1,1,3}},
     {g6,{ioc,5,0,1}},
     {g8,{ioc,8,1,2}},
     {g9,{ioc,8,1,3}},
     {h3,{ioc,2,0,2}},
     {h4,{ioc,2,0,0}},
     {h5,{ioc,3,0,2}},
     {h6,{ioc,5,0,0}},
     {h7,{ioc,6,0,2}},
     {h8,{ioc,6,0,0}},
     {h9,{ioc,8,1,4}},
     {j2,{ioc,2,0,3}},
     {j3,{ioc,2,0,1}},
     {j4,{ioc,3,0,3}},
     {j6,{ioc,6,0,3}},
     {j7,{ioc,6,0,1}},
     {j8,{ioc,7,0,2}},
     {j9,{ioc,7,0,1}}
    ].

-spec iocs(iob()) -> [{pin(), ioc()}].

iocs({iob,1,4}) ->
    [{a1,{ioc,1,4,1}},
     {b1,{ioc,1,4,2}},
     {c1,{ioc,1,4,3}}];
iocs({iob,1,3}) ->
    [{c2,{ioc,1,3,0}},
     {d1,{ioc,1,3,1}},
     {d2,{ioc,1,3,2}},
     {e2,{ioc,1,3,3}}];
iocs({iob,1,2}) ->
    [{e1,{ioc,1,2,0}}];
iocs({iob,1,1}) ->
    [{f2,{ioc,1,1,1}},
     {f3,{ioc,1,1,2}},
     {g2,{ioc,1,1,3}}];
iocs({iob,2,5}) ->
    [{a3,{ioc,2,5,0}},
     {b2,{ioc,2,5,1}},
     {a2,{ioc,2,5,2}}];
iocs({iob,3,5}) ->
    [{a5,{ioc,3,5,0}},
     {b4,{ioc,3,5,1}},
     {a4,{ioc,3,5,2}},
     {b3,{ioc,3,5,3}}];
iocs({iob,4,5}) ->
    [{b6,{ioc,4,5,0}},
     {b5,{ioc,4,5,2}}];
iocs({iob,5,5}) ->
    [{a7,{ioc,5,5,0}},
     {a6,{ioc,5,5,1}},
     {b7,{ioc,5,5,2}}];
iocs({iob,6,5}) ->
    [{a8,{ioc,6,5,2}},
     {b8,{ioc,6,5,3}}];
iocs({iob,7,5}) ->
    [];
iocs({iob,8,4}) ->
    [{a9,{ioc,8,4,1}},
     {b9,{ioc,8,4,2}},
     {c8,{ioc,8,4,3}},
     {d8,{ioc,8,4,4}}];
iocs({iob,8,3}) ->
    [{c9,{ioc,8,3,0}},
     {d9,{ioc,8,3,1}},
     {e8,{ioc,8,3,4}}];
iocs({iob,8,2}) ->
    [{e9,{ioc,8,2,0}},
     {f9,{ioc,8,2,3}}];
iocs({iob,8,1}) ->
    [{f8,{ioc,8,1,1}},
     {g8,{ioc,8,1,2}},
     {g9,{ioc,8,1,3}},
     {h9,{ioc,8,1,4}}];
iocs({iob,7,0}) ->
    [{j9,{ioc,7,0,1}},
     {j8,{ioc,7,0,2}}];
iocs({iob,6,0}) ->
    [{h8,{ioc,6,0,0}},
     {j7,{ioc,6,0,1}},
     {h7,{ioc,6,0,2}},
     {j6,{ioc,6,0,3}}];
iocs({iob,5,0}) ->
    [{h6,{ioc,5,0,0}},
     {g6,{ioc,5,0,1}}];
iocs({iob,4,0}) ->
    [];
iocs({iob,3,0}) ->
    [{h5,{ioc,3,0,2}},
     {j4,{ioc,3,0,3}}];
iocs({iob,2,0}) ->
    [{h4,{ioc,2,0,0}},
     {j3,{ioc,2,0,1}},
     {h3,{ioc,2,0,2}},
     {j2,{ioc,2,0,3}}].

-spec pins() -> [pin()].

pins() ->
    [a1,
     a2,
     a3,
     a4,
     a5,
     a6,
     a7,
     a8,
     a9,
     b1,
     b2,
     b3,
     b4,
     b5,
     b6,
     b7,
     b8,
     b9,
     c1,
     c2,
     c8,
     c9,
     d1,
     d2,
     d8,
     d9,
     e1,
     e2,
     e8,
     e9,
     f2,
     f3,
     f8,
     f9,
     g2,
     g6,
     g8,
     g9,
     h3,
     h4,
     h5,
     h6,
     h7,
     h8,
     h9,
     j2,
     j3,
     j4,
     j6,
     j7,
     j8,
     j9
    ].

-spec top_iocs(x()) -> [{pin(), ioc()}].

top_iocs(2) ->
    [{a3,{ioc,2,5,0}},
     {b2,{ioc,2,5,1}},
     {a2,{ioc,2,5,2}}];
top_iocs(3) ->
    [{a5,{ioc,3,5,0}},
     {b4,{ioc,3,5,1}},
     {a4,{ioc,3,5,2}},
     {b3,{ioc,3,5,3}}];
top_iocs(4) ->
    [{b6,{ioc,4,5,0}},
     {b5,{ioc,4,5,2}}];
top_iocs(5) ->
    [{a7,{ioc,5,5,0}},
     {a6,{ioc,5,5,1}},
     {b7,{ioc,5,5,2}}];
top_iocs(6) ->
    [{a8,{ioc,6,5,2}},
     {b8,{ioc,6,5,3}}];
top_iocs(7) ->
    [].

-spec top_pins(x()) -> [pin()].

top_pins(2) ->
    [a2, a3, b2];
top_pins(3) ->
    [a4, a5, b3, b4];
top_pins(4) ->
    [b5, b6];
top_pins(5) ->
    [a6, a7, b7];
top_pins(6) ->
    [a8, b8];
top_pins(7) ->
    [].

-spec left_iocs(y()) -> [{pin(), ioc()}].

left_iocs(1) ->
    [{f2,{ioc,1,1,1}},
     {f3,{ioc,1,1,2}},
     {g2,{ioc,1,1,3}}];
left_iocs(2) ->
    [{e1,{ioc,1,2,0}}];
left_iocs(3) ->
    [{c2,{ioc,1,3,0}},
     {d1,{ioc,1,3,1}},
     {d2,{ioc,1,3,2}},
     {e2,{ioc,1,3,3}}];
left_iocs(4) ->
    [{a1,{ioc,1,4,1}},
     {b1,{ioc,1,4,2}},
     {c1,{ioc,1,4,3}}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [f2, f3, g2];
left_pins(2) ->
    [e1];
left_pins(3) ->
    [c2, d1, d2, e2];
left_pins(4) ->
    [a1, b1, c1].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{f8,{ioc,8,1,1}},
     {g8,{ioc,8,1,2}},
     {g9,{ioc,8,1,3}},
     {h9,{ioc,8,1,4}}];
right_iocs(2) ->
    [{e9,{ioc,8,2,0}},
     {f9,{ioc,8,2,3}}];
right_iocs(3) ->
    [{c9,{ioc,8,3,0}},
     {d9,{ioc,8,3,1}},
     {e8,{ioc,8,3,4}}];
right_iocs(4) ->
    [{a9,{ioc,8,4,1}},
     {b9,{ioc,8,4,2}},
     {c8,{ioc,8,4,3}},
     {d8,{ioc,8,4,4}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [f8, g8, g9, h9];
right_pins(2) ->
    [e9, f9];
right_pins(3) ->
    [c9, d9, e8];
right_pins(4) ->
    [a9, b9, c8, d8].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(2) ->
    [{h4,{ioc,2,0,0}},
     {j3,{ioc,2,0,1}},
     {h3,{ioc,2,0,2}},
     {j2,{ioc,2,0,3}}];
bottom_iocs(3) ->
    [{h5,{ioc,3,0,2}},
     {j4,{ioc,3,0,3}}];
bottom_iocs(4) ->
    [];
bottom_iocs(5) ->
    [{h6,{ioc,5,0,0}},
     {g6,{ioc,5,0,1}}];
bottom_iocs(6) ->
    [{h8,{ioc,6,0,0}},
     {j7,{ioc,6,0,1}},
     {h7,{ioc,6,0,2}},
     {j6,{ioc,6,0,3}}];
bottom_iocs(7) ->
    [{j9,{ioc,7,0,1}},
     {j8,{ioc,7,0,2}}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(2) ->
    [h3, h4, j2, j3];
bottom_pins(3) ->
    [h5, j4];
bottom_pins(4) ->
    [];
bottom_pins(5) ->
    [g6, h6];
bottom_pins(6) ->
    [h7, h8, j6, j7];
bottom_pins(7) ->
    [j8, j9].

