-module(max_v_570z_f256).

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
    [{{ioc,0,4,0},k2},
     {{ioc,0,4,1},m2},
     {{ioc,0,4,2},l1},
     {{ioc,0,4,3},m3},
     {{ioc,0,4,4},l2},
     {{ioc,0,4,5},m4},
     {{ioc,0,4,6},m1},
     {{ioc,0,5,0},h5},
     {{ioc,0,5,1},j5},
     {{ioc,0,5,2},k3},
     {{ioc,0,5,3},j2},
     {{ioc,0,5,4},l3},
     {{ioc,0,5,5},k1},
     {{ioc,0,5,6},l4},
     {{ioc,0,6,0},g1},
     {{ioc,0,6,1},g3},
     {{ioc,0,6,2},h2},
     {{ioc,0,6,3},h3},
     {{ioc,0,6,4},h1},
     {{ioc,0,6,5},j3},
     {{ioc,0,6,6},j1},
     {{ioc,0,7,0},e4},
     {{ioc,0,7,1},f2},
     {{ioc,0,7,2},e3},
     {{ioc,0,7,3},f1},
     {{ioc,0,7,4},e2},
     {{ioc,0,7,5},g2},
     {{ioc,0,7,6},f3},
     {{ioc,1,8,0},d3},
     {{ioc,1,8,1},d1},
     {{ioc,1,8,2},d2},
     {{ioc,1,8,3},e1},
     {{ioc,2,8,0},b3},
     {{ioc,2,8,1},b1},
     {{ioc,2,8,2},c3},
     {{ioc,2,8,3},c2},
     {{ioc,3,8,0},c4},
     {{ioc,3,8,1},a4},
     {{ioc,3,8,2},d4},
     {{ioc,3,8,3},a2},
     {{ioc,4,8,0},d5},
     {{ioc,4,8,1},b6},
     {{ioc,4,8,2},b4},
     {{ioc,4,8,3},a5},
     {{ioc,5,8,1},c5},
     {{ioc,5,8,2},a6},
     {{ioc,6,8,0},b8},
     {{ioc,6,8,1},c6},
     {{ioc,6,8,2},a7},
     {{ioc,6,8,3},b5},
     {{ioc,7,8,0},b9},
     {{ioc,7,8,1},a9},
     {{ioc,7,8,2},a8},
     {{ioc,7,8,3},c7},
     {{ioc,8,8,0},b10},
     {{ioc,8,8,1},c9},
     {{ioc,8,8,2},a10},
     {{ioc,8,8,3},c8},
     {{ioc,9,8,0},d11},
     {{ioc,9,8,1},a11},
     {{ioc,9,8,2},c10},
     {{ioc,10,8,0},d12},
     {{ioc,10,8,1},a12},
     {{ioc,10,8,2},c11},
     {{ioc,10,8,3},b11},
     {{ioc,11,8,0},b13},
     {{ioc,11,8,1},a13},
     {{ioc,11,8,2},c12},
     {{ioc,11,8,3},b12},
     {{ioc,12,8,0},b14},
     {{ioc,12,8,1},b16},
     {{ioc,12,8,2},c13},
     {{ioc,12,8,3},a15},
     {{ioc,13,7,5},d13},
     {{ioc,13,7,4},e15},
     {{ioc,13,7,3},c15},
     {{ioc,13,7,2},d16},
     {{ioc,13,7,1},c14},
     {{ioc,13,7,0},d15},
     {{ioc,13,6,5},e14},
     {{ioc,13,6,4},f16},
     {{ioc,13,6,3},e13},
     {{ioc,13,6,2},f15},
     {{ioc,13,6,1},d14},
     {{ioc,13,6,0},e16},
     {{ioc,13,5,5},g14},
     {{ioc,13,5,4},h15},
     {{ioc,13,5,3},f14},
     {{ioc,13,5,2},g16},
     {{ioc,13,5,1},f13},
     {{ioc,13,5,0},g15},
     {{ioc,13,4,5},j14},
     {{ioc,13,4,4},j12},
     {{ioc,13,4,3},h12},
     {{ioc,13,4,2},j16},
     {{ioc,13,4,1},h14},
     {{ioc,13,4,0},h16},
     {{ioc,13,3,5},l13},
     {{ioc,13,3,4},k15},
     {{ioc,13,3,3},l14},
     {{ioc,13,3,2},k16},
     {{ioc,13,3,1},k14},
     {{ioc,13,3,0},j15},
     {{ioc,13,2,5},n14},
     {{ioc,13,2,4},m16},
     {{ioc,13,2,3},m13},
     {{ioc,13,2,2},l15},
     {{ioc,13,2,1},m14},
     {{ioc,13,2,0},l16},
     {{ioc,13,1,5},p14},
     {{ioc,13,1,4},n15},
     {{ioc,13,1,3},p15},
     {{ioc,13,1,2},n16},
     {{ioc,13,1,1},n13},
     {{ioc,13,1,0},m15},
     {{ioc,12,0,3},p13},
     {{ioc,12,0,2},t15},
     {{ioc,12,0,1},r14},
     {{ioc,12,0,0},r16},
     {{ioc,11,0,3},n12},
     {{ioc,11,0,2},t12},
     {{ioc,11,0,1},r13},
     {{ioc,11,0,0},t13},
     {{ioc,10,0,3},r12},
     {{ioc,10,0,2},t11},
     {{ioc,10,0,1},p12},
     {{ioc,10,0,0},r11},
     {{ioc,8,3,3},m8},
     {{ioc,8,3,2},m9},
     {{ioc,8,3,1},p11},
     {{ioc,8,3,0},r10},
     {{ioc,7,3,3},t9},
     {{ioc,7,3,2},r9},
     {{ioc,7,3,1},p10},
     {{ioc,7,3,0},t10},
     {{ioc,6,3,3},p8},
     {{ioc,6,3,2},r8},
     {{ioc,6,3,1},p9},
     {{ioc,6,3,0},t8},
     {{ioc,5,3,3},p6},
     {{ioc,5,3,2},r7},
     {{ioc,5,3,1},p7},
     {{ioc,5,3,0},t7},
     {{ioc,4,3,3},p5},
     {{ioc,4,3,2},r6},
     {{ioc,4,3,1},r5},
     {{ioc,4,3,0},t6},
     {{ioc,3,3,3},r4},
     {{ioc,3,3,2},t4},
     {{ioc,3,3,1},n5},
     {{ioc,3,3,0},t5},
     {{ioc,2,3,3},r3},
     {{ioc,2,3,2},r1},
     {{ioc,2,3,1},p4},
     {{ioc,2,3,0},t2},
     {{ioc,1,3,3},n2},
     {{ioc,1,3,2},n1},
     {{ioc,1,3,1},n3},
     {{ioc,1,3,0},p2}
    ].

-spec iocs(iob()) -> [{ioc(), pin()}].

iocs({iob,0,4}) ->
    [{{ioc,0,4,0},k2},
     {{ioc,0,4,1},m2},
     {{ioc,0,4,2},l1},
     {{ioc,0,4,3},m3},
     {{ioc,0,4,4},l2},
     {{ioc,0,4,5},m4},
     {{ioc,0,4,6},m1}];
iocs({iob,0,5}) ->
    [{{ioc,0,5,0},h5},
     {{ioc,0,5,1},j5},
     {{ioc,0,5,2},k3},
     {{ioc,0,5,3},j2},
     {{ioc,0,5,4},l3},
     {{ioc,0,5,5},k1},
     {{ioc,0,5,6},l4}];
iocs({iob,0,6}) ->
    [{{ioc,0,6,0},g1},
     {{ioc,0,6,1},g3},
     {{ioc,0,6,2},h2},
     {{ioc,0,6,3},h3},
     {{ioc,0,6,4},h1},
     {{ioc,0,6,5},j3},
     {{ioc,0,6,6},j1}];
iocs({iob,0,7}) ->
    [{{ioc,0,7,0},e4},
     {{ioc,0,7,1},f2},
     {{ioc,0,7,2},e3},
     {{ioc,0,7,3},f1},
     {{ioc,0,7,4},e2},
     {{ioc,0,7,5},g2},
     {{ioc,0,7,6},f3}];
iocs({iob,1,8}) ->
    [{{ioc,1,8,0},d3},
     {{ioc,1,8,1},d1},
     {{ioc,1,8,2},d2},
     {{ioc,1,8,3},e1}];
iocs({iob,2,8}) ->
    [{{ioc,2,8,0},b3},
     {{ioc,2,8,1},b1},
     {{ioc,2,8,2},c3},
     {{ioc,2,8,3},c2}];
iocs({iob,3,8}) ->
    [{{ioc,3,8,0},c4},
     {{ioc,3,8,1},a4},
     {{ioc,3,8,2},d4},
     {{ioc,3,8,3},a2}];
iocs({iob,4,8}) ->
    [{{ioc,4,8,0},d5},
     {{ioc,4,8,1},b6},
     {{ioc,4,8,2},b4},
     {{ioc,4,8,3},a5}];
iocs({iob,5,8}) ->
    [{{ioc,5,8,1},c5},
     {{ioc,5,8,2},a6}];
iocs({iob,6,8}) ->
    [{{ioc,6,8,0},b8},
     {{ioc,6,8,1},c6},
     {{ioc,6,8,2},a7},
     {{ioc,6,8,3},b5}];
iocs({iob,7,8}) ->
    [{{ioc,7,8,0},b9},
     {{ioc,7,8,1},a9},
     {{ioc,7,8,2},a8},
     {{ioc,7,8,3},c7}];
iocs({iob,8,8}) ->
    [{{ioc,8,8,0},b10},
     {{ioc,8,8,1},c9},
     {{ioc,8,8,2},a10},
     {{ioc,8,8,3},c8}];
iocs({iob,9,8}) ->
    [{{ioc,9,8,0},d11},
     {{ioc,9,8,1},a11},
     {{ioc,9,8,2},c10}];
iocs({iob,10,8}) ->
    [{{ioc,10,8,0},d12},
     {{ioc,10,8,1},a12},
     {{ioc,10,8,2},c11},
     {{ioc,10,8,3},b11}];
iocs({iob,11,8}) ->
    [{{ioc,11,8,0},b13},
     {{ioc,11,8,1},a13},
     {{ioc,11,8,2},c12},
     {{ioc,11,8,3},b12}];
iocs({iob,12,8}) ->
    [{{ioc,12,8,0},b14},
     {{ioc,12,8,1},b16},
     {{ioc,12,8,2},c13},
     {{ioc,12,8,3},a15}];
iocs({iob,13,7}) ->
    [{{ioc,13,7,5},d13},
     {{ioc,13,7,4},e15},
     {{ioc,13,7,3},c15},
     {{ioc,13,7,2},d16},
     {{ioc,13,7,1},c14},
     {{ioc,13,7,0},d15}];
iocs({iob,13,6}) ->
    [{{ioc,13,6,5},e14},
     {{ioc,13,6,4},f16},
     {{ioc,13,6,3},e13},
     {{ioc,13,6,2},f15},
     {{ioc,13,6,1},d14},
     {{ioc,13,6,0},e16}];
iocs({iob,13,5}) ->
    [{{ioc,13,5,5},g14},
     {{ioc,13,5,4},h15},
     {{ioc,13,5,3},f14},
     {{ioc,13,5,2},g16},
     {{ioc,13,5,1},f13},
     {{ioc,13,5,0},g15}];
iocs({iob,13,4}) ->
    [{{ioc,13,4,5},j14},
     {{ioc,13,4,4},j12},
     {{ioc,13,4,3},h12},
     {{ioc,13,4,2},j16},
     {{ioc,13,4,1},h14},
     {{ioc,13,4,0},h16}];
iocs({iob,13,3}) ->
    [{{ioc,13,3,5},l13},
     {{ioc,13,3,4},k15},
     {{ioc,13,3,3},l14},
     {{ioc,13,3,2},k16},
     {{ioc,13,3,1},k14},
     {{ioc,13,3,0},j15}];
iocs({iob,13,2}) ->
    [{{ioc,13,2,5},n14},
     {{ioc,13,2,4},m16},
     {{ioc,13,2,3},m13},
     {{ioc,13,2,2},l15},
     {{ioc,13,2,1},m14},
     {{ioc,13,2,0},l16}];
iocs({iob,13,1}) ->
    [{{ioc,13,1,5},p14},
     {{ioc,13,1,4},n15},
     {{ioc,13,1,3},p15},
     {{ioc,13,1,2},n16},
     {{ioc,13,1,1},n13},
     {{ioc,13,1,0},m15}];
iocs({iob,12,0}) ->
    [{{ioc,12,0,3},p13},
     {{ioc,12,0,2},t15},
     {{ioc,12,0,1},r14},
     {{ioc,12,0,0},r16}];
iocs({iob,11,0}) ->
    [{{ioc,11,0,3},n12},
     {{ioc,11,0,2},t12},
     {{ioc,11,0,1},r13},
     {{ioc,11,0,0},t13}];
iocs({iob,10,0}) ->
    [{{ioc,10,0,3},r12},
     {{ioc,10,0,2},t11},
     {{ioc,10,0,1},p12},
     {{ioc,10,0,0},r11}];
iocs({iob,8,3}) ->
    [{{ioc,8,3,3},m8},
     {{ioc,8,3,2},m9},
     {{ioc,8,3,1},p11},
     {{ioc,8,3,0},r10}];
iocs({iob,7,3}) ->
    [{{ioc,7,3,3},t9},
     {{ioc,7,3,2},r9},
     {{ioc,7,3,1},p10},
     {{ioc,7,3,0},t10}];
iocs({iob,6,3}) ->
    [{{ioc,6,3,3},p8},
     {{ioc,6,3,2},r8},
     {{ioc,6,3,1},p9},
     {{ioc,6,3,0},t8}];
iocs({iob,5,3}) ->
    [{{ioc,5,3,3},p6},
     {{ioc,5,3,2},r7},
     {{ioc,5,3,1},p7},
     {{ioc,5,3,0},t7}];
iocs({iob,4,3}) ->
    [{{ioc,4,3,3},p5},
     {{ioc,4,3,2},r6},
     {{ioc,4,3,1},r5},
     {{ioc,4,3,0},t6}];
iocs({iob,3,3}) ->
    [{{ioc,3,3,3},r4},
     {{ioc,3,3,2},t4},
     {{ioc,3,3,1},n5},
     {{ioc,3,3,0},t5}];
iocs({iob,2,3}) ->
    [{{ioc,2,3,3},r3},
     {{ioc,2,3,2},r1},
     {{ioc,2,3,1},p4},
     {{ioc,2,3,0},t2}];
iocs({iob,1,3}) ->
    [{{ioc,1,3,3},n2},
     {{ioc,1,3,2},n1},
     {{ioc,1,3,1},n3},
     {{ioc,1,3,0},p2}].

-spec pins() -> [pin()].

pins() ->
    [k2,
     m2,
     l1,
     m3,
     l2,
     m4,
     m1,
     h5,
     j5,
     k3,
     j2,
     l3,
     k1,
     l4,
     g1,
     g3,
     h2,
     h3,
     h1,
     j3,
     j1,
     e4,
     f2,
     e3,
     f1,
     e2,
     g2,
     f3,
     d3,
     d1,
     d2,
     e1,
     b3,
     b1,
     c3,
     c2,
     c4,
     a4,
     d4,
     a2,
     d5,
     b6,
     b4,
     a5,
     c5,
     a6,
     b8,
     c6,
     a7,
     b5,
     b9,
     a9,
     a8,
     c7,
     b10,
     c9,
     a10,
     c8,
     d11,
     a11,
     c10,
     d12,
     a12,
     c11,
     b11,
     b13,
     a13,
     c12,
     b12,
     b14,
     b16,
     c13,
     a15,
     d13,
     e15,
     c15,
     d16,
     c14,
     d15,
     e14,
     f16,
     e13,
     f15,
     d14,
     e16,
     g14,
     h15,
     f14,
     g16,
     f13,
     g15,
     j14,
     j12,
     h12,
     j16,
     h14,
     h16,
     l13,
     k15,
     l14,
     k16,
     k14,
     j15,
     n14,
     m16,
     m13,
     l15,
     m14,
     l16,
     p14,
     n15,
     p15,
     n16,
     n13,
     m15,
     p13,
     t15,
     r14,
     r16,
     n12,
     t12,
     r13,
     t13,
     r12,
     t11,
     p12,
     r11,
     m8,
     m9,
     p11,
     r10,
     t9,
     r9,
     p10,
     t10,
     p8,
     r8,
     p9,
     t8,
     p6,
     r7,
     p7,
     t7,
     p5,
     r6,
     r5,
     t6,
     r4,
     t4,
     n5,
     t5,
     r3,
     r1,
     p4,
     t2,
     n2,
     n1,
     n3,
     p2
    ].

-spec top_iocs(x()) -> [{ioc(), pin()}].

top_iocs(1) ->
    [{{ioc,1,8,0},d3},
     {{ioc,1,8,1},d1},
     {{ioc,1,8,2},d2},
     {{ioc,1,8,3},e1}];
top_iocs(2) ->
    [{{ioc,2,8,0},b3},
     {{ioc,2,8,1},b1},
     {{ioc,2,8,2},c3},
     {{ioc,2,8,3},c2}];
top_iocs(3) ->
    [{{ioc,3,8,0},c4},
     {{ioc,3,8,1},a4},
     {{ioc,3,8,2},d4},
     {{ioc,3,8,3},a2}];
top_iocs(4) ->
    [{{ioc,4,8,0},d5},
     {{ioc,4,8,1},b6},
     {{ioc,4,8,2},b4},
     {{ioc,4,8,3},a5}];
top_iocs(5) ->
    [{{ioc,5,8,1},c5},
     {{ioc,5,8,2},a6}];
top_iocs(6) ->
    [{{ioc,6,8,0},b8},
     {{ioc,6,8,1},c6},
     {{ioc,6,8,2},a7},
     {{ioc,6,8,3},b5}];
top_iocs(7) ->
    [{{ioc,7,8,0},b9},
     {{ioc,7,8,1},a9},
     {{ioc,7,8,2},a8},
     {{ioc,7,8,3},c7}];
top_iocs(8) ->
    [{{ioc,8,8,0},b10},
     {{ioc,8,8,1},c9},
     {{ioc,8,8,2},a10},
     {{ioc,8,8,3},c8}];
top_iocs(9) ->
    [{{ioc,9,8,0},d11},
     {{ioc,9,8,1},a11},
     {{ioc,9,8,2},c10}];
top_iocs(10) ->
    [{{ioc,10,8,0},d12},
     {{ioc,10,8,1},a12},
     {{ioc,10,8,2},c11},
     {{ioc,10,8,3},b11}];
top_iocs(11) ->
    [{{ioc,11,8,0},b13},
     {{ioc,11,8,1},a13},
     {{ioc,11,8,2},c12},
     {{ioc,11,8,3},b12}];
top_iocs(12) ->
    [{{ioc,12,8,0},b14},
     {{ioc,12,8,1},b16},
     {{ioc,12,8,2},c13},
     {{ioc,12,8,3},a15}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [d3, d1, d2, e1];
top_pins(2) ->
    [b3, b1, c3, c2];
top_pins(3) ->
    [c4, a4, d4, a2];
top_pins(4) ->
    [d5, b6, b4, a5];
top_pins(5) ->
    [c5, a6];
top_pins(6) ->
    [b8, c6, a7, b5];
top_pins(7) ->
    [b9, a9, a8, c7];
top_pins(8) ->
    [b10, c9, a10, c8];
top_pins(9) ->
    [d11, a11, c10];
top_pins(10) ->
    [d12, a12, c11, b11];
top_pins(11) ->
    [b13, a13, c12, b12];
top_pins(12) ->
    [b14, b16, c13, a15].

-spec left_iocs(y()) -> [{ioc(), pin()}].

left_iocs(1) ->
    [];
left_iocs(2) ->
    [];
left_iocs(3) ->
    [];
left_iocs(4) ->
    [{{ioc,0,4,0},k2},
     {{ioc,0,4,1},m2},
     {{ioc,0,4,2},l1},
     {{ioc,0,4,3},m3},
     {{ioc,0,4,4},l2},
     {{ioc,0,4,5},m4},
     {{ioc,0,4,6},m1}];
left_iocs(5) ->
    [{{ioc,0,5,0},h5},
     {{ioc,0,5,1},j5},
     {{ioc,0,5,2},k3},
     {{ioc,0,5,3},j2},
     {{ioc,0,5,4},l3},
     {{ioc,0,5,5},k1},
     {{ioc,0,5,6},l4}];
left_iocs(6) ->
    [{{ioc,0,6,0},g1},
     {{ioc,0,6,1},g3},
     {{ioc,0,6,2},h2},
     {{ioc,0,6,3},h3},
     {{ioc,0,6,4},h1},
     {{ioc,0,6,5},j3},
     {{ioc,0,6,6},j1}];
left_iocs(7) ->
    [{{ioc,0,7,0},e4},
     {{ioc,0,7,1},f2},
     {{ioc,0,7,2},e3},
     {{ioc,0,7,3},f1},
     {{ioc,0,7,4},e2},
     {{ioc,0,7,5},g2},
     {{ioc,0,7,6},f3}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [];
left_pins(2) ->
    [];
left_pins(3) ->
    [];
left_pins(4) ->
    [k2, m2, l1, m3, l2, m4, m1];
left_pins(5) ->
    [h5, j5, k3, j2, l3, k1, l4];
left_pins(6) ->
    [g1, g3, h2, h3, h1, j3, j1];
left_pins(7) ->
    [e4, f2, e3, f1, e2, g2, f3].

-spec right_iocs(y()) -> [{ioc(), pin()}].

right_iocs(1) ->
    [{{ioc,13,1,5},p14},
     {{ioc,13,1,4},n15},
     {{ioc,13,1,3},p15},
     {{ioc,13,1,2},n16},
     {{ioc,13,1,1},n13},
     {{ioc,13,1,0},m15}];
right_iocs(2) ->
    [{{ioc,13,2,5},n14},
     {{ioc,13,2,4},m16},
     {{ioc,13,2,3},m13},
     {{ioc,13,2,2},l15},
     {{ioc,13,2,1},m14},
     {{ioc,13,2,0},l16}];
right_iocs(3) ->
    [{{ioc,13,3,5},l13},
     {{ioc,13,3,4},k15},
     {{ioc,13,3,3},l14},
     {{ioc,13,3,2},k16},
     {{ioc,13,3,1},k14},
     {{ioc,13,3,0},j15}];
right_iocs(4) ->
    [{{ioc,13,4,5},j14},
     {{ioc,13,4,4},j12},
     {{ioc,13,4,3},h12},
     {{ioc,13,4,2},j16},
     {{ioc,13,4,1},h14},
     {{ioc,13,4,0},h16}];
right_iocs(5) ->
    [{{ioc,13,5,5},g14},
     {{ioc,13,5,4},h15},
     {{ioc,13,5,3},f14},
     {{ioc,13,5,2},g16},
     {{ioc,13,5,1},f13},
     {{ioc,13,5,0},g15}];
right_iocs(6) ->
    [{{ioc,13,6,5},e14},
     {{ioc,13,6,4},f16},
     {{ioc,13,6,3},e13},
     {{ioc,13,6,2},f15},
     {{ioc,13,6,1},d14},
     {{ioc,13,6,0},e16}];
right_iocs(7) ->
    [{{ioc,13,7,5},d13},
     {{ioc,13,7,4},e15},
     {{ioc,13,7,3},c15},
     {{ioc,13,7,2},d16},
     {{ioc,13,7,1},c14},
     {{ioc,13,7,0},d15}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [p14, n15, p15, n16, n13, m15];
right_pins(2) ->
    [n14, m16, m13, l15, m14, l16];
right_pins(3) ->
    [l13, k15, l14, k16, k14, j15];
right_pins(4) ->
    [j14, j12, h12, j16, h14, h16];
right_pins(5) ->
    [g14, h15, f14, g16, f13, g15];
right_pins(6) ->
    [e14, f16, e13, f15, d14, e16];
right_pins(7) ->
    [d13, e15, c15, d16, c14, d15].

-spec bottom_iocs(x()) -> [{ioc(), pin()}].

bottom_iocs(1) ->
    [{{ioc,1,3,3},n2},
     {{ioc,1,3,2},n1},
     {{ioc,1,3,1},n3},
     {{ioc,1,3,0},p2}];
bottom_iocs(2) ->
    [{{ioc,2,3,3},r3},
     {{ioc,2,3,2},r1},
     {{ioc,2,3,1},p4},
     {{ioc,2,3,0},t2}];
bottom_iocs(3) ->
    [{{ioc,3,3,3},r4},
     {{ioc,3,3,2},t4},
     {{ioc,3,3,1},n5},
     {{ioc,3,3,0},t5}];
bottom_iocs(4) ->
    [{{ioc,4,3,3},p5},
     {{ioc,4,3,2},r6},
     {{ioc,4,3,1},r5},
     {{ioc,4,3,0},t6}];
bottom_iocs(5) ->
    [{{ioc,5,3,3},p6},
     {{ioc,5,3,2},r7},
     {{ioc,5,3,1},p7},
     {{ioc,5,3,0},t7}];
bottom_iocs(6) ->
    [{{ioc,6,3,3},p8},
     {{ioc,6,3,2},r8},
     {{ioc,6,3,1},p9},
     {{ioc,6,3,0},t8}];
bottom_iocs(7) ->
    [{{ioc,7,3,3},t9},
     {{ioc,7,3,2},r9},
     {{ioc,7,3,1},p10},
     {{ioc,7,3,0},t10}];
bottom_iocs(8) ->
    [{{ioc,8,3,3},m8},
     {{ioc,8,3,2},m9},
     {{ioc,8,3,1},p11},
     {{ioc,8,3,0},r10}];
bottom_iocs(9) ->
    [];
bottom_iocs(10) ->
    [{{ioc,10,0,3},r12},
     {{ioc,10,0,2},t11},
     {{ioc,10,0,1},p12},
     {{ioc,10,0,0},r11}];
bottom_iocs(11) ->
    [{{ioc,11,0,3},n12},
     {{ioc,11,0,2},t12},
     {{ioc,11,0,1},r13},
     {{ioc,11,0,0},t13}];
bottom_iocs(12) ->
    [{{ioc,12,0,3},p13},
     {{ioc,12,0,2},t15},
     {{ioc,12,0,1},r14},
     {{ioc,12,0,0},r16}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [n2, n1, n3, p2];
bottom_pins(2) ->
    [r3, r1, p4, t2];
bottom_pins(3) ->
    [r4, t4, n5, t5];
bottom_pins(4) ->
    [p5, r6, r5, t6];
bottom_pins(5) ->
    [p6, r7, p7, t7];
bottom_pins(6) ->
    [p8, r8, p9, t8];
bottom_pins(7) ->
    [t9, r9, p10, t10];
bottom_pins(8) ->
    [m8, m9, p11, r10];
bottom_pins(9) ->
    [];
bottom_pins(10) ->
    [r12, t11, p12, r11];
bottom_pins(11) ->
    [n12, t12, r13, t13];
bottom_pins(12) ->
    [p13, t15, r14, r16].

