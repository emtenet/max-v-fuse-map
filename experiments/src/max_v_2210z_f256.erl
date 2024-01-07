-module(max_v_2210z_f256).

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
    [{{ioc,0,4,0},n2},
     {{ioc,0,4,1},m3},
     {{ioc,0,4,3},n3},
     {{ioc,0,4,5},p2},
     {{ioc,0,5,0},l4},
     {{ioc,0,5,2},l3},
     {{ioc,0,5,5},n1},
     {{ioc,0,5,6},m4},
     {{ioc,0,6,1},l2},
     {{ioc,0,6,2},k3},
     {{ioc,0,6,3},m1},
     {{ioc,0,6,4},l5},
     {{ioc,0,6,5},m2},
     {{ioc,0,7,1},j3},
     {{ioc,0,7,2},k2},
     {{ioc,0,7,3},k5},
     {{ioc,0,7,4},l1},
     {{ioc,0,7,5},k4},
     {{ioc,0,8,0},h5},
     {{ioc,0,8,1},j5},
     {{ioc,0,8,2},h4},
     {{ioc,0,8,3},j2},
     {{ioc,0,8,4},j4},
     {{ioc,0,8,5},k1},
     {{ioc,0,9,0},g1},
     {{ioc,0,9,1},g4},
     {{ioc,0,9,2},h2},
     {{ioc,0,9,3},g5},
     {{ioc,0,9,4},h1},
     {{ioc,0,9,5},h3},
     {{ioc,0,9,6},j1},
     {{ioc,0,10,0},f4},
     {{ioc,0,10,1},f2},
     {{ioc,0,10,2},f5},
     {{ioc,0,10,3},f1},
     {{ioc,0,10,4},f6},
     {{ioc,0,10,5},g2},
     {{ioc,0,10,6},g3},
     {{ioc,0,11,4},f3},
     {{ioc,0,11,5},e1},
     {{ioc,0,12,0},d2},
     {{ioc,0,12,1},e4},
     {{ioc,0,12,2},d1},
     {{ioc,0,12,3},e5},
     {{ioc,0,12,4},e2},
     {{ioc,0,13,1},d3},
     {{ioc,0,13,2},c2},
     {{ioc,0,13,5},c3},
     {{ioc,0,13,6},e3},
     {{ioc,1,14,0},a2},
     {{ioc,1,14,1},b1},
     {{ioc,1,14,2},d4},
     {{ioc,3,14,0},d5},
     {{ioc,4,14,0},c4},
     {{ioc,4,14,1},c5},
     {{ioc,4,14,2},b3},
     {{ioc,5,14,0},d6},
     {{ioc,5,14,1},b4},
     {{ioc,5,14,2},c6},
     {{ioc,6,14,0},b5},
     {{ioc,6,14,1},e6},
     {{ioc,6,14,2},a4},
     {{ioc,7,14,0},d7},
     {{ioc,7,14,1},a5},
     {{ioc,7,14,2},c7},
     {{ioc,8,14,0},a6},
     {{ioc,8,14,1},e7},
     {{ioc,8,14,2},b6},
     {{ioc,9,14,0},a7},
     {{ioc,9,14,1},d8},
     {{ioc,9,14,2},b7},
     {{ioc,9,14,3},c8},
     {{ioc,10,14,0},a8},
     {{ioc,10,14,1},b8},
     {{ioc,10,14,2},e8},
     {{ioc,11,14,0},d9},
     {{ioc,11,14,1},b9},
     {{ioc,11,14,2},e9},
     {{ioc,11,14,3},a9},
     {{ioc,12,14,0},b10},
     {{ioc,12,14,1},c9},
     {{ioc,12,14,2},a10},
     {{ioc,13,14,0},d10},
     {{ioc,13,14,1},a11},
     {{ioc,13,14,2},e10},
     {{ioc,14,14,0},a12},
     {{ioc,14,14,1},c10},
     {{ioc,14,14,2},b11},
     {{ioc,15,14,0},d11},
     {{ioc,15,14,1},b12},
     {{ioc,15,14,2},e11},
     {{ioc,16,14,0},b13},
     {{ioc,16,14,1},c11},
     {{ioc,16,14,2},a13},
     {{ioc,17,14,2},b14},
     {{ioc,17,14,3},d12},
     {{ioc,19,14,0},c12},
     {{ioc,20,14,0},b16},
     {{ioc,20,14,1},c13},
     {{ioc,20,14,2},a15},
     {{ioc,21,13,3},c14},
     {{ioc,21,13,2},e14},
     {{ioc,21,13,0},d13},
     {{ioc,21,12,3},e13},
     {{ioc,21,12,0},c15},
     {{ioc,21,11,5},d15},
     {{ioc,21,11,4},e12},
     {{ioc,21,11,1},d14},
     {{ioc,21,10,4},f12},
     {{ioc,21,10,3},e15},
     {{ioc,21,10,2},f13},
     {{ioc,21,10,1},d16},
     {{ioc,21,10,0},f14},
     {{ioc,21,9,4},f16},
     {{ioc,21,9,3},g14},
     {{ioc,21,9,2},f15},
     {{ioc,21,9,1},f11},
     {{ioc,21,9,0},e16},
     {{ioc,21,8,5},h15},
     {{ioc,21,8,4},h14},
     {{ioc,21,8,3},g16},
     {{ioc,21,8,2},g12},
     {{ioc,21,8,1},g15},
     {{ioc,21,8,0},g13},
     {{ioc,21,7,5},j16},
     {{ioc,21,7,4},j13},
     {{ioc,21,7,3},j12},
     {{ioc,21,7,2},h12},
     {{ioc,21,7,1},h16},
     {{ioc,21,7,0},h13},
     {{ioc,21,6,4},k13},
     {{ioc,21,6,3},k16},
     {{ioc,21,6,2},k12},
     {{ioc,21,6,1},j15},
     {{ioc,21,6,0},j14},
     {{ioc,21,5,5},l12},
     {{ioc,21,5,4},l15},
     {{ioc,21,5,3},l11},
     {{ioc,21,5,2},l16},
     {{ioc,21,5,1},k14},
     {{ioc,21,5,0},k15},
     {{ioc,21,4,3},l14},
     {{ioc,21,4,2},m15},
     {{ioc,21,4,1},l13},
     {{ioc,21,4,0},m16},
     {{ioc,21,3,5},n14},
     {{ioc,21,3,4},m14},
     {{ioc,21,3,3},n15},
     {{ioc,21,3,2},m13},
     {{ioc,21,3,1},n16},
     {{ioc,21,2,4},n13},
     {{ioc,21,1,3},p14},
     {{ioc,21,1,0},p15},
     {{ioc,20,0,1},r16},
     {{ioc,20,0,0},p13},
     {{ioc,17,0,3},n12},
     {{ioc,17,0,2},r14},
     {{ioc,17,0,1},p12},
     {{ioc,17,0,0},t15},
     {{ioc,16,0,2},t13},
     {{ioc,16,0,1},m12},
     {{ioc,16,0,0},r13},
     {{ioc,15,0,3},t12},
     {{ioc,15,0,2},n11},
     {{ioc,15,0,1},r12},
     {{ioc,15,0,0},p11},
     {{ioc,14,0,2},p10},
     {{ioc,14,0,1},r11},
     {{ioc,14,0,0},m11},
     {{ioc,12,3,3},m10},
     {{ioc,12,3,2},r10},
     {{ioc,12,3,1},n10},
     {{ioc,12,3,0},t11},
     {{ioc,11,3,2},t10},
     {{ioc,11,3,1},m8},
     {{ioc,11,3,0},m9},
     {{ioc,10,3,3},t8},
     {{ioc,10,3,2},t9},
     {{ioc,10,3,1},r9},
     {{ioc,10,3,0},p9},
     {{ioc,9,3,2},n8},
     {{ioc,9,3,1},r8},
     {{ioc,9,3,0},n9},
     {{ioc,8,3,3},m7},
     {{ioc,8,3,2},r7},
     {{ioc,8,3,1},p8},
     {{ioc,8,3,0},t7},
     {{ioc,7,3,2},r6},
     {{ioc,7,3,1},n7},
     {{ioc,6,3,3},r5},
     {{ioc,6,3,2},m6},
     {{ioc,6,3,1},t5},
     {{ioc,6,3,0},p7},
     {{ioc,5,3,2},p6},
     {{ioc,5,3,1},t4},
     {{ioc,5,3,0},n6},
     {{ioc,4,3,0},r4},
     {{ioc,2,3,3},t2},
     {{ioc,2,3,2},p5},
     {{ioc,2,3,1},r3},
     {{ioc,2,3,0},n5},
     {{ioc,1,3,1},r1},
     {{ioc,1,3,0},p4}
    ].

-spec iocs(iob()) -> [{ioc(), pin()}].

iocs({iob,0,4}) ->
    [{{ioc,0,4,0},n2},
     {{ioc,0,4,1},m3},
     {{ioc,0,4,3},n3},
     {{ioc,0,4,5},p2}];
iocs({iob,0,5}) ->
    [{{ioc,0,5,0},l4},
     {{ioc,0,5,2},l3},
     {{ioc,0,5,5},n1},
     {{ioc,0,5,6},m4}];
iocs({iob,0,6}) ->
    [{{ioc,0,6,1},l2},
     {{ioc,0,6,2},k3},
     {{ioc,0,6,3},m1},
     {{ioc,0,6,4},l5},
     {{ioc,0,6,5},m2}];
iocs({iob,0,7}) ->
    [{{ioc,0,7,1},j3},
     {{ioc,0,7,2},k2},
     {{ioc,0,7,3},k5},
     {{ioc,0,7,4},l1},
     {{ioc,0,7,5},k4}];
iocs({iob,0,8}) ->
    [{{ioc,0,8,0},h5},
     {{ioc,0,8,1},j5},
     {{ioc,0,8,2},h4},
     {{ioc,0,8,3},j2},
     {{ioc,0,8,4},j4},
     {{ioc,0,8,5},k1}];
iocs({iob,0,9}) ->
    [{{ioc,0,9,0},g1},
     {{ioc,0,9,1},g4},
     {{ioc,0,9,2},h2},
     {{ioc,0,9,3},g5},
     {{ioc,0,9,4},h1},
     {{ioc,0,9,5},h3},
     {{ioc,0,9,6},j1}];
iocs({iob,0,10}) ->
    [{{ioc,0,10,0},f4},
     {{ioc,0,10,1},f2},
     {{ioc,0,10,2},f5},
     {{ioc,0,10,3},f1},
     {{ioc,0,10,4},f6},
     {{ioc,0,10,5},g2},
     {{ioc,0,10,6},g3}];
iocs({iob,0,11}) ->
    [{{ioc,0,11,4},f3},
     {{ioc,0,11,5},e1}];
iocs({iob,0,12}) ->
    [{{ioc,0,12,0},d2},
     {{ioc,0,12,1},e4},
     {{ioc,0,12,2},d1},
     {{ioc,0,12,3},e5},
     {{ioc,0,12,4},e2}];
iocs({iob,0,13}) ->
    [{{ioc,0,13,1},d3},
     {{ioc,0,13,2},c2},
     {{ioc,0,13,5},c3},
     {{ioc,0,13,6},e3}];
iocs({iob,1,14}) ->
    [{{ioc,1,14,0},a2},
     {{ioc,1,14,1},b1},
     {{ioc,1,14,2},d4}];
iocs({iob,2,14}) ->
    [];
iocs({iob,3,14}) ->
    [{{ioc,3,14,0},d5}];
iocs({iob,4,14}) ->
    [{{ioc,4,14,0},c4},
     {{ioc,4,14,1},c5},
     {{ioc,4,14,2},b3}];
iocs({iob,5,14}) ->
    [{{ioc,5,14,0},d6},
     {{ioc,5,14,1},b4},
     {{ioc,5,14,2},c6}];
iocs({iob,6,14}) ->
    [{{ioc,6,14,0},b5},
     {{ioc,6,14,1},e6},
     {{ioc,6,14,2},a4}];
iocs({iob,7,14}) ->
    [{{ioc,7,14,0},d7},
     {{ioc,7,14,1},a5},
     {{ioc,7,14,2},c7}];
iocs({iob,8,14}) ->
    [{{ioc,8,14,0},a6},
     {{ioc,8,14,1},e7},
     {{ioc,8,14,2},b6}];
iocs({iob,9,14}) ->
    [{{ioc,9,14,0},a7},
     {{ioc,9,14,1},d8},
     {{ioc,9,14,2},b7},
     {{ioc,9,14,3},c8}];
iocs({iob,10,14}) ->
    [{{ioc,10,14,0},a8},
     {{ioc,10,14,1},b8},
     {{ioc,10,14,2},e8}];
iocs({iob,11,14}) ->
    [{{ioc,11,14,0},d9},
     {{ioc,11,14,1},b9},
     {{ioc,11,14,2},e9},
     {{ioc,11,14,3},a9}];
iocs({iob,12,14}) ->
    [{{ioc,12,14,0},b10},
     {{ioc,12,14,1},c9},
     {{ioc,12,14,2},a10}];
iocs({iob,13,14}) ->
    [{{ioc,13,14,0},d10},
     {{ioc,13,14,1},a11},
     {{ioc,13,14,2},e10}];
iocs({iob,14,14}) ->
    [{{ioc,14,14,0},a12},
     {{ioc,14,14,1},c10},
     {{ioc,14,14,2},b11}];
iocs({iob,15,14}) ->
    [{{ioc,15,14,0},d11},
     {{ioc,15,14,1},b12},
     {{ioc,15,14,2},e11}];
iocs({iob,16,14}) ->
    [{{ioc,16,14,0},b13},
     {{ioc,16,14,1},c11},
     {{ioc,16,14,2},a13}];
iocs({iob,17,14}) ->
    [{{ioc,17,14,2},b14},
     {{ioc,17,14,3},d12}];
iocs({iob,18,14}) ->
    [];
iocs({iob,19,14}) ->
    [{{ioc,19,14,0},c12}];
iocs({iob,20,14}) ->
    [{{ioc,20,14,0},b16},
     {{ioc,20,14,1},c13},
     {{ioc,20,14,2},a15}];
iocs({iob,21,13}) ->
    [{{ioc,21,13,3},c14},
     {{ioc,21,13,2},e14},
     {{ioc,21,13,0},d13}];
iocs({iob,21,12}) ->
    [{{ioc,21,12,3},e13},
     {{ioc,21,12,0},c15}];
iocs({iob,21,11}) ->
    [{{ioc,21,11,5},d15},
     {{ioc,21,11,4},e12},
     {{ioc,21,11,1},d14}];
iocs({iob,21,10}) ->
    [{{ioc,21,10,4},f12},
     {{ioc,21,10,3},e15},
     {{ioc,21,10,2},f13},
     {{ioc,21,10,1},d16},
     {{ioc,21,10,0},f14}];
iocs({iob,21,9}) ->
    [{{ioc,21,9,4},f16},
     {{ioc,21,9,3},g14},
     {{ioc,21,9,2},f15},
     {{ioc,21,9,1},f11},
     {{ioc,21,9,0},e16}];
iocs({iob,21,8}) ->
    [{{ioc,21,8,5},h15},
     {{ioc,21,8,4},h14},
     {{ioc,21,8,3},g16},
     {{ioc,21,8,2},g12},
     {{ioc,21,8,1},g15},
     {{ioc,21,8,0},g13}];
iocs({iob,21,7}) ->
    [{{ioc,21,7,5},j16},
     {{ioc,21,7,4},j13},
     {{ioc,21,7,3},j12},
     {{ioc,21,7,2},h12},
     {{ioc,21,7,1},h16},
     {{ioc,21,7,0},h13}];
iocs({iob,21,6}) ->
    [{{ioc,21,6,4},k13},
     {{ioc,21,6,3},k16},
     {{ioc,21,6,2},k12},
     {{ioc,21,6,1},j15},
     {{ioc,21,6,0},j14}];
iocs({iob,21,5}) ->
    [{{ioc,21,5,5},l12},
     {{ioc,21,5,4},l15},
     {{ioc,21,5,3},l11},
     {{ioc,21,5,2},l16},
     {{ioc,21,5,1},k14},
     {{ioc,21,5,0},k15}];
iocs({iob,21,4}) ->
    [{{ioc,21,4,3},l14},
     {{ioc,21,4,2},m15},
     {{ioc,21,4,1},l13},
     {{ioc,21,4,0},m16}];
iocs({iob,21,3}) ->
    [{{ioc,21,3,5},n14},
     {{ioc,21,3,4},m14},
     {{ioc,21,3,3},n15},
     {{ioc,21,3,2},m13},
     {{ioc,21,3,1},n16}];
iocs({iob,21,2}) ->
    [{{ioc,21,2,4},n13}];
iocs({iob,21,1}) ->
    [{{ioc,21,1,3},p14},
     {{ioc,21,1,0},p15}];
iocs({iob,20,0}) ->
    [{{ioc,20,0,1},r16},
     {{ioc,20,0,0},p13}];
iocs({iob,19,0}) ->
    [];
iocs({iob,18,0}) ->
    [];
iocs({iob,17,0}) ->
    [{{ioc,17,0,3},n12},
     {{ioc,17,0,2},r14},
     {{ioc,17,0,1},p12},
     {{ioc,17,0,0},t15}];
iocs({iob,16,0}) ->
    [{{ioc,16,0,2},t13},
     {{ioc,16,0,1},m12},
     {{ioc,16,0,0},r13}];
iocs({iob,15,0}) ->
    [{{ioc,15,0,3},t12},
     {{ioc,15,0,2},n11},
     {{ioc,15,0,1},r12},
     {{ioc,15,0,0},p11}];
iocs({iob,14,0}) ->
    [{{ioc,14,0,2},p10},
     {{ioc,14,0,1},r11},
     {{ioc,14,0,0},m11}];
iocs({iob,12,3}) ->
    [{{ioc,12,3,3},m10},
     {{ioc,12,3,2},r10},
     {{ioc,12,3,1},n10},
     {{ioc,12,3,0},t11}];
iocs({iob,11,3}) ->
    [{{ioc,11,3,2},t10},
     {{ioc,11,3,1},m8},
     {{ioc,11,3,0},m9}];
iocs({iob,10,3}) ->
    [{{ioc,10,3,3},t8},
     {{ioc,10,3,2},t9},
     {{ioc,10,3,1},r9},
     {{ioc,10,3,0},p9}];
iocs({iob,9,3}) ->
    [{{ioc,9,3,2},n8},
     {{ioc,9,3,1},r8},
     {{ioc,9,3,0},n9}];
iocs({iob,8,3}) ->
    [{{ioc,8,3,3},m7},
     {{ioc,8,3,2},r7},
     {{ioc,8,3,1},p8},
     {{ioc,8,3,0},t7}];
iocs({iob,7,3}) ->
    [{{ioc,7,3,2},r6},
     {{ioc,7,3,1},n7}];
iocs({iob,6,3}) ->
    [{{ioc,6,3,3},r5},
     {{ioc,6,3,2},m6},
     {{ioc,6,3,1},t5},
     {{ioc,6,3,0},p7}];
iocs({iob,5,3}) ->
    [{{ioc,5,3,2},p6},
     {{ioc,5,3,1},t4},
     {{ioc,5,3,0},n6}];
iocs({iob,4,3}) ->
    [{{ioc,4,3,0},r4}];
iocs({iob,3,3}) ->
    [];
iocs({iob,2,3}) ->
    [{{ioc,2,3,3},t2},
     {{ioc,2,3,2},p5},
     {{ioc,2,3,1},r3},
     {{ioc,2,3,0},n5}];
iocs({iob,1,3}) ->
    [{{ioc,1,3,1},r1},
     {{ioc,1,3,0},p4}].

-spec pins() -> [pin()].

pins() ->
    [n2,
     m3,
     n3,
     p2,
     l4,
     l3,
     n1,
     m4,
     l2,
     k3,
     m1,
     l5,
     m2,
     j3,
     k2,
     k5,
     l1,
     k4,
     h5,
     j5,
     h4,
     j2,
     j4,
     k1,
     g1,
     g4,
     h2,
     g5,
     h1,
     h3,
     j1,
     f4,
     f2,
     f5,
     f1,
     f6,
     g2,
     g3,
     f3,
     e1,
     d2,
     e4,
     d1,
     e5,
     e2,
     d3,
     c2,
     c3,
     e3,
     a2,
     b1,
     d4,
     d5,
     c4,
     c5,
     b3,
     d6,
     b4,
     c6,
     b5,
     e6,
     a4,
     d7,
     a5,
     c7,
     a6,
     e7,
     b6,
     a7,
     d8,
     b7,
     c8,
     a8,
     b8,
     e8,
     d9,
     b9,
     e9,
     a9,
     b10,
     c9,
     a10,
     d10,
     a11,
     e10,
     a12,
     c10,
     b11,
     d11,
     b12,
     e11,
     b13,
     c11,
     a13,
     b14,
     d12,
     c12,
     b16,
     c13,
     a15,
     c14,
     e14,
     d13,
     e13,
     c15,
     d15,
     e12,
     d14,
     f12,
     e15,
     f13,
     d16,
     f14,
     f16,
     g14,
     f15,
     f11,
     e16,
     h15,
     h14,
     g16,
     g12,
     g15,
     g13,
     j16,
     j13,
     j12,
     h12,
     h16,
     h13,
     k13,
     k16,
     k12,
     j15,
     j14,
     l12,
     l15,
     l11,
     l16,
     k14,
     k15,
     l14,
     m15,
     l13,
     m16,
     n14,
     m14,
     n15,
     m13,
     n16,
     n13,
     p14,
     p15,
     r16,
     p13,
     n12,
     r14,
     p12,
     t15,
     t13,
     m12,
     r13,
     t12,
     n11,
     r12,
     p11,
     p10,
     r11,
     m11,
     m10,
     r10,
     n10,
     t11,
     t10,
     m8,
     m9,
     t8,
     t9,
     r9,
     p9,
     n8,
     r8,
     n9,
     m7,
     r7,
     p8,
     t7,
     r6,
     n7,
     r5,
     m6,
     t5,
     p7,
     p6,
     t4,
     n6,
     r4,
     t2,
     p5,
     r3,
     n5,
     r1,
     p4
    ].

-spec top_iocs(x()) -> [{ioc(), pin()}].

top_iocs(1) ->
    [{{ioc,1,14,0},a2},
     {{ioc,1,14,1},b1},
     {{ioc,1,14,2},d4}];
top_iocs(2) ->
    [];
top_iocs(3) ->
    [{{ioc,3,14,0},d5}];
top_iocs(4) ->
    [{{ioc,4,14,0},c4},
     {{ioc,4,14,1},c5},
     {{ioc,4,14,2},b3}];
top_iocs(5) ->
    [{{ioc,5,14,0},d6},
     {{ioc,5,14,1},b4},
     {{ioc,5,14,2},c6}];
top_iocs(6) ->
    [{{ioc,6,14,0},b5},
     {{ioc,6,14,1},e6},
     {{ioc,6,14,2},a4}];
top_iocs(7) ->
    [{{ioc,7,14,0},d7},
     {{ioc,7,14,1},a5},
     {{ioc,7,14,2},c7}];
top_iocs(8) ->
    [{{ioc,8,14,0},a6},
     {{ioc,8,14,1},e7},
     {{ioc,8,14,2},b6}];
top_iocs(9) ->
    [{{ioc,9,14,0},a7},
     {{ioc,9,14,1},d8},
     {{ioc,9,14,2},b7},
     {{ioc,9,14,3},c8}];
top_iocs(10) ->
    [{{ioc,10,14,0},a8},
     {{ioc,10,14,1},b8},
     {{ioc,10,14,2},e8}];
top_iocs(11) ->
    [{{ioc,11,14,0},d9},
     {{ioc,11,14,1},b9},
     {{ioc,11,14,2},e9},
     {{ioc,11,14,3},a9}];
top_iocs(12) ->
    [{{ioc,12,14,0},b10},
     {{ioc,12,14,1},c9},
     {{ioc,12,14,2},a10}];
top_iocs(13) ->
    [{{ioc,13,14,0},d10},
     {{ioc,13,14,1},a11},
     {{ioc,13,14,2},e10}];
top_iocs(14) ->
    [{{ioc,14,14,0},a12},
     {{ioc,14,14,1},c10},
     {{ioc,14,14,2},b11}];
top_iocs(15) ->
    [{{ioc,15,14,0},d11},
     {{ioc,15,14,1},b12},
     {{ioc,15,14,2},e11}];
top_iocs(16) ->
    [{{ioc,16,14,0},b13},
     {{ioc,16,14,1},c11},
     {{ioc,16,14,2},a13}];
top_iocs(17) ->
    [{{ioc,17,14,2},b14},
     {{ioc,17,14,3},d12}];
top_iocs(18) ->
    [];
top_iocs(19) ->
    [{{ioc,19,14,0},c12}];
top_iocs(20) ->
    [{{ioc,20,14,0},b16},
     {{ioc,20,14,1},c13},
     {{ioc,20,14,2},a15}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [a2, b1, d4];
top_pins(2) ->
    [];
top_pins(3) ->
    [d5];
top_pins(4) ->
    [c4, c5, b3];
top_pins(5) ->
    [d6, b4, c6];
top_pins(6) ->
    [b5, e6, a4];
top_pins(7) ->
    [d7, a5, c7];
top_pins(8) ->
    [a6, e7, b6];
top_pins(9) ->
    [a7, d8, b7, c8];
top_pins(10) ->
    [a8, b8, e8];
top_pins(11) ->
    [d9, b9, e9, a9];
top_pins(12) ->
    [b10, c9, a10];
top_pins(13) ->
    [d10, a11, e10];
top_pins(14) ->
    [a12, c10, b11];
top_pins(15) ->
    [d11, b12, e11];
top_pins(16) ->
    [b13, c11, a13];
top_pins(17) ->
    [b14, d12];
top_pins(18) ->
    [];
top_pins(19) ->
    [c12];
top_pins(20) ->
    [b16, c13, a15].

-spec left_iocs(y()) -> [{ioc(), pin()}].

left_iocs(1) ->
    [];
left_iocs(2) ->
    [];
left_iocs(3) ->
    [];
left_iocs(4) ->
    [{{ioc,0,4,0},n2},
     {{ioc,0,4,1},m3},
     {{ioc,0,4,3},n3},
     {{ioc,0,4,5},p2}];
left_iocs(5) ->
    [{{ioc,0,5,0},l4},
     {{ioc,0,5,2},l3},
     {{ioc,0,5,5},n1},
     {{ioc,0,5,6},m4}];
left_iocs(6) ->
    [{{ioc,0,6,1},l2},
     {{ioc,0,6,2},k3},
     {{ioc,0,6,3},m1},
     {{ioc,0,6,4},l5},
     {{ioc,0,6,5},m2}];
left_iocs(7) ->
    [{{ioc,0,7,1},j3},
     {{ioc,0,7,2},k2},
     {{ioc,0,7,3},k5},
     {{ioc,0,7,4},l1},
     {{ioc,0,7,5},k4}];
left_iocs(8) ->
    [{{ioc,0,8,0},h5},
     {{ioc,0,8,1},j5},
     {{ioc,0,8,2},h4},
     {{ioc,0,8,3},j2},
     {{ioc,0,8,4},j4},
     {{ioc,0,8,5},k1}];
left_iocs(9) ->
    [{{ioc,0,9,0},g1},
     {{ioc,0,9,1},g4},
     {{ioc,0,9,2},h2},
     {{ioc,0,9,3},g5},
     {{ioc,0,9,4},h1},
     {{ioc,0,9,5},h3},
     {{ioc,0,9,6},j1}];
left_iocs(10) ->
    [{{ioc,0,10,0},f4},
     {{ioc,0,10,1},f2},
     {{ioc,0,10,2},f5},
     {{ioc,0,10,3},f1},
     {{ioc,0,10,4},f6},
     {{ioc,0,10,5},g2},
     {{ioc,0,10,6},g3}];
left_iocs(11) ->
    [{{ioc,0,11,4},f3},
     {{ioc,0,11,5},e1}];
left_iocs(12) ->
    [{{ioc,0,12,0},d2},
     {{ioc,0,12,1},e4},
     {{ioc,0,12,2},d1},
     {{ioc,0,12,3},e5},
     {{ioc,0,12,4},e2}];
left_iocs(13) ->
    [{{ioc,0,13,1},d3},
     {{ioc,0,13,2},c2},
     {{ioc,0,13,5},c3},
     {{ioc,0,13,6},e3}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [];
left_pins(2) ->
    [];
left_pins(3) ->
    [];
left_pins(4) ->
    [n2, m3, n3, p2];
left_pins(5) ->
    [l4, l3, n1, m4];
left_pins(6) ->
    [l2, k3, m1, l5, m2];
left_pins(7) ->
    [j3, k2, k5, l1, k4];
left_pins(8) ->
    [h5, j5, h4, j2, j4, k1];
left_pins(9) ->
    [g1, g4, h2, g5, h1, h3, j1];
left_pins(10) ->
    [f4, f2, f5, f1, f6, g2, g3];
left_pins(11) ->
    [f3, e1];
left_pins(12) ->
    [d2, e4, d1, e5, e2];
left_pins(13) ->
    [d3, c2, c3, e3].

-spec right_iocs(y()) -> [{ioc(), pin()}].

right_iocs(1) ->
    [{{ioc,21,1,3},p14},
     {{ioc,21,1,0},p15}];
right_iocs(2) ->
    [{{ioc,21,2,4},n13}];
right_iocs(3) ->
    [{{ioc,21,3,5},n14},
     {{ioc,21,3,4},m14},
     {{ioc,21,3,3},n15},
     {{ioc,21,3,2},m13},
     {{ioc,21,3,1},n16}];
right_iocs(4) ->
    [{{ioc,21,4,3},l14},
     {{ioc,21,4,2},m15},
     {{ioc,21,4,1},l13},
     {{ioc,21,4,0},m16}];
right_iocs(5) ->
    [{{ioc,21,5,5},l12},
     {{ioc,21,5,4},l15},
     {{ioc,21,5,3},l11},
     {{ioc,21,5,2},l16},
     {{ioc,21,5,1},k14},
     {{ioc,21,5,0},k15}];
right_iocs(6) ->
    [{{ioc,21,6,4},k13},
     {{ioc,21,6,3},k16},
     {{ioc,21,6,2},k12},
     {{ioc,21,6,1},j15},
     {{ioc,21,6,0},j14}];
right_iocs(7) ->
    [{{ioc,21,7,5},j16},
     {{ioc,21,7,4},j13},
     {{ioc,21,7,3},j12},
     {{ioc,21,7,2},h12},
     {{ioc,21,7,1},h16},
     {{ioc,21,7,0},h13}];
right_iocs(8) ->
    [{{ioc,21,8,5},h15},
     {{ioc,21,8,4},h14},
     {{ioc,21,8,3},g16},
     {{ioc,21,8,2},g12},
     {{ioc,21,8,1},g15},
     {{ioc,21,8,0},g13}];
right_iocs(9) ->
    [{{ioc,21,9,4},f16},
     {{ioc,21,9,3},g14},
     {{ioc,21,9,2},f15},
     {{ioc,21,9,1},f11},
     {{ioc,21,9,0},e16}];
right_iocs(10) ->
    [{{ioc,21,10,4},f12},
     {{ioc,21,10,3},e15},
     {{ioc,21,10,2},f13},
     {{ioc,21,10,1},d16},
     {{ioc,21,10,0},f14}];
right_iocs(11) ->
    [{{ioc,21,11,5},d15},
     {{ioc,21,11,4},e12},
     {{ioc,21,11,1},d14}];
right_iocs(12) ->
    [{{ioc,21,12,3},e13},
     {{ioc,21,12,0},c15}];
right_iocs(13) ->
    [{{ioc,21,13,3},c14},
     {{ioc,21,13,2},e14},
     {{ioc,21,13,0},d13}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [p14, p15];
right_pins(2) ->
    [n13];
right_pins(3) ->
    [n14, m14, n15, m13, n16];
right_pins(4) ->
    [l14, m15, l13, m16];
right_pins(5) ->
    [l12, l15, l11, l16, k14, k15];
right_pins(6) ->
    [k13, k16, k12, j15, j14];
right_pins(7) ->
    [j16, j13, j12, h12, h16, h13];
right_pins(8) ->
    [h15, h14, g16, g12, g15, g13];
right_pins(9) ->
    [f16, g14, f15, f11, e16];
right_pins(10) ->
    [f12, e15, f13, d16, f14];
right_pins(11) ->
    [d15, e12, d14];
right_pins(12) ->
    [e13, c15];
right_pins(13) ->
    [c14, e14, d13].

-spec bottom_iocs(x()) -> [{ioc(), pin()}].

bottom_iocs(1) ->
    [{{ioc,1,3,1},r1},
     {{ioc,1,3,0},p4}];
bottom_iocs(2) ->
    [{{ioc,2,3,3},t2},
     {{ioc,2,3,2},p5},
     {{ioc,2,3,1},r3},
     {{ioc,2,3,0},n5}];
bottom_iocs(3) ->
    [];
bottom_iocs(4) ->
    [{{ioc,4,3,0},r4}];
bottom_iocs(5) ->
    [{{ioc,5,3,2},p6},
     {{ioc,5,3,1},t4},
     {{ioc,5,3,0},n6}];
bottom_iocs(6) ->
    [{{ioc,6,3,3},r5},
     {{ioc,6,3,2},m6},
     {{ioc,6,3,1},t5},
     {{ioc,6,3,0},p7}];
bottom_iocs(7) ->
    [{{ioc,7,3,2},r6},
     {{ioc,7,3,1},n7}];
bottom_iocs(8) ->
    [{{ioc,8,3,3},m7},
     {{ioc,8,3,2},r7},
     {{ioc,8,3,1},p8},
     {{ioc,8,3,0},t7}];
bottom_iocs(9) ->
    [{{ioc,9,3,2},n8},
     {{ioc,9,3,1},r8},
     {{ioc,9,3,0},n9}];
bottom_iocs(10) ->
    [{{ioc,10,3,3},t8},
     {{ioc,10,3,2},t9},
     {{ioc,10,3,1},r9},
     {{ioc,10,3,0},p9}];
bottom_iocs(11) ->
    [{{ioc,11,3,2},t10},
     {{ioc,11,3,1},m8},
     {{ioc,11,3,0},m9}];
bottom_iocs(12) ->
    [{{ioc,12,3,3},m10},
     {{ioc,12,3,2},r10},
     {{ioc,12,3,1},n10},
     {{ioc,12,3,0},t11}];
bottom_iocs(13) ->
    [];
bottom_iocs(14) ->
    [{{ioc,14,0,2},p10},
     {{ioc,14,0,1},r11},
     {{ioc,14,0,0},m11}];
bottom_iocs(15) ->
    [{{ioc,15,0,3},t12},
     {{ioc,15,0,2},n11},
     {{ioc,15,0,1},r12},
     {{ioc,15,0,0},p11}];
bottom_iocs(16) ->
    [{{ioc,16,0,2},t13},
     {{ioc,16,0,1},m12},
     {{ioc,16,0,0},r13}];
bottom_iocs(17) ->
    [{{ioc,17,0,3},n12},
     {{ioc,17,0,2},r14},
     {{ioc,17,0,1},p12},
     {{ioc,17,0,0},t15}];
bottom_iocs(18) ->
    [];
bottom_iocs(19) ->
    [];
bottom_iocs(20) ->
    [{{ioc,20,0,1},r16},
     {{ioc,20,0,0},p13}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [r1, p4];
bottom_pins(2) ->
    [t2, p5, r3, n5];
bottom_pins(3) ->
    [];
bottom_pins(4) ->
    [r4];
bottom_pins(5) ->
    [p6, t4, n6];
bottom_pins(6) ->
    [r5, m6, t5, p7];
bottom_pins(7) ->
    [r6, n7];
bottom_pins(8) ->
    [m7, r7, p8, t7];
bottom_pins(9) ->
    [n8, r8, n9];
bottom_pins(10) ->
    [t8, t9, r9, p9];
bottom_pins(11) ->
    [t10, m8, m9];
bottom_pins(12) ->
    [m10, r10, n10, t11];
bottom_pins(13) ->
    [];
bottom_pins(14) ->
    [p10, r11, m11];
bottom_pins(15) ->
    [t12, n11, r12, p11];
bottom_pins(16) ->
    [t13, m12, r13];
bottom_pins(17) ->
    [n12, r14, p12, t15];
bottom_pins(18) ->
    [];
bottom_pins(19) ->
    [];
bottom_pins(20) ->
    [r16, p13].

