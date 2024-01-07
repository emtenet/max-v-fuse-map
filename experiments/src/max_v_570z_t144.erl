-module(max_v_570z_t144).

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
    [{{ioc,0,4,0},pin27},
     {{ioc,0,4,2},pin28},
     {{ioc,0,4,3},pin29},
     {{ioc,0,4,5},pin30},
     {{ioc,0,5,0},pin18},
     {{ioc,0,5,1},pin20},
     {{ioc,0,5,2},pin21},
     {{ioc,0,5,3},pin22},
     {{ioc,0,5,4},pin23},
     {{ioc,0,5,5},pin24},
     {{ioc,0,6,1},pin11},
     {{ioc,0,6,2},pin12},
     {{ioc,0,6,3},pin13},
     {{ioc,0,6,4},pin14},
     {{ioc,0,6,5},pin15},
     {{ioc,0,6,6},pin16},
     {{ioc,0,7,0},pin4},
     {{ioc,0,7,2},pin5},
     {{ioc,0,7,3},pin6},
     {{ioc,0,7,4},pin7},
     {{ioc,1,8,0},pin2},
     {{ioc,1,8,2},pin3},
     {{ioc,2,8,1},pin144},
     {{ioc,2,8,3},pin1},
     {{ioc,3,8,0},pin142},
     {{ioc,3,8,3},pin143},
     {{ioc,4,8,1},pin140},
     {{ioc,4,8,2},pin141},
     {{ioc,5,8,1},pin138},
     {{ioc,5,8,2},pin139},
     {{ioc,6,8,0},pin131},
     {{ioc,6,8,1},pin132},
     {{ioc,6,8,2},pin133},
     {{ioc,6,8,3},pin134},
     {{ioc,7,8,0},pin125},
     {{ioc,7,8,1},pin127},
     {{ioc,7,8,2},pin129},
     {{ioc,7,8,3},pin130},
     {{ioc,8,8,0},pin121},
     {{ioc,8,8,1},pin122},
     {{ioc,8,8,2},pin123},
     {{ioc,8,8,3},pin124},
     {{ioc,9,8,0},pin118},
     {{ioc,9,8,1},pin119},
     {{ioc,9,8,2},pin120},
     {{ioc,10,8,1},pin114},
     {{ioc,10,8,3},pin117},
     {{ioc,11,8,1},pin112},
     {{ioc,11,8,3},pin113},
     {{ioc,12,8,1},pin109},
     {{ioc,12,8,2},pin110},
     {{ioc,12,8,3},pin111},
     {{ioc,13,7,5},pin106},
     {{ioc,13,7,3},pin107},
     {{ioc,13,7,1},pin108},
     {{ioc,13,6,5},pin103},
     {{ioc,13,6,2},pin104},
     {{ioc,13,6,1},pin105},
     {{ioc,13,5,5},pin96},
     {{ioc,13,5,4},pin97},
     {{ioc,13,5,3},pin98},
     {{ioc,13,5,2},pin101},
     {{ioc,13,5,0},pin102},
     {{ioc,13,4,5},pin88},
     {{ioc,13,4,4},pin89},
     {{ioc,13,4,3},pin91},
     {{ioc,13,4,2},pin93},
     {{ioc,13,4,1},pin94},
     {{ioc,13,4,0},pin95},
     {{ioc,13,3,5},pin81},
     {{ioc,13,3,3},pin84},
     {{ioc,13,3,2},pin85},
     {{ioc,13,3,1},pin86},
     {{ioc,13,3,0},pin87},
     {{ioc,13,2,4},pin77},
     {{ioc,13,2,2},pin78},
     {{ioc,13,2,1},pin79},
     {{ioc,13,2,0},pin80},
     {{ioc,13,1,5},pin73},
     {{ioc,13,1,4},pin74},
     {{ioc,13,1,3},pin75},
     {{ioc,13,1,1},pin76},
     {{ioc,12,0,2},pin71},
     {{ioc,12,0,0},pin72},
     {{ioc,11,0,2},pin69},
     {{ioc,11,0,1},pin70},
     {{ioc,10,0,2},pin66},
     {{ioc,10,0,1},pin67},
     {{ioc,10,0,0},pin68},
     {{ioc,8,3,3},pin60},
     {{ioc,8,3,2},pin61},
     {{ioc,8,3,1},pin62},
     {{ioc,8,3,0},pin63},
     {{ioc,7,3,3},pin55},
     {{ioc,7,3,2},pin57},
     {{ioc,7,3,1},pin58},
     {{ioc,7,3,0},pin59},
     {{ioc,6,3,3},pin50},
     {{ioc,6,3,2},pin51},
     {{ioc,6,3,1},pin52},
     {{ioc,6,3,0},pin53},
     {{ioc,5,3,2},pin45},
     {{ioc,5,3,1},pin48},
     {{ioc,5,3,0},pin49},
     {{ioc,4,3,3},pin42},
     {{ioc,4,3,2},pin43},
     {{ioc,4,3,1},pin44},
     {{ioc,3,3,2},pin40},
     {{ioc,3,3,1},pin41},
     {{ioc,2,3,3},pin37},
     {{ioc,2,3,2},pin38},
     {{ioc,2,3,0},pin39},
     {{ioc,1,3,3},pin31},
     {{ioc,1,3,1},pin32}
    ].

-spec iocs(iob()) -> [{ioc(), pin()}].

iocs({iob,0,4}) ->
    [{{ioc,0,4,0},pin27},
     {{ioc,0,4,2},pin28},
     {{ioc,0,4,3},pin29},
     {{ioc,0,4,5},pin30}];
iocs({iob,0,5}) ->
    [{{ioc,0,5,0},pin18},
     {{ioc,0,5,1},pin20},
     {{ioc,0,5,2},pin21},
     {{ioc,0,5,3},pin22},
     {{ioc,0,5,4},pin23},
     {{ioc,0,5,5},pin24}];
iocs({iob,0,6}) ->
    [{{ioc,0,6,1},pin11},
     {{ioc,0,6,2},pin12},
     {{ioc,0,6,3},pin13},
     {{ioc,0,6,4},pin14},
     {{ioc,0,6,5},pin15},
     {{ioc,0,6,6},pin16}];
iocs({iob,0,7}) ->
    [{{ioc,0,7,0},pin4},
     {{ioc,0,7,2},pin5},
     {{ioc,0,7,3},pin6},
     {{ioc,0,7,4},pin7}];
iocs({iob,1,8}) ->
    [{{ioc,1,8,0},pin2},
     {{ioc,1,8,2},pin3}];
iocs({iob,2,8}) ->
    [{{ioc,2,8,1},pin144},
     {{ioc,2,8,3},pin1}];
iocs({iob,3,8}) ->
    [{{ioc,3,8,0},pin142},
     {{ioc,3,8,3},pin143}];
iocs({iob,4,8}) ->
    [{{ioc,4,8,1},pin140},
     {{ioc,4,8,2},pin141}];
iocs({iob,5,8}) ->
    [{{ioc,5,8,1},pin138},
     {{ioc,5,8,2},pin139}];
iocs({iob,6,8}) ->
    [{{ioc,6,8,0},pin131},
     {{ioc,6,8,1},pin132},
     {{ioc,6,8,2},pin133},
     {{ioc,6,8,3},pin134}];
iocs({iob,7,8}) ->
    [{{ioc,7,8,0},pin125},
     {{ioc,7,8,1},pin127},
     {{ioc,7,8,2},pin129},
     {{ioc,7,8,3},pin130}];
iocs({iob,8,8}) ->
    [{{ioc,8,8,0},pin121},
     {{ioc,8,8,1},pin122},
     {{ioc,8,8,2},pin123},
     {{ioc,8,8,3},pin124}];
iocs({iob,9,8}) ->
    [{{ioc,9,8,0},pin118},
     {{ioc,9,8,1},pin119},
     {{ioc,9,8,2},pin120}];
iocs({iob,10,8}) ->
    [{{ioc,10,8,1},pin114},
     {{ioc,10,8,3},pin117}];
iocs({iob,11,8}) ->
    [{{ioc,11,8,1},pin112},
     {{ioc,11,8,3},pin113}];
iocs({iob,12,8}) ->
    [{{ioc,12,8,1},pin109},
     {{ioc,12,8,2},pin110},
     {{ioc,12,8,3},pin111}];
iocs({iob,13,7}) ->
    [{{ioc,13,7,5},pin106},
     {{ioc,13,7,3},pin107},
     {{ioc,13,7,1},pin108}];
iocs({iob,13,6}) ->
    [{{ioc,13,6,5},pin103},
     {{ioc,13,6,2},pin104},
     {{ioc,13,6,1},pin105}];
iocs({iob,13,5}) ->
    [{{ioc,13,5,5},pin96},
     {{ioc,13,5,4},pin97},
     {{ioc,13,5,3},pin98},
     {{ioc,13,5,2},pin101},
     {{ioc,13,5,0},pin102}];
iocs({iob,13,4}) ->
    [{{ioc,13,4,5},pin88},
     {{ioc,13,4,4},pin89},
     {{ioc,13,4,3},pin91},
     {{ioc,13,4,2},pin93},
     {{ioc,13,4,1},pin94},
     {{ioc,13,4,0},pin95}];
iocs({iob,13,3}) ->
    [{{ioc,13,3,5},pin81},
     {{ioc,13,3,3},pin84},
     {{ioc,13,3,2},pin85},
     {{ioc,13,3,1},pin86},
     {{ioc,13,3,0},pin87}];
iocs({iob,13,2}) ->
    [{{ioc,13,2,4},pin77},
     {{ioc,13,2,2},pin78},
     {{ioc,13,2,1},pin79},
     {{ioc,13,2,0},pin80}];
iocs({iob,13,1}) ->
    [{{ioc,13,1,5},pin73},
     {{ioc,13,1,4},pin74},
     {{ioc,13,1,3},pin75},
     {{ioc,13,1,1},pin76}];
iocs({iob,12,0}) ->
    [{{ioc,12,0,2},pin71},
     {{ioc,12,0,0},pin72}];
iocs({iob,11,0}) ->
    [{{ioc,11,0,2},pin69},
     {{ioc,11,0,1},pin70}];
iocs({iob,10,0}) ->
    [{{ioc,10,0,2},pin66},
     {{ioc,10,0,1},pin67},
     {{ioc,10,0,0},pin68}];
iocs({iob,8,3}) ->
    [{{ioc,8,3,3},pin60},
     {{ioc,8,3,2},pin61},
     {{ioc,8,3,1},pin62},
     {{ioc,8,3,0},pin63}];
iocs({iob,7,3}) ->
    [{{ioc,7,3,3},pin55},
     {{ioc,7,3,2},pin57},
     {{ioc,7,3,1},pin58},
     {{ioc,7,3,0},pin59}];
iocs({iob,6,3}) ->
    [{{ioc,6,3,3},pin50},
     {{ioc,6,3,2},pin51},
     {{ioc,6,3,1},pin52},
     {{ioc,6,3,0},pin53}];
iocs({iob,5,3}) ->
    [{{ioc,5,3,2},pin45},
     {{ioc,5,3,1},pin48},
     {{ioc,5,3,0},pin49}];
iocs({iob,4,3}) ->
    [{{ioc,4,3,3},pin42},
     {{ioc,4,3,2},pin43},
     {{ioc,4,3,1},pin44}];
iocs({iob,3,3}) ->
    [{{ioc,3,3,2},pin40},
     {{ioc,3,3,1},pin41}];
iocs({iob,2,3}) ->
    [{{ioc,2,3,3},pin37},
     {{ioc,2,3,2},pin38},
     {{ioc,2,3,0},pin39}];
iocs({iob,1,3}) ->
    [{{ioc,1,3,3},pin31},
     {{ioc,1,3,1},pin32}].

-spec pins() -> [pin()].

pins() ->
    [pin27,
     pin28,
     pin29,
     pin30,
     pin18,
     pin20,
     pin21,
     pin22,
     pin23,
     pin24,
     pin11,
     pin12,
     pin13,
     pin14,
     pin15,
     pin16,
     pin4,
     pin5,
     pin6,
     pin7,
     pin2,
     pin3,
     pin144,
     pin1,
     pin142,
     pin143,
     pin140,
     pin141,
     pin138,
     pin139,
     pin131,
     pin132,
     pin133,
     pin134,
     pin125,
     pin127,
     pin129,
     pin130,
     pin121,
     pin122,
     pin123,
     pin124,
     pin118,
     pin119,
     pin120,
     pin114,
     pin117,
     pin112,
     pin113,
     pin109,
     pin110,
     pin111,
     pin106,
     pin107,
     pin108,
     pin103,
     pin104,
     pin105,
     pin96,
     pin97,
     pin98,
     pin101,
     pin102,
     pin88,
     pin89,
     pin91,
     pin93,
     pin94,
     pin95,
     pin81,
     pin84,
     pin85,
     pin86,
     pin87,
     pin77,
     pin78,
     pin79,
     pin80,
     pin73,
     pin74,
     pin75,
     pin76,
     pin71,
     pin72,
     pin69,
     pin70,
     pin66,
     pin67,
     pin68,
     pin60,
     pin61,
     pin62,
     pin63,
     pin55,
     pin57,
     pin58,
     pin59,
     pin50,
     pin51,
     pin52,
     pin53,
     pin45,
     pin48,
     pin49,
     pin42,
     pin43,
     pin44,
     pin40,
     pin41,
     pin37,
     pin38,
     pin39,
     pin31,
     pin32
    ].

-spec top_iocs(x()) -> [{ioc(), pin()}].

top_iocs(1) ->
    [{{ioc,1,8,0},pin2},
     {{ioc,1,8,2},pin3}];
top_iocs(2) ->
    [{{ioc,2,8,1},pin144},
     {{ioc,2,8,3},pin1}];
top_iocs(3) ->
    [{{ioc,3,8,0},pin142},
     {{ioc,3,8,3},pin143}];
top_iocs(4) ->
    [{{ioc,4,8,1},pin140},
     {{ioc,4,8,2},pin141}];
top_iocs(5) ->
    [{{ioc,5,8,1},pin138},
     {{ioc,5,8,2},pin139}];
top_iocs(6) ->
    [{{ioc,6,8,0},pin131},
     {{ioc,6,8,1},pin132},
     {{ioc,6,8,2},pin133},
     {{ioc,6,8,3},pin134}];
top_iocs(7) ->
    [{{ioc,7,8,0},pin125},
     {{ioc,7,8,1},pin127},
     {{ioc,7,8,2},pin129},
     {{ioc,7,8,3},pin130}];
top_iocs(8) ->
    [{{ioc,8,8,0},pin121},
     {{ioc,8,8,1},pin122},
     {{ioc,8,8,2},pin123},
     {{ioc,8,8,3},pin124}];
top_iocs(9) ->
    [{{ioc,9,8,0},pin118},
     {{ioc,9,8,1},pin119},
     {{ioc,9,8,2},pin120}];
top_iocs(10) ->
    [{{ioc,10,8,1},pin114},
     {{ioc,10,8,3},pin117}];
top_iocs(11) ->
    [{{ioc,11,8,1},pin112},
     {{ioc,11,8,3},pin113}];
top_iocs(12) ->
    [{{ioc,12,8,1},pin109},
     {{ioc,12,8,2},pin110},
     {{ioc,12,8,3},pin111}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [pin2, pin3];
top_pins(2) ->
    [pin144, pin1];
top_pins(3) ->
    [pin142, pin143];
top_pins(4) ->
    [pin140, pin141];
top_pins(5) ->
    [pin138, pin139];
top_pins(6) ->
    [pin131, pin132, pin133, pin134];
top_pins(7) ->
    [pin125, pin127, pin129, pin130];
top_pins(8) ->
    [pin121, pin122, pin123, pin124];
top_pins(9) ->
    [pin118, pin119, pin120];
top_pins(10) ->
    [pin114, pin117];
top_pins(11) ->
    [pin112, pin113];
top_pins(12) ->
    [pin109, pin110, pin111].

-spec left_iocs(y()) -> [{ioc(), pin()}].

left_iocs(1) ->
    [];
left_iocs(2) ->
    [];
left_iocs(3) ->
    [];
left_iocs(4) ->
    [{{ioc,0,4,0},pin27},
     {{ioc,0,4,2},pin28},
     {{ioc,0,4,3},pin29},
     {{ioc,0,4,5},pin30}];
left_iocs(5) ->
    [{{ioc,0,5,0},pin18},
     {{ioc,0,5,1},pin20},
     {{ioc,0,5,2},pin21},
     {{ioc,0,5,3},pin22},
     {{ioc,0,5,4},pin23},
     {{ioc,0,5,5},pin24}];
left_iocs(6) ->
    [{{ioc,0,6,1},pin11},
     {{ioc,0,6,2},pin12},
     {{ioc,0,6,3},pin13},
     {{ioc,0,6,4},pin14},
     {{ioc,0,6,5},pin15},
     {{ioc,0,6,6},pin16}];
left_iocs(7) ->
    [{{ioc,0,7,0},pin4},
     {{ioc,0,7,2},pin5},
     {{ioc,0,7,3},pin6},
     {{ioc,0,7,4},pin7}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [];
left_pins(2) ->
    [];
left_pins(3) ->
    [];
left_pins(4) ->
    [pin27, pin28, pin29, pin30];
left_pins(5) ->
    [pin18, pin20, pin21, pin22, pin23, pin24];
left_pins(6) ->
    [pin11, pin12, pin13, pin14, pin15, pin16];
left_pins(7) ->
    [pin4, pin5, pin6, pin7].

-spec right_iocs(y()) -> [{ioc(), pin()}].

right_iocs(1) ->
    [{{ioc,13,1,5},pin73},
     {{ioc,13,1,4},pin74},
     {{ioc,13,1,3},pin75},
     {{ioc,13,1,1},pin76}];
right_iocs(2) ->
    [{{ioc,13,2,4},pin77},
     {{ioc,13,2,2},pin78},
     {{ioc,13,2,1},pin79},
     {{ioc,13,2,0},pin80}];
right_iocs(3) ->
    [{{ioc,13,3,5},pin81},
     {{ioc,13,3,3},pin84},
     {{ioc,13,3,2},pin85},
     {{ioc,13,3,1},pin86},
     {{ioc,13,3,0},pin87}];
right_iocs(4) ->
    [{{ioc,13,4,5},pin88},
     {{ioc,13,4,4},pin89},
     {{ioc,13,4,3},pin91},
     {{ioc,13,4,2},pin93},
     {{ioc,13,4,1},pin94},
     {{ioc,13,4,0},pin95}];
right_iocs(5) ->
    [{{ioc,13,5,5},pin96},
     {{ioc,13,5,4},pin97},
     {{ioc,13,5,3},pin98},
     {{ioc,13,5,2},pin101},
     {{ioc,13,5,0},pin102}];
right_iocs(6) ->
    [{{ioc,13,6,5},pin103},
     {{ioc,13,6,2},pin104},
     {{ioc,13,6,1},pin105}];
right_iocs(7) ->
    [{{ioc,13,7,5},pin106},
     {{ioc,13,7,3},pin107},
     {{ioc,13,7,1},pin108}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [pin73, pin74, pin75, pin76];
right_pins(2) ->
    [pin77, pin78, pin79, pin80];
right_pins(3) ->
    [pin81, pin84, pin85, pin86, pin87];
right_pins(4) ->
    [pin88, pin89, pin91, pin93, pin94, pin95];
right_pins(5) ->
    [pin96, pin97, pin98, pin101, pin102];
right_pins(6) ->
    [pin103, pin104, pin105];
right_pins(7) ->
    [pin106, pin107, pin108].

-spec bottom_iocs(x()) -> [{ioc(), pin()}].

bottom_iocs(1) ->
    [{{ioc,1,3,3},pin31},
     {{ioc,1,3,1},pin32}];
bottom_iocs(2) ->
    [{{ioc,2,3,3},pin37},
     {{ioc,2,3,2},pin38},
     {{ioc,2,3,0},pin39}];
bottom_iocs(3) ->
    [{{ioc,3,3,2},pin40},
     {{ioc,3,3,1},pin41}];
bottom_iocs(4) ->
    [{{ioc,4,3,3},pin42},
     {{ioc,4,3,2},pin43},
     {{ioc,4,3,1},pin44}];
bottom_iocs(5) ->
    [{{ioc,5,3,2},pin45},
     {{ioc,5,3,1},pin48},
     {{ioc,5,3,0},pin49}];
bottom_iocs(6) ->
    [{{ioc,6,3,3},pin50},
     {{ioc,6,3,2},pin51},
     {{ioc,6,3,1},pin52},
     {{ioc,6,3,0},pin53}];
bottom_iocs(7) ->
    [{{ioc,7,3,3},pin55},
     {{ioc,7,3,2},pin57},
     {{ioc,7,3,1},pin58},
     {{ioc,7,3,0},pin59}];
bottom_iocs(8) ->
    [{{ioc,8,3,3},pin60},
     {{ioc,8,3,2},pin61},
     {{ioc,8,3,1},pin62},
     {{ioc,8,3,0},pin63}];
bottom_iocs(9) ->
    [];
bottom_iocs(10) ->
    [{{ioc,10,0,2},pin66},
     {{ioc,10,0,1},pin67},
     {{ioc,10,0,0},pin68}];
bottom_iocs(11) ->
    [{{ioc,11,0,2},pin69},
     {{ioc,11,0,1},pin70}];
bottom_iocs(12) ->
    [{{ioc,12,0,2},pin71},
     {{ioc,12,0,0},pin72}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [pin31, pin32];
bottom_pins(2) ->
    [pin37, pin38, pin39];
bottom_pins(3) ->
    [pin40, pin41];
bottom_pins(4) ->
    [pin42, pin43, pin44];
bottom_pins(5) ->
    [pin45, pin48, pin49];
bottom_pins(6) ->
    [pin50, pin51, pin52, pin53];
bottom_pins(7) ->
    [pin55, pin57, pin58, pin59];
bottom_pins(8) ->
    [pin60, pin61, pin62, pin63];
bottom_pins(9) ->
    [];
bottom_pins(10) ->
    [pin66, pin67, pin68];
bottom_pins(11) ->
    [pin69, pin70];
bottom_pins(12) ->
    [pin71, pin72].

