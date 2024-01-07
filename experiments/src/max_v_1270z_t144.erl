-module(max_v_1270z_t144).

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
    [{{ioc,0,4,2},pin31},
     {{ioc,0,4,5},pin32},
     {{ioc,0,5,1},pin28},
     {{ioc,0,5,2},pin29},
     {{ioc,0,5,6},pin30},
     {{ioc,0,6,0},pin21},
     {{ioc,0,6,1},pin22},
     {{ioc,0,6,2},pin23},
     {{ioc,0,6,3},pin24},
     {{ioc,0,6,5},pin27},
     {{ioc,0,7,0},pin12},
     {{ioc,0,7,1},pin13},
     {{ioc,0,7,2},pin14},
     {{ioc,0,7,3},pin15},
     {{ioc,0,7,4},pin16},
     {{ioc,0,7,5},pin18},
     {{ioc,0,7,6},pin20},
     {{ioc,0,8,0},pin7},
     {{ioc,0,8,1},pin8},
     {{ioc,0,9,0},pin4},
     {{ioc,0,9,2},pin5},
     {{ioc,0,9,3},pin6},
     {{ioc,0,10,2},pin1},
     {{ioc,0,10,3},pin2},
     {{ioc,0,10,5},pin3},
     {{ioc,1,11,2},pin144},
     {{ioc,2,11,3},pin143},
     {{ioc,3,11,2},pin142},
     {{ioc,4,11,1},pin140},
     {{ioc,4,11,2},pin141},
     {{ioc,5,11,0},pin138},
     {{ioc,5,11,1},pin139},
     {{ioc,6,11,3},pin137},
     {{ioc,7,11,0},pin132},
     {{ioc,7,11,1},pin133},
     {{ioc,7,11,2},pin134},
     {{ioc,8,11,0},pin129},
     {{ioc,8,11,1},pin130},
     {{ioc,8,11,2},pin131},
     {{ioc,9,11,0},pin123},
     {{ioc,9,11,1},pin124},
     {{ioc,9,11,2},pin125},
     {{ioc,9,11,3},pin127},
     {{ioc,10,11,0},pin120},
     {{ioc,10,11,1},pin121},
     {{ioc,10,11,2},pin122},
     {{ioc,11,11,1},pin117},
     {{ioc,11,11,2},pin118},
     {{ioc,11,11,3},pin119},
     {{ioc,12,11,1},pin114},
     {{ioc,13,11,1},pin113},
     {{ioc,14,11,1},pin112},
     {{ioc,15,11,0},pin111},
     {{ioc,16,11,1},pin109},
     {{ioc,16,11,2},pin110},
     {{ioc,17,10,4},pin108},
     {{ioc,17,9,3},pin106},
     {{ioc,17,9,1},pin107},
     {{ioc,17,8,5},pin103},
     {{ioc,17,8,1},pin104},
     {{ioc,17,8,0},pin105},
     {{ioc,17,7,4},pin101},
     {{ioc,17,7,0},pin102},
     {{ioc,17,6,5},pin93},
     {{ioc,17,6,4},pin94},
     {{ioc,17,6,3},pin95},
     {{ioc,17,6,2},pin96},
     {{ioc,17,6,1},pin97},
     {{ioc,17,6,0},pin98},
     {{ioc,17,5,5},pin85},
     {{ioc,17,5,4},pin86},
     {{ioc,17,5,3},pin87},
     {{ioc,17,5,2},pin88},
     {{ioc,17,5,1},pin89},
     {{ioc,17,5,0},pin91},
     {{ioc,17,4,2},pin81},
     {{ioc,17,4,0},pin84},
     {{ioc,17,3,1},pin79},
     {{ioc,17,3,0},pin80},
     {{ioc,17,2,3},pin76},
     {{ioc,17,2,0},pin77},
     {{ioc,17,1,2},pin73},
     {{ioc,17,1,1},pin74},
     {{ioc,17,1,0},pin75},
     {{ioc,16,0,3},pin71},
     {{ioc,16,0,0},pin72},
     {{ioc,15,0,2},pin70},
     {{ioc,14,0,2},pin69},
     {{ioc,13,0,1},pin68},
     {{ioc,12,0,1},pin66},
     {{ioc,12,0,0},pin67},
     {{ioc,10,3,3},pin61},
     {{ioc,10,3,2},pin62},
     {{ioc,10,3,1},pin63},
     {{ioc,9,3,2},pin58},
     {{ioc,9,3,1},pin59},
     {{ioc,9,3,0},pin60},
     {{ioc,8,3,3},pin52},
     {{ioc,8,3,2},pin53},
     {{ioc,8,3,1},pin55},
     {{ioc,8,3,0},pin57},
     {{ioc,7,3,3},pin48},
     {{ioc,7,3,2},pin49},
     {{ioc,7,3,1},pin50},
     {{ioc,7,3,0},pin51},
     {{ioc,6,3,0},pin45},
     {{ioc,5,3,3},pin43},
     {{ioc,5,3,2},pin44},
     {{ioc,4,3,0},pin42},
     {{ioc,3,3,1},pin40},
     {{ioc,3,3,0},pin41},
     {{ioc,2,3,3},pin38},
     {{ioc,2,3,0},pin39},
     {{ioc,1,3,0},pin37}
    ].

-spec iocs(iob()) -> [{ioc(), pin()}].

iocs({iob,0,4}) ->
    [{{ioc,0,4,2},pin31},
     {{ioc,0,4,5},pin32}];
iocs({iob,0,5}) ->
    [{{ioc,0,5,1},pin28},
     {{ioc,0,5,2},pin29},
     {{ioc,0,5,6},pin30}];
iocs({iob,0,6}) ->
    [{{ioc,0,6,0},pin21},
     {{ioc,0,6,1},pin22},
     {{ioc,0,6,2},pin23},
     {{ioc,0,6,3},pin24},
     {{ioc,0,6,5},pin27}];
iocs({iob,0,7}) ->
    [{{ioc,0,7,0},pin12},
     {{ioc,0,7,1},pin13},
     {{ioc,0,7,2},pin14},
     {{ioc,0,7,3},pin15},
     {{ioc,0,7,4},pin16},
     {{ioc,0,7,5},pin18},
     {{ioc,0,7,6},pin20}];
iocs({iob,0,8}) ->
    [{{ioc,0,8,0},pin7},
     {{ioc,0,8,1},pin8}];
iocs({iob,0,9}) ->
    [{{ioc,0,9,0},pin4},
     {{ioc,0,9,2},pin5},
     {{ioc,0,9,3},pin6}];
iocs({iob,0,10}) ->
    [{{ioc,0,10,2},pin1},
     {{ioc,0,10,3},pin2},
     {{ioc,0,10,5},pin3}];
iocs({iob,1,11}) ->
    [{{ioc,1,11,2},pin144}];
iocs({iob,2,11}) ->
    [{{ioc,2,11,3},pin143}];
iocs({iob,3,11}) ->
    [{{ioc,3,11,2},pin142}];
iocs({iob,4,11}) ->
    [{{ioc,4,11,1},pin140},
     {{ioc,4,11,2},pin141}];
iocs({iob,5,11}) ->
    [{{ioc,5,11,0},pin138},
     {{ioc,5,11,1},pin139}];
iocs({iob,6,11}) ->
    [{{ioc,6,11,3},pin137}];
iocs({iob,7,11}) ->
    [{{ioc,7,11,0},pin132},
     {{ioc,7,11,1},pin133},
     {{ioc,7,11,2},pin134}];
iocs({iob,8,11}) ->
    [{{ioc,8,11,0},pin129},
     {{ioc,8,11,1},pin130},
     {{ioc,8,11,2},pin131}];
iocs({iob,9,11}) ->
    [{{ioc,9,11,0},pin123},
     {{ioc,9,11,1},pin124},
     {{ioc,9,11,2},pin125},
     {{ioc,9,11,3},pin127}];
iocs({iob,10,11}) ->
    [{{ioc,10,11,0},pin120},
     {{ioc,10,11,1},pin121},
     {{ioc,10,11,2},pin122}];
iocs({iob,11,11}) ->
    [{{ioc,11,11,1},pin117},
     {{ioc,11,11,2},pin118},
     {{ioc,11,11,3},pin119}];
iocs({iob,12,11}) ->
    [{{ioc,12,11,1},pin114}];
iocs({iob,13,11}) ->
    [{{ioc,13,11,1},pin113}];
iocs({iob,14,11}) ->
    [{{ioc,14,11,1},pin112}];
iocs({iob,15,11}) ->
    [{{ioc,15,11,0},pin111}];
iocs({iob,16,11}) ->
    [{{ioc,16,11,1},pin109},
     {{ioc,16,11,2},pin110}];
iocs({iob,17,10}) ->
    [{{ioc,17,10,4},pin108}];
iocs({iob,17,9}) ->
    [{{ioc,17,9,3},pin106},
     {{ioc,17,9,1},pin107}];
iocs({iob,17,8}) ->
    [{{ioc,17,8,5},pin103},
     {{ioc,17,8,1},pin104},
     {{ioc,17,8,0},pin105}];
iocs({iob,17,7}) ->
    [{{ioc,17,7,4},pin101},
     {{ioc,17,7,0},pin102}];
iocs({iob,17,6}) ->
    [{{ioc,17,6,5},pin93},
     {{ioc,17,6,4},pin94},
     {{ioc,17,6,3},pin95},
     {{ioc,17,6,2},pin96},
     {{ioc,17,6,1},pin97},
     {{ioc,17,6,0},pin98}];
iocs({iob,17,5}) ->
    [{{ioc,17,5,5},pin85},
     {{ioc,17,5,4},pin86},
     {{ioc,17,5,3},pin87},
     {{ioc,17,5,2},pin88},
     {{ioc,17,5,1},pin89},
     {{ioc,17,5,0},pin91}];
iocs({iob,17,4}) ->
    [{{ioc,17,4,2},pin81},
     {{ioc,17,4,0},pin84}];
iocs({iob,17,3}) ->
    [{{ioc,17,3,1},pin79},
     {{ioc,17,3,0},pin80}];
iocs({iob,17,2}) ->
    [{{ioc,17,2,3},pin76},
     {{ioc,17,2,0},pin77}];
iocs({iob,17,1}) ->
    [{{ioc,17,1,2},pin73},
     {{ioc,17,1,1},pin74},
     {{ioc,17,1,0},pin75}];
iocs({iob,16,0}) ->
    [{{ioc,16,0,3},pin71},
     {{ioc,16,0,0},pin72}];
iocs({iob,15,0}) ->
    [{{ioc,15,0,2},pin70}];
iocs({iob,14,0}) ->
    [{{ioc,14,0,2},pin69}];
iocs({iob,13,0}) ->
    [{{ioc,13,0,1},pin68}];
iocs({iob,12,0}) ->
    [{{ioc,12,0,1},pin66},
     {{ioc,12,0,0},pin67}];
iocs({iob,10,3}) ->
    [{{ioc,10,3,3},pin61},
     {{ioc,10,3,2},pin62},
     {{ioc,10,3,1},pin63}];
iocs({iob,9,3}) ->
    [{{ioc,9,3,2},pin58},
     {{ioc,9,3,1},pin59},
     {{ioc,9,3,0},pin60}];
iocs({iob,8,3}) ->
    [{{ioc,8,3,3},pin52},
     {{ioc,8,3,2},pin53},
     {{ioc,8,3,1},pin55},
     {{ioc,8,3,0},pin57}];
iocs({iob,7,3}) ->
    [{{ioc,7,3,3},pin48},
     {{ioc,7,3,2},pin49},
     {{ioc,7,3,1},pin50},
     {{ioc,7,3,0},pin51}];
iocs({iob,6,3}) ->
    [{{ioc,6,3,0},pin45}];
iocs({iob,5,3}) ->
    [{{ioc,5,3,3},pin43},
     {{ioc,5,3,2},pin44}];
iocs({iob,4,3}) ->
    [{{ioc,4,3,0},pin42}];
iocs({iob,3,3}) ->
    [{{ioc,3,3,1},pin40},
     {{ioc,3,3,0},pin41}];
iocs({iob,2,3}) ->
    [{{ioc,2,3,3},pin38},
     {{ioc,2,3,0},pin39}];
iocs({iob,1,3}) ->
    [{{ioc,1,3,0},pin37}].

-spec pins() -> [pin()].

pins() ->
    [pin31,
     pin32,
     pin28,
     pin29,
     pin30,
     pin21,
     pin22,
     pin23,
     pin24,
     pin27,
     pin12,
     pin13,
     pin14,
     pin15,
     pin16,
     pin18,
     pin20,
     pin7,
     pin8,
     pin4,
     pin5,
     pin6,
     pin1,
     pin2,
     pin3,
     pin144,
     pin143,
     pin142,
     pin140,
     pin141,
     pin138,
     pin139,
     pin137,
     pin132,
     pin133,
     pin134,
     pin129,
     pin130,
     pin131,
     pin123,
     pin124,
     pin125,
     pin127,
     pin120,
     pin121,
     pin122,
     pin117,
     pin118,
     pin119,
     pin114,
     pin113,
     pin112,
     pin111,
     pin109,
     pin110,
     pin108,
     pin106,
     pin107,
     pin103,
     pin104,
     pin105,
     pin101,
     pin102,
     pin93,
     pin94,
     pin95,
     pin96,
     pin97,
     pin98,
     pin85,
     pin86,
     pin87,
     pin88,
     pin89,
     pin91,
     pin81,
     pin84,
     pin79,
     pin80,
     pin76,
     pin77,
     pin73,
     pin74,
     pin75,
     pin71,
     pin72,
     pin70,
     pin69,
     pin68,
     pin66,
     pin67,
     pin61,
     pin62,
     pin63,
     pin58,
     pin59,
     pin60,
     pin52,
     pin53,
     pin55,
     pin57,
     pin48,
     pin49,
     pin50,
     pin51,
     pin45,
     pin43,
     pin44,
     pin42,
     pin40,
     pin41,
     pin38,
     pin39,
     pin37
    ].

-spec top_iocs(x()) -> [{ioc(), pin()}].

top_iocs(1) ->
    [{{ioc,1,11,2},pin144}];
top_iocs(2) ->
    [{{ioc,2,11,3},pin143}];
top_iocs(3) ->
    [{{ioc,3,11,2},pin142}];
top_iocs(4) ->
    [{{ioc,4,11,1},pin140},
     {{ioc,4,11,2},pin141}];
top_iocs(5) ->
    [{{ioc,5,11,0},pin138},
     {{ioc,5,11,1},pin139}];
top_iocs(6) ->
    [{{ioc,6,11,3},pin137}];
top_iocs(7) ->
    [{{ioc,7,11,0},pin132},
     {{ioc,7,11,1},pin133},
     {{ioc,7,11,2},pin134}];
top_iocs(8) ->
    [{{ioc,8,11,0},pin129},
     {{ioc,8,11,1},pin130},
     {{ioc,8,11,2},pin131}];
top_iocs(9) ->
    [{{ioc,9,11,0},pin123},
     {{ioc,9,11,1},pin124},
     {{ioc,9,11,2},pin125},
     {{ioc,9,11,3},pin127}];
top_iocs(10) ->
    [{{ioc,10,11,0},pin120},
     {{ioc,10,11,1},pin121},
     {{ioc,10,11,2},pin122}];
top_iocs(11) ->
    [{{ioc,11,11,1},pin117},
     {{ioc,11,11,2},pin118},
     {{ioc,11,11,3},pin119}];
top_iocs(12) ->
    [{{ioc,12,11,1},pin114}];
top_iocs(13) ->
    [{{ioc,13,11,1},pin113}];
top_iocs(14) ->
    [{{ioc,14,11,1},pin112}];
top_iocs(15) ->
    [{{ioc,15,11,0},pin111}];
top_iocs(16) ->
    [{{ioc,16,11,1},pin109},
     {{ioc,16,11,2},pin110}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [pin144];
top_pins(2) ->
    [pin143];
top_pins(3) ->
    [pin142];
top_pins(4) ->
    [pin140, pin141];
top_pins(5) ->
    [pin138, pin139];
top_pins(6) ->
    [pin137];
top_pins(7) ->
    [pin132, pin133, pin134];
top_pins(8) ->
    [pin129, pin130, pin131];
top_pins(9) ->
    [pin123, pin124, pin125, pin127];
top_pins(10) ->
    [pin120, pin121, pin122];
top_pins(11) ->
    [pin117, pin118, pin119];
top_pins(12) ->
    [pin114];
top_pins(13) ->
    [pin113];
top_pins(14) ->
    [pin112];
top_pins(15) ->
    [pin111];
top_pins(16) ->
    [pin109, pin110].

-spec left_iocs(y()) -> [{ioc(), pin()}].

left_iocs(1) ->
    [];
left_iocs(2) ->
    [];
left_iocs(3) ->
    [];
left_iocs(4) ->
    [{{ioc,0,4,2},pin31},
     {{ioc,0,4,5},pin32}];
left_iocs(5) ->
    [{{ioc,0,5,1},pin28},
     {{ioc,0,5,2},pin29},
     {{ioc,0,5,6},pin30}];
left_iocs(6) ->
    [{{ioc,0,6,0},pin21},
     {{ioc,0,6,1},pin22},
     {{ioc,0,6,2},pin23},
     {{ioc,0,6,3},pin24},
     {{ioc,0,6,5},pin27}];
left_iocs(7) ->
    [{{ioc,0,7,0},pin12},
     {{ioc,0,7,1},pin13},
     {{ioc,0,7,2},pin14},
     {{ioc,0,7,3},pin15},
     {{ioc,0,7,4},pin16},
     {{ioc,0,7,5},pin18},
     {{ioc,0,7,6},pin20}];
left_iocs(8) ->
    [{{ioc,0,8,0},pin7},
     {{ioc,0,8,1},pin8}];
left_iocs(9) ->
    [{{ioc,0,9,0},pin4},
     {{ioc,0,9,2},pin5},
     {{ioc,0,9,3},pin6}];
left_iocs(10) ->
    [{{ioc,0,10,2},pin1},
     {{ioc,0,10,3},pin2},
     {{ioc,0,10,5},pin3}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [];
left_pins(2) ->
    [];
left_pins(3) ->
    [];
left_pins(4) ->
    [pin31, pin32];
left_pins(5) ->
    [pin28, pin29, pin30];
left_pins(6) ->
    [pin21, pin22, pin23, pin24, pin27];
left_pins(7) ->
    [pin12, pin13, pin14, pin15, pin16, pin18, pin20];
left_pins(8) ->
    [pin7, pin8];
left_pins(9) ->
    [pin4, pin5, pin6];
left_pins(10) ->
    [pin1, pin2, pin3].

-spec right_iocs(y()) -> [{ioc(), pin()}].

right_iocs(1) ->
    [{{ioc,17,1,2},pin73},
     {{ioc,17,1,1},pin74},
     {{ioc,17,1,0},pin75}];
right_iocs(2) ->
    [{{ioc,17,2,3},pin76},
     {{ioc,17,2,0},pin77}];
right_iocs(3) ->
    [{{ioc,17,3,1},pin79},
     {{ioc,17,3,0},pin80}];
right_iocs(4) ->
    [{{ioc,17,4,2},pin81},
     {{ioc,17,4,0},pin84}];
right_iocs(5) ->
    [{{ioc,17,5,5},pin85},
     {{ioc,17,5,4},pin86},
     {{ioc,17,5,3},pin87},
     {{ioc,17,5,2},pin88},
     {{ioc,17,5,1},pin89},
     {{ioc,17,5,0},pin91}];
right_iocs(6) ->
    [{{ioc,17,6,5},pin93},
     {{ioc,17,6,4},pin94},
     {{ioc,17,6,3},pin95},
     {{ioc,17,6,2},pin96},
     {{ioc,17,6,1},pin97},
     {{ioc,17,6,0},pin98}];
right_iocs(7) ->
    [{{ioc,17,7,4},pin101},
     {{ioc,17,7,0},pin102}];
right_iocs(8) ->
    [{{ioc,17,8,5},pin103},
     {{ioc,17,8,1},pin104},
     {{ioc,17,8,0},pin105}];
right_iocs(9) ->
    [{{ioc,17,9,3},pin106},
     {{ioc,17,9,1},pin107}];
right_iocs(10) ->
    [{{ioc,17,10,4},pin108}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [pin73, pin74, pin75];
right_pins(2) ->
    [pin76, pin77];
right_pins(3) ->
    [pin79, pin80];
right_pins(4) ->
    [pin81, pin84];
right_pins(5) ->
    [pin85, pin86, pin87, pin88, pin89, pin91];
right_pins(6) ->
    [pin93, pin94, pin95, pin96, pin97, pin98];
right_pins(7) ->
    [pin101, pin102];
right_pins(8) ->
    [pin103, pin104, pin105];
right_pins(9) ->
    [pin106, pin107];
right_pins(10) ->
    [pin108].

-spec bottom_iocs(x()) -> [{ioc(), pin()}].

bottom_iocs(1) ->
    [{{ioc,1,3,0},pin37}];
bottom_iocs(2) ->
    [{{ioc,2,3,3},pin38},
     {{ioc,2,3,0},pin39}];
bottom_iocs(3) ->
    [{{ioc,3,3,1},pin40},
     {{ioc,3,3,0},pin41}];
bottom_iocs(4) ->
    [{{ioc,4,3,0},pin42}];
bottom_iocs(5) ->
    [{{ioc,5,3,3},pin43},
     {{ioc,5,3,2},pin44}];
bottom_iocs(6) ->
    [{{ioc,6,3,0},pin45}];
bottom_iocs(7) ->
    [{{ioc,7,3,3},pin48},
     {{ioc,7,3,2},pin49},
     {{ioc,7,3,1},pin50},
     {{ioc,7,3,0},pin51}];
bottom_iocs(8) ->
    [{{ioc,8,3,3},pin52},
     {{ioc,8,3,2},pin53},
     {{ioc,8,3,1},pin55},
     {{ioc,8,3,0},pin57}];
bottom_iocs(9) ->
    [{{ioc,9,3,2},pin58},
     {{ioc,9,3,1},pin59},
     {{ioc,9,3,0},pin60}];
bottom_iocs(10) ->
    [{{ioc,10,3,3},pin61},
     {{ioc,10,3,2},pin62},
     {{ioc,10,3,1},pin63}];
bottom_iocs(11) ->
    [];
bottom_iocs(12) ->
    [{{ioc,12,0,1},pin66},
     {{ioc,12,0,0},pin67}];
bottom_iocs(13) ->
    [{{ioc,13,0,1},pin68}];
bottom_iocs(14) ->
    [{{ioc,14,0,2},pin69}];
bottom_iocs(15) ->
    [{{ioc,15,0,2},pin70}];
bottom_iocs(16) ->
    [{{ioc,16,0,3},pin71},
     {{ioc,16,0,0},pin72}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [pin37];
bottom_pins(2) ->
    [pin38, pin39];
bottom_pins(3) ->
    [pin40, pin41];
bottom_pins(4) ->
    [pin42];
bottom_pins(5) ->
    [pin43, pin44];
bottom_pins(6) ->
    [pin45];
bottom_pins(7) ->
    [pin48, pin49, pin50, pin51];
bottom_pins(8) ->
    [pin52, pin53, pin55, pin57];
bottom_pins(9) ->
    [pin58, pin59, pin60];
bottom_pins(10) ->
    [pin61, pin62, pin63];
bottom_pins(11) ->
    [];
bottom_pins(12) ->
    [pin66, pin67];
bottom_pins(13) ->
    [pin68];
bottom_pins(14) ->
    [pin69];
bottom_pins(15) ->
    [pin70];
bottom_pins(16) ->
    [pin71, pin72].

