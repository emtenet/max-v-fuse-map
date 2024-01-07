-module(max_v_80z_t100).

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
    [{{ioc,1,4,0},pin2},
     {{ioc,1,4,1},pin3},
     {{ioc,1,4,2},pin4},
     {{ioc,1,4,3},pin5},
     {{ioc,1,3,0},pin6},
     {{ioc,1,3,1},pin7},
     {{ioc,1,3,2},pin8},
     {{ioc,1,3,3},pin12},
     {{ioc,1,2,0},pin14},
     {{ioc,1,2,1},pin15},
     {{ioc,1,2,2},pin16},
     {{ioc,1,2,3},pin17},
     {{ioc,1,1,0},pin18},
     {{ioc,1,1,1},pin19},
     {{ioc,1,1,2},pin20},
     {{ioc,1,1,3},pin21},
     {{ioc,2,5,0},pin98},
     {{ioc,2,5,1},pin99},
     {{ioc,2,5,2},pin100},
     {{ioc,3,5,0},pin92},
     {{ioc,3,5,1},pin95},
     {{ioc,3,5,2},pin96},
     {{ioc,3,5,3},pin97},
     {{ioc,4,5,0},pin89},
     {{ioc,4,5,1},pin90},
     {{ioc,4,5,2},pin91},
     {{ioc,5,5,0},pin85},
     {{ioc,5,5,1},pin86},
     {{ioc,5,5,2},pin87},
     {{ioc,5,5,3},pin88},
     {{ioc,6,5,0},pin81},
     {{ioc,6,5,1},pin82},
     {{ioc,6,5,2},pin83},
     {{ioc,6,5,3},pin84},
     {{ioc,7,5,0},pin75},
     {{ioc,7,5,1},pin76},
     {{ioc,7,5,2},pin77},
     {{ioc,7,5,3},pin78},
     {{ioc,8,4,4},pin70},
     {{ioc,8,4,3},pin71},
     {{ioc,8,4,2},pin72},
     {{ioc,8,4,1},pin73},
     {{ioc,8,4,0},pin74},
     {{ioc,8,3,4},pin64},
     {{ioc,8,3,3},pin66},
     {{ioc,8,3,2},pin67},
     {{ioc,8,3,1},pin68},
     {{ioc,8,3,0},pin69},
     {{ioc,8,2,3},pin57},
     {{ioc,8,2,2},pin58},
     {{ioc,8,2,1},pin61},
     {{ioc,8,2,0},pin62},
     {{ioc,8,1,4},pin52},
     {{ioc,8,1,3},pin53},
     {{ioc,8,1,2},pin54},
     {{ioc,8,1,1},pin55},
     {{ioc,8,1,0},pin56},
     {{ioc,7,0,2},pin49},
     {{ioc,7,0,1},pin50},
     {{ioc,7,0,0},pin51},
     {{ioc,6,0,3},pin43},
     {{ioc,6,0,2},pin44},
     {{ioc,6,0,1},pin47},
     {{ioc,6,0,0},pin48},
     {{ioc,5,0,3},pin39},
     {{ioc,5,0,2},pin40},
     {{ioc,5,0,1},pin41},
     {{ioc,5,0,0},pin42},
     {{ioc,4,0,2},pin36},
     {{ioc,4,0,1},pin37},
     {{ioc,4,0,0},pin38},
     {{ioc,3,0,3},pin30},
     {{ioc,3,0,2},pin33},
     {{ioc,3,0,1},pin34},
     {{ioc,3,0,0},pin35},
     {{ioc,2,0,3},pin26},
     {{ioc,2,0,2},pin27},
     {{ioc,2,0,1},pin28},
     {{ioc,2,0,0},pin29}
    ].

-spec iocs(iob()) -> [{ioc(), pin()}].

iocs({iob,1,4}) ->
    [{{ioc,1,4,0},pin2},
     {{ioc,1,4,1},pin3},
     {{ioc,1,4,2},pin4},
     {{ioc,1,4,3},pin5}];
iocs({iob,1,3}) ->
    [{{ioc,1,3,0},pin6},
     {{ioc,1,3,1},pin7},
     {{ioc,1,3,2},pin8},
     {{ioc,1,3,3},pin12}];
iocs({iob,1,2}) ->
    [{{ioc,1,2,0},pin14},
     {{ioc,1,2,1},pin15},
     {{ioc,1,2,2},pin16},
     {{ioc,1,2,3},pin17}];
iocs({iob,1,1}) ->
    [{{ioc,1,1,0},pin18},
     {{ioc,1,1,1},pin19},
     {{ioc,1,1,2},pin20},
     {{ioc,1,1,3},pin21}];
iocs({iob,2,5}) ->
    [{{ioc,2,5,0},pin98},
     {{ioc,2,5,1},pin99},
     {{ioc,2,5,2},pin100}];
iocs({iob,3,5}) ->
    [{{ioc,3,5,0},pin92},
     {{ioc,3,5,1},pin95},
     {{ioc,3,5,2},pin96},
     {{ioc,3,5,3},pin97}];
iocs({iob,4,5}) ->
    [{{ioc,4,5,0},pin89},
     {{ioc,4,5,1},pin90},
     {{ioc,4,5,2},pin91}];
iocs({iob,5,5}) ->
    [{{ioc,5,5,0},pin85},
     {{ioc,5,5,1},pin86},
     {{ioc,5,5,2},pin87},
     {{ioc,5,5,3},pin88}];
iocs({iob,6,5}) ->
    [{{ioc,6,5,0},pin81},
     {{ioc,6,5,1},pin82},
     {{ioc,6,5,2},pin83},
     {{ioc,6,5,3},pin84}];
iocs({iob,7,5}) ->
    [{{ioc,7,5,0},pin75},
     {{ioc,7,5,1},pin76},
     {{ioc,7,5,2},pin77},
     {{ioc,7,5,3},pin78}];
iocs({iob,8,4}) ->
    [{{ioc,8,4,4},pin70},
     {{ioc,8,4,3},pin71},
     {{ioc,8,4,2},pin72},
     {{ioc,8,4,1},pin73},
     {{ioc,8,4,0},pin74}];
iocs({iob,8,3}) ->
    [{{ioc,8,3,4},pin64},
     {{ioc,8,3,3},pin66},
     {{ioc,8,3,2},pin67},
     {{ioc,8,3,1},pin68},
     {{ioc,8,3,0},pin69}];
iocs({iob,8,2}) ->
    [{{ioc,8,2,3},pin57},
     {{ioc,8,2,2},pin58},
     {{ioc,8,2,1},pin61},
     {{ioc,8,2,0},pin62}];
iocs({iob,8,1}) ->
    [{{ioc,8,1,4},pin52},
     {{ioc,8,1,3},pin53},
     {{ioc,8,1,2},pin54},
     {{ioc,8,1,1},pin55},
     {{ioc,8,1,0},pin56}];
iocs({iob,7,0}) ->
    [{{ioc,7,0,2},pin49},
     {{ioc,7,0,1},pin50},
     {{ioc,7,0,0},pin51}];
iocs({iob,6,0}) ->
    [{{ioc,6,0,3},pin43},
     {{ioc,6,0,2},pin44},
     {{ioc,6,0,1},pin47},
     {{ioc,6,0,0},pin48}];
iocs({iob,5,0}) ->
    [{{ioc,5,0,3},pin39},
     {{ioc,5,0,2},pin40},
     {{ioc,5,0,1},pin41},
     {{ioc,5,0,0},pin42}];
iocs({iob,4,0}) ->
    [{{ioc,4,0,2},pin36},
     {{ioc,4,0,1},pin37},
     {{ioc,4,0,0},pin38}];
iocs({iob,3,0}) ->
    [{{ioc,3,0,3},pin30},
     {{ioc,3,0,2},pin33},
     {{ioc,3,0,1},pin34},
     {{ioc,3,0,0},pin35}];
iocs({iob,2,0}) ->
    [{{ioc,2,0,3},pin26},
     {{ioc,2,0,2},pin27},
     {{ioc,2,0,1},pin28},
     {{ioc,2,0,0},pin29}].

-spec pins() -> [pin()].

pins() ->
    [pin2,
     pin3,
     pin4,
     pin5,
     pin6,
     pin7,
     pin8,
     pin12,
     pin14,
     pin15,
     pin16,
     pin17,
     pin18,
     pin19,
     pin20,
     pin21,
     pin98,
     pin99,
     pin100,
     pin92,
     pin95,
     pin96,
     pin97,
     pin89,
     pin90,
     pin91,
     pin85,
     pin86,
     pin87,
     pin88,
     pin81,
     pin82,
     pin83,
     pin84,
     pin75,
     pin76,
     pin77,
     pin78,
     pin70,
     pin71,
     pin72,
     pin73,
     pin74,
     pin64,
     pin66,
     pin67,
     pin68,
     pin69,
     pin57,
     pin58,
     pin61,
     pin62,
     pin52,
     pin53,
     pin54,
     pin55,
     pin56,
     pin49,
     pin50,
     pin51,
     pin43,
     pin44,
     pin47,
     pin48,
     pin39,
     pin40,
     pin41,
     pin42,
     pin36,
     pin37,
     pin38,
     pin30,
     pin33,
     pin34,
     pin35,
     pin26,
     pin27,
     pin28,
     pin29
    ].

-spec top_iocs(x()) -> [{ioc(), pin()}].

top_iocs(2) ->
    [{{ioc,2,5,0},pin98},
     {{ioc,2,5,1},pin99},
     {{ioc,2,5,2},pin100}];
top_iocs(3) ->
    [{{ioc,3,5,0},pin92},
     {{ioc,3,5,1},pin95},
     {{ioc,3,5,2},pin96},
     {{ioc,3,5,3},pin97}];
top_iocs(4) ->
    [{{ioc,4,5,0},pin89},
     {{ioc,4,5,1},pin90},
     {{ioc,4,5,2},pin91}];
top_iocs(5) ->
    [{{ioc,5,5,0},pin85},
     {{ioc,5,5,1},pin86},
     {{ioc,5,5,2},pin87},
     {{ioc,5,5,3},pin88}];
top_iocs(6) ->
    [{{ioc,6,5,0},pin81},
     {{ioc,6,5,1},pin82},
     {{ioc,6,5,2},pin83},
     {{ioc,6,5,3},pin84}];
top_iocs(7) ->
    [{{ioc,7,5,0},pin75},
     {{ioc,7,5,1},pin76},
     {{ioc,7,5,2},pin77},
     {{ioc,7,5,3},pin78}].

-spec top_pins(x()) -> [pin()].

top_pins(2) ->
    [pin98, pin99, pin100];
top_pins(3) ->
    [pin92, pin95, pin96, pin97];
top_pins(4) ->
    [pin89, pin90, pin91];
top_pins(5) ->
    [pin85, pin86, pin87, pin88];
top_pins(6) ->
    [pin81, pin82, pin83, pin84];
top_pins(7) ->
    [pin75, pin76, pin77, pin78].

-spec left_iocs(y()) -> [{ioc(), pin()}].

left_iocs(1) ->
    [{{ioc,1,1,0},pin18},
     {{ioc,1,1,1},pin19},
     {{ioc,1,1,2},pin20},
     {{ioc,1,1,3},pin21}];
left_iocs(2) ->
    [{{ioc,1,2,0},pin14},
     {{ioc,1,2,1},pin15},
     {{ioc,1,2,2},pin16},
     {{ioc,1,2,3},pin17}];
left_iocs(3) ->
    [{{ioc,1,3,0},pin6},
     {{ioc,1,3,1},pin7},
     {{ioc,1,3,2},pin8},
     {{ioc,1,3,3},pin12}];
left_iocs(4) ->
    [{{ioc,1,4,0},pin2},
     {{ioc,1,4,1},pin3},
     {{ioc,1,4,2},pin4},
     {{ioc,1,4,3},pin5}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [pin18, pin19, pin20, pin21];
left_pins(2) ->
    [pin14, pin15, pin16, pin17];
left_pins(3) ->
    [pin6, pin7, pin8, pin12];
left_pins(4) ->
    [pin2, pin3, pin4, pin5].

-spec right_iocs(y()) -> [{ioc(), pin()}].

right_iocs(1) ->
    [{{ioc,8,1,4},pin52},
     {{ioc,8,1,3},pin53},
     {{ioc,8,1,2},pin54},
     {{ioc,8,1,1},pin55},
     {{ioc,8,1,0},pin56}];
right_iocs(2) ->
    [{{ioc,8,2,3},pin57},
     {{ioc,8,2,2},pin58},
     {{ioc,8,2,1},pin61},
     {{ioc,8,2,0},pin62}];
right_iocs(3) ->
    [{{ioc,8,3,4},pin64},
     {{ioc,8,3,3},pin66},
     {{ioc,8,3,2},pin67},
     {{ioc,8,3,1},pin68},
     {{ioc,8,3,0},pin69}];
right_iocs(4) ->
    [{{ioc,8,4,4},pin70},
     {{ioc,8,4,3},pin71},
     {{ioc,8,4,2},pin72},
     {{ioc,8,4,1},pin73},
     {{ioc,8,4,0},pin74}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [pin52, pin53, pin54, pin55, pin56];
right_pins(2) ->
    [pin57, pin58, pin61, pin62];
right_pins(3) ->
    [pin64, pin66, pin67, pin68, pin69];
right_pins(4) ->
    [pin70, pin71, pin72, pin73, pin74].

-spec bottom_iocs(x()) -> [{ioc(), pin()}].

bottom_iocs(2) ->
    [{{ioc,2,0,3},pin26},
     {{ioc,2,0,2},pin27},
     {{ioc,2,0,1},pin28},
     {{ioc,2,0,0},pin29}];
bottom_iocs(3) ->
    [{{ioc,3,0,3},pin30},
     {{ioc,3,0,2},pin33},
     {{ioc,3,0,1},pin34},
     {{ioc,3,0,0},pin35}];
bottom_iocs(4) ->
    [{{ioc,4,0,2},pin36},
     {{ioc,4,0,1},pin37},
     {{ioc,4,0,0},pin38}];
bottom_iocs(5) ->
    [{{ioc,5,0,3},pin39},
     {{ioc,5,0,2},pin40},
     {{ioc,5,0,1},pin41},
     {{ioc,5,0,0},pin42}];
bottom_iocs(6) ->
    [{{ioc,6,0,3},pin43},
     {{ioc,6,0,2},pin44},
     {{ioc,6,0,1},pin47},
     {{ioc,6,0,0},pin48}];
bottom_iocs(7) ->
    [{{ioc,7,0,2},pin49},
     {{ioc,7,0,1},pin50},
     {{ioc,7,0,0},pin51}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(2) ->
    [pin26, pin27, pin28, pin29];
bottom_pins(3) ->
    [pin30, pin33, pin34, pin35];
bottom_pins(4) ->
    [pin36, pin37, pin38];
bottom_pins(5) ->
    [pin39, pin40, pin41, pin42];
bottom_pins(6) ->
    [pin43, pin44, pin47, pin48];
bottom_pins(7) ->
    [pin49, pin50, pin51].

