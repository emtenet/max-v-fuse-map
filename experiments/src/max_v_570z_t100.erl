-module(max_v_570z_t100).

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
    [{{ioc,0,4,5},pin19},
     {{ioc,0,5,0},pin12},
     {{ioc,0,5,1},pin14},
     {{ioc,0,5,2},pin15},
     {{ioc,0,5,3},pin16},
     {{ioc,0,5,4},pin17},
     {{ioc,0,5,5},pin18},
     {{ioc,0,7,0},pin4},
     {{ioc,0,7,2},pin5},
     {{ioc,0,7,3},pin6},
     {{ioc,0,7,4},pin7},
     {{ioc,1,8,0},pin2},
     {{ioc,1,8,2},pin3},
     {{ioc,3,8,0},pin100},
     {{ioc,3,8,3},pin1},
     {{ioc,4,8,1},pin98},
     {{ioc,4,8,2},pin99},
     {{ioc,5,8,1},pin96},
     {{ioc,5,8,2},pin97},
     {{ioc,6,8,2},pin91},
     {{ioc,6,8,3},pin92},
     {{ioc,7,8,0},pin87},
     {{ioc,7,8,1},pin89},
     {{ioc,8,8,0},pin83},
     {{ioc,8,8,1},pin84},
     {{ioc,8,8,2},pin85},
     {{ioc,8,8,3},pin86},
     {{ioc,9,8,0},pin82},
     {{ioc,10,8,3},pin81},
     {{ioc,12,8,1},pin76},
     {{ioc,12,8,2},pin77},
     {{ioc,12,8,3},pin78},
     {{ioc,13,7,5},pin73},
     {{ioc,13,7,3},pin74},
     {{ioc,13,7,1},pin75},
     {{ioc,13,6,5},pin70},
     {{ioc,13,6,2},pin71},
     {{ioc,13,6,1},pin72},
     {{ioc,13,5,0},pin69},
     {{ioc,13,4,4},pin62},
     {{ioc,13,4,3},pin64},
     {{ioc,13,4,2},pin66},
     {{ioc,13,4,1},pin67},
     {{ioc,13,4,0},pin68},
     {{ioc,13,3,1},pin61},
     {{ioc,13,2,4},pin55},
     {{ioc,13,2,2},pin56},
     {{ioc,13,2,1},pin57},
     {{ioc,13,2,0},pin58},
     {{ioc,13,1,4},pin52},
     {{ioc,13,1,3},pin53},
     {{ioc,13,1,1},pin54},
     {{ioc,12,0,2},pin51},
     {{ioc,11,0,2},pin50},
     {{ioc,10,0,2},pin47},
     {{ioc,10,0,1},pin48},
     {{ioc,10,0,0},pin49},
     {{ioc,8,3,3},pin43},
     {{ioc,8,3,2},pin44},
     {{ioc,7,3,3},pin38},
     {{ioc,7,3,2},pin40},
     {{ioc,7,3,1},pin41},
     {{ioc,7,3,0},pin42},
     {{ioc,6,3,3},pin33},
     {{ioc,6,3,2},pin34},
     {{ioc,6,3,1},pin35},
     {{ioc,6,3,0},pin36},
     {{ioc,4,3,3},pin28},
     {{ioc,4,3,2},pin29},
     {{ioc,4,3,1},pin30},
     {{ioc,3,3,2},pin26},
     {{ioc,3,3,1},pin27},
     {{ioc,1,3,3},pin20},
     {{ioc,1,3,1},pin21}
    ].

-spec iocs(iob()) -> [{ioc(), pin()}].

iocs({iob,0,4}) ->
    [{{ioc,0,4,5},pin19}];
iocs({iob,0,5}) ->
    [{{ioc,0,5,0},pin12},
     {{ioc,0,5,1},pin14},
     {{ioc,0,5,2},pin15},
     {{ioc,0,5,3},pin16},
     {{ioc,0,5,4},pin17},
     {{ioc,0,5,5},pin18}];
iocs({iob,0,6}) ->
    [];
iocs({iob,0,7}) ->
    [{{ioc,0,7,0},pin4},
     {{ioc,0,7,2},pin5},
     {{ioc,0,7,3},pin6},
     {{ioc,0,7,4},pin7}];
iocs({iob,1,8}) ->
    [{{ioc,1,8,0},pin2},
     {{ioc,1,8,2},pin3}];
iocs({iob,2,8}) ->
    [];
iocs({iob,3,8}) ->
    [{{ioc,3,8,0},pin100},
     {{ioc,3,8,3},pin1}];
iocs({iob,4,8}) ->
    [{{ioc,4,8,1},pin98},
     {{ioc,4,8,2},pin99}];
iocs({iob,5,8}) ->
    [{{ioc,5,8,1},pin96},
     {{ioc,5,8,2},pin97}];
iocs({iob,6,8}) ->
    [{{ioc,6,8,2},pin91},
     {{ioc,6,8,3},pin92}];
iocs({iob,7,8}) ->
    [{{ioc,7,8,0},pin87},
     {{ioc,7,8,1},pin89}];
iocs({iob,8,8}) ->
    [{{ioc,8,8,0},pin83},
     {{ioc,8,8,1},pin84},
     {{ioc,8,8,2},pin85},
     {{ioc,8,8,3},pin86}];
iocs({iob,9,8}) ->
    [{{ioc,9,8,0},pin82}];
iocs({iob,10,8}) ->
    [{{ioc,10,8,3},pin81}];
iocs({iob,11,8}) ->
    [];
iocs({iob,12,8}) ->
    [{{ioc,12,8,1},pin76},
     {{ioc,12,8,2},pin77},
     {{ioc,12,8,3},pin78}];
iocs({iob,13,7}) ->
    [{{ioc,13,7,5},pin73},
     {{ioc,13,7,3},pin74},
     {{ioc,13,7,1},pin75}];
iocs({iob,13,6}) ->
    [{{ioc,13,6,5},pin70},
     {{ioc,13,6,2},pin71},
     {{ioc,13,6,1},pin72}];
iocs({iob,13,5}) ->
    [{{ioc,13,5,0},pin69}];
iocs({iob,13,4}) ->
    [{{ioc,13,4,4},pin62},
     {{ioc,13,4,3},pin64},
     {{ioc,13,4,2},pin66},
     {{ioc,13,4,1},pin67},
     {{ioc,13,4,0},pin68}];
iocs({iob,13,3}) ->
    [{{ioc,13,3,1},pin61}];
iocs({iob,13,2}) ->
    [{{ioc,13,2,4},pin55},
     {{ioc,13,2,2},pin56},
     {{ioc,13,2,1},pin57},
     {{ioc,13,2,0},pin58}];
iocs({iob,13,1}) ->
    [{{ioc,13,1,4},pin52},
     {{ioc,13,1,3},pin53},
     {{ioc,13,1,1},pin54}];
iocs({iob,12,0}) ->
    [{{ioc,12,0,2},pin51}];
iocs({iob,11,0}) ->
    [{{ioc,11,0,2},pin50}];
iocs({iob,10,0}) ->
    [{{ioc,10,0,2},pin47},
     {{ioc,10,0,1},pin48},
     {{ioc,10,0,0},pin49}];
iocs({iob,8,3}) ->
    [{{ioc,8,3,3},pin43},
     {{ioc,8,3,2},pin44}];
iocs({iob,7,3}) ->
    [{{ioc,7,3,3},pin38},
     {{ioc,7,3,2},pin40},
     {{ioc,7,3,1},pin41},
     {{ioc,7,3,0},pin42}];
iocs({iob,6,3}) ->
    [{{ioc,6,3,3},pin33},
     {{ioc,6,3,2},pin34},
     {{ioc,6,3,1},pin35},
     {{ioc,6,3,0},pin36}];
iocs({iob,5,3}) ->
    [];
iocs({iob,4,3}) ->
    [{{ioc,4,3,3},pin28},
     {{ioc,4,3,2},pin29},
     {{ioc,4,3,1},pin30}];
iocs({iob,3,3}) ->
    [{{ioc,3,3,2},pin26},
     {{ioc,3,3,1},pin27}];
iocs({iob,2,3}) ->
    [];
iocs({iob,1,3}) ->
    [{{ioc,1,3,3},pin20},
     {{ioc,1,3,1},pin21}].

-spec pins() -> [pin()].

pins() ->
    [pin19,
     pin12,
     pin14,
     pin15,
     pin16,
     pin17,
     pin18,
     pin4,
     pin5,
     pin6,
     pin7,
     pin2,
     pin3,
     pin100,
     pin1,
     pin98,
     pin99,
     pin96,
     pin97,
     pin91,
     pin92,
     pin87,
     pin89,
     pin83,
     pin84,
     pin85,
     pin86,
     pin82,
     pin81,
     pin76,
     pin77,
     pin78,
     pin73,
     pin74,
     pin75,
     pin70,
     pin71,
     pin72,
     pin69,
     pin62,
     pin64,
     pin66,
     pin67,
     pin68,
     pin61,
     pin55,
     pin56,
     pin57,
     pin58,
     pin52,
     pin53,
     pin54,
     pin51,
     pin50,
     pin47,
     pin48,
     pin49,
     pin43,
     pin44,
     pin38,
     pin40,
     pin41,
     pin42,
     pin33,
     pin34,
     pin35,
     pin36,
     pin28,
     pin29,
     pin30,
     pin26,
     pin27,
     pin20,
     pin21
    ].

-spec top_iocs(x()) -> [{ioc(), pin()}].

top_iocs(1) ->
    [{{ioc,1,8,0},pin2},
     {{ioc,1,8,2},pin3}];
top_iocs(2) ->
    [];
top_iocs(3) ->
    [{{ioc,3,8,0},pin100},
     {{ioc,3,8,3},pin1}];
top_iocs(4) ->
    [{{ioc,4,8,1},pin98},
     {{ioc,4,8,2},pin99}];
top_iocs(5) ->
    [{{ioc,5,8,1},pin96},
     {{ioc,5,8,2},pin97}];
top_iocs(6) ->
    [{{ioc,6,8,2},pin91},
     {{ioc,6,8,3},pin92}];
top_iocs(7) ->
    [{{ioc,7,8,0},pin87},
     {{ioc,7,8,1},pin89}];
top_iocs(8) ->
    [{{ioc,8,8,0},pin83},
     {{ioc,8,8,1},pin84},
     {{ioc,8,8,2},pin85},
     {{ioc,8,8,3},pin86}];
top_iocs(9) ->
    [{{ioc,9,8,0},pin82}];
top_iocs(10) ->
    [{{ioc,10,8,3},pin81}];
top_iocs(11) ->
    [];
top_iocs(12) ->
    [{{ioc,12,8,1},pin76},
     {{ioc,12,8,2},pin77},
     {{ioc,12,8,3},pin78}].

-spec top_pins(x()) -> [pin()].

top_pins(1) ->
    [pin2, pin3];
top_pins(2) ->
    [];
top_pins(3) ->
    [pin100, pin1];
top_pins(4) ->
    [pin98, pin99];
top_pins(5) ->
    [pin96, pin97];
top_pins(6) ->
    [pin91, pin92];
top_pins(7) ->
    [pin87, pin89];
top_pins(8) ->
    [pin83, pin84, pin85, pin86];
top_pins(9) ->
    [pin82];
top_pins(10) ->
    [pin81];
top_pins(11) ->
    [];
top_pins(12) ->
    [pin76, pin77, pin78].

-spec left_iocs(y()) -> [{ioc(), pin()}].

left_iocs(1) ->
    [];
left_iocs(2) ->
    [];
left_iocs(3) ->
    [];
left_iocs(4) ->
    [{{ioc,0,4,5},pin19}];
left_iocs(5) ->
    [{{ioc,0,5,0},pin12},
     {{ioc,0,5,1},pin14},
     {{ioc,0,5,2},pin15},
     {{ioc,0,5,3},pin16},
     {{ioc,0,5,4},pin17},
     {{ioc,0,5,5},pin18}];
left_iocs(6) ->
    [];
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
    [pin19];
left_pins(5) ->
    [pin12, pin14, pin15, pin16, pin17, pin18];
left_pins(6) ->
    [];
left_pins(7) ->
    [pin4, pin5, pin6, pin7].

-spec right_iocs(y()) -> [{ioc(), pin()}].

right_iocs(1) ->
    [{{ioc,13,1,4},pin52},
     {{ioc,13,1,3},pin53},
     {{ioc,13,1,1},pin54}];
right_iocs(2) ->
    [{{ioc,13,2,4},pin55},
     {{ioc,13,2,2},pin56},
     {{ioc,13,2,1},pin57},
     {{ioc,13,2,0},pin58}];
right_iocs(3) ->
    [{{ioc,13,3,1},pin61}];
right_iocs(4) ->
    [{{ioc,13,4,4},pin62},
     {{ioc,13,4,3},pin64},
     {{ioc,13,4,2},pin66},
     {{ioc,13,4,1},pin67},
     {{ioc,13,4,0},pin68}];
right_iocs(5) ->
    [{{ioc,13,5,0},pin69}];
right_iocs(6) ->
    [{{ioc,13,6,5},pin70},
     {{ioc,13,6,2},pin71},
     {{ioc,13,6,1},pin72}];
right_iocs(7) ->
    [{{ioc,13,7,5},pin73},
     {{ioc,13,7,3},pin74},
     {{ioc,13,7,1},pin75}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [pin52, pin53, pin54];
right_pins(2) ->
    [pin55, pin56, pin57, pin58];
right_pins(3) ->
    [pin61];
right_pins(4) ->
    [pin62, pin64, pin66, pin67, pin68];
right_pins(5) ->
    [pin69];
right_pins(6) ->
    [pin70, pin71, pin72];
right_pins(7) ->
    [pin73, pin74, pin75].

-spec bottom_iocs(x()) -> [{ioc(), pin()}].

bottom_iocs(1) ->
    [{{ioc,1,3,3},pin20},
     {{ioc,1,3,1},pin21}];
bottom_iocs(2) ->
    [];
bottom_iocs(3) ->
    [{{ioc,3,3,2},pin26},
     {{ioc,3,3,1},pin27}];
bottom_iocs(4) ->
    [{{ioc,4,3,3},pin28},
     {{ioc,4,3,2},pin29},
     {{ioc,4,3,1},pin30}];
bottom_iocs(5) ->
    [];
bottom_iocs(6) ->
    [{{ioc,6,3,3},pin33},
     {{ioc,6,3,2},pin34},
     {{ioc,6,3,1},pin35},
     {{ioc,6,3,0},pin36}];
bottom_iocs(7) ->
    [{{ioc,7,3,3},pin38},
     {{ioc,7,3,2},pin40},
     {{ioc,7,3,1},pin41},
     {{ioc,7,3,0},pin42}];
bottom_iocs(8) ->
    [{{ioc,8,3,3},pin43},
     {{ioc,8,3,2},pin44}];
bottom_iocs(9) ->
    [];
bottom_iocs(10) ->
    [{{ioc,10,0,2},pin47},
     {{ioc,10,0,1},pin48},
     {{ioc,10,0,0},pin49}];
bottom_iocs(11) ->
    [{{ioc,11,0,2},pin50}];
bottom_iocs(12) ->
    [{{ioc,12,0,2},pin51}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(1) ->
    [pin20, pin21];
bottom_pins(2) ->
    [];
bottom_pins(3) ->
    [pin26, pin27];
bottom_pins(4) ->
    [pin28, pin29, pin30];
bottom_pins(5) ->
    [];
bottom_pins(6) ->
    [pin33, pin34, pin35, pin36];
bottom_pins(7) ->
    [pin38, pin40, pin41, pin42];
bottom_pins(8) ->
    [pin43, pin44];
bottom_pins(9) ->
    [];
bottom_pins(10) ->
    [pin47, pin48, pin49];
bottom_pins(11) ->
    [pin50];
bottom_pins(12) ->
    [pin51].

