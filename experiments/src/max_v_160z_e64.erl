-module(max_v_160z_e64).

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
    [{{ioc,1,4,1},pin64},
     {{ioc,1,4,2},pin1},
     {{ioc,1,4,3},pin2},
     {{ioc,1,3,0},pin3},
     {{ioc,1,3,1},pin4},
     {{ioc,1,3,2},pin5},
     {{ioc,1,3,3},pin7},
     {{ioc,1,2,0},pin9},
     {{ioc,1,2,3},pin10},
     {{ioc,1,1,1},pin11},
     {{ioc,1,1,2},pin12},
     {{ioc,1,1,3},pin13},
     {{ioc,2,5,0},pin61},
     {{ioc,2,5,1},pin62},
     {{ioc,2,5,2},pin63},
     {{ioc,3,5,0},pin56},
     {{ioc,3,5,1},pin58},
     {{ioc,3,5,2},pin59},
     {{ioc,3,5,3},pin60},
     {{ioc,4,5,0},pin54},
     {{ioc,4,5,2},pin55},
     {{ioc,5,5,0},pin51},
     {{ioc,5,5,1},pin52},
     {{ioc,5,5,2},pin53},
     {{ioc,6,5,2},pin49},
     {{ioc,6,5,3},pin50},
     {{ioc,8,4,4},pin45},
     {{ioc,8,4,3},pin46},
     {{ioc,8,4,2},pin47},
     {{ioc,8,4,1},pin48},
     {{ioc,8,3,4},pin42},
     {{ioc,8,3,1},pin43},
     {{ioc,8,3,0},pin44},
     {{ioc,8,2,3},pin38},
     {{ioc,8,2,0},pin40},
     {{ioc,8,1,4},pin34},
     {{ioc,8,1,3},pin35},
     {{ioc,8,1,2},pin36},
     {{ioc,8,1,1},pin37},
     {{ioc,7,0,2},pin32},
     {{ioc,7,0,1},pin33},
     {{ioc,6,0,3},pin28},
     {{ioc,6,0,2},pin29},
     {{ioc,6,0,1},pin30},
     {{ioc,6,0,0},pin31},
     {{ioc,5,0,1},pin26},
     {{ioc,5,0,0},pin27},
     {{ioc,3,0,3},pin22},
     {{ioc,3,0,2},pin24},
     {{ioc,3,0,1},pin25},
     {{ioc,2,0,3},pin18},
     {{ioc,2,0,2},pin19},
     {{ioc,2,0,1},pin20},
     {{ioc,2,0,0},pin21}
    ].

-spec iocs(iob()) -> [{ioc(), pin()}].

iocs({iob,1,4}) ->
    [{{ioc,1,4,1},pin64},
     {{ioc,1,4,2},pin1},
     {{ioc,1,4,3},pin2}];
iocs({iob,1,3}) ->
    [{{ioc,1,3,0},pin3},
     {{ioc,1,3,1},pin4},
     {{ioc,1,3,2},pin5},
     {{ioc,1,3,3},pin7}];
iocs({iob,1,2}) ->
    [{{ioc,1,2,0},pin9},
     {{ioc,1,2,3},pin10}];
iocs({iob,1,1}) ->
    [{{ioc,1,1,1},pin11},
     {{ioc,1,1,2},pin12},
     {{ioc,1,1,3},pin13}];
iocs({iob,2,5}) ->
    [{{ioc,2,5,0},pin61},
     {{ioc,2,5,1},pin62},
     {{ioc,2,5,2},pin63}];
iocs({iob,3,5}) ->
    [{{ioc,3,5,0},pin56},
     {{ioc,3,5,1},pin58},
     {{ioc,3,5,2},pin59},
     {{ioc,3,5,3},pin60}];
iocs({iob,4,5}) ->
    [{{ioc,4,5,0},pin54},
     {{ioc,4,5,2},pin55}];
iocs({iob,5,5}) ->
    [{{ioc,5,5,0},pin51},
     {{ioc,5,5,1},pin52},
     {{ioc,5,5,2},pin53}];
iocs({iob,6,5}) ->
    [{{ioc,6,5,2},pin49},
     {{ioc,6,5,3},pin50}];
iocs({iob,7,5}) ->
    [];
iocs({iob,8,4}) ->
    [{{ioc,8,4,4},pin45},
     {{ioc,8,4,3},pin46},
     {{ioc,8,4,2},pin47},
     {{ioc,8,4,1},pin48}];
iocs({iob,8,3}) ->
    [{{ioc,8,3,4},pin42},
     {{ioc,8,3,1},pin43},
     {{ioc,8,3,0},pin44}];
iocs({iob,8,2}) ->
    [{{ioc,8,2,3},pin38},
     {{ioc,8,2,0},pin40}];
iocs({iob,8,1}) ->
    [{{ioc,8,1,4},pin34},
     {{ioc,8,1,3},pin35},
     {{ioc,8,1,2},pin36},
     {{ioc,8,1,1},pin37}];
iocs({iob,7,0}) ->
    [{{ioc,7,0,2},pin32},
     {{ioc,7,0,1},pin33}];
iocs({iob,6,0}) ->
    [{{ioc,6,0,3},pin28},
     {{ioc,6,0,2},pin29},
     {{ioc,6,0,1},pin30},
     {{ioc,6,0,0},pin31}];
iocs({iob,5,0}) ->
    [{{ioc,5,0,1},pin26},
     {{ioc,5,0,0},pin27}];
iocs({iob,4,0}) ->
    [];
iocs({iob,3,0}) ->
    [{{ioc,3,0,3},pin22},
     {{ioc,3,0,2},pin24},
     {{ioc,3,0,1},pin25}];
iocs({iob,2,0}) ->
    [{{ioc,2,0,3},pin18},
     {{ioc,2,0,2},pin19},
     {{ioc,2,0,1},pin20},
     {{ioc,2,0,0},pin21}].

-spec pins() -> [pin()].

pins() ->
    [pin64,
     pin1,
     pin2,
     pin3,
     pin4,
     pin5,
     pin7,
     pin9,
     pin10,
     pin11,
     pin12,
     pin13,
     pin61,
     pin62,
     pin63,
     pin56,
     pin58,
     pin59,
     pin60,
     pin54,
     pin55,
     pin51,
     pin52,
     pin53,
     pin49,
     pin50,
     pin45,
     pin46,
     pin47,
     pin48,
     pin42,
     pin43,
     pin44,
     pin38,
     pin40,
     pin34,
     pin35,
     pin36,
     pin37,
     pin32,
     pin33,
     pin28,
     pin29,
     pin30,
     pin31,
     pin26,
     pin27,
     pin22,
     pin24,
     pin25,
     pin18,
     pin19,
     pin20,
     pin21
    ].

-spec top_iocs(x()) -> [{ioc(), pin()}].

top_iocs(2) ->
    [{{ioc,2,5,0},pin61},
     {{ioc,2,5,1},pin62},
     {{ioc,2,5,2},pin63}];
top_iocs(3) ->
    [{{ioc,3,5,0},pin56},
     {{ioc,3,5,1},pin58},
     {{ioc,3,5,2},pin59},
     {{ioc,3,5,3},pin60}];
top_iocs(4) ->
    [{{ioc,4,5,0},pin54},
     {{ioc,4,5,2},pin55}];
top_iocs(5) ->
    [{{ioc,5,5,0},pin51},
     {{ioc,5,5,1},pin52},
     {{ioc,5,5,2},pin53}];
top_iocs(6) ->
    [{{ioc,6,5,2},pin49},
     {{ioc,6,5,3},pin50}];
top_iocs(7) ->
    [].

-spec top_pins(x()) -> [pin()].

top_pins(2) ->
    [pin61, pin62, pin63];
top_pins(3) ->
    [pin56, pin58, pin59, pin60];
top_pins(4) ->
    [pin54, pin55];
top_pins(5) ->
    [pin51, pin52, pin53];
top_pins(6) ->
    [pin49, pin50];
top_pins(7) ->
    [].

-spec left_iocs(y()) -> [{ioc(), pin()}].

left_iocs(1) ->
    [{{ioc,1,1,1},pin11},
     {{ioc,1,1,2},pin12},
     {{ioc,1,1,3},pin13}];
left_iocs(2) ->
    [{{ioc,1,2,0},pin9},
     {{ioc,1,2,3},pin10}];
left_iocs(3) ->
    [{{ioc,1,3,0},pin3},
     {{ioc,1,3,1},pin4},
     {{ioc,1,3,2},pin5},
     {{ioc,1,3,3},pin7}];
left_iocs(4) ->
    [{{ioc,1,4,1},pin64},
     {{ioc,1,4,2},pin1},
     {{ioc,1,4,3},pin2}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [pin11, pin12, pin13];
left_pins(2) ->
    [pin9, pin10];
left_pins(3) ->
    [pin3, pin4, pin5, pin7];
left_pins(4) ->
    [pin64, pin1, pin2].

-spec right_iocs(y()) -> [{ioc(), pin()}].

right_iocs(1) ->
    [{{ioc,8,1,4},pin34},
     {{ioc,8,1,3},pin35},
     {{ioc,8,1,2},pin36},
     {{ioc,8,1,1},pin37}];
right_iocs(2) ->
    [{{ioc,8,2,3},pin38},
     {{ioc,8,2,0},pin40}];
right_iocs(3) ->
    [{{ioc,8,3,4},pin42},
     {{ioc,8,3,1},pin43},
     {{ioc,8,3,0},pin44}];
right_iocs(4) ->
    [{{ioc,8,4,4},pin45},
     {{ioc,8,4,3},pin46},
     {{ioc,8,4,2},pin47},
     {{ioc,8,4,1},pin48}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [pin34, pin35, pin36, pin37];
right_pins(2) ->
    [pin38, pin40];
right_pins(3) ->
    [pin42, pin43, pin44];
right_pins(4) ->
    [pin45, pin46, pin47, pin48].

-spec bottom_iocs(x()) -> [{ioc(), pin()}].

bottom_iocs(2) ->
    [{{ioc,2,0,3},pin18},
     {{ioc,2,0,2},pin19},
     {{ioc,2,0,1},pin20},
     {{ioc,2,0,0},pin21}];
bottom_iocs(3) ->
    [{{ioc,3,0,3},pin22},
     {{ioc,3,0,2},pin24},
     {{ioc,3,0,1},pin25}];
bottom_iocs(4) ->
    [];
bottom_iocs(5) ->
    [{{ioc,5,0,1},pin26},
     {{ioc,5,0,0},pin27}];
bottom_iocs(6) ->
    [{{ioc,6,0,3},pin28},
     {{ioc,6,0,2},pin29},
     {{ioc,6,0,1},pin30},
     {{ioc,6,0,0},pin31}];
bottom_iocs(7) ->
    [{{ioc,7,0,2},pin32},
     {{ioc,7,0,1},pin33}].

-spec bottom_pins(x()) -> [pin()].

bottom_pins(2) ->
    [pin18, pin19, pin20, pin21];
bottom_pins(3) ->
    [pin22, pin24, pin25];
bottom_pins(4) ->
    [];
bottom_pins(5) ->
    [pin26, pin27];
bottom_pins(6) ->
    [pin28, pin29, pin30, pin31];
bottom_pins(7) ->
    [pin32, pin33].

