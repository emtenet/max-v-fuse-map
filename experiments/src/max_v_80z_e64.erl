-module(max_v_80z_e64).

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
    [{pin1,{ioc,1,4,2}},
     {pin2,{ioc,1,4,3}},
     {pin3,{ioc,1,3,0}},
     {pin4,{ioc,1,3,1}},
     {pin5,{ioc,1,3,2}},
     {pin7,{ioc,1,3,3}},
     {pin9,{ioc,1,2,0}},
     {pin10,{ioc,1,2,3}},
     {pin11,{ioc,1,1,1}},
     {pin12,{ioc,1,1,2}},
     {pin13,{ioc,1,1,3}},
     {pin18,{ioc,2,0,3}},
     {pin19,{ioc,2,0,2}},
     {pin20,{ioc,2,0,1}},
     {pin21,{ioc,2,0,0}},
     {pin22,{ioc,3,0,3}},
     {pin24,{ioc,3,0,2}},
     {pin25,{ioc,3,0,1}},
     {pin26,{ioc,5,0,1}},
     {pin27,{ioc,5,0,0}},
     {pin28,{ioc,6,0,3}},
     {pin29,{ioc,6,0,2}},
     {pin30,{ioc,6,0,1}},
     {pin31,{ioc,6,0,0}},
     {pin32,{ioc,7,0,2}},
     {pin33,{ioc,7,0,1}},
     {pin34,{ioc,8,1,4}},
     {pin35,{ioc,8,1,3}},
     {pin36,{ioc,8,1,2}},
     {pin37,{ioc,8,1,1}},
     {pin38,{ioc,8,2,3}},
     {pin40,{ioc,8,2,0}},
     {pin42,{ioc,8,3,4}},
     {pin43,{ioc,8,3,1}},
     {pin44,{ioc,8,3,0}},
     {pin45,{ioc,8,4,4}},
     {pin46,{ioc,8,4,3}},
     {pin47,{ioc,8,4,2}},
     {pin48,{ioc,8,4,1}},
     {pin49,{ioc,6,5,2}},
     {pin50,{ioc,6,5,3}},
     {pin51,{ioc,5,5,0}},
     {pin52,{ioc,5,5,1}},
     {pin53,{ioc,5,5,2}},
     {pin54,{ioc,4,5,0}},
     {pin55,{ioc,4,5,2}},
     {pin56,{ioc,3,5,0}},
     {pin58,{ioc,3,5,1}},
     {pin59,{ioc,3,5,2}},
     {pin60,{ioc,3,5,3}},
     {pin61,{ioc,2,5,0}},
     {pin62,{ioc,2,5,1}},
     {pin63,{ioc,2,5,2}},
     {pin64,{ioc,1,4,1}}
    ].

-spec iocs(iob()) -> [{pin(), ioc()}].

iocs({iob,1,4}) ->
    [{pin64,{ioc,1,4,1}},
     {pin1,{ioc,1,4,2}},
     {pin2,{ioc,1,4,3}}];
iocs({iob,1,3}) ->
    [{pin3,{ioc,1,3,0}},
     {pin4,{ioc,1,3,1}},
     {pin5,{ioc,1,3,2}},
     {pin7,{ioc,1,3,3}}];
iocs({iob,1,2}) ->
    [{pin9,{ioc,1,2,0}},
     {pin10,{ioc,1,2,3}}];
iocs({iob,1,1}) ->
    [{pin11,{ioc,1,1,1}},
     {pin12,{ioc,1,1,2}},
     {pin13,{ioc,1,1,3}}];
iocs({iob,2,5}) ->
    [{pin61,{ioc,2,5,0}},
     {pin62,{ioc,2,5,1}},
     {pin63,{ioc,2,5,2}}];
iocs({iob,3,5}) ->
    [{pin56,{ioc,3,5,0}},
     {pin58,{ioc,3,5,1}},
     {pin59,{ioc,3,5,2}},
     {pin60,{ioc,3,5,3}}];
iocs({iob,4,5}) ->
    [{pin54,{ioc,4,5,0}},
     {pin55,{ioc,4,5,2}}];
iocs({iob,5,5}) ->
    [{pin51,{ioc,5,5,0}},
     {pin52,{ioc,5,5,1}},
     {pin53,{ioc,5,5,2}}];
iocs({iob,6,5}) ->
    [{pin49,{ioc,6,5,2}},
     {pin50,{ioc,6,5,3}}];
iocs({iob,7,5}) ->
    [];
iocs({iob,8,4}) ->
    [{pin48,{ioc,8,4,1}},
     {pin47,{ioc,8,4,2}},
     {pin46,{ioc,8,4,3}},
     {pin45,{ioc,8,4,4}}];
iocs({iob,8,3}) ->
    [{pin44,{ioc,8,3,0}},
     {pin43,{ioc,8,3,1}},
     {pin42,{ioc,8,3,4}}];
iocs({iob,8,2}) ->
    [{pin40,{ioc,8,2,0}},
     {pin38,{ioc,8,2,3}}];
iocs({iob,8,1}) ->
    [{pin37,{ioc,8,1,1}},
     {pin36,{ioc,8,1,2}},
     {pin35,{ioc,8,1,3}},
     {pin34,{ioc,8,1,4}}];
iocs({iob,7,0}) ->
    [{pin33,{ioc,7,0,1}},
     {pin32,{ioc,7,0,2}}];
iocs({iob,6,0}) ->
    [{pin31,{ioc,6,0,0}},
     {pin30,{ioc,6,0,1}},
     {pin29,{ioc,6,0,2}},
     {pin28,{ioc,6,0,3}}];
iocs({iob,5,0}) ->
    [{pin27,{ioc,5,0,0}},
     {pin26,{ioc,5,0,1}}];
iocs({iob,4,0}) ->
    [];
iocs({iob,3,0}) ->
    [{pin25,{ioc,3,0,1}},
     {pin24,{ioc,3,0,2}},
     {pin22,{ioc,3,0,3}}];
iocs({iob,2,0}) ->
    [{pin21,{ioc,2,0,0}},
     {pin20,{ioc,2,0,1}},
     {pin19,{ioc,2,0,2}},
     {pin18,{ioc,2,0,3}}].

-spec pins() -> [pin()].

pins() ->
    [pin1,
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
     pin18,
     pin19,
     pin20,
     pin21,
     pin22,
     pin24,
     pin25,
     pin26,
     pin27,
     pin28,
     pin29,
     pin30,
     pin31,
     pin32,
     pin33,
     pin34,
     pin35,
     pin36,
     pin37,
     pin38,
     pin40,
     pin42,
     pin43,
     pin44,
     pin45,
     pin46,
     pin47,
     pin48,
     pin49,
     pin50,
     pin51,
     pin52,
     pin53,
     pin54,
     pin55,
     pin56,
     pin58,
     pin59,
     pin60,
     pin61,
     pin62,
     pin63,
     pin64
    ].

-spec top_iocs(x()) -> [{pin(), ioc()}].

top_iocs(2) ->
    [{pin61,{ioc,2,5,0}},
     {pin62,{ioc,2,5,1}},
     {pin63,{ioc,2,5,2}}];
top_iocs(3) ->
    [{pin56,{ioc,3,5,0}},
     {pin58,{ioc,3,5,1}},
     {pin59,{ioc,3,5,2}},
     {pin60,{ioc,3,5,3}}];
top_iocs(4) ->
    [{pin54,{ioc,4,5,0}},
     {pin55,{ioc,4,5,2}}];
top_iocs(5) ->
    [{pin51,{ioc,5,5,0}},
     {pin52,{ioc,5,5,1}},
     {pin53,{ioc,5,5,2}}];
top_iocs(6) ->
    [{pin49,{ioc,6,5,2}},
     {pin50,{ioc,6,5,3}}];
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

-spec left_iocs(y()) -> [{pin(), ioc()}].

left_iocs(1) ->
    [{pin11,{ioc,1,1,1}},
     {pin12,{ioc,1,1,2}},
     {pin13,{ioc,1,1,3}}];
left_iocs(2) ->
    [{pin9,{ioc,1,2,0}},
     {pin10,{ioc,1,2,3}}];
left_iocs(3) ->
    [{pin3,{ioc,1,3,0}},
     {pin4,{ioc,1,3,1}},
     {pin5,{ioc,1,3,2}},
     {pin7,{ioc,1,3,3}}];
left_iocs(4) ->
    [{pin64,{ioc,1,4,1}},
     {pin1,{ioc,1,4,2}},
     {pin2,{ioc,1,4,3}}].

-spec left_pins(y()) -> [pin()].

left_pins(1) ->
    [pin11, pin12, pin13];
left_pins(2) ->
    [pin9, pin10];
left_pins(3) ->
    [pin3, pin4, pin5, pin7];
left_pins(4) ->
    [pin1, pin2, pin64].

-spec right_iocs(y()) -> [{pin(), ioc()}].

right_iocs(1) ->
    [{pin37,{ioc,8,1,1}},
     {pin36,{ioc,8,1,2}},
     {pin35,{ioc,8,1,3}},
     {pin34,{ioc,8,1,4}}];
right_iocs(2) ->
    [{pin40,{ioc,8,2,0}},
     {pin38,{ioc,8,2,3}}];
right_iocs(3) ->
    [{pin44,{ioc,8,3,0}},
     {pin43,{ioc,8,3,1}},
     {pin42,{ioc,8,3,4}}];
right_iocs(4) ->
    [{pin48,{ioc,8,4,1}},
     {pin47,{ioc,8,4,2}},
     {pin46,{ioc,8,4,3}},
     {pin45,{ioc,8,4,4}}].

-spec right_pins(y()) -> [pin()].

right_pins(1) ->
    [pin34, pin35, pin36, pin37];
right_pins(2) ->
    [pin38, pin40];
right_pins(3) ->
    [pin42, pin43, pin44];
right_pins(4) ->
    [pin45, pin46, pin47, pin48].

-spec bottom_iocs(x()) -> [{pin(), ioc()}].

bottom_iocs(2) ->
    [{pin21,{ioc,2,0,0}},
     {pin20,{ioc,2,0,1}},
     {pin19,{ioc,2,0,2}},
     {pin18,{ioc,2,0,3}}];
bottom_iocs(3) ->
    [{pin25,{ioc,3,0,1}},
     {pin24,{ioc,3,0,2}},
     {pin22,{ioc,3,0,3}}];
bottom_iocs(4) ->
    [];
bottom_iocs(5) ->
    [{pin27,{ioc,5,0,0}},
     {pin26,{ioc,5,0,1}}];
bottom_iocs(6) ->
    [{pin31,{ioc,6,0,0}},
     {pin30,{ioc,6,0,1}},
     {pin29,{ioc,6,0,2}},
     {pin28,{ioc,6,0,3}}];
bottom_iocs(7) ->
    [{pin33,{ioc,7,0,1}},
     {pin32,{ioc,7,0,2}}].

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

