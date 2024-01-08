-module(max_v_2210z_fast_out).

-export([iocs/0]).

-type ioc() :: ioc:ioc().
-type lc() :: lc:lc().

-spec iocs() -> [{ioc(), lc(), left | right}].

iocs() ->
    [{{ioc,0,4,0},{lc,1,4,2},left},
     {{ioc,0,4,1},{lc,1,4,3},left},
     {{ioc,0,4,2},{lc,1,4,4},left},
     {{ioc,0,4,3},{lc,1,4,9},left},
     {{ioc,0,4,4},{lc,1,4,8},left},
     {{ioc,0,4,5},{lc,1,4,7},left},
     {{ioc,0,4,6},{lc,1,4,6},left},
     {{ioc,0,5,0},{lc,1,5,2},left},
     {{ioc,0,5,1},{lc,1,5,3},left},
     {{ioc,0,5,2},{lc,1,5,4},left},
     {{ioc,0,5,3},{lc,1,5,9},left},
     {{ioc,0,5,4},{lc,1,5,8},left},
     {{ioc,0,5,5},{lc,1,5,7},left},
     {{ioc,0,5,6},{lc,1,5,6},left},
     {{ioc,0,6,0},{lc,1,6,2},left},
     {{ioc,0,6,1},{lc,1,6,3},left},
     {{ioc,0,6,2},{lc,1,6,4},left},
     {{ioc,0,6,3},{lc,1,6,9},left},
     {{ioc,0,6,4},{lc,1,6,8},left},
     {{ioc,0,6,5},{lc,1,6,7},left},
     {{ioc,0,7,0},{lc,1,7,2},left},
     {{ioc,0,7,1},{lc,1,7,3},left},
     {{ioc,0,7,2},{lc,1,7,4},left},
     {{ioc,0,7,3},{lc,1,7,9},left},
     {{ioc,0,7,4},{lc,1,7,8},left},
     {{ioc,0,7,5},{lc,1,7,7},left},
     {{ioc,0,7,6},{lc,1,7,6},left},
     {{ioc,0,8,0},{lc,1,8,2},left},
     {{ioc,0,8,1},{lc,1,8,3},left},
     {{ioc,0,8,2},{lc,1,8,4},left},
     {{ioc,0,8,3},{lc,1,8,9},left},
     {{ioc,0,8,4},{lc,1,8,8},left},
     {{ioc,0,8,5},{lc,1,8,7},left},
     {{ioc,0,8,6},{lc,1,8,6},left},
     {{ioc,0,9,0},{lc,1,9,2},left},
     {{ioc,0,9,1},{lc,1,9,3},left},
     {{ioc,0,9,2},{lc,1,9,4},left},
     {{ioc,0,9,3},{lc,1,9,9},left},
     {{ioc,0,9,4},{lc,1,9,8},left},
     {{ioc,0,9,5},{lc,1,9,7},left},
     {{ioc,0,9,6},{lc,1,9,6},left},
     {{ioc,0,10,0},{lc,1,10,2},left},
     {{ioc,0,10,1},{lc,1,10,3},left},
     {{ioc,0,10,2},{lc,1,10,4},left},
     {{ioc,0,10,3},{lc,1,10,9},left},
     {{ioc,0,10,4},{lc,1,10,8},left},
     {{ioc,0,10,5},{lc,1,10,7},left},
     {{ioc,0,10,6},{lc,1,10,6},left},
     {{ioc,0,11,0},{lc,1,11,2},left},
     {{ioc,0,11,1},{lc,1,11,3},left},
     {{ioc,0,11,2},{lc,1,11,4},left},
     {{ioc,0,11,3},{lc,1,11,9},left},
     {{ioc,0,11,4},{lc,1,11,8},left},
     {{ioc,0,11,5},{lc,1,11,7},left},
     {{ioc,0,12,0},{lc,1,12,2},left},
     {{ioc,0,12,1},{lc,1,12,3},left},
     {{ioc,0,12,2},{lc,1,12,4},left},
     {{ioc,0,12,3},{lc,1,12,9},left},
     {{ioc,0,12,4},{lc,1,12,8},left},
     {{ioc,0,12,5},{lc,1,12,7},left},
     {{ioc,0,12,6},{lc,1,12,6},left},
     {{ioc,0,13,0},{lc,1,13,2},left},
     {{ioc,0,13,1},{lc,1,13,3},left},
     {{ioc,0,13,2},{lc,1,13,4},left},
     {{ioc,0,13,3},{lc,1,13,9},left},
     {{ioc,0,13,4},{lc,1,13,8},left},
     {{ioc,0,13,5},{lc,1,13,7},left},
     {{ioc,0,13,6},{lc,1,13,6},left},
     {{ioc,1,3,0},{lc,1,4,5},right},
     {{ioc,1,3,1},{lc,1,4,6},right},
     {{ioc,1,3,2},{lc,1,4,7},right},
     {{ioc,1,14,0},{lc,1,13,3},right},
     {{ioc,1,14,1},{lc,1,13,2},right},
     {{ioc,1,14,2},{lc,1,13,1},right},
     {{ioc,1,14,3},{lc,1,13,0},right},
     {{ioc,2,3,0},{lc,2,4,5},right},
     {{ioc,2,3,1},{lc,2,4,6},right},
     {{ioc,2,3,2},{lc,2,4,7},right},
     {{ioc,2,3,3},{lc,2,4,8},right},
     {{ioc,2,14,0},{lc,2,13,3},right},
     {{ioc,2,14,1},{lc,2,13,2},right},
     {{ioc,2,14,2},{lc,2,13,1},right},
     {{ioc,3,3,0},{lc,3,4,5},right},
     {{ioc,3,3,1},{lc,3,4,6},right},
     {{ioc,3,3,2},{lc,3,4,7},right},
     {{ioc,3,14,0},{lc,3,13,3},right},
     {{ioc,3,14,1},{lc,3,13,2},right},
     {{ioc,3,14,2},{lc,3,13,1},right},
     {{ioc,3,14,3},{lc,3,13,0},right},
     {{ioc,4,3,0},{lc,4,4,5},right},
     {{ioc,4,3,1},{lc,4,4,6},right},
     {{ioc,4,3,2},{lc,4,4,7},right},
     {{ioc,4,3,3},{lc,4,4,8},right},
     {{ioc,4,14,0},{lc,4,13,3},right},
     {{ioc,4,14,1},{lc,4,13,2},right},
     {{ioc,4,14,2},{lc,4,13,1},right},
     {{ioc,5,3,0},{lc,5,4,5},right},
     {{ioc,5,3,1},{lc,5,4,6},right},
     {{ioc,5,3,2},{lc,5,4,7},right},
     {{ioc,5,14,0},{lc,5,13,3},right},
     {{ioc,5,14,1},{lc,5,13,2},right},
     {{ioc,5,14,2},{lc,5,13,1},right},
     {{ioc,6,3,0},{lc,6,4,5},right},
     {{ioc,6,3,1},{lc,6,4,6},right},
     {{ioc,6,3,2},{lc,6,4,7},right},
     {{ioc,6,3,3},{lc,6,4,8},right},
     {{ioc,6,14,0},{lc,6,13,3},right},
     {{ioc,6,14,1},{lc,6,13,2},right},
     {{ioc,6,14,2},{lc,6,13,1},right},
     {{ioc,7,3,1},{lc,7,4,6},right},
     {{ioc,7,3,2},{lc,7,4,7},right},
     {{ioc,7,14,0},{lc,7,13,3},right},
     {{ioc,7,14,1},{lc,7,13,2},right},
     {{ioc,7,14,2},{lc,7,13,1},right},
     {{ioc,8,3,0},{lc,8,4,5},right},
     {{ioc,8,3,1},{lc,8,4,6},right},
     {{ioc,8,3,2},{lc,8,4,7},right},
     {{ioc,8,3,3},{lc,8,4,8},right},
     {{ioc,8,14,0},{lc,8,13,3},right},
     {{ioc,8,14,1},{lc,8,13,2},right},
     {{ioc,8,14,2},{lc,8,13,1},right},
     {{ioc,9,3,0},{lc,9,4,5},right},
     {{ioc,9,3,1},{lc,9,4,6},right},
     {{ioc,9,3,2},{lc,9,4,7},right},
     {{ioc,9,14,0},{lc,9,13,3},right},
     {{ioc,9,14,1},{lc,9,13,2},right},
     {{ioc,9,14,2},{lc,9,13,1},right},
     {{ioc,9,14,3},{lc,9,13,0},right},
     {{ioc,10,3,0},{lc,10,4,5},right},
     {{ioc,10,3,1},{lc,10,4,6},right},
     {{ioc,10,3,2},{lc,10,4,7},right},
     {{ioc,10,3,3},{lc,10,4,8},right},
     {{ioc,10,14,0},{lc,10,13,3},right},
     {{ioc,10,14,1},{lc,10,13,2},right},
     {{ioc,10,14,2},{lc,10,13,1},right},
     {{ioc,11,3,0},{lc,11,4,5},right},
     {{ioc,11,3,1},{lc,11,4,6},right},
     {{ioc,11,3,2},{lc,11,4,7},right},
     {{ioc,11,14,0},{lc,11,13,3},right},
     {{ioc,11,14,1},{lc,11,13,2},right},
     {{ioc,11,14,2},{lc,11,13,1},right},
     {{ioc,11,14,3},{lc,11,13,0},right},
     {{ioc,12,3,0},{lc,12,4,5},right},
     {{ioc,12,3,1},{lc,12,4,6},right},
     {{ioc,12,3,2},{lc,12,4,7},right},
     {{ioc,12,3,3},{lc,12,4,8},right},
     {{ioc,12,14,0},{lc,12,13,3},right},
     {{ioc,12,14,1},{lc,12,13,2},right},
     {{ioc,12,14,2},{lc,12,13,1},right},
     {{ioc,13,14,0},{lc,13,13,3},right},
     {{ioc,13,14,1},{lc,13,13,2},right},
     {{ioc,13,14,2},{lc,13,13,1},right},
     {{ioc,14,0,0},{lc,14,1,5},right},
     {{ioc,14,0,1},{lc,14,1,6},right},
     {{ioc,14,0,2},{lc,14,1,7},right},
     {{ioc,14,14,0},{lc,14,13,3},right},
     {{ioc,14,14,1},{lc,14,13,2},right},
     {{ioc,14,14,2},{lc,14,13,1},right},
     {{ioc,15,0,0},{lc,15,1,5},right},
     {{ioc,15,0,1},{lc,15,1,6},right},
     {{ioc,15,0,2},{lc,15,1,7},right},
     {{ioc,15,0,3},{lc,15,1,8},right},
     {{ioc,15,14,0},{lc,15,13,3},right},
     {{ioc,15,14,1},{lc,15,13,2},right},
     {{ioc,15,14,2},{lc,15,13,1},right},
     {{ioc,16,0,0},{lc,16,1,5},right},
     {{ioc,16,0,1},{lc,16,1,6},right},
     {{ioc,16,0,2},{lc,16,1,7},right},
     {{ioc,16,14,0},{lc,16,13,3},right},
     {{ioc,16,14,1},{lc,16,13,2},right},
     {{ioc,16,14,2},{lc,16,13,1},right},
     {{ioc,17,0,0},{lc,17,1,5},right},
     {{ioc,17,0,1},{lc,17,1,6},right},
     {{ioc,17,0,2},{lc,17,1,7},right},
     {{ioc,17,0,3},{lc,17,1,8},right},
     {{ioc,17,14,0},{lc,17,13,3},right},
     {{ioc,17,14,1},{lc,17,13,2},right},
     {{ioc,17,14,2},{lc,17,13,1},right},
     {{ioc,17,14,3},{lc,17,13,0},right},
     {{ioc,18,0,0},{lc,18,1,5},right},
     {{ioc,18,0,1},{lc,18,1,6},right},
     {{ioc,18,0,2},{lc,18,1,7},right},
     {{ioc,18,14,0},{lc,18,13,3},right},
     {{ioc,18,14,1},{lc,18,13,2},right},
     {{ioc,18,14,2},{lc,18,13,1},right},
     {{ioc,19,0,0},{lc,19,1,5},right},
     {{ioc,19,0,1},{lc,19,1,6},right},
     {{ioc,19,0,2},{lc,19,1,7},right},
     {{ioc,19,0,3},{lc,19,1,8},right},
     {{ioc,19,14,0},{lc,19,13,3},right},
     {{ioc,19,14,1},{lc,19,13,2},right},
     {{ioc,19,14,2},{lc,19,13,1},right},
     {{ioc,19,14,3},{lc,19,13,0},right},
     {{ioc,20,0,0},{lc,20,1,5},right},
     {{ioc,20,0,1},{lc,20,1,6},right},
     {{ioc,20,0,2},{lc,20,1,7},right},
     {{ioc,20,14,0},{lc,20,13,3},right},
     {{ioc,20,14,1},{lc,20,13,2},right},
     {{ioc,20,14,2},{lc,20,13,1},right},
     {{ioc,21,1,0},{lc,20,1,2},right},
     {{ioc,21,1,1},{lc,20,1,3},right},
     {{ioc,21,1,2},{lc,20,1,4},right},
     {{ioc,21,1,3},{lc,20,1,9},right},
     {{ioc,21,1,4},{lc,20,1,8},right},
     {{ioc,21,1,5},{lc,20,1,7},right},
     {{ioc,21,2,0},{lc,20,2,2},right},
     {{ioc,21,2,1},{lc,20,2,3},right},
     {{ioc,21,2,2},{lc,20,2,4},right},
     {{ioc,21,2,3},{lc,20,2,9},right},
     {{ioc,21,2,4},{lc,20,2,8},right},
     {{ioc,21,3,0},{lc,20,3,2},right},
     {{ioc,21,3,1},{lc,20,3,3},right},
     {{ioc,21,3,2},{lc,20,3,4},right},
     {{ioc,21,3,3},{lc,20,3,9},right},
     {{ioc,21,3,4},{lc,20,3,8},right},
     {{ioc,21,3,5},{lc,20,3,7},right},
     {{ioc,21,4,0},{lc,20,4,2},right},
     {{ioc,21,4,1},{lc,20,4,3},right},
     {{ioc,21,4,2},{lc,20,4,4},right},
     {{ioc,21,4,3},{lc,20,4,9},right},
     {{ioc,21,4,4},{lc,20,4,8},right},
     {{ioc,21,5,0},{lc,20,5,2},right},
     {{ioc,21,5,1},{lc,20,5,3},right},
     {{ioc,21,5,2},{lc,20,5,4},right},
     {{ioc,21,5,3},{lc,20,5,9},right},
     {{ioc,21,5,4},{lc,20,5,8},right},
     {{ioc,21,5,5},{lc,20,5,7},right},
     {{ioc,21,6,0},{lc,20,6,2},right},
     {{ioc,21,6,1},{lc,20,6,3},right},
     {{ioc,21,6,2},{lc,20,6,4},right},
     {{ioc,21,6,3},{lc,20,6,9},right},
     {{ioc,21,6,4},{lc,20,6,8},right},
     {{ioc,21,7,0},{lc,20,7,2},right},
     {{ioc,21,7,1},{lc,20,7,3},right},
     {{ioc,21,7,2},{lc,20,7,4},right},
     {{ioc,21,7,3},{lc,20,7,9},right},
     {{ioc,21,7,4},{lc,20,7,8},right},
     {{ioc,21,7,5},{lc,20,7,7},right},
     {{ioc,21,8,0},{lc,20,8,2},right},
     {{ioc,21,8,1},{lc,20,8,3},right},
     {{ioc,21,8,2},{lc,20,8,4},right},
     {{ioc,21,8,3},{lc,20,8,9},right},
     {{ioc,21,8,4},{lc,20,8,8},right},
     {{ioc,21,8,5},{lc,20,8,7},right},
     {{ioc,21,9,0},{lc,20,9,2},right},
     {{ioc,21,9,1},{lc,20,9,3},right},
     {{ioc,21,9,2},{lc,20,9,4},right},
     {{ioc,21,9,3},{lc,20,9,9},right},
     {{ioc,21,9,4},{lc,20,9,8},right},
     {{ioc,21,10,0},{lc,20,10,2},right},
     {{ioc,21,10,1},{lc,20,10,3},right},
     {{ioc,21,10,2},{lc,20,10,4},right},
     {{ioc,21,10,3},{lc,20,10,9},right},
     {{ioc,21,10,4},{lc,20,10,8},right},
     {{ioc,21,11,0},{lc,20,11,2},right},
     {{ioc,21,11,1},{lc,20,11,3},right},
     {{ioc,21,11,2},{lc,20,11,4},right},
     {{ioc,21,11,3},{lc,20,11,9},right},
     {{ioc,21,11,4},{lc,20,11,8},right},
     {{ioc,21,11,5},{lc,20,11,7},right},
     {{ioc,21,12,0},{lc,20,12,2},right},
     {{ioc,21,12,1},{lc,20,12,3},right},
     {{ioc,21,12,2},{lc,20,12,4},right},
     {{ioc,21,12,3},{lc,20,12,9},right},
     {{ioc,21,12,4},{lc,20,12,8},right},
     {{ioc,21,13,0},{lc,20,13,2},right},
     {{ioc,21,13,1},{lc,20,13,3},right},
     {{ioc,21,13,2},{lc,20,13,4},right},
     {{ioc,21,13,3},{lc,20,13,9},right},
     {{ioc,21,13,4},{lc,20,13,8},right},
     {{ioc,21,13,5},{lc,20,13,7},right}
    ].

