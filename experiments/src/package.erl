-module(package).

-export([list/0]).
-export([title/1]).
-export([gclk_pins/1]).
-export([reset/1]).
-export([output_enable/1]).

-export_type([package/0]).

-type package() ::
    e64 |
    m64 |
    m68 |
    m100 |
    t100 |
    t144 |
    f256 |
    f324.

-type pin() :: pin:pin().

%%====================================================================
%% list
%%====================================================================

-spec list() -> [package()].

list() ->
    [e64,
     m64,
     m68,
     m100,
     t100,
     t144,
     f256,
     f324
    ].

%%====================================================================
%% title
%%====================================================================

-spec title(package()) -> string().

title(e64) -> "64-Pin Enhanced Quad Flat Pack";
title(m64) -> "64-Pin Micro FineLine BGA";
title(m68) -> "68-Pin Micro FineLine BGA";
title(m100) -> "100-Pin Micro FineLine BGA";
title(t100) -> "100-Pin Thin Quad Flat Pack";
title(t144) -> "144-Pin Thin Quad Flat Pack";
title(f256) -> "256-Pin FineLine BGA";
title(f324) -> "324-Pin FineLine BGA".

%%====================================================================
%% gclk_pins
%%====================================================================

-spec gclk_pins(package()) -> [pin()].

gclk_pins(e64) -> [pin7, pin9, pin40, pin42];
gclk_pins(m64) -> [undefined, e2, d8, undefined];
gclk_pins(m68) -> [e2, e1, e9, e8];
gclk_pins(m100) -> [f2, e1, f10, g11];
gclk_pins(t100) -> [pin12, pin14, pin62, pin64];
gclk_pins(t144) -> [pin18 ,pin20, pin89, pin91];
gclk_pins(f256) -> [h5, j5, j12, h12];
gclk_pins(f324) -> [j6, k6, k13, j13].

%%====================================================================
%% reset
%%====================================================================

-spec reset(package()) -> pin().

reset(e64) -> pin29;
reset(m64) -> h7;
reset(m68) -> h7;
reset(m100) -> k8;
reset(t100) -> pin44;
reset(t144) -> pin61;
reset(f256) -> m9;
reset(f324) -> n10.

%%====================================================================
%% output_enable
%%====================================================================

-spec output_enable(package()) -> pin().

output_enable(e64) -> pin28;
output_enable(m64) -> f6;
output_enable(m68) -> j6;
output_enable(m100) -> l8;
output_enable(t100) -> pin43;
output_enable(t144) -> pin60;
output_enable(f256) -> m8;
output_enable(f324) -> n9.

