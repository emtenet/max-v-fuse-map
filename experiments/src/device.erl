-module(device).

-export([list/0]).
-export([name/1]).
-export([from_name/1]).
-export([density/1]).
-export([package/1]).
-export([gclk_pins/1]).
-export([iobs/1]).
-export([iocs/1]).
-export([iocs/2]).
-export([pins/1]).
-export([labs/1]).
-export([metric/1]).
-export([top_io/1]).
-export([top_lab/1]).
-export([left_io/2]).
-export([left_lab/2]).
-export([right_io/1]).
-export([right_lab/1]).
-export([bottom_io/2]).
-export([bottom_lab/2]).

-export_type([device/0]).

-include("max_v.hrl").

-type density() :: density:density().

-type device() ::
    max_v_40z_e64 |
    max_v_40z_m64 |
    max_v_80z_e64 |
    max_v_80z_m64 |
    max_v_80z_m68 |
    max_v_80z_t100 |
    max_v_160z_e64 |
    max_v_160z_m68 |
    max_v_160z_m100 |
    max_v_160z_t100 |
    max_v_240z_m68 |
    max_v_240z_m100 |
    max_v_240z_t100 |
    max_v_240z_t144 |
    max_v_570z_m100 |
    max_v_570z_t100 |
    max_v_570z_t144 |
    max_v_570z_f256 |
    max_v_1270z_t144 |
    max_v_1270z_f256 |
    max_v_1270z_f324 |
    max_v_2210z_f256 |
    max_v_2210z_f324.

-type iob() :: iob:iob().
-type ioc() :: iob:ioc().
-type lab() :: lab:lab().
-type package() :: package:package().
-type pin() :: pin:pin().
-type x() :: max_v:x().
-type y() :: max_v:y().

%%====================================================================
%% list
%%====================================================================

-spec list() -> [device()].

list() ->
    [max_v_40z_e64,
     max_v_40z_m64,
     max_v_80z_e64,
     max_v_80z_m64,
     max_v_80z_m68,
     max_v_80z_t100,
     max_v_160z_e64,
     max_v_160z_m68,
     max_v_160z_m100,
     max_v_160z_t100,
     max_v_240z_m68,
     max_v_240z_m100,
     max_v_240z_t100,
     max_v_240z_t144,
     max_v_570z_m100,
     max_v_570z_t100,
     max_v_570z_t144,
     max_v_570z_f256,
     max_v_1270z_t144,
     max_v_1270z_f256,
     max_v_1270z_f324,
     max_v_2210z_f256,
     max_v_2210z_f324
    ].

%%====================================================================
%% name
%%====================================================================

-spec name(device()) -> binary().

name(max_v_40z_e64) -> <<"5M40ZE64C5">>;
name(max_v_40z_m64) -> <<"5M40ZM64C5">>;
name(max_v_80z_e64) -> <<"5M80ZE64C5">>;
name(max_v_80z_m64) -> <<"5M80ZM64C5">>;
name(max_v_80z_m68) -> <<"5M80ZM68C5">>;
name(max_v_80z_t100) -> <<"5M80ZT100C5">>;
name(max_v_160z_e64) -> <<"5M160ZE64C5">>;
name(max_v_160z_m68) -> <<"5M160ZM68C5">>;
name(max_v_160z_m100) -> <<"5M160ZM100C5">>;
name(max_v_160z_t100) -> <<"5M160ZT100C5">>;
name(max_v_240z_m68) -> <<"5M240ZM68C5">>;
name(max_v_240z_m100) -> <<"5M240ZM100C5">>;
name(max_v_240z_t100) -> <<"5M240ZT100C5">>;
name(max_v_240z_t144) -> <<"5M240ZT144C5">>;
name(max_v_570z_m100) -> <<"5M570ZM100C5">>;
name(max_v_570z_t100) -> <<"5M570ZT100C5">>;
name(max_v_570z_t144) -> <<"5M570ZT144C5">>;
name(max_v_570z_f256) -> <<"5M570ZF256C5">>;
name(max_v_1270z_t144) -> <<"5M1270ZT144C5">>;
name(max_v_1270z_f256) -> <<"5M1270ZF256C5">>;
name(max_v_1270z_f324) -> <<"5M1270ZF324C5">>;
name(max_v_2210z_f256) -> <<"5M2210ZF256C5">>;
name(max_v_2210z_f324) -> <<"5M2210ZF324C5">>.

%%====================================================================
%% from_name
%%====================================================================

-spec from_name(binary()) -> device().

from_name(<<"5M40ZE64C5">>) -> max_v_40z_e64;
from_name(<<"5M40ZM64C5">>) -> max_v_40z_m64;
from_name(<<"5M80ZE64C5">>) -> max_v_80z_e64;
from_name(<<"5M80ZM64C5">>) -> max_v_80z_m64;
from_name(<<"5M80ZM68C5">>) -> max_v_80z_m68;
from_name(<<"5M80ZT100C5">>) -> max_v_80z_t100;
from_name(<<"5M160ZE64C5">>) -> max_v_160z_e64;
from_name(<<"5M160ZM68C5">>) -> max_v_160z_m68;
from_name(<<"5M160ZM100C5">>) -> max_v_160z_m100;
from_name(<<"5M160ZT100C5">>) -> max_v_160z_t100;
from_name(<<"5M240ZM68C5">>) -> max_v_240z_m68;
from_name(<<"5M240ZM100C5">>) -> max_v_240z_m100;
from_name(<<"5M240ZT100C5">>) -> max_v_240z_t100;
from_name(<<"5M240ZT144C5">>) -> max_v_240z_t144;
from_name(<<"5M570ZM100C5">>) -> max_v_570z_m100;
from_name(<<"5M570ZT100C5">>) -> max_v_570z_t100;
from_name(<<"5M570ZT144C5">>) -> max_v_570z_t144;
from_name(<<"5M570ZF256C5">>) -> max_v_570z_f256;
from_name(<<"5M1270ZT144C5">>) -> max_v_1270z_t144;
from_name(<<"5M1270ZF256C5">>) -> max_v_1270z_f256;
from_name(<<"5M1270ZF324C5">>) -> max_v_1270z_f324;
from_name(<<"5M2210ZF256C5">>) -> max_v_2210z_f256;
from_name(<<"5M2210ZF324C5">>) -> max_v_2210z_f324.

%%====================================================================
%% density
%%====================================================================

-spec density(device()) -> density().

density(max_v_40z_e64) -> max_v_240z;
density(max_v_40z_m64) -> max_v_240z;
density(max_v_80z_e64) -> max_v_240z;
density(max_v_80z_m64) -> max_v_240z;
density(max_v_80z_m68) -> max_v_240z;
density(max_v_80z_t100) -> max_v_240z;
density(max_v_160z_e64) -> max_v_240z;
density(max_v_160z_m68) -> max_v_240z;
density(max_v_160z_m100) -> max_v_240z;
density(max_v_160z_t100) -> max_v_240z;
density(max_v_240z_m68) -> max_v_240z;
density(max_v_240z_m100) -> max_v_240z;
density(max_v_240z_t100) -> max_v_240z;
density(max_v_240z_t144) -> max_v_570z;
density(max_v_570z_m100) -> max_v_570z;
density(max_v_570z_t100) -> max_v_570z;
density(max_v_570z_t144) -> max_v_570z;
density(max_v_570z_f256) -> max_v_570z;
density(max_v_1270z_t144) -> max_v_1270z;
density(max_v_1270z_f256) -> max_v_1270z;
density(max_v_1270z_f324) -> max_v_2210z;
density(max_v_2210z_f256) -> max_v_2210z;
density(max_v_2210z_f324) -> max_v_2210z.

%%====================================================================
%% gclk_pins
%%====================================================================

-spec gclk_pins(device()) -> [pin()].

gclk_pins(Device) ->
    package:gclk_pins(package(Device)).

%%====================================================================
%% iobs
%%====================================================================

-spec iobs(device()) -> [{iob(), lab()}].

iobs(Device) ->
    density:iobs(density(Device)).

%%====================================================================
%% iocs
%%====================================================================

-spec iocs(device()) -> [{pin(), ioc()}].

iocs(max_v_40z_e64) -> max_v_40z_e64:iocs();
iocs(max_v_40z_m64) -> max_v_40z_m64:iocs();
iocs(max_v_80z_e64) -> max_v_80z_e64:iocs();
iocs(max_v_80z_m64) -> max_v_80z_m64:iocs();
iocs(max_v_80z_m68) -> max_v_80z_m68:iocs();
iocs(max_v_80z_t100) -> max_v_80z_t100:iocs();
iocs(max_v_160z_e64) -> max_v_160z_e64:iocs();
iocs(max_v_160z_m68) -> max_v_160z_m68:iocs();
iocs(max_v_160z_m100) -> max_v_160z_m100:iocs();
iocs(max_v_160z_t100) -> max_v_160z_t100:iocs();
iocs(max_v_240z_m68) -> max_v_240z_m68:iocs();
iocs(max_v_240z_m100) -> max_v_240z_m100:iocs();
iocs(max_v_240z_t100) -> max_v_240z_t100:iocs();
iocs(max_v_240z_t144) -> max_v_240z_t144:iocs();
iocs(max_v_570z_m100) -> max_v_570z_m100:iocs();
iocs(max_v_570z_t100) -> max_v_570z_t100:iocs();
iocs(max_v_570z_t144) -> max_v_570z_t144:iocs();
iocs(max_v_570z_f256) -> max_v_570z_f256:iocs();
iocs(max_v_1270z_t144) -> max_v_1270z_t144:iocs();
iocs(max_v_1270z_f256) -> max_v_1270z_f256:iocs();
iocs(max_v_1270z_f324) -> max_v_1270z_f324:iocs();
iocs(max_v_2210z_f256) -> max_v_2210z_f256:iocs();
iocs(max_v_2210z_f324) -> max_v_2210z_f324:iocs().

%%====================================================================
%% iocs
%%====================================================================

-spec iocs(device(), iob()) -> [{pin(), ioc()}].

iocs(max_v_40z_e64, IOB) -> max_v_40z_e64:iocs(IOB);
iocs(max_v_40z_m64, IOB) -> max_v_40z_m64:iocs(IOB);
iocs(max_v_80z_e64, IOB) -> max_v_80z_e64:iocs(IOB);
iocs(max_v_80z_m64, IOB) -> max_v_80z_m64:iocs(IOB);
iocs(max_v_80z_m68, IOB) -> max_v_80z_m68:iocs(IOB);
iocs(max_v_80z_t100, IOB) -> max_v_80z_t100:iocs(IOB);
iocs(max_v_160z_e64, IOB) -> max_v_160z_e64:iocs(IOB);
iocs(max_v_160z_m68, IOB) -> max_v_160z_m68:iocs(IOB);
iocs(max_v_160z_m100, IOB) -> max_v_160z_m100:iocs(IOB);
iocs(max_v_160z_t100, IOB) -> max_v_160z_t100:iocs(IOB);
iocs(max_v_240z_m68, IOB) -> max_v_240z_m68:iocs(IOB);
iocs(max_v_240z_m100, IOB) -> max_v_240z_m100:iocs(IOB);
iocs(max_v_240z_t100, IOB) -> max_v_240z_t100:iocs(IOB);
iocs(max_v_240z_t144, IOB) -> max_v_240z_t144:iocs(IOB);
iocs(max_v_570z_m100, IOB) -> max_v_570z_m100:iocs(IOB);
iocs(max_v_570z_t100, IOB) -> max_v_570z_t100:iocs(IOB);
iocs(max_v_570z_t144, IOB) -> max_v_570z_t144:iocs(IOB);
iocs(max_v_570z_f256, IOB) -> max_v_570z_f256:iocs(IOB);
iocs(max_v_1270z_t144, IOB) -> max_v_1270z_t144:iocs(IOB);
iocs(max_v_1270z_f256, IOB) -> max_v_1270z_f256:iocs(IOB);
iocs(max_v_1270z_f324, IOB) -> max_v_1270z_f324:iocs(IOB);
iocs(max_v_2210z_f256, IOB) -> max_v_2210z_f256:iocs(IOB);
iocs(max_v_2210z_f324, IOB) -> max_v_2210z_f324:iocs(IOB).

%%====================================================================
%% pins
%%====================================================================

-spec pins(device()) -> [pin()].

pins(max_v_40z_e64) -> max_v_40z_e64:pins();
pins(max_v_40z_m64) -> max_v_40z_m64:pins();
pins(max_v_80z_e64) -> max_v_80z_e64:pins();
pins(max_v_80z_m64) -> max_v_80z_m64:pins();
pins(max_v_80z_m68) -> max_v_80z_m68:pins();
pins(max_v_80z_t100) -> max_v_80z_t100:pins();
pins(max_v_160z_e64) -> max_v_160z_e64:pins();
pins(max_v_160z_m68) -> max_v_160z_m68:pins();
pins(max_v_160z_m100) -> max_v_160z_m100:pins();
pins(max_v_160z_t100) -> max_v_160z_t100:pins();
pins(max_v_240z_m68) -> max_v_240z_m68:pins();
pins(max_v_240z_m100) -> max_v_240z_m100:pins();
pins(max_v_240z_t100) -> max_v_240z_t100:pins();
pins(max_v_240z_t144) -> max_v_240z_t144:pins();
pins(max_v_570z_m100) -> max_v_570z_m100:pins();
pins(max_v_570z_t100) -> max_v_570z_t100:pins();
pins(max_v_570z_t144) -> max_v_570z_t144:pins();
pins(max_v_570z_f256) -> max_v_570z_f256:pins();
pins(max_v_1270z_t144) -> max_v_1270z_t144:pins();
pins(max_v_1270z_f256) -> max_v_1270z_f256:pins();
pins(max_v_1270z_f324) -> max_v_1270z_f324:pins();
pins(max_v_2210z_f256) -> max_v_2210z_f256:pins();
pins(max_v_2210z_f324) -> max_v_2210z_f324:pins().

%%====================================================================
%% labs
%%====================================================================

-spec labs(device()) -> [lab()].

labs(Device) ->
    density:labs(density(Device)).

%%====================================================================
%% package
%%====================================================================

-spec package(device()) -> package().

package(max_v_40z_e64) -> e64;
package(max_v_40z_m64) -> m64;
package(max_v_80z_e64) -> e64;
package(max_v_80z_m64) -> m64;
package(max_v_80z_m68) -> m68;
package(max_v_80z_t100) -> t100;
package(max_v_160z_e64) -> e64;
package(max_v_160z_m68) -> m68;
package(max_v_160z_m100) -> m100;
package(max_v_160z_t100) -> t100;
package(max_v_240z_m68) -> m68;
package(max_v_240z_m100) -> m100;
package(max_v_240z_t100) -> t100;
package(max_v_240z_t144) -> t144;
package(max_v_570z_m100) -> m100;
package(max_v_570z_t100) -> t100;
package(max_v_570z_t144) -> t144;
package(max_v_570z_f256) -> f256;
package(max_v_1270z_t144) -> t144;
package(max_v_1270z_f256) -> f256;
package(max_v_1270z_f324) -> f324;
package(max_v_2210z_f256) -> f256;
package(max_v_2210z_f324) -> f324.

%%====================================================================
%% metric
%%====================================================================

-spec metric(device()) -> #metric{}.

metric(Device) ->
    density:metric(density(Device)).

%%--------------------------------------------------------------------

-spec top_io(device()) -> y().

top_io(Device) ->
    density:top_io(density(Device)).

%%--------------------------------------------------------------------

-spec top_lab(device()) -> y().

top_lab(Device) ->
    density:top_lab(density(Device)).

%%--------------------------------------------------------------------

-spec left_io(y(), device()) -> x().

left_io(Y, Device) ->
    density:left_io(Y, density(Device)).

%%--------------------------------------------------------------------

-spec left_lab(y(), device()) -> x().

left_lab(Y, Device) ->
    density:left_lab(Y, density(Device)).

%%--------------------------------------------------------------------

-spec right_io(device()) -> x().

right_io(Device) ->
    density:right_io(density(Device)).

%%--------------------------------------------------------------------

-spec right_lab(device()) -> x().

right_lab(Device) ->
    density:right_lab(density(Device)).

%%--------------------------------------------------------------------

-spec bottom_io(x(), device()) -> y().

bottom_io(X, Device) ->
    density:bottom_io(X, density(Device)).

%%--------------------------------------------------------------------

-spec bottom_lab(x(), device()) -> y().

bottom_lab(X, Device) ->
    density:bottom_lab(X, density(Device)).

