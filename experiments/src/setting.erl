-module(setting).

-export([encode/1]).
-export([io_standards/0]).
-export([io_standard_output/1]).
-export([unused_pins/0]).

-export_type([setting/0]).

-export_type([current_strength/0]).
-export_type([io_standard/0]).
-export_type([unused_pins/0]).

-type setting() ::
    % global
    {auto_global_clock, boolean()} |
    {not_gate_push_back, boolean()} |
    {seed, pos_integer()} |
    {unused_pins, unused_pins()} |
    {user_code_as_checksum, boolean()} |
    {user_code, binary()} |
    % instance
    {bus_hold, signal(), boolean()} |
    {current_strength, signal(), current_strength()} |
    {global_clock, signal(), boolean()} |
    {io_standard, signal(), io_standard()} |
    {slow_slew_rate, signal(), boolean()} |
    {weak_pull_up, signal(), boolean()} |
    {input_delay, signal(), boolean()} |
    {input_delay, signal(), signal(), boolean()} |
    % location
    {location, signal(), lc_or_pin()} |
    %
    {raw, binary()}.

-type signal() :: atom() | binary().

-type current_strength() ::
    current_2ma |
    current_3ma |
    current_4ma |
    current_6ma |
    current_7ma |
    current_8ma |
    current_14ma |
    current_16ma |
    minimum |
    maximum.

-type io_standard() ::
    v1_5 |
    v1_8 |
    v2_5 |
    v2_5_schmitt_trigger |
    v3_3_cmos |
    v3_3_ttl |
    v3_3_schmitt_trigger.

-type unused_pins() ::
    input_tri_stated |
    input_tri_stated_with_bus_hold |
    input_tri_stated_with_weak_pull_up |
    output_driving_unspecified |
    output_driving_ground.

-type lc() :: lc:lc().
-type lc_or_pin() :: lc() | pin().
-type pin() :: pin:pin().

%%====================================================================
%% encode
%%====================================================================

-spec encode([setting()]) -> binary().

encode(Settings0) ->
    Settings = default(Settings0),
    iolist_to_binary(lists:sort(lists:map(fun setting/1, Settings))).

%%--------------------------------------------------------------------

default(Settings) ->
    Defaults = defaults(Settings, #{
        seed => 1,
        user_code_as_checksum => false,
        user_code => <<"00000000">>
    }),
    maps:fold(fun (Key, Value, Settings0) ->
        [{Key, Value} | Settings0]
    end, Settings, Defaults).

%%--------------------------------------------------------------------

defaults([], Defaults) ->
    Defaults;
defaults([{Key, _} | Settings], Defaults) when is_map_key(Key, Defaults) ->
    defaults(Settings, maps:remove(Key, Defaults));
defaults([_ | Settings], Defaults) ->
    defaults(Settings, Defaults).

%%--------------------------------------------------------------------

setting({auto_global_clock, Value}) ->
    global(<<"AUTO_GLOBAL_CLOCK">>, boolean(Value));
setting({not_gate_push_back, Value}) ->
    instance(<<"NOT_GATE_PUSH_BACK">>, <<"*">>, boolean(Value));
setting({unused_pins, Value}) ->
    global(<<"RESERVE_ALL_UNUSED_PINS">>, unused_pins(Value));
setting({seed, Value}) ->
    global(<<"SEED">>, integer(Value));
setting({user_code_as_checksum, Value}) ->
    global(<<"USE_CHECKSUM_AS_USERCODE">>, boolean(Value));
setting({user_code, Value}) ->
    global(<<"STRATIX_JTAG_USER_CODE">>, Value);
setting({bus_hold, Signal, Value}) ->
    instance(<<"ENABLE_BUS_HOLD_CIRCUITRY">>, Signal, boolean(Value));
setting({current_strength, Signal, Value}) ->
    instance(<<"CURRENT_STRENGTH_NEW">>, Signal, current_strength(Value));
setting({global_clock, Signal, Value}) ->
    instance(<<"GLOBAL_SIGNAL">>, Signal, global_clock(Value));
setting({io_standard, Signal, Value}) ->
    instance(<<"IO_STANDARD">>, Signal, io_standard(Value));
setting({slow_slew_rate, Signal, Value}) ->
    instance(<<"SLOW_SLEW_RATE">>, Signal, boolean(Value));
setting({weak_pull_up, Signal, Value}) ->
    instance(<<"WEAK_PULL_UP_RESISTOR">>, Signal, boolean(Value));
setting({input_delay, From, To, Delay}) ->
    instance(<<"PAD_TO_CORE_DELAY">>, From, To, delay(Delay));
setting({input_delay, From, Delay}) ->
    instance(<<"PAD_TO_CORE_DELAY">>, From, <<"*">>, delay(Delay));
setting({location, Signal, Value}) ->
    location(Signal, lc_or_pin(Value));
setting({raw, Line}) ->
    Size = byte_size(Line) - 1,
    <<_:Size/binary, "\n">> = Line,
    Line.

%%--------------------------------------------------------------------

global(Name, Value) ->
    <<"set_global_assignment"
      " -name ", Name/binary,
      " ", Value/binary,
      "\n"
    >>.

%%--------------------------------------------------------------------

instance(Name, Signal0, Value) ->
    Signal = signal(Signal0),
    <<"set_instance_assignment"
      " -name ", Name/binary,
      " -to ", Signal/binary,
      " ", Value/binary,
      "\n"
    >>.

%%--------------------------------------------------------------------

instance(Name, From0, To0, Value) ->
    From = signal(From0),
    To = signal(To0),
    <<"set_instance_assignment"
      " -name ", Name/binary,
      " -from ", From/binary,
      " -to ", To/binary,
      " ", Value/binary,
      "\n"
    >>.

%%--------------------------------------------------------------------

location(Signal0, Value) ->
    Signal = signal(Signal0),
    <<"set_location_assignment"
      " -to ", Signal/binary,
      " ", Value/binary,
      "\n"
    >>.

%%====================================================================
%% io_standards
%%====================================================================

io_standards() ->
    [v1_2,
     v1_5,
     v1_8,
     v2_5,
     v2_5_schmitt_trigger,
     v3_3_cmos,
     v3_3_ttl,
     v3_3_schmitt_trigger
    ].

%%--------------------------------------------------------------------

io_standard_output(v2_5_schmitt_trigger) -> v2_5;
io_standard_output(v3_3_schmitt_trigger) -> v3_3_ttl;
io_standard_output(Standard) -> Standard.

%%====================================================================
%% values
%%====================================================================

boolean(true) ->
    <<"On">>;
boolean(false) ->
    <<"Off">>.

%%--------------------------------------------------------------------

integer(Value) ->
    list_to_binary(io_lib:format("~b", [Value])).

%%--------------------------------------------------------------------

current_strength(current_2ma) -> <<"\"2mA\"">>;
current_strength(current_3ma) -> <<"\"3mA\"">>;
current_strength(current_4ma) -> <<"\"4mA\"">>;
current_strength(current_6ma) -> <<"\"6mA\"">>;
current_strength(current_7ma) -> <<"\"7mA\"">>;
current_strength(current_8ma) -> <<"\"8mA\"">>;
current_strength(current_14ma) -> <<"\"14mA\"">>;
current_strength(current_16ma) -> <<"\"16mA\"">>;
current_strength(minimum) -> <<"\"Minimum current\"">>;
current_strength(maximum) -> <<"\"Maximum current\"">>.

%%--------------------------------------------------------------------

delay(true) ->
    <<"1">>;
delay(false) ->
    <<"0">>.

%%--------------------------------------------------------------------

global_clock(true) ->
    <<"\"Global clock\"">>;
global_clock(false) ->
    <<"Off">>.

%%--------------------------------------------------------------------

io_standard(v1_2) ->
    <<"\"1.2 V\"">>;
io_standard(v1_5) ->
    <<"\"1.5 V\"">>;
io_standard(v1_8) ->
    <<"\"1.8 V\"">>;
io_standard(v2_5) ->
    <<"\"2.5 V\"">>;
io_standard(v2_5_schmitt_trigger) ->
    <<"\"2.5V SCHMITT TRIGGER INPUT\"">>;
io_standard(v3_3_cmos) ->
    <<"\"3.3-V LVCMOS\"">>;
io_standard(v3_3_ttl) ->
    <<"\"3.3-V LVTTL\"">>;
io_standard(v3_3_schmitt_trigger) ->
    <<"\"3.3V SCHMITT TRIGGER INPUT\"">>.

%%--------------------------------------------------------------------

lc_or_pin(LC = {lc, _, _, _}) ->
    lc:name(LC);
lc_or_pin(Pin) ->
    pin:name(Pin).

%%--------------------------------------------------------------------

signal(Signal) when is_binary(Signal) ->
    quote(Signal);
signal(Signal) when is_atom(Signal) ->
    quote(atom_to_binary(Signal)).

%%--------------------------------------------------------------------

quote(Value) ->
    case binary:match(Value, <<"\\">>) of
        nomatch ->
            Value;

        _ ->
            <<"\"", Value/binary, "\"">>
    end.

%%--------------------------------------------------------------------

unused_pins() ->
    [input_tri_stated,
     input_tri_stated_with_bus_hold,
     input_tri_stated_with_weak_pull_up,
     output_driving_unspecified,
     output_driving_ground
    ].

%%--------------------------------------------------------------------

unused_pins(input_tri_stated) ->
    <<"\"As input tri-stated\"">>;
unused_pins(input_tri_stated_with_bus_hold) ->
    <<"\"As input tri-stated with bus-hold\"">>;
unused_pins(input_tri_stated_with_weak_pull_up) ->
    <<"\"As input tri-stated with weak pull-up\"">>;
unused_pins(output_driving_unspecified) ->
    <<"\"As output driving an unspecified signal\"">>;
unused_pins(output_driving_ground) ->
    <<"\"As output driving ground\"">>.

