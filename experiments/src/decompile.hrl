
-type density() :: density:density().
-type fuses() :: [fuse:fuse()].
-type ioc() :: ioc:ioc().
-type jtag() :: jtag:jtag().
-type lc() :: lc:lc().
-type title() :: term().

-define(LUT_INIT, 2#1111111111111111).

-record(lc, {
    carry_in = false :: boolean(),
    carry_out = false :: boolean(),
    feedback = false :: boolean(),
    lut = ?LUT_INIT :: non_neg_integer(),
    lut_chain = true :: boolean(),
    lut_name :: binary() | undefined,
    lut_ports = #{} :: #{},
    output_left = reg :: lut | reg,
    output_local = reg :: lut | reg,
    output_right = reg :: lut | reg,
    reg_chain = true :: boolean(),
    reg_name :: binary() | undefined,
    reg_ports = #{} :: #{}
}).

-record(ioc, {
    enable = normal :: normal | invert,
    enable_port :: {binary(), list()} | undefined,
    input_name :: binary() | undefined,
    output = normal :: normal | invert,
    output_name :: binary() | undefined,
    output_port :: {binary(), list()} | undefined
}).

-record(jtag, {
    name :: binary() | undefined,
    ports = #{} :: #{}
}).

-record(ufm, {
    name :: binary() | undefined,
    ports = #{} :: #{}
}).

-type logic() :: #{
    lc() => #lc{},
    ioc() => #ioc{},
    jtag() => #jtag{}
}.

