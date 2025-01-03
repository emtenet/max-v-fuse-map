
-type density() :: density:density().
-type fuses() :: [fuse:fuse()].
-type ioc() :: ioc:ioc().
-type jtag() :: jtag:jtag().
-type lc() :: lc:lc().
-type title() :: term().

-define(LUT_INIT, 2#1111111111111111).
-define(LUT_A_and_B_and_C_and_D, 2#1000000000000000).
-define(LUT_A_and_B_and_C_and_not_D, 2#0000000010000000).
-define(LUT_A_xor_C, 2#0101101001011010).
-define(LUT_A_xor_B_carry, 2#0110011010001000).
-define(LUT_A_xor_B_xor_C_carry, 2#1001011000010111).
-define(LUT_A_xor_B_xor_not_C_carry, 2#0110100110001110).
-define(LUT_B_xor_C, 2#0011110000111100).
-define(LUT_C, 2#1111000011110000).
-define(LUT_C_xor_D, 2#0000111111110000).
-define(LUT_not_C, 2#0000111100001111).

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

