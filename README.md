# Altera MAX V Fuse Map (by experiment)

This is a puzzle.
Can I work out the fuse map of MAX V devices by
feeding the Quartus tools
and observing the outputs?

My experiments are being run in the following
[environment](doc/environment.md).

## Assumptions

 * [Fuse ordering](doc/fuse-ordering.md)
 * [Fuse vs bit](doc/fuse-vs-bit.md)
 * [Default settings](doc/default-settings.md)

## Experiments

 * [User code](experiments/src/user_code_experiment.erl)
 * [Fuse count](experiments/src/fuse_count_experiment.erl)
 * [Generate pins](experiments/src/generate_pins.erl)
 * [Unused pins](experiments/src/unused_experiment.erl)
 * [I/O features](experiments/src/io_features_experiment.erl)
 * [PCI compliance](experiments/src/pci_compliance_experiment.erl)
 * [I/O modes](experiments/src/io_modes_experiment.erl)
 * [LUT](experiments/src/lut_experiment.erl)
 * [Generate fast out](experiments/src/generate_fast_out.erl)
 * [LAB clk1](experiments/src/lab_clk1_experiment.erl)
 * [LAB s-load](experiments/src/lab_s_load_experiment.erl)
 * [LAB a-clr1](experiments/src/lab_a_clr1_experiment.erl)
 * [LAB clk2](experiments/src/lab_clk2_experiment.erl)
 * [LAB a-clr2](experiments/src/lab_a_clr2_experiment.erl)
 * [LC local line](experiments/src/lc_local_line_experiment.erl)
 * [LAB interconnect limit](experiments/src/lab_interconnect_limit_experiment.erl)
 * LC data MUX [playground](experiments/src/lc_data_mux_playground.erl)
   and [theory](experiments/src/lc_data_mux_theory.erl)
 * IOC output MUX [playground](experiments/src/ioc_output_mux_playground.erl)
   and [theory](experiments/src/ioc_output_mux_theory.erl)
 * IOC enable MUX [playground](experiments/src/ioc_output_mux_playground.erl)
   and [theory](experiments/src/ioc_output_mux_theory.erl)
 * [LC direct-link](experiments/src/lc_direct_link_experiment.erl)
 * [LAB ena1](experiments/src/lab_ena1_experiment.erl)
 * [LAB ena2](experiments/src/lab_ena2_experiment.erl)
 * [LAB ena2 vs s-load](experiments/src/lab_ena2_s_load_experiment.erl)
 * [LAB a-load](experiments/src/lab_a_load_experiment.erl)
 * [LAB s-clr](experiments/src/lab_s_clr_experiment.erl)
 * [LAB s-load always](experiments/src/lab_s_load_always_experiment.erl)
 * [LC a-load](experiments/src/lc_a_load_experiment.erl)
 * LAB interconnect MUX
   [playground 1](experiments/src/lab_interconnect_mux_playground.erl)
   and [playground 2](experiments/src/lab_interconnect_mux_playground2.erl)
   and [database](experiments/src/lab_interconnect_mux_database.erl)
 * [LAB control](experiments/src/lab_control_experiment.erl)
 * LAB control MUX
   [playground](experiments/src/lab_control_mux_playground.erl)
   and [theory](experiments/src/lab_control_mux_theory.erl)
 * Global network
   [LAB playground](experiments/src/lab_global_network_playground.erl)
   and [IOB playground](experiments/src/iob_global_network_playground.erl)
   and [theory](experiments/src/global_network_theory.erl)
 * IOB interconnect MUX
   [playground](experiments/src/iob_interconnect_mux_playground.erl)
   and [database](experiments/src/iob_interconnect_mux_database.erl)
 * [Global internal](experiments/src/global_internal_experiment.erl)
 * Global MUX
   [playground](experiments/src/global_mux_playground.erl)
   and [theory](experiments/src/global_mux_theory.erl)
 * Global interconnect MUX
   [playground](experiments/src/global_interconnect_mux_playground.erl)
 * C4 interconnect MUX
   [playground](experiments/src/c4_interconnect_mux_playground.erl)
   and [database](experiments/src/c4_interconnect_mux_database.erl)
 * R4 interconnect MUX
   [playground](experiments/src/r4_interconnect_mux_playground.erl)
   and [database](experiments/src/r4_interconnect_mux_database.erl)
 * C4 fuse map
   [generate](experiments/src/c4_fuse_map_generate.erl)
   and [theory](experiments/src/c4_fuse_map_theory.erl)
   and [inverse](experiments/src/c4_fuse_map_inverse.erl)
 * R4 fuse map
   [generate](experiments/src/r4_fuse_map_generate.erl)
   and [theory](experiments/src/r4_fuse_map_theory.erl)
   and [inverse](experiments/src/r4_fuse_map_inverse.erl)
 * [Device pins](experiments/src/device_pins_experiment.erl)
 * [LC register types](experiments/src/lc_register_types_experiment.erl)
 * [IO standards](experiments/src/io_standards_experiment.erl)
 * [LUT chain](experiments/src/lc_lut_chain_experiment.erl)
 * [Security bit](experiments/src/security_bit_experiment.erl)
 * [ISP clamp](experiments/src/isp_clamp_experiment.erl)
 * [6502](experiments/src/sample_6502_experiment.erl)
 * [SAP-1](experiments/src/sample_sap1_experiment.erl)
 * [LC feedback](experiments/src/lc_feedback_experiment.erl)
 * [Register chain](experiments/src/lc_register_chain_experiment.erl)
 * [megafunctions](experiments/src/megafunctions_experiment.erl)
 * [Carry chain 3](experiments/src/lc_carry_chain_3_experiment.erl)
   ([4](experiments/src/lc_carry_chain_4_experiment.erl)
   , [5](experiments/src/lc_carry_chain_5_experiment.erl)
   , [9](experiments/src/lc_carry_chain_9_experiment.erl))
 * [UFM](experiments/src/ufm_experiment.erl)
 * UFM interconnect MUX
   [database](experiments/src/ufm_interconnect_mux_database.erl)
 * Fuse [database](experiments/src/fuse_database.erl)
 * Interconnect [sources](experiments/src/rust_sources.erl)

## Maps

A mapping to and from fuse numbers, locations & names is encoded in
[fuse map](experiments/src/fuse_map.erl).

The following mux mappings are encoded:

 * [Global MUX](experiments/src/global_mux_map.erl)
 * [LC data MUX](experiments/src/lc_data_mux_map.erl)
 * [LAB control MUX](experiments/src/lab_control_mux_map.erl)
 * [IOC output MUX](experiments/src/ioc_output_mux_map.erl)
 * [IOC enable MUX](experiments/src/ioc_enable_mux_map.erl)
 * [C4 fuse](experiments/src/c4_fuse_map.erl)
 * [R4 fuse](experiments/src/r4_fuse_map.erl)

## Databases

Some MUX databases have been collected:

 * [LAB interconnect MUX](experiments/src/lab_interconnect_mux_database.erl)
   `database/*.lab-interconnect`
 * [IOB interconnect MUX](experiments/src/iob_interconnect_mux_database.erl)
   `database/*.iob-interconnect`
 * [C4 interconnect MUX](experiments/src/c4_interconnect_mux_database.erl)
   `database/*.c4-interconnect`
 * [R4 interconnect MUX](experiments/src/r4_interconnect_mux_database.erl)
   `database/*.r4-interconnect`
 * [UFM interconnect MUX](experiments/src/ufm_interconnect_mux_database.erl)
   `database/*.ufm-interconnect`

All known fuses are also dumped for import into rust tools:

 * [Fuses](experiments/src/fuse_database.erl) `database/*.fuses`
 * Interconnect [sources](experiments/src/rust_sources.erl)
   `rust/device/*.sources`

## Device Fuses

### `{device, reset}`

This enables the device wide reset pin
that resets all registers in the device

### `{device, output_enable}`

This enables the device wide output enable pin
to control the output enable for every output pin on the device.

## Global Fuses

### `{global(), row, off}` and `{global(), {column, #}, off}`

Each of the four global clock networks are driven horizontally (row)
and then to each column. Unused clock networks are turned off either
at individual columns or for the whole row.

### `{global(), internal}`

Each of the global clock networks can be driven from either:

 * dedicated pins, or
 * internal interconnects.

This fuse selects the internal interconnects.

### `{{global, X, Y}, N, from3, mux#}`, `{{global, X, Y}, N, from4/6, mux#}` and `{{global, X, Y}, N, invert}`

Selects an interconnect into each of the four global networks.

For the 5M240Z, 6 x 3 muxes are repurposed from unused IOC output
and enable muxes.

For other densities, 4 x 3 muxes selecting from 10 interconnects
are located in the special UFM blocks.

### `{user_code, bit()}`

There are 32 bits of user oode numbered LSB (0) to MSB (31).

The user code bits are stored in the POF file inverted,
so a user code bit of `1` is stored as a `0`.

## IOB Fuses

### `{iob(), {interconnect, #}, direct_link}`

Selects a direct-link from a neighbouring LAB onto an IOB's interconnect.

Only applicable for IOBs on the left and right sides.

Not used at the same time as the interconnect muxes below.

### `{iob(), {interconnect, #}, from4, mux#}`, `{iob(), {interconnect, #}, from3, mux#}` and `{iob(), {interconnect, 8/17}, from4, gclk}`

Selects a direct-link, C4 or R4 onto an IOB's interconnect.

Not used at the same time as the dedicated direct-link above.

Each interconnect has a two dimentional mux of size 4 x 3
selecting from 12 alternative sources.

As a special case, row interconnects 8 and 17 have an extra fuse expanding the
mux to size 5 x 3. The extra 3 alternatives source from the global clock
networks.

## IOC Fuses

### `{ioc(), input_off}`

The IOC input is turned off when the POF but is `0`.

This could also be called *output only*.

### `{ioc(), output3, mux#}` and `{ioc(), output4/6, mux#}`

The IOC outputs are selected from local interconnects
via two dimentional muxes.
Side IOCs have one of size 3, and the other of size 6.
Top/bottom IOCS have one of size 3, and the other of size 4.

These muxes are one-cold.

For example the fuses
`{ioc(), output6, mux2}` and `{ioc(), output3, mux1}`
select local interconnect 7.

For example the fuses
`{ioc(), output4, mux3}` and `{ioc(), output3, mux2}`
select local line 9.

### `{ioc(), fast_out}`

Selects output value from fast-out link of neighbouring LAB
instead of via the output muxes.

### `{ioc(), output_invert}`

The IOC output is inverted when the POF but is `0`.

### `{ioc(), enable3, mux#}` and `{ioc(), enable4/6, mux#}`

The IOC output enables are selected from local interconnects.

See `{ioc(), output#, mux#}` for details.

### `{ioc(), enable_invert}`

The IOC is an output when the POF but is `0`.

### `{ioc(), bus_hold}` and `{ioc(), weak_pull_up}`

Each IOC can have bus-hold or weak pull-up enabled.

The feature is enabled when the POF bit is `0`.

Warning: only enable __one__ at a time.

### `{ioc(), low_current_0}` and `{ioc(), low_current_1}`

Drive strength of IOC outputs can be minimum or maximum current.

Default drive strength is maximum current.

A low current strength is enabled with a POF bit of `0`.

NOTE: Both fuses are always the same as each other.

### `{ioc(), fast_slew_rate}`

Slew Rate of IOC outputs is fast with a POF bit of `0`.

### `{ioc(), input_delay}`

Input delay on IOC inputs enabled with a POF bit of `0`.

### `{ioc(), open_drain}`

Open-Drain outputs enabled with a POF bit of `0`.

### `{ioc(), pci_compliance}`

PCI compliant outputs enabled with a POF bit of `0`.

NOTE: Only avaailable in Bank 3 of 5M1270Z and 5M2210Z.

### `{ioc(), schmitt_trigger}`

Schmitt Trigger on IOC inputs enabled with a POF bit of `0`.

NOTE: Also enabled when in output mode.

## LAB Fuses

### `{lab(), {interconnect, #}, direct_link}`

Selects a direct-link from a neighbouring LAB onto a LAB's interconnect.

Not used at the same time as the interconnect muxes below.

### `{lab(), {interconnect, #}, from4, mux#}`, `{lab(), {interconnect, #}, from3, mux#}` and `{lab(), {interconnect, 12/25}, from4, gclk}`

Selects a direct-link, C4 or R4 onto a LAB's interconnect.

Not used at the same time as the dedicated direct-link above.

Each interconnect has a two dimentional mux of size 4 x 3
selecting from 12 alternative sources.

As a special case, interconnects 12 and 25 have an extra fuse expanding the
mux to size 5 x 3. The extra 3 alternatives source from the global clock
networks.

### `{lab(), clk#, global#}` and `{lab(), clk#, control}`

Each LAB's clk# can be selected fron the four global signals (0..3)
or a control line.

These fuses form a one-shot mux with the active selection with a bit of `0`.

## `{lab(), clk1, control_0_not_1}`

When the LAB's clk1 line is selected from a control line.

The specific control line selected is:

 * a `0` bit selects 0,
 * a `1` bit selects 1.

## `{lab(), clk2_a_load, control_3_not_2}`

The LAB's clk2 & a-load lines share a common control source.

When the LAB's clk2 / a-load line is selected from a control line.

The specific control line selected is:

 * a `0` bit selects 3,
 * a `1` bit selects 2.

### `{lab(), clk#, invert}`

The LAB's clk# line is inverted.

### `{lab(), ena1, off}`

This turns off the LAB's ena1 line. LC's are then continuously enabled.

### `{lab(), ena1, control_3_not_2}`

This selects the LAB's ena1 line from either:

 * a `0` bit selects control line 3
 * a `1` bit selects control line 2

### `{lab(), ena1, invert}`

This inverts the LAB's ena1 line.

### `{lab(), ena2, off}`

This turns off the LAB's ena2 line. LC's are then continuously enabled.

## `{lab(), ena2_s_load, control_0_not_1}`

The LAB's ena2 & s-load lines share a common control source.

When the LAB's ena2 / s-load line is selected from a control line.

The specific control line selected is:

 * a `0` bit selects 0,
 * a `1` bit selects 1.

## `{lab(), ena2_s_load, invert}`

The LAB's ena2 & s-load lines share a common inversion.

The LAB's ena2 / s-load line is inverted.

## `{lab(), a_clr, global#}`

Only one of the two a-clr# lines can select from a global line.
This mux is common to those two lines and selected by `{lab(), a_clr#, global}`.

These fuses form a one-shot mux with the active selection with a bit of `0`.

## `{lab(), a_clr#, off}`

Turns of the LAB's a-clr# line.

## `{lab(), a_clr#, global}`

The LAB's a-clr# line is selected from one of the global lines (also when off).

Alternatively the line is selected from a control line.

## `{lab(), a_clr#, control_5_not_4}`

When the LAB's a-clr# line is selected from a control line.

The specific control line selected is:

 * a `0` bit selects 5,
 * a `1` bit selects 4.

## `{lab(), a_clr#, invert}`

The LAB's a-clr# line is inverted.

## `{lab(), a_load, control}`

The LAB's a-load line is selected from one of the control lines.

## `{lab(), a_load, invert}`

The LAB's a-load line is inverted.

## `{lab(), s_clr, control}`

The LAB's s-clr line is selected from one of the control lines.

## `{lab(), s_clr, invert}`

The LAB's s-clr line is inverted.

## `{lab(), s_clr, control_5_not_4}`

When the LAB's s-clr line is selected from a control line.

The specific control line selected is:

 * a `0` bit selects 5,
 * a `1` bit selects 4.

## `{lab(), s_load, control}`

The LAB's s-load line is selected from one of the control lines.

## `{lab(), s_load, not_always}`

It has a bit `0` when the s-load is disconnected or connected to a signal.

It has a bit `1` when the s-load line is a constant `1`.

## `{lab(), invert_a}`

The LAB's invert-a line (addnsub) is enabled.

## `{lab(), invert_a, control_4_not_3}`

When the LAB's invert-a line is selected from a control line.

The specific control line selected is:

 * a `0` bit selects 3,
 * a `1` bit selects 4.

## `{lab(), carry_in, invert_a}`

The LAB's carry-in is selected from:

 * a `0` bit selects from the previous LAB`s carry-out signal,
 * a `1` bit selects from the LAB's invert-a control line.

## LC Fuses

### `{lc(), clk2}`

Each LC can select between two LAB wide clocks, clk1 & clk2.

This fuse selects clk2.

### `{lc(), a_clr1}`

Each LC can select between two LAB wide a-clrs, a-clr1 & a-clr2.

This fuse selects a-clr1.

It _also_ elects out of the LAB's a-load line.

### `{lc(), data_#3, mux#}` and `{lc(), data_#6, mux#}`

The LUT inputs `data_a`, `data_b`, `data_c` & `data_d` are
selected from local interconnects via two dimentional muxes,
one of size 3, and the other of size 6.

These muxes are one-hot.

For example the fuses
`{lc(), data_a3, mux1}` and `{lc(), data_a6, mux0}`
select local interconnect 3.

For example the fuses
`{lc(), data_c3, mux2}` and `{lc(), data_c6, mux5}`
select local line 7.

### `{lc(), feedback}`

Each LC can __feedback__ the register output into it's own LUT at `data_c`.

This fuse enables that feedback.

The feedback is __inverted__, the LUT logic must take that into account.

NOTE: The normal `data_c` selection can then only be used as a `s_load` source.

### `{lc(), output_local, lut}`

Each LC can drive the LUT or register output to the local interconnect
via it's __local line__.

This fuse selects the LUT output (instead of register output).

### `{lc(), output_left | output_right, lut}`

Each LC can drive the LUT or register output to the left and right
via direct-links, fast-outs, R4s & C4s.

These fuses select the LUT output (instead of register output).

### `{lc(), carry_in}`

Most LCs can receive a carry-in from the "previous" LC.

The carry-in always comes into the `data_c` input.

The __previous__ LC to `{lc, X, Y, N}` is:

 * `{lc, X, Y, N - 1}` for `N` in `1..9`, otherwise
 * `{lc, X - 1, Y, 9}` for `N` is `0`.

NOTE: The carry-chain can go across the whole row.

NOTE: The left most `{lc, _, _, 0}` of each row cannot receive a carry-in.

This turns the __previous__ LC into __arithmetic__ mode:

 * the sum is the equivilent to the LUT with `data_d = 1` (top 8 bits of LUT)
 * the carry is the equivilent to the LUT with `data_d = 0`

The carry-out is split two ways:

 * one, to the next sum / standard LUT stage,
 * two, to the next carry stage.

One and only one of these two carry's is __inverted__ and
the the LUT logic of the next LC must take that into account.

The carry-out is __inverted__:
 * to the next carry stage for `{lc, X, Y, 4}` and `{lc, X, Y, 9}`
 * to the next sum / LUT for all other `{lc, X, Y, N}`.

### `{lc(), lut_chain, off}`

Each LC can receive a LUT input from the LUT output of the previous LC.

The input always comes into the `data_d` input.

The lut-chain input is __inverted__, the LUT logic must take that into account.

The first LC in a LAB has this fuse, but not sure where that input comes from.

### `{lc(), lut, a#b#c#d#}`

Each LC as a 16-entry LUT with a fuse per entry.

The fuse for LUT term `a AND (NOT b) AND c AND d`
is named `{lc(), lut, a1b0c1d1}`.

The stored bit is the result of the lookup.

### `{lc(), register_chain, off}`

Each LC register can chain it's output directly to the next LC without using
the second LC's LUT. It enters the second LC just before the `s-load` selection.

The first LC in a LAB has this fuse, but not sure where that input comes from.

## `{lc(), s_clr_load}`

The LC's s-clr & s-load lines are enabled.

## JTAG Fuses

### `{jtag(), tdo, from3, mux#}`, `{jtag(), tdo, from4/6, mux#}` and `{jtag(), tdo, invert}`

Selects a direct-link, C4 or R4 onto the JTAG user TDO port.

For the 5M240Z, 6 x 3 muxes are repurposed from unused IOC output
and enable muxes.

For other densities, 4 x 3 muxes selecting from 10 interconnects
are located in the special UFM blocks.

## UFM Fuses

### `{ufm(), {interconnect, #}, from4, mux#}`, `{ufm(), {interconnect, #}, from3, mux#}` and `{ufm(), {interconnect, #}, direct_link}`

Selects a direct-link, C4 or R4 onto the UFM interconnects.

Each interconnect has a two dimentional mux of size 4 x 3
selecting from 12 alternative sources.

This does not exist on the 5M240Z.

### `{ufm(), ufm_input(), from3, mux#}`, `{ufm(), ufm_input(), from4/6, mux#}` and `{ufm(), ufm_input(), invert}`

Selects a local interconnect into the UFM inputs:
 * `dr_in`,
 * `dr_shift`,
 * `dr_clk`,
 * `ar_in`,
 * `ar_shift`,
 * `ar_clk`,
 * `program`,
 * `erase` &
 * `osc_ena`.

For the 5M240Z, 6 x 3 muxes are repurposed from unused IOC output
and enable muxes.

For other densities, 4 x 3 muxes selecting from 10 interconnects
are located in the special UFM blocks.

