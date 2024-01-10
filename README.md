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
 * [local line](experiments/src/local_line_experiment.erl)
 * [LAB interconnect limit](experiments/src/lab_interconnect_limit_experiment.erl)
 * LC data MUX [playground](experiments/src/lc_data_mux_playground.erl)
   and [theory](experiments/src/lc_data_mux_theory.erl)
 * IOC output MUX [playground](experiments/src/ioc_output_mux_playground.erl)
   and [theory](experiments/src/ioc_output_mux_theory.erl)
 * IOC enable MUX [playground](experiments/src/ioc_output_mux_playground.erl)
   and [theory](experiments/src/ioc_output_mux_theory.erl)
 * [LC output](experiments/src/lc_output_experiment.erl)
 * [LAB ena1](experiments/src/lab_ena1_experiment.erl)
 * [LAB ena2](experiments/src/lab_ena2_experiment.erl)
 * [LAB ena2 vs s-load](experiments/src/lab_ena2_s_load_experiment.erl)
 * [LAB a-load](experiments/src/lab_a_load_experiment.erl)

## Maps

A mapping to and from fuse numbers, locations & names is encoded in
[fuse map](experiments/src/fuse_map.erl).

The following mux mappings are encoded:

 * [LC data MUX](experiments/src/lc_data_mux_map.erl)
 * [IOC output MUX](experiments/src/ioc_output_mux_map.erl)
 * [IOC enable MUX](experiments/src/ioc_enable_mux_map.erl)

## Global Fuses

### `{user_code, bit()}`

There are 32 bits of user oode numbered LSB (0) to MSB (31).

The user code bits are stored in the POF file inverted,
so a user code bit of `1` is stored as a `0`.

## IOC Fuses

### `{ioc(), input_off}`

The IOC input is turned off when the POF but is `0`.

This could also be called *output only*.

### `{ioc(), output3, mux#}`, `{ioc(), output4, mux#}` and `{ioc(), output6, mux#}`

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

### `{ioc(), enable3, mux#}`, `{ioc(), enable4, mux#}` and `{ioc(), enable6, mux#}`

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

## `{lab(), s_load, control}`

The LAB's s-load line is selected from one of the control lines.

## `{lab(), s_load, unknown}`

This fuse seems to be relate to a LAB's s-load line.

It has a bit `1` when the s-load line is a constant `1`.

## LC Fuses

### `{lc(), clk2}`

Each LC can select between two LAB wide clocks, clk1 & clk2.

This fuse selects clk2.

### `{lc(), a_clr1}`

Each LC can select between two LAB wide a-clrs, a-clr1 & a-clr2.

This fuse selects a-clr1.

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

### `{lc(), local_line}`

Each LC can drive the LUT or register output to the local interconnect
via it's __local line__.

This fuse either:

 * enables that output, or
 * selects between the LUT and registers to output.

### `{lc(), lut_out, left | right}`

Each LC can drive the LUT output to the left or right
via direct-links, fast-outs, r4s & c4s.

### `{lc(), lut, a#b#c#d#}`

Each LC as a 16-entry LUT with a fuse per entry.

The fuse for LUT term `a AND (NOT b) AND c AND d`
is named `{lc(), lut, a1b0c1d1}`.

The stored bit is the result of the lookup.

## `{lc(), s_load}`

The LC's s-load line is enabled.

