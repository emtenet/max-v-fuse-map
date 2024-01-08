
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

## Global Fuses

### `{user_code, bit()}`

There are 32 bits of user oode numbered LSB (0) to MSB (31).

The user code bits are stored in the POF file inverted,
so a user code bit of `1` is stored as a `0`.

## IOC Fuses

### `{ioc(), input_off}`

The IOC input is turned off when the POF but is `0`.

This could also be called *output only*.

### `{ioc(), output_invert}`

The IOC output is inverted when the POF but is `0`.

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

### `{lab(), clk#, global#}`

Each LAB can select a clk1 & clk2
from amongst the four global signals (0..3) (and others).

The four global signals are selected with a one-shot fuse per selection,
the selection is active with a bit of `0`.

### `{lab(), clk#, invert}`

Each LAB's clk1 & clk2 can be inverted.

Invert is selected when the bit is `0`

## LC Fuses

### `{lc(), lut, a#b#c#d#}`

Each LC as a 16-entry LUT with a fuse per entry.

The fuse for LUT term `a AND (NOT b) AND c AND d`
is named `{lc(), lut, a1b0c1d1}`.

The stored bit is the result of the lookup.

