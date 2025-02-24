use crate::*;
use max_v::*;

pub fn run(
    devices: &[DeviceSources],
    axis: PortAxis,
    interconnect: R4InterconnectIndex,
    source: usize,
) {
    match axis {
        PortAxis::Kind =>
            print(devices, interconnect, source, &print_kind),

        PortAxis::X =>
            print(devices, interconnect, source, &print_x),

        PortAxis::Y =>
            print(devices, interconnect, source, &print_y),

        PortAxis::I =>
            print(devices, interconnect, source, &print_i),
    }
}

pub fn run_rel_x(
    devices: &[DeviceSources],
    interconnect: R4InterconnectIndex,
    source: usize,
) {
    print(devices, interconnect, source, &print_rel_x);
}

pub fn run_rel_y(
    devices: &[DeviceSources],
    interconnect: R4InterconnectIndex,
    source: usize,
) {
    print(devices, interconnect, source, &print_rel_y);
}

pub fn run_count(
    devices: &[DeviceSources],
    interconnect: R4InterconnectIndex,
) {
    block_grid::print(devices, |device, x, y|
        print_count(device, x, y, interconnect)
    );
}

fn print_count(
    device: &DeviceSources,
    x: X,
    y: Y,
    interconnect: R4InterconnectIndex,
) -> Print {
    if let Some(interconnect) =  device.r4_interconnect(x, y, interconnect) {
        Print::Unsigned(interconnect.source_count())
    } else {
        Print::Blank
    }
}

fn print<W>(
    devices: &[DeviceSources],
    interconnect: R4InterconnectIndex,
    source: usize,
    with: &W,
)
where
    W: Fn(X, Y, Port) -> Print,
{
    block_grid::print(devices, |device, x, y|
        print_block(device, x, y, interconnect, source, with)
    );
}

fn print_block<W>(
    device: &DeviceSources,
    x: X,
    y: Y,
    interconnect: R4InterconnectIndex,
    source: usize,
    with: &W,
) -> Print
where
    W: Fn(X, Y, Port) -> Print,
{
    if let Some(interconnect) =  device.r4_interconnect(x, y, interconnect) {
        if let Some(source) = interconnect.source(source) {
            with(x, y, source)
        } else {
            Print::Dash
        }
    } else {
        Print::Blank
    }
}

fn print_kind(_x: X, _y: Y, source: Port) -> Print {
    if let Some(kind) = source.kind() {
        Print::Kind(kind)
    } else {
        Print::Dot
    }
}

fn print_x(_x: X, _y: Y, source: Port) -> Print {
    if let Some(x) = source.x() {
        Print::Unsigned(usize::from(x))
    } else {
        Print::Dot
    }
}

fn print_rel_x(xx: X, _y: Y, source: Port) -> Print {
    if let Some(x) = source.x() {
        Print::Signed(isize::from(x) - isize::from(xx))
    } else {
        Print::Dot
    }
}

fn print_y(_x: X, _y: Y, source: Port) -> Print {
    if let Some(y) = source.y() {
        Print::Unsigned(usize::from(y))
    } else {
        Print::Dot
    }
}

fn print_rel_y(_x: X, yy: Y, source: Port) -> Print {
    if let Some(y) = source.y() {
        Print::Signed(isize::from(y) - isize::from(yy))
    } else {
        Print::Dot
    }
}

fn print_i(_x: X, _y: Y, source: Port) -> Print {
    if let Some(i) = source.i() {
        Print::Unsigned(i)
    } else {
        Print::Dot
    }
}

