use crate::*;
use max_v::*;

pub fn run<I>(
    devices: &[DeviceSources],
    axis: PortAxis,
    interconnect: I,
    source: usize,
)
where
    I: BlockInterconnectIndex,
{
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

pub fn run_rel_x<I>(
    devices: &[DeviceSources],
    interconnect: I,
    source: usize,
)
where
    I: BlockInterconnectIndex,
{
    print(devices, interconnect, source, &print_rel_x);
}

pub fn run_rel_y<I>(
    devices: &[DeviceSources],
    interconnect: I,
    source: usize,
)
where
    I: BlockInterconnectIndex,
{
    print(devices, interconnect, source, &print_rel_y);
}

pub fn run_count<I>(
    devices: &[DeviceSources],
    interconnect: I,
)
where
    I: BlockInterconnectIndex,
{
    block_grid::print(devices, |device, x, y|
        print_count(device, x, y, interconnect)
    );
}

fn print_count<I>(
    device: &DeviceSources,
    x: X,
    y: Y,
    interconnect: I,
) -> Print
where
    I: BlockInterconnectIndex,
{
    if let Some(interconnect) = interconnect.interconnect_at(device, x, y) {
        Print::Unsigned(interconnect.source_count())
    } else {
        Print::Blank
    }
}

fn print<I, W>(
    devices: &[DeviceSources],
    interconnect: I,
    source: usize,
    with: &W,
)
where
    I: BlockInterconnectIndex,
    W: Fn(X, Y, Port) -> Print,
{
    block_grid::print(devices, |device, x, y|
        print_block(device, x, y, interconnect, source, with)
    );
}

fn print_block<I, W>(
    device: &DeviceSources,
    x: X,
    y: Y,
    interconnect: I,
    source: usize,
    with: &W,
) -> Print
where
    I: BlockInterconnectIndex,
    W: Fn(X, Y, Port) -> Print,
{
    if let Some(interconnect) = interconnect.interconnect_at(device, x, y) {
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


