use crate::*;
use max_v::*;
use std::collections::HashMap;

pub fn run<I>(devices: &[DeviceSources])
where
    I: BlockInterconnectIndex,
{
    block_table::print::<I, _>(|interconnect, source|
        print(devices, interconnect, source)
    );
}

fn print<I>(devices: &[DeviceSources], interconnect: I, source: usize) -> Print
where
    I: BlockInterconnectIndex,
{
    match get(devices, interconnect, source) {
        Some(kind) =>
            Print::Kind(kind),

        None =>
            Print::Dash,
    }
}

fn get<I>(devices: &[DeviceSources], i: I, source: usize) -> Option<PortKind>
where
    I: BlockInterconnectIndex,
{
    let mut map: HashMap<PortKind, usize> = HashMap::new();

    for device in devices {
        let density = device.density_layout();

        for y in density.y_iter() {
            for x in density.x_iter() {
                if let Some(interconnect) = i.interconnect_at(device, x, y) {
                    if let Some(source) = interconnect.source(source) {
                        if let Some(kind) = source.kind() {
                            *map.entry(kind).or_insert(0) += 1;
                        }
                    }
                }
            }
        }
    }

    let mut found = map.into_iter();

    if let Some((mut best_kind, mut best_count)) = found.next() {
        while let Some((kind, count)) = found.next() {
            if count > best_count {
                best_kind = kind;
                best_count = count;
            }
        }

        Some(best_kind)
    } else {
        None
    }
}

