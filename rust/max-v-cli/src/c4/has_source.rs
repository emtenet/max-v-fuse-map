use crate::*;
use max_v::*;

pub fn run(devices: &[DeviceSources; 4], kind: PortKind) {
    block_table::print::<C4InterconnectIndex, _>(|interconnect, source|
        Print::Has {
            max_v_240z: has(&devices[0], interconnect, source, kind),
            max_v_570z: has(&devices[1], interconnect, source, kind),
            max_v_1270z: has(&devices[2], interconnect, source, kind),
            max_v_2210z: has(&devices[3], interconnect, source, kind),
        }
    );
}

fn has(
    device: &DeviceSources,
    i: C4InterconnectIndex,
    source: usize,
    kind: PortKind,
) -> bool {
    let density = device.density_layout();

    for y in density.y_iter() {
        for x in density.x_iter() {
            if let Some(interconnect) = device.c4_interconnect(x, y, i) {
                if let Some(source) = interconnect.source(source) {
                    if let Some(k) = source.kind() {
                        if k == kind {
                            return true;
                        }
                    }
                }
            }
        }
    }

    false
}

