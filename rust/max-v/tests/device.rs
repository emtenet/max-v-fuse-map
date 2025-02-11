use max_v::*;

#[test]
fn max_v_40z_e64() {
    let device = DeviceSources::read("../device/max_v_40z_e64.toml").unwrap();
    let interconnect = device.logic_interconnect(
        X(2), Y(3), LogicInterconnectIndex::LogicInterconnect5,
    ).unwrap();
    let mut sources = interconnect.iter();
    assert_eq!(
        Port::LogicCellOutput {
            x: X(2),
            y: Y(3),
            n: LogicCellNumber::LogicCell0,
            output: LogicCellOutput::Left,
        },
        sources.next().unwrap().1,
    );
    let interconnect = device.logic_interconnect(
        X(7), Y(4), LogicInterconnectIndex::LogicInterconnect25,
    ).unwrap();
    let sources = interconnect.iter();
    assert_eq!(
        Port::Global {
            global: Global::Global3,
        },
        sources.last().unwrap().1,
    );
}

#[test]
fn max_v_160z_t100() {
    let _ = DeviceSources::read("../device/max_v_160z_t100.toml").unwrap();
}

#[test]
fn max_v_240z_t144() {
    let _ = DeviceSources::read("../device/max_v_240z_t144.toml").unwrap();
}

#[test]
fn max_v_570z_t100() {
    let _ = DeviceSources::read("../device/max_v_570z_t100.toml").unwrap();
}

#[test]
fn max_v_1270z_t144() {
    let _ = DeviceSources::read("../device/max_v_1270z_t144.toml").unwrap();
}

