use max_v::*;
use Control::*;
use LogicCellNumber::*;
use LogicInterconnectIndex::*;

#[test]
fn max_v_40z_e64() {
    let device = DeviceSources::read("../device/max_v_40z_e64.toml").unwrap();

    // logic-cell (a)
    let interconnect = device.logic_cell(
        X(6), Y(4),
        LogicCell1, LogicCellInput::A,
    ).unwrap();
    assert_eq!(
        Port::LogicCellInput {
            x: X(6), y: Y(4),
            n: LogicCell1,
            input: LogicCellInput::A,
        },
        interconnect.port(),
    );
    assert_eq!(
        Port::LogicInterconnect {
            x: X(6), y: Y(4), i: LogicInterconnect0,
        },
        interconnect.source(0).unwrap(),
    );

    // logic-cell (b)
    let interconnect = device.logic_cell(
        X(5), Y(2),
        LogicCell7, LogicCellInput::B,
    ).unwrap();
    assert_eq!(
        Port::LogicCellOutput {
            x: X(5), y: Y(2), n: LogicCell0,
            output: LogicCellOutput::Local,
        },
        interconnect.source(13).unwrap(),
    );

    // logic-control (even, c)
    let interconnect = device.logic_control(X(3), Y(1), Control2).unwrap();
    assert_eq!(
        Port::LogicControl { x: X(3), y: Y(1), control: Control2 },
        interconnect.port(),
    );
    assert_eq!(
        Port::LogicInterconnect {
            x: X(3), y: Y(1), i: LogicInterconnect7,
        },
        interconnect.source(4).unwrap(),
    );

    // logic-control (odd, d)
    let interconnect = device.logic_control(X(3), Y(1), Control5).unwrap();
    assert_eq!(
        Port::LogicCellOutput {
            x: X(3), y: Y(1), n: LogicCell1,
            output: LogicCellOutput::Local,
        },
        interconnect.source(15).unwrap(),
    );

    // logic-interconnect
    let interconnect = device.logic_interconnect(
        X(2), Y(3), LogicInterconnect5,
    ).unwrap();
    let mut sources = interconnect.sources();
    assert_eq!(
        Port::LogicCellOutput {
            x: X(2), y: Y(3), n: LogicCell0,
            output: LogicCellOutput::Left,
        },
        sources.next().unwrap().1,
    );

    // logic-interconnect (global)
    let interconnect = device.logic_interconnect(
        X(7), Y(4), LogicInterconnect25,
    ).unwrap();
    let sources = interconnect.sources();
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

