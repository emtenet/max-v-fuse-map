use max_v::*;
use Control::*;
use C4InterconnectIndex::*;
use IOColumnCellNumber::*;
use IORowCellNumber::*;
use LogicCellNumber::*;
use LogicInterconnectIndex::*;

macro_rules! assert_c4_interconnect {
    {$device:ident ( $x:literal, $y:literal, $n:ident) => $port:expr} => {{
        let interconnect = $device.c4_interconnect(
            X($x), Y($y), $n,
        ).unwrap();
        let mut sources = interconnect.sources();
        assert_eq!($port, sources.next().unwrap().1);
    }};
}

macro_rules! io_column {
    ($x:literal, $y:literal, $n:ident) => {
        Port::IOColumnCellOutput { x: X($x), y: Y($y), n: $n }
    };
}

macro_rules! io_row {
    ($x:literal, $y:literal, $n:ident) => {
        Port::IORowCellOutput { x: X($x), y: Y($y), n: $n }
    };
}

macro_rules! logic_cell {
    ($x:literal, $y:literal, $n:ident, $output:ident) => {
        Port::LogicCellOutput {
            x: X($x), y: Y($y), n: $n, output: LogicCellOutput::$output
        }
    };
}

#[test]
fn max_v_40z_e64() {
    let device = DeviceSources::read("../device/max_v_40z_e64.sources")
        .unwrap();

    assert_c4_interconnect! { // top left
        device(1, 5, C4Interconnect11) => io_column!(2, 5, IOColumnCell2)
    }

    assert_c4_interconnect! { // top
        device(3, 5, C4Interconnect2) => io_column!(3, 5, IOColumnCell3)
    }

    assert_c4_interconnect! { // top right
        device(7, 5, C4Interconnect1) => io_column!(7, 5, IOColumnCell3)
    }

    assert_c4_interconnect! { // left
        device(1, 4, C4Interconnect7) => io_row!(1, 4, IORowCell0)
    }

    assert_c4_interconnect! { // logic
        device(5, 2, C4Interconnect1) => logic_cell!(6, 2, LogicCell1, Left)
    }

    assert_c4_interconnect! { // bottom left
        device(1, 0, C4Interconnect11) => io_column!(2, 0, IOColumnCell3)
    }

    assert_c4_interconnect! { // bottom
        device(4, 0, C4Interconnect9) => io_column!(4, 0, IOColumnCell2)
    }

    assert_c4_interconnect! { // bottom right
        device(7, 0, C4Interconnect7) => io_column!(7, 0, IOColumnCell1)
    }

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

    assert_eq!(
        Some(PinSource::Row {
            x: X(1), y: Y(3), n: IORowCell3,
        }),
        device.pin("7"),
    );
    assert_eq!(
        "7",
        device.io_row_cell(X(1), Y(3), IORowCell3).unwrap()
            .pin_name().as_str(),
    );
}

#[test]
fn max_v_160z_t100() {
    let device = DeviceSources::read("../device/max_v_160z_t100.sources")
        .unwrap();

    assert_eq!(
        Some(PinSource::Column {
            x: X(2), y: Y(0), n: IOColumnCell2,
        }),
        device.pin("27"),
    );
    assert_eq!(
        "27",
        device.io_column_cell(X(2), Y(0), IOColumnCell2).unwrap()
            .pin_name().as_str(),
    );
}

#[test]
fn max_v_240z_t144() {
    let _ = DeviceSources::read("../device/max_v_240z_t144.sources").unwrap();
}

#[test]
fn max_v_570z_t100() {
    let device = DeviceSources::read("../device/max_v_570z_t100.sources")
        .unwrap();

    assert_c4_interconnect! { // bottom left
        device(0, 3, C4Interconnect3) => io_column!(1, 3, IOColumnCell3)
    }

    assert_c4_interconnect! { // grow 3
        device(9, 3, C4Interconnect11) => logic_cell!(10, 3, LogicCell6, Left)
    }

    assert_c4_interconnect! { // grow 2
        device(9, 2, C4Interconnect6) => logic_cell!(10, 2, LogicCell8, Left)
    }

    assert_c4_interconnect! { // grow 1
        device(9, 1, C4Interconnect0) => logic_cell!(10, 1, LogicCell0, Left)
    }

    assert_c4_interconnect! { // grow corner
        device(9, 0, C4Interconnect12) => io_column!(10, 0, IOColumnCell2)
    }

    assert_c4_interconnect! { // bottom right
        device(12, 0, C4Interconnect7) => io_column!(12, 0, IOColumnCell1)
    }
}

#[test]
fn max_v_1270z_t144() {
    let _ = DeviceSources::read("../device/max_v_1270z_t144.sources").unwrap();
}

