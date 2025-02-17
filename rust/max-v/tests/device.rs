use max_v::*;
use Control::*;
use C4InterconnectIndex::*;
use IOColumnCellNumber::*;
use IORowCellNumber::*;
use LogicCellNumber::*;
use LogicInterconnectIndex::*;

#[macro_use]
mod macros;

#[test]
fn max_v_40z_e64() {
    let device = DeviceSources::read("../device/max_v_40z_e64.sources")
        .unwrap();

    assert_device! { device
        // top left
        c4_interconnect(1, 5, C4Interconnect11).first() =>
            io_column!(2, 5, IOColumnCell2);

        // top
        c4_interconnect(3, 5, C4Interconnect2).last() =>
            io_column!(3, 5, IOColumnCell0);

        // top right
        c4_interconnect(7, 5, C4Interconnect1).source(0) =>
            io_column!(7, 5, IOColumnCell3);

        // left
        c4_interconnect(1, 4, C4Interconnect7).first() =>
            io_row!(1, 4, IORowCell0);

        // logic
        c4_interconnect(5, 2, C4Interconnect1).first() =>
            logic_cell!(6, 2, LogicCell1, Left);

        // bottom left
        c4_interconnect(1, 0, C4Interconnect11).first() =>
            io_column!(2, 0, IOColumnCell3);

        // bottom
        c4_interconnect(4, 0, C4Interconnect9).first() =>
            io_column!(4, 0, IOColumnCell2);

        // bottom right
        c4_interconnect(7, 0, C4Interconnect7).first() =>
            io_column!(7, 0, IOColumnCell1);

        // logic-cell (a)
        logic_cell(6, 4, LogicCell1, A).source(0) =>
            logic_interconnect!(6, 4, LogicInterconnect0);

        // logic-cell (b)
        logic_cell(5, 2, LogicCell7, B).source(13) =>
            logic_cell!(5, 2, LogicCell0, Local);

        // logic-control (even, c)
        logic_control(3, 1, Control2).source(4) =>
            logic_interconnect!(3, 1, LogicInterconnect7);

        // logic-control (odd, d)
        logic_control(3, 1, Control5).source(15) =>
            logic_cell!(3, 1, LogicCell1, Local);

        // logic-interconnect
        logic_interconnect(2, 4, LogicInterconnect5).first() =>
            logic_cell!(2, 4, LogicCell0, Left);

        // logic-interconnect (global)
        logic_interconnect(7, 4, LogicInterconnect25).last() =>
            global!(Global3);

        io_row_pin(1, 3, IORowCell3) => "7";
    }

}

#[test]
fn max_v_160z_t100() {
    let device = DeviceSources::read("../device/max_v_160z_t100.sources")
        .unwrap();

    assert_device! { device
        io_column_pin(2, 0, IOColumnCell2) => "27";
    }
}

#[test]
fn max_v_240z_m100() {
    let _ = DeviceSources::read("../device/max_v_240z_m100.sources").unwrap();
}

#[test]
fn max_v_240z_t144() {
    let _ = DeviceSources::read("../device/max_v_240z_t144.sources").unwrap();
}

#[test]
fn max_v_570z_f256() {
    let device = DeviceSources::read("../device/max_v_570z_f256.sources")
        .unwrap();

    assert_device! { device
        io_column_pin(7, 8, IOColumnCell3) => "C7";

        io_row_pin(13, 1, IORowCell0) => "M15";
    }
}

#[test]
fn max_v_570z_t100() {
    let device = DeviceSources::read("../device/max_v_570z_t100.sources")
        .unwrap();

    assert_device! { device
        // bottom left
        c4_interconnect(0, 3, C4Interconnect3).source(0) =>
            io_column!(1, 3, IOColumnCell3);

        // grow 3
        c4_interconnect(9, 3, C4Interconnect11).source(0) =>
            logic_cell!(10, 3, LogicCell6, Left);

        // grow 2
        c4_interconnect(9, 2, C4Interconnect6).source(0) =>
            logic_cell!(10, 2, LogicCell8, Left);

        // grow 1
        c4_interconnect(9, 1, C4Interconnect0).source(0) =>
            logic_cell!(10, 1, LogicCell0, Left);

        // grow corner
        c4_interconnect(9, 0, C4Interconnect12).source(0) =>
            io_column!(10, 0, IOColumnCell2);

        // bottom right
        c4_interconnect(12, 0, C4Interconnect7).source(0) =>
            io_column!(12, 0, IOColumnCell1);
    }
}

#[test]
fn max_v_1270z_t144() {
    let _ = DeviceSources::read("../device/max_v_1270z_t144.sources").unwrap();
}

#[test]
fn max_v_2210z_f324() {
    let device = DeviceSources::read("../device/max_v_2210z_f324.sources")
        .unwrap();

    assert_device! { device
        io_column_pin(16, 14, IOColumnCell1) => "F11";

        io_row_pin(21, 7, IORowCell3) => "K13";
    }
}

