use max_v::*;
use Control::*;
use C4InterconnectIndex::*;
use Global::*;
use IOColumnCellNumber::*;
use IOColumnInterconnectIndex::*;
use IORowCellNumber::*;
use IORowInterconnectIndex::*;
use LogicCellNumber::*;
use LogicInterconnectIndex::*;
use R4InterconnectIndex::*;
use UFMInterconnectIndex::*;

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

        global_input(Global0).first() =>
            io_row_interconnect!(1, 3, IORowInterconnect0);

        global_pin(Global0) => Some(1, 3, IORowCell3);
        global_pin(Global1) => Some(1, 2, IORowCell0);
        global_pin(Global2) => Some(8, 2, IORowCell0);
        global_pin(Global3) => Some(8, 3, IORowCell4);

        io_row_pin(1, 3, IORowCell3) => "7";

        // io interconnect (bottom)
        io_column_interconnect(2, 0, IOColumnInterconnect4).first() =>
            c4_interconnect!(2, 1, C4Interconnect12);

        // io interconnect (bottom)
        io_column_interconnect(2, 0, IOColumnInterconnect4).source(7) =>
            global!(Global0);

        // io interconnect (top)
        io_column_interconnect(4, 5, IOColumnInterconnect2).first() =>
            c4_interconnect!(4, 1, C4Interconnect3);

        // io interconnect (left)
        io_row_interconnect(1, 2, IORowInterconnect14).first() =>
            logic_cell!(2, 2, LogicCell8, Left);

        // io interconnect (right)
        io_row_interconnect(8, 2, IORowInterconnect17).first() =>
            logic_cell!(7, 2, LogicCell9, Right);

        jtag_input(TDO).source(6) =>
            io_row_interconnect!(1, 4, IORowInterconnect6);

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

        // r4 (left)
        r4_interconnect(1, 2, R4Interconnect5).first() =>
            jtag!(TMS);

        // r4 (logic)
        r4_interconnect(3, 3, R4Interconnect15).source(8) =>
            logic_cell!(3, 3, LogicCell8, Right);

        // r4 (right)
        r4_interconnect(8, 4, R4Interconnect0).source(6) =>
            c4_interconnect!(7, 0, C4Interconnect7);

        ufm_input(DrClk).last() =>
            io_row_interconnect!(1, 1, IORowInterconnect17);

        ufm_input(Erase).source(7) =>
            io_row_interconnect!(1, 2, IORowInterconnect7);
    }
}

#[test]
fn max_v_40z_m64() {
    let device = DeviceSources::read("../device/max_v_40z_m64.sources")
        .unwrap();

    assert_device! { device
        global_pin(Global0) => None;
        global_pin(Global1) => Some(1, 2, IORowCell0);
        global_pin(Global2) => Some(8, 2, IORowCell0);
        global_pin(Global3) => None;
    }
}

#[test]
fn max_v_160z_t100() {
    let device = DeviceSources::read("../device/max_v_160z_t100.sources")
        .unwrap();

    assert_device! { device
        global_pin(Global0) => Some(1, 3, IORowCell3);
        global_pin(Global1) => Some(1, 2, IORowCell0);
        global_pin(Global2) => Some(8, 2, IORowCell0);
        global_pin(Global3) => Some(8, 3, IORowCell4);

        io_column_pin(2, 0, IOColumnCell2) => "27";
    }
}

#[test]
fn max_v_240z_m100() {
    let _ = DeviceSources::read("../device/max_v_240z_m100.sources").unwrap();
}

#[test]
fn max_v_240z_t100() {
    let _ = DeviceSources::read("../device/max_v_240z_t100.sources").unwrap();
}

#[test]
fn max_v_240z_t144() {
    let _ = DeviceSources::read("../device/max_v_240z_t144.sources").unwrap();
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
fn max_v_570z_t144() {
    let _ = DeviceSources::read("../device/max_v_570z_t144.sources").unwrap();
}

#[test]
fn max_v_570z_f256() {
    let device = DeviceSources::read("../device/max_v_570z_f256.sources")
        .unwrap();

    assert_device! { device
        global_input(Global1).source(3) =>
            ufm_interconnect!(9, 3, UFMInterconnect3);

        global_pin(Global0) => Some(0, 5, IORowCell0);
        global_pin(Global1) => Some(0, 5, IORowCell1);
        global_pin(Global2) => Some(13, 4, IORowCell4);
        global_pin(Global3) => Some(13, 4, IORowCell3);

        io_column_pin(7, 8, IOColumnCell3) => "C7";

        io_row_pin(13, 1, IORowCell0) => "M15";

        jtag_input(TDO).source(1) =>
            ufm_interconnect!(9, 2, UFMInterconnect1);

        // r4 (grow)
        r4_interconnect(9, 1, R4Interconnect8).source(4) =>
            r4_interconnect!(10, 1, R4Interconnect8);

        // r4 (ufm)
        r4_interconnect(9, 2, R4Interconnect15).first() =>
            logic_cell!(10, 2, LogicCell9, Left);

        // r4 (ufm)
        r4_interconnect(9, 3, R4Interconnect9).source(3) =>
            ufm!(Busy);

        ufm_input(ArIn).last() =>
            ufm_interconnect!(9, 3, UFMInterconnect9);

        ufm_input(Program).source(2) =>
            ufm_interconnect!(9, 2, UFMInterconnect2);
    }
}

#[test]
fn max_v_1270z_t144() {
    let _ = DeviceSources::read("../device/max_v_1270z_t144.sources").unwrap();
}

#[test]
fn max_v_1270z_f324() {
    let _ = DeviceSources::read("../device/max_v_1270z_f324.sources") .unwrap();
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

