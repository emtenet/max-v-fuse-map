use std::collections::HashSet;
use std::fmt::Debug;
use std::fs::read_to_string;
use max_v::*;

#[test]
fn max_v_240z() {
    let s = read_to_string("../../database/max_v_240z.fuses").unwrap();
    let mut db: HashSet<Fuse> = HashSet::with_capacity(34_000);
    fuses(s, &mut db, &MAX_V_240Z);
    only_fuses(&db, &MAX_V_240Z);
}

#[test]
fn max_v_570z() {
    use C4InterconnectIndex::*;

    let s = read_to_string("../../database/max_v_570z.fuses").unwrap();
    let mut db: HashSet<Fuse> = HashSet::with_capacity(77_000);
    fuses(s, &mut db, &MAX_V_570Z);
    // should be in the database?
    db.insert(Fuse::C4Interconnect {
        x: X(8), y: Y(3), i: C4Interconnect3,
        fuse: C4InterconnectFuse::IODataIn1,
    });
    only_fuses(&db, &MAX_V_570Z);
}

#[test]
fn max_v_1270z() {
    use C4InterconnectIndex::*;

    let s = read_to_string("../../database/max_v_1270z.fuses").unwrap();
    let mut db: HashSet<Fuse> = HashSet::with_capacity(163_000);
    fuses(s, &mut db, &MAX_V_1270Z);
    // should be in the database?
    db.insert(Fuse::C4Interconnect {
        x: X(10), y: Y(3), i: C4Interconnect3,
        fuse: C4InterconnectFuse::IODataIn1,
    });
    only_fuses(&db, &MAX_V_1270Z);
}

#[test]
fn max_v_2210z() {
    use C4InterconnectIndex::*;

    let s = read_to_string("../../database/max_v_2210z.fuses").unwrap();
    let mut db: HashSet<Fuse> = HashSet::with_capacity(277_000);
    fuses(s, &mut db, &MAX_V_2210Z);
    // should be in the database?
    db.insert(Fuse::C4Interconnect {
        x: X(12), y: Y(3), i: C4Interconnect3,
        fuse: C4InterconnectFuse::IODataIn1,
    });
    only_fuses(&db, &MAX_V_2210Z);
}

fn fuses(s: String, fuses: &mut HashSet<Fuse>, density: &DensityLayout) {
    for s in s.lines() {
        let (index, fuse) = fuse(s, density);
        assert_eq!(
            (fuse, Ok(index)),
            (fuse, fuse.to_index(density)),
        );
        fuses.insert(fuse);
    }
}

fn fuse(s: &str, density: &DensityLayout) -> (usize, Fuse) {
    let (index, s) = usize_once(s, "=");
    if let Some(s) = s.strip_prefix("{{c4,") {
        let (x, s) = x_once(s, ",");
        let (y, s) = y_once(s, "},{mux,");
        let (i, s) = try_usize_once(s, "},");
        let fuse = c4_fuse(s);
        (index, Fuse::C4Interconnect { x, y, i, fuse })
    } else if s == "{device,output_enable}" {
        (index, Fuse::DeviceOutputEnable)
    } else if s == "{device,reset}" {
        (index, Fuse::DeviceReset)
    } else if let Some(s) = s.strip_prefix("{{global,") {
        if let Some(1) = s.find('}') {
            let (i, s) = try_usize_once(s, "},");
            let fuse = global_fuse(s);
            (index, Fuse::Global { index: i, fuse })
        } else {
            let (_x, s) = x_once(s, ",");
            let (_y, s) = y_once(s, "},");
            let (i, s) = try_usize_once(s, ",");
            let fuse = source_fuse(s, density);
            (index, Fuse::GlobalSource { index: i, fuse })
        }
    } else if let Some(s) = s.strip_prefix("{{iob,") {
        let (x, s) = x_once(s, ",");
        let (y, s) = y_once(s, "},");
        if let Some(s) = s.strip_prefix("{interconnect,") {
            match density.io_block(x, y) {
                Some(DensityIOBlock::Top) | Some(DensityIOBlock::Bottom) => {
                    let (i, s) = try_usize_once(s, "},");
                    let fuse = io_interconnect_fuse(s);
                    (index, Fuse::IOColumnInterconnect { x, y, i, fuse })
                }

                Some(DensityIOBlock::Left) | Some(DensityIOBlock::Right) => {
                    let (i, s) = try_usize_once(s, "},");
                    let fuse = io_interconnect_fuse(s);
                    (index, Fuse::IORowInterconnect { x, y, i, fuse })
                }

                None =>
                    todo!("{x:?} {y:?}"),
            }
        } else {
            todo!("{s}");
        }
    } else if let Some(s) = s.strip_prefix("{{ioc,") {
        let (x, s) = x_once(s, ",");
        let (y, s) = y_once(s, ",");
        match density.io_block(x, y) {
            Some(DensityIOBlock::Top) | Some(DensityIOBlock::Bottom) => {
                let (n, s) = try_usize_once(s, "},");
                let fuse = io_column_cell_fuse(s);
                (index, Fuse::IOColumnCell { x, y, n, fuse })
            }

            Some(DensityIOBlock::Left) | Some(DensityIOBlock::Right) => {
                let (n, s) = try_usize_once(s, "},");
                let fuse = io_row_cell_fuse(s);
                (index, Fuse::IORowCell { x, y, n, fuse })
            }

            None =>
                todo!("{x:?} {y:?}"),
        }
    } else if let Some(s) = s.strip_prefix("{{jtag,") {
        let (_x, s) = x_once(s, ",");
        let (_y, s) = y_once(s, "},");
        let (signal, s) = jtag_signal(s);
        let fuse = source_fuse(s, density);
        (index, Fuse::JTAG { signal, fuse })
    } else if let Some(s) = s.strip_prefix("{{lab,") {
        let (x, s) = x_once(s, ",");
        let (y, s) = y_once(s, "},");
        if let Some(s) = s.strip_prefix("{interconnect,") {
            let (i, s) = try_usize_once(s, "},");
            let fuse = logic_interconnect_fuse(s);
            (index, Fuse::LogicInterconnect { x, y, i, fuse })
        } else {
            let fuse = logic_block_fuse(s);
            (index, Fuse::LogicBlock { x, y, fuse })
        }
    } else if let Some(s) = s.strip_prefix("{{lc,") {
        let (x, s) = x_once(s, ",");
        let (y, s) = y_once(s, ",");
        let (n, s) = try_usize_once(s, "},");
        let fuse = logic_cell_fuse(s);
        (index, Fuse::LogicCell { x, y, n, fuse })
    } else if let Some(s) = s.strip_prefix("{{r4,") {
        let (x, s) = x_once(s, ",");
        let (y, s) = y_once(s, "},{mux,");
        let (i, s) = try_usize_once(s, "},");
        let fuse = r4_fuse(s);
        (index, Fuse::R4Interconnect { x, y, i, fuse })
    } else if let Some(s) = s.strip_prefix("{{ufm,") {
        let (x, s) = x_once(s, ",");
        let (y, s) = y_once(s, "},");
        if let Some(s) = s.strip_prefix("{interconnect,") {
            let (i, s) = try_usize_once(s, "},");
            let fuse = ufm_interconnect_fuse(s);
            (index, Fuse::UFMInterconnect { x, y, i, fuse })
        } else {
            let (signal, s) = ufm_signal(s);
            let fuse = source_fuse(s, density);
            (index, Fuse::UFM { signal, fuse })
        }
    } else if let Some(s) = s.strip_prefix("{user_code,") {
        let (bit, _) = try_usize_once(s, "}");
        (index, Fuse::UserCode { bit })
    } else {
        todo!("{s}");
    }
}

fn x_once<'a>(s: &'a str, delimiter: &str) -> (X, &'a str) {
    let (x, s) = s.split_once(delimiter).unwrap();
    let x: u8 = x.parse().unwrap();
    (X(x), s)
}

fn y_once<'a>(s: &'a str, delimiter: &str) -> (Y, &'a str) {
    let (y, s) = s.split_once(delimiter).unwrap();
    let y: u8 = y.parse().unwrap();
    (Y(y), s)
}

fn usize_once<'a>(s: &'a str, delimiter: &str) -> (usize, &'a str) {
    let (n, s) = s.split_once(delimiter).unwrap();
    let n: usize = n.parse().unwrap();
    (n, s)
}

fn try_usize_once<'a, N>(s: &'a str, delimiter: &str) -> (N, &'a str)
where
    N: TryFrom<usize, Error: Debug>,
{
    let (n, s) = s.split_once(delimiter).unwrap();
    let n: usize = n.parse().unwrap();
    let n: N = N::try_from(n).unwrap();
    (n, s)
}

fn c4_fuse(s: &str) -> C4InterconnectFuse {
    if s == "direct_link}" {
        C4InterconnectFuse::DirectLink
    } else if let Some(s) = s.strip_prefix("from3,") {
        let select = select3(s);
        C4InterconnectFuse::Source3(select)
    } else if let Some(s) = s.strip_prefix("from4,") {
        let select = select4(s);
        C4InterconnectFuse::Source4(select)
    } else if s == "io_data_in0}" {
        C4InterconnectFuse::IODataIn0
    } else if s == "io_data_in1}" {
        C4InterconnectFuse::IODataIn1
    } else {
        todo!("{s}")
    }
}

fn control<'a>(s: &'a str, delimiter: &str) -> (Control, &'a str) {
    use Control::*;

    match s.split_once(delimiter) {
        Some(("0", s)) => (Control0, s),
        Some(("1", s)) => (Control1, s),
        Some(("2", s)) => (Control2, s),
        Some(("3", s)) => (Control3, s),
        Some(("4", s)) => (Control4, s),
        Some(("5", s)) => (Control5, s),
        _ => todo!("{s}"),
    }
}

fn global(s: &str) -> Global {
    use Global::*;

    match s {
        "global0}" => Global0,
        "global1}" => Global1,
        "global2}" => Global2,
        "global3}" => Global3,
        _ => todo!("{s}")
    }
}

fn global_fuse(s: &str) -> GlobalFuse {
    if let Some(s) = s.strip_prefix("{column,") {
        let (column, _) = x_once(s, "},off}");
        GlobalFuse::ColumnOff(column)
    } else if s == "internal}" {
        GlobalFuse::Internal
    } else if s == "row,off}" {
        GlobalFuse::RowOff
    } else {
        todo!("{s}")
    }
}

fn io_column_cell_fuse(s: &str) -> IOCellFuse<IOColumnSourceFuse> {
    use IOColumnSourceFuse as Fuse;

    if s == "enable_invert}" {
        IOCellFuse::Enable(Fuse::Invert)
    } else if let Some(s) = s.strip_prefix("enable3,") {
        let select = select3(s);
        IOCellFuse::Enable(Fuse::Source3(select))
    } else if let Some(s) = s.strip_prefix("enable4,") {
        let select = select4(s);
        IOCellFuse::Enable(Fuse::Source4(select))
    } else if s == "output_invert}" {
        IOCellFuse::Output(Fuse::Invert)
    } else if let Some(s) = s.strip_prefix("output3,") {
        let select = select3(s);
        IOCellFuse::Output(Fuse::Source3(select))
    } else if let Some(s) = s.strip_prefix("output4,") {
        let select = select4(s);
        IOCellFuse::Output(Fuse::Source4(select))
    } else {
        io_cell_fuse(s)
    }
}

fn io_row_cell_fuse(s: &str) -> IOCellFuse<IORowSourceFuse> {
    use IORowSourceFuse as Fuse;

    if s == "enable_invert}" {
        IOCellFuse::Enable(Fuse::Invert)
    } else if let Some(s) = s.strip_prefix("enable3,") {
        let select = select3(s);
        IOCellFuse::Enable(Fuse::Source3(select))
    } else if let Some(s) = s.strip_prefix("enable6,") {
        let select = select6(s);
        IOCellFuse::Enable(Fuse::Source6(select))
    } else if s == "output_invert}" {
        IOCellFuse::Output(Fuse::Invert)
    } else if let Some(s) = s.strip_prefix("output3,") {
        let select = select3(s);
        IOCellFuse::Output(Fuse::Source3(select))
    } else if let Some(s) = s.strip_prefix("output6,") {
        let select = select6(s);
        IOCellFuse::Output(Fuse::Source6(select))
    } else {
        io_cell_fuse(s)
    }
}

fn io_cell_fuse<T>(s: &str) -> IOCellFuse<T> {
    if s == "bus_hold}" {
        IOCellFuse::BusHold
    } else if s == "fast_out}" {
        IOCellFuse::OutputFast
    } else if s == "fast_slew_rate}" {
        IOCellFuse::FastSlewRate
    } else if s == "input_delay}" {
        IOCellFuse::InputDelay
    } else if s == "input_off}" {
        IOCellFuse::InputOff
    } else if s == "low_current_0}" {
        IOCellFuse::LowCurrent0
    } else if s == "low_current_1}" {
        IOCellFuse::LowCurrent1
    } else if s == "open_drain}" {
        IOCellFuse::OpenDrain
    } else if s == "pci_compliance}" {
        IOCellFuse::PCICompliance
    } else if s == "schmitt_trigger}" {
        IOCellFuse::SchmittTrigger
    } else if s == "weak_pull_up}" {
        IOCellFuse::WeakPullUp
    } else {
        todo!("{s}")
    }
}

fn io_interconnect_fuse(s: &str) -> IOInterconnectFuse {
    if s == "direct_link}" {
        IOInterconnectFuse::DirectLink
    } else if let Some(s) = s.strip_prefix("from3,") {
        let select = select3(s);
        IOInterconnectFuse::Source3(select)
    } else if s == "from4,gclk}" {
        IOInterconnectFuse::SourceGlobal
    } else if let Some(s) = s.strip_prefix("from4,") {
        let select = select4(s);
        IOInterconnectFuse::Source4(select)
    } else {
        todo!("{s}")
    }
}

fn jtag_signal(s: &str) -> (JTAGInput, &str) {
    use JTAGInput::*;

    if let Some(s) = s.strip_prefix("tdo,") {
        (TDO, s)
    } else {
        todo!("{s}")
    }
}

fn logic_block_fuse(s: &str) -> LogicBlockFuse {
    if let Some(s) = s.strip_prefix("a_clr,") {
        let global = global(s);
        LogicBlockFuse::AClearGlobal(global)
    } else if s == "a_clr1,control_5_not_4}" {
        LogicBlockFuse::AClear1Control5Not4
    } else if s == "a_clr1,global}" {
        LogicBlockFuse::AClear1Global
    } else if s == "a_clr1,invert}" {
        LogicBlockFuse::AClear1Invert
    } else if s == "a_clr1,off}" {
        LogicBlockFuse::AClear1Off
    } else if s == "a_clr2,control_5_not_4}" {
        LogicBlockFuse::AClear2Control5Not4
    } else if s == "a_clr2,global}" {
        LogicBlockFuse::AClear2Global
    } else if s == "a_clr2,invert}" {
        LogicBlockFuse::AClear2Invert
    } else if s == "a_clr2,off}" {
        LogicBlockFuse::AClear2Off
    } else if s == "a_load,control}" {
        LogicBlockFuse::ALoadControl
    } else if s == "a_load,invert}" {
        LogicBlockFuse::ALoadInvert
    } else if s == "carry_in,invert_a}" {
        LogicBlockFuse::CarryInInvertA
    } else if s == "clk1,control}" {
        LogicBlockFuse::Clock1Control
    } else if s == "clk1,control_0_not_1}" {
        LogicBlockFuse::Clock1Control0Not1
    } else if s == "clk1,invert}" {
        LogicBlockFuse::Clock1Invert
    } else if let Some(s) = s.strip_prefix("clk1,") {
        let global = global(s);
        LogicBlockFuse::Clock1Global(global)
    } else if s == "clk2,control}" {
        LogicBlockFuse::Clock2Control
    } else if s == "clk2,invert}" {
        LogicBlockFuse::Clock2Invert
    } else if let Some(s) = s.strip_prefix("clk2,") {
        let global = global(s);
        LogicBlockFuse::Clock2Global(global)
    } else if s == "clk2_a_load,control_3_not_2}" {
        LogicBlockFuse::Clock2ALoadControl3Not2
    } else if let Some(s) = s.strip_prefix("{control,") {
        let (control, s) = control(s, "},");
        let fuse = logic_block_control_fuse(s);
        LogicBlockFuse::Control { control, fuse }
    } else if s == "ena1,invert}" {
        LogicBlockFuse::Enable1Invert
    } else if s == "ena1,control_3_not_2}" {
        LogicBlockFuse::Enable1Control3Not2
    } else if s == "ena1,off}" {
        LogicBlockFuse::Enable1Off
    } else if s == "ena2_s_load,invert}" {
        LogicBlockFuse::Enable2SLoadInvert
    } else if s == "ena2_s_load,control_0_not_1}" {
        LogicBlockFuse::Enable2SLoadControl0Not1
    } else if s == "ena2,off}" {
        LogicBlockFuse::Enable2Off
    } else if s == "invert_a}" {
        LogicBlockFuse::InvertA
    } else if s == "invert_a,control_4_not_3}" {
        LogicBlockFuse::InvertAControl4Not3
    } else if s == "s_clr,control}" {
        LogicBlockFuse::SClearControl
    } else if s == "s_clr,control_5_not_4}" {
        LogicBlockFuse::SClearControl5Not4
    } else if s == "s_clr,invert}" {
        LogicBlockFuse::SClearInvert
    } else if s == "s_load,control}" {
        LogicBlockFuse::SLoadControl
    } else if s == "s_load,not_always}" {
        LogicBlockFuse::SLoadNotAlways
    } else {
        todo!("{s}")
    }
}

fn logic_block_control_fuse(s: &str) -> LogicBlockControlFuse {
    if let Some(s) = s.strip_prefix("from3,") {
        let select = select3(s);
        LogicBlockControlFuse::Source3(select)
    } else if let Some(s) = s.strip_prefix("from6,") {
        let select = select6(s);
        LogicBlockControlFuse::Source6(select)
    } else {
        todo!("{s}")
    }
}

fn logic_cell_fuse(s: &str) -> LogicCellFuse {
    use LUTBit::*;

    if s == "a_clr1}" {
        LogicCellFuse::AClear1
    } else if s == "carry_in}" {
        LogicCellFuse::CarryIn
    } else if s == "clk2}" {
        LogicCellFuse::Clock2
    } else if let Some(s) = s.strip_prefix("data_a") {
        let fuse = logic_cell_input_fuse(s);
        LogicCellFuse::Input { input: LogicCellInput::A, fuse }
    } else if let Some(s) = s.strip_prefix("data_b") {
        let fuse = logic_cell_input_fuse(s);
        LogicCellFuse::Input { input: LogicCellInput::B, fuse }
    } else if let Some(s) = s.strip_prefix("data_c") {
        let fuse = logic_cell_input_fuse(s);
        LogicCellFuse::Input { input: LogicCellInput::C, fuse }
    } else if let Some(s) = s.strip_prefix("data_d") {
        let fuse = logic_cell_input_fuse(s);
        LogicCellFuse::Input { input: LogicCellInput::D, fuse }
    } else if s == "feedback}" {
        LogicCellFuse::Feedback
    } else if s == "lut,a0b0c0d0}" { LogicCellFuse::LUTBit(LUTBit0000)
    } else if s == "lut,a1b0c0d0}" { LogicCellFuse::LUTBit(LUTBit1000)
    } else if s == "lut,a0b1c0d0}" { LogicCellFuse::LUTBit(LUTBit0100)
    } else if s == "lut,a1b1c0d0}" { LogicCellFuse::LUTBit(LUTBit1100)
    } else if s == "lut,a0b0c1d0}" { LogicCellFuse::LUTBit(LUTBit0010)
    } else if s == "lut,a1b0c1d0}" { LogicCellFuse::LUTBit(LUTBit1010)
    } else if s == "lut,a0b1c1d0}" { LogicCellFuse::LUTBit(LUTBit0110)
    } else if s == "lut,a1b1c1d0}" { LogicCellFuse::LUTBit(LUTBit1110)
    } else if s == "lut,a0b0c0d1}" { LogicCellFuse::LUTBit(LUTBit0001)
    } else if s == "lut,a1b0c0d1}" { LogicCellFuse::LUTBit(LUTBit1001)
    } else if s == "lut,a0b1c0d1}" { LogicCellFuse::LUTBit(LUTBit0101)
    } else if s == "lut,a1b1c0d1}" { LogicCellFuse::LUTBit(LUTBit1101)
    } else if s == "lut,a0b0c1d1}" { LogicCellFuse::LUTBit(LUTBit0011)
    } else if s == "lut,a1b0c1d1}" { LogicCellFuse::LUTBit(LUTBit1011)
    } else if s == "lut,a0b1c1d1}" { LogicCellFuse::LUTBit(LUTBit0111)
    } else if s == "lut,a1b1c1d1}" { LogicCellFuse::LUTBit(LUTBit1111)
    } else if s == "lut_chain,off}" {
        LogicCellFuse::LUTChainOff
    } else if s == "output_left,lut}" {
        LogicCellFuse::OutputLeftLUT
    } else if s == "output_local,lut}" {
        LogicCellFuse::OutputLocalLUT
    } else if s == "output_right,lut}" {
        LogicCellFuse::OutputRightLUT
    } else if s == "register_chain,off}" {
        LogicCellFuse::RegisterChainOff
    } else if s == "s_clr_load}" {
        LogicCellFuse::Syncronous
    } else {
        todo!("{s}")
    }
}

fn logic_cell_input_fuse(s: &str) -> LogicCellSourceFuse {
    if let Some(s) = s.strip_prefix("3,") {
        let select = select3(s);
        LogicCellSourceFuse::Source3(select)
    } else if let Some(s) = s.strip_prefix("6,") {
        let select = select6(s);
        LogicCellSourceFuse::Source6(select)
    } else {
        todo!("{s}")
    }
}

fn logic_interconnect_fuse(s: &str) -> LogicInterconnectFuse {
    if s == "direct_link}" {
        LogicInterconnectFuse::DirectLink
    } else if let Some(s) = s.strip_prefix("from3,") {
        let select = select3(s);
        LogicInterconnectFuse::Source3(select)
    } else if s == "from4,gclk}" {
        LogicInterconnectFuse::SourceGlobal
    } else if let Some(s) = s.strip_prefix("from4,") {
        let select = select4(s);
        LogicInterconnectFuse::Source4(select)
    } else {
        todo!("{s}")
    }
}

fn r4_fuse(s: &str) -> R4InterconnectFuse {
    if s == "direct_link}" {
        R4InterconnectFuse::DirectLink
    } else if s == "io_data_in0}" {
        R4InterconnectFuse::IODataIn0
    } else if s == "io_data_in1}" {
        R4InterconnectFuse::IODataIn1
    } else if let Some(s) = s.strip_prefix("from3,") {
        let select = select3(s);
        R4InterconnectFuse::Source3(select)
    } else if let Some(s) = s.strip_prefix("from4,") {
        let select = select4(s);
        R4InterconnectFuse::Source4(select)
    } else {
        todo!("{s}")
    }
}

fn select3(s: &str) -> Select3 {
    if s == "mux0}" {
        Select3::Select3_0
    } else if s == "mux1}" {
        Select3::Select3_1
    } else if s == "mux2}" {
        Select3::Select3_2
    } else {
        todo!("{s}")
    }
}

fn select4(s: &str) -> Select4 {
    if s == "mux0}" {
        Select4::Select4_0
    } else if s == "mux1}" {
        Select4::Select4_1
    } else if s == "mux2}" {
        Select4::Select4_2
    } else if s == "mux3}" {
        Select4::Select4_3
    } else {
        todo!("{s}")
    }
}

fn select6(s: &str) -> Select6 {
    if s == "mux0}" {
        Select6::Select6_0
    } else if s == "mux1}" {
        Select6::Select6_1
    } else if s == "mux2}" {
        Select6::Select6_2
    } else if s == "mux3}" {
        Select6::Select6_3
    } else if s == "mux4}" {
        Select6::Select6_4
    } else if s == "mux5}" {
        Select6::Select6_5
    } else {
        todo!("{s}")
    }
}

fn source_fuse(s: &str, density: &DensityLayout) -> SourceFuse {
    use SourceFuse::*;

    if density.large() {
        if s == "invert}" {
            Large(UFMSourceFuse::Invert)
        } else if let Some(s) = s.strip_prefix("from3,") {
            let select = select3(s);
            Large(UFMSourceFuse::Source3(select))
        } else if let Some(s) = s.strip_prefix("from4,") {
            let select = select4(s);
            Large(UFMSourceFuse::Source4(select))
        } else {
            todo!("{s}")
        }
    } else {
        if s == "invert}" {
            Small(IORowSourceFuse::Invert)
        } else if let Some(s) = s.strip_prefix("from3,") {
            let select = select3(s);
            Small(IORowSourceFuse::Source3(select))
        } else if let Some(s) = s.strip_prefix("from6,") {
            let select = select6(s);
            Small(IORowSourceFuse::Source6(select))
        } else {
            todo!("{s}")
        }
    }
}

fn ufm_signal(s: &str) -> (UFMInput, &str) {
    use UFMInput::*;

    if let Some(s) = s.strip_prefix("ar_clk,") {
        (ArClk, s)
    } else if let Some(s) = s.strip_prefix("ar_in,") {
        (ArIn, s)
    } else if let Some(s) = s.strip_prefix("ar_shift,") {
        (ArShift, s)
    } else if let Some(s) = s.strip_prefix("dr_clk,") {
        (DrClk, s)
    } else if let Some(s) = s.strip_prefix("dr_in,") {
        (DrIn, s)
    } else if let Some(s) = s.strip_prefix("dr_shift,") {
        (DrShift, s)
    } else if let Some(s) = s.strip_prefix("erase,") {
        (Erase, s)
    } else if let Some(s) = s.strip_prefix("osc_ena,") {
        (OscEna, s)
    } else if let Some(s) = s.strip_prefix("program,") {
        (Program, s)
    } else {
        todo!("{s}")
    }
}

fn ufm_interconnect_fuse(s: &str) -> UFMInterconnectFuse {
    if s == "direct_link}" {
        UFMInterconnectFuse::DirectLink
    } else if let Some(s) = s.strip_prefix("from3,") {
        let select = select3(s);
        UFMInterconnectFuse::Source3(select)
    } else if let Some(s) = s.strip_prefix("from4,") {
        let select = select4(s);
        UFMInterconnectFuse::Source4(select)
    } else {
        todo!("{s}")
    }
}

fn only(fuses: &HashSet<Fuse>, density: &DensityLayout, fuse: Fuse) {
    if !fuses.contains(&fuse) {
        assert_eq!(
            (fuse, true),
            (fuse, fuse.to_index(density).is_err()),
        );
    }
}

fn only_fuses(fuses: &HashSet<Fuse>, density: &DensityLayout) {
    only(fuses, density, Fuse:: DeviceOutputEnable);
    only(fuses, density, Fuse::DeviceReset);
    only_global(fuses, density);
    only_jtag(fuses, density);
    only_ufm(fuses, density);
    only_user_code(fuses, density);
    for x in 0..23u8 {
        let x = X(x);
        for y in 0..16u8 {
            let y = Y(y);
            only_c4_interconnect(&fuses, density, x, y);
            only_io_column_cell(&fuses, density, x, y);
            only_io_column_interconnect(&fuses, density, x, y);
            only_io_row_cell(&fuses, density, x, y);
            only_io_row_interconnect(&fuses, density, x, y);
            only_logic_block(&fuses, density, x, y);
            only_logic_cell(&fuses, density, x, y);
            only_logic_interconnect(&fuses, density, x, y);
            only_r4_interconnect(&fuses, density, x, y);
            only_ufm_interconnect(&fuses, density, x, y);
        }
    }
}

fn only_c4_interconnect(
    fuses: &HashSet<Fuse>,
    density: &DensityLayout,
    x: X,
    y: Y,
) {
    use C4InterconnectFuse::*;

    for i in C4InterconnectIndex::iter() {
        only(fuses, density, Fuse::C4Interconnect {
            x, y, i, fuse: DirectLink,
        });
        only(fuses, density, Fuse::C4Interconnect {
            x, y, i, fuse: IODataIn0,
        });
        only(fuses, density, Fuse::C4Interconnect {
            x, y, i, fuse: IODataIn1,
        });
        for select in Select3::iter() {
            only(fuses, density, Fuse::C4Interconnect {
                x, y, i, fuse: Source3(select),
            });
        }
        for select in Select4::iter() {
            only(fuses, density, Fuse::C4Interconnect {
                x, y, i, fuse: Source4(select),
            });
        }
    }
}

fn only_global(fuses: &HashSet<Fuse>, density: &DensityLayout) {
    use GlobalFuse::*;
    use SourceFuse::*;

    for index in Global::iter() {
        for x in 0..23u8 {
            let x = X(x);
            only(fuses, density, Fuse::Global {
                index, fuse: ColumnOff(x),
            });
        }
        only(fuses, density, Fuse::Global {
            index, fuse: Internal,
        });
        only(fuses, density, Fuse::Global {
            index, fuse: RowOff,
        });
        only(fuses, density, Fuse::GlobalSource {
            index, fuse: Small(IORowSourceFuse::Invert),
        });
        only(fuses, density, Fuse::GlobalSource {
            index, fuse: Large(UFMSourceFuse::Invert),
        });
        for select in Select3::iter() {
            only(fuses, density, Fuse::GlobalSource {
                index, fuse: Small(IORowSourceFuse::Source3(select)),
            });
            only(fuses, density, Fuse::GlobalSource {
                index, fuse: Large(UFMSourceFuse::Source3(select)),
            });
        }
        for select in Select4::iter() {
            only(fuses, density, Fuse::GlobalSource {
                index, fuse: Large(UFMSourceFuse::Source4(select)),
            });
        }
        for select in Select6::iter() {
            only(fuses, density, Fuse::GlobalSource {
                index, fuse: Small(IORowSourceFuse::Source6(select)),
            });
        }
    }
}

fn only_io_column_cell(
    fuses: &HashSet<Fuse>,
    density: &DensityLayout,
    x: X,
    y: Y,
) {
    use IOCellFuse::*;
    use IOColumnSourceFuse::*;

    for n in IOColumnCellNumber::iter() {
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: BusHold,
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: FastSlewRate,
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: InputDelay,
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: InputOff,
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: LowCurrent0,
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: LowCurrent1,
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: OpenDrain,
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: OutputFast,
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: PCICompliance,
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: SchmittTrigger,
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: WeakPullUp,
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: Enable(Invert),
        });
        only(fuses, density, Fuse::IOColumnCell {
            x, y, n, fuse: Output(Invert),
        });
        for select in Select3::iter() {
            only(fuses, density, Fuse::IOColumnCell {
                x, y, n, fuse: Enable(Source3(select)),
            });
            only(fuses, density, Fuse::IOColumnCell {
                x, y, n, fuse: Output(Source3(select)),
            });
        }
        for select in Select4::iter() {
            only(fuses, density, Fuse::IOColumnCell {
                x, y, n, fuse: Enable(Source4(select)),
            });
            only(fuses, density, Fuse::IOColumnCell {
                x, y, n, fuse: Output(Source4(select)),
            });
        }
    }
}

fn only_io_column_interconnect(
    fuses: &HashSet<Fuse>,
    density: &DensityLayout,
    x: X,
    y: Y,
) {
    use IOInterconnectFuse::*;

    for i in IOColumnInterconnectIndex::iter() {
        only(fuses, density, Fuse::IOColumnInterconnect {
            x, y, i, fuse: DirectLink,
        });
        for select in Select3::iter() {
            only(fuses, density, Fuse::IOColumnInterconnect {
                x, y, i, fuse: Source3(select),
            });
        }
        for select in Select4::iter() {
            only(fuses, density, Fuse::IOColumnInterconnect {
                x, y, i, fuse: Source4(select),
            });
        }
        only(fuses, density, Fuse::IOColumnInterconnect {
            x, y, i, fuse: SourceGlobal,
        });
    }
}

fn only_io_row_cell(
    fuses: &HashSet<Fuse>,
    density: &DensityLayout,
    x: X,
    y: Y,
) {
    use IOCellFuse::*;
    use IORowSourceFuse::*;

    for n in IORowCellNumber::iter() {
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: BusHold,
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: FastSlewRate,
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: InputDelay,
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: InputOff,
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: LowCurrent0,
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: LowCurrent1,
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: OpenDrain,
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: OutputFast,
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: PCICompliance,
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: SchmittTrigger,
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: WeakPullUp,
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: Enable(Invert),
        });
        only(fuses, density, Fuse::IORowCell {
            x, y, n, fuse: Output(Invert),
        });
        for select in Select3::iter() {
            only(fuses, density, Fuse::IORowCell {
                x, y, n, fuse: Enable(Source3(select)),
            });
            only(fuses, density, Fuse::IORowCell {
                x, y, n, fuse: Output(Source3(select)),
            });
        }
        for select in Select6::iter() {
            only(fuses, density, Fuse::IORowCell {
                x, y, n, fuse: Enable(Source6(select)),
            });
            only(fuses, density, Fuse::IORowCell {
                x, y, n, fuse: Output(Source6(select)),
            });
        }
    }
}

fn only_io_row_interconnect(
    fuses: &HashSet<Fuse>,
    density: &DensityLayout,
    x: X,
    y: Y,
) {
    use IOInterconnectFuse::*;

    for i in IORowInterconnectIndex::iter() {
        only(fuses, density, Fuse::IORowInterconnect {
            x, y, i, fuse: DirectLink,
        });
        for select in Select3::iter() {
            only(fuses, density, Fuse::IORowInterconnect {
                x, y, i, fuse: Source3(select),
            });
        }
        for select in Select4::iter() {
            only(fuses, density, Fuse::IORowInterconnect {
                x, y, i, fuse: Source4(select),
            });
        }
        only(fuses, density, Fuse::IORowInterconnect {
            x, y, i, fuse: SourceGlobal,
        });
    }
}

fn only_jtag(fuses: &HashSet<Fuse>, density: &DensityLayout) {
    use JTAGInput::*;
    use SourceFuse::*;

    for select in Select3::iter() {
        only(fuses, density, Fuse::JTAG {
            signal: TDO, fuse: Small(IORowSourceFuse::Source3(select)),
        });
        only(fuses, density, Fuse::JTAG {
            signal: TDO, fuse: Large(UFMSourceFuse::Source3(select)),
        });
    }
    for select in Select4::iter() {
        only(fuses, density, Fuse::JTAG {
            signal: TDO, fuse: Large(UFMSourceFuse::Source4(select)),
        });
    }
    for select in Select6::iter() {
        only(fuses, density, Fuse::JTAG {
            signal: TDO, fuse: Small(IORowSourceFuse::Source6(select)),
        });
    }
}

fn only_logic_block(
    fuses: &HashSet<Fuse>,
    density: &DensityLayout,
    x: X,
    y: Y,
) {
    use LogicBlockFuse::*;
    use LogicBlockControlFuse::*;

    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: AClear1Control5Not4,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: AClear1Global,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: AClear1Invert,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: AClear1Off,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: AClear2Control5Not4,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: AClear2Global,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: AClear2Invert,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: AClear2Off,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: ALoadControl,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: ALoadInvert,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: CarryInInvertA,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Clock1Control,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Clock1Control0Not1,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Clock1Invert,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Clock2ALoadControl3Not2,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Clock2Control,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Clock2Invert,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Enable1Off,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Enable1Control3Not2,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Enable1Invert,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Enable2Off,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Enable2SLoadControl0Not1,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: Enable2SLoadInvert,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: InvertA,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: InvertAControl4Not3,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: SClearControl,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: SClearControl5Not4,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: SClearInvert,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: SLoadControl,
    });
    only(fuses, density, Fuse::LogicBlock {
        x, y, fuse: SLoadNotAlways,
    });
    for global in Global::iter() {
        only(fuses, density, Fuse::LogicBlock {
            x, y, fuse: AClearGlobal(global),
        });
        only(fuses, density, Fuse::LogicBlock {
            x, y, fuse: Clock1Global(global),
        });
        only(fuses, density, Fuse::LogicBlock {
            x, y, fuse: Clock2Global(global),
        });
    }
    for control in max_v::Control::iter() {
        for select in Select3::iter() {
            only(fuses, density, Fuse::LogicBlock {
                x, y, fuse: LogicBlockFuse::Control {
                    control, fuse: Source3(select),
                },
            });
        }
        for select in Select6::iter() {
            only(fuses, density, Fuse::LogicBlock {
                x, y, fuse: LogicBlockFuse::Control {
                    control, fuse: Source6(select),
                },
            });
        }
    }
}

fn only_logic_cell(
    fuses: &HashSet<Fuse>,
    density: &DensityLayout,
    x: X,
    y: Y,
) {
    use LogicCellFuse::*;
    use LogicCellSourceFuse::*;

    for n in LogicCellNumber::iter() {
        only(fuses, density, Fuse::LogicCell {
            x, y, n, fuse: AClear1,
        });
        only(fuses, density, Fuse::LogicCell {
            x, y, n, fuse: CarryIn,
        });
        only(fuses, density, Fuse::LogicCell {
            x, y, n, fuse: Clock2,
        });
        only(fuses, density, Fuse::LogicCell {
            x, y, n, fuse: Feedback,
        });
        for input in LogicCellInput::iter() {
            for select in Select3::iter() {
                only(fuses, density, Fuse::LogicCell {
                    x, y, n, fuse: Input {
                        input, fuse: Source3(select),
                    },
                });
            }
            for select in Select6::iter() {
                only(fuses, density, Fuse::LogicCell {
                    x, y, n, fuse: Input {
                        input, fuse: Source6(select),
                    },
                });
            }
        }
        for bit in max_v::LUTBit::iter() {
            only(fuses, density, Fuse::LogicCell {
                x, y, n, fuse: LUTBit(bit),
            });
        }
        only(fuses, density, Fuse::LogicCell {
            x, y, n, fuse: LUTChainOff,
        });
        only(fuses, density, Fuse::LogicCell {
            x, y, n, fuse: OutputLeftLUT,
        });
        only(fuses, density, Fuse::LogicCell {
            x, y, n, fuse: OutputLocalLUT,
        });
        only(fuses, density, Fuse::LogicCell {
            x, y, n, fuse: OutputRightLUT,
        });
        only(fuses, density, Fuse::LogicCell {
            x, y, n, fuse: RegisterChainOff,
        });
        only(fuses, density, Fuse::LogicCell {
            x, y, n, fuse: Syncronous,
        });
    }
}

fn only_logic_interconnect(
    fuses: &HashSet<Fuse>,
    density: &DensityLayout,
    x: X,
    y: Y,
) {
    use LogicInterconnectFuse::*;

    for i in LogicInterconnectIndex::iter() {
        only(fuses, density, Fuse::LogicInterconnect {
            x, y, i, fuse: DirectLink,
        });
        for select in Select3::iter() {
            only(fuses, density, Fuse::LogicInterconnect {
                x, y, i, fuse: Source3(select),
            });
        }
        for select in Select4::iter() {
            only(fuses, density, Fuse::LogicInterconnect {
                x, y, i, fuse: Source4(select),
            });
        }
        only(fuses, density, Fuse::LogicInterconnect {
            x, y, i, fuse: SourceGlobal,
        });
    }
}

fn only_r4_interconnect(
    fuses: &HashSet<Fuse>,
    density: &DensityLayout,
    x: X,
    y: Y,
) {
    use R4InterconnectFuse::*;

    for i in R4InterconnectIndex::iter() {
        only(fuses, density, Fuse::R4Interconnect {
            x, y, i, fuse: DirectLink,
        });
        only(fuses, density, Fuse::R4Interconnect {
            x, y, i, fuse: IODataIn0,
        });
        only(fuses, density, Fuse::R4Interconnect {
            x, y, i, fuse: IODataIn1,
        });
        for select in Select3::iter() {
            only(fuses, density, Fuse::R4Interconnect {
                x, y, i, fuse: Source3(select),
            });
        }
        for select in Select4::iter() {
            only(fuses, density, Fuse::R4Interconnect {
                x, y, i, fuse: Source4(select),
            });
        }
    }
}

fn only_user_code(fuses: &HashSet<Fuse>, density: &DensityLayout) {
    for bit in UserCodeBit::iter() {
        only(fuses, density, Fuse::UserCode { bit });
    }
}

fn only_ufm(fuses: &HashSet<Fuse>, density: &DensityLayout) {
    use SourceFuse::*;

    for signal in UFMInput::iter() {
        for select in Select3::iter() {
            only(fuses, density, Fuse::UFM {
                signal, fuse: Small(IORowSourceFuse::Source3(select)),
            });
            only(fuses, density, Fuse::UFM {
                signal, fuse: Large(UFMSourceFuse::Source3(select)),
            });
        }
        for select in Select4::iter() {
            only(fuses, density, Fuse::UFM {
                signal, fuse: Large(UFMSourceFuse::Source4(select)),
            });
        }
        for select in Select6::iter() {
            only(fuses, density, Fuse::UFM {
                signal, fuse: Small(IORowSourceFuse::Source6(select)),
            });
        }
    }
}

fn only_ufm_interconnect(
    fuses: &HashSet<Fuse>,
    density: &DensityLayout,
    x: X,
    y: Y,
) {
    use UFMInterconnectFuse::*;

    for i in UFMInterconnectIndex::iter() {
        only(fuses, density, Fuse::UFMInterconnect {
            x, y, i, fuse: DirectLink,
        });
        for select in Select3::iter() {
            only(fuses, density, Fuse::UFMInterconnect {
                x, y, i, fuse: Source3(select),
            });
        }
        for select in Select4::iter() {
            only(fuses, density, Fuse::UFMInterconnect {
                x, y, i, fuse: Source4(select),
            });
        }
    }
}

