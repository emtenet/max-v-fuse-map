
use crate::*;

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum Fuse {
    C4Interconnect {
        x: u8,
        y: u8,
        i: C4InterconnectIndex,
        fuse: C4InterconnectFuse,
    },
    DeviceOutputEnable,
    DeviceReset,
    Global {
        index: Global,
        fuse: GlobalFuse,
    },
    IOCell {
        x: u8,
        y: u8,
        n: IOCellNumber,
        fuse: IOCellFuse,
    },
    IOInterconnect {
        x: u8,
        y: u8,
        i: IOInterconnectIndex,
        fuse: IOInterconnectFuse,
    },
    JTAG {
        fuse: JTAGFuse,
    },
    LogicBlock {
        x: u8,
        y: u8,
        fuse: LogicBlockFuse,
    },
    LogicCell {
        x: u8,
        y: u8,
        n: LogicCellNumber,
        fuse: LogicCellFuse,
    },
    LogicInterconnect {
        x: u8,
        y: u8,
        i: LogicInterconnectIndex,
        fuse: LogicInterconnectFuse,
    },
    R4Interconnect {
        x: u8,
        y: u8,
        i: R4InterconnectIndex,
        fuse: R4InterconnectFuse,
    },
    UserCode {
        bit: UserCodeBit,
    },
    UFM {
        fuse: UFMFuse,
    },
    UFMInterconnect {
        x: u8,
        y: u8,
        i: UFMInterconnectIndex,
        fuse: UFMInterconnectFuse,
    },
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum C4InterconnectFuse {
    DirectLink,
    IODataIn0,
    IODataIn1,
    Source3(Select3),
    Source4(Select4),
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum GlobalFuse {
    ColumnOff(u8),
    Internal,
    Invert,
    RowOff,
    Source3(Select3),
    Source4(Select4),
    Source6(Select6),
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum IOCellFuse {
    BusHold,
    EnableInvert,
    Enable3(Select3),
    Enable4(Select4),
    Enable6(Select6),
    FastSlewRate,
    InputDelay,
    InputOff,
    LowCurrent0,
    LowCurrent1,
    OpenDrain,
    OutputInvert,
    OutputFast,
    Output3(Select3),
    Output4(Select4),
    Output6(Select6),
    PCICompliance,
    SchmittTrigger,
    WeakPullUp,
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum IOInterconnectFuse {
    DirectLink,
    Source3(Select3),
    Source4(Select4),
    SourceGlobal,
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum JTAGFuse {
    Input {
        input: JTAGInput,
        fuse: JTAGInputFuse,
    },
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum JTAGInputFuse {
    Invert,
    Source3(Select3),
    Source4(Select4),
    Source6(Select6),
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum LogicBlockFuse {
    AClearGlobal(Global),
    AClear1Control5Not4,
    AClear1Global,
    AClear1Invert,
    AClear1Off,
    AClear2Control5Not4,
    AClear2Global,
    AClear2Invert,
    AClear2Off,
    ALoadControl,
    ALoadInvert,
    CarryInInvertA,
    Clock1Control,
    Clock1Control0Not1,
    Clock1Global(Global),
    Clock1Invert,
    Clock2ALoadControl3Not2,
    Clock2Control,
    Clock2Global(Global),
    Clock2Invert,
    Control {
        control: Control,
        fuse: LogicBlockControlFuse,
    },
    Enable1Off,
    Enable1Control3Not2,
    Enable1Invert,
    Enable2Off,
    Enable2SLoadControl0Not1,
    Enable2SLoadInvert,
    InvertA,
    InvertAControl4Not3,
    SClearControl,
    SClearControl5Not4,
    SClearInvert,
    SLoadControl,
    SLoadNotAlways,
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum LogicBlockControlFuse {
    Source3(Select3),
    Source6(Select6),
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum LogicCellFuse {
    AClear1,
    CarryIn,
    Clock2,
    Feedback,
    Input {
        input: LogicCellInput,
        fuse: LogicCellInputFuse,
    },
    LUTBit(LUTBit),
    LUTChainOff,
    OutputLeftLUT,
    OutputLocalLUT,
    OutputRightLUT,
    RegisterChainOff,
    Syncronous,
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum LogicCellInputFuse {
    Source3(Select3),
    Source6(Select6),
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum LogicInterconnectFuse {
    DirectLink,
    Source3(Select3),
    Source4(Select4),
    SourceGlobal,
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum R4InterconnectFuse {
    DirectLink,
    IODataIn0,
    IODataIn1,
    Source3(Select3),
    Source4(Select4),
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum UFMFuse {
    Input {
        input: UFMInput,
        fuse: UFMInputFuse,
    },
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum UFMInputFuse {
    Invert,
    Source3(Select3),
    Source4(Select4),
    Source6(Select6),
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum UFMInterconnectFuse {
    DirectLink,
    Source3(Select3),
    Source4(Select4),
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum FuseOutOfRange {
    IO,
    PCICompliance,
    Sector {
        x: u8,
        sector: u8,
    },
    SectorBlock,
    SectorCell,
    SectorEnd,
    SectorX,
    SectorY,
    SourceGlobal,
    X,
    Y,
    Unimplemented,
}

impl Fuse {
    pub fn to_index(self, device: &Device) -> Result<usize, FuseOutOfRange> {
        self.to_location(device)?.to_index(device)
    }

    fn to_location(self, device: &Device)
        -> Result<FuseLocation, FuseOutOfRange>
    {
        use FuseLocation as At;

        match self {
            Fuse::C4Interconnect { .. } =>
                Err(FuseOutOfRange::Unimplemented),

            Fuse::DeviceOutputEnable => {
                let x = device.grow;
                if device.has_grow {
                    Ok(At::End { x, top: false, sector: 9, index: 9 })
                } else {
                    Ok(At::End { x, top: false, sector: 8, index: 1 })
                }
            }

            Fuse::DeviceReset => {
                let x = device.grow;
                if device.has_grow {
                    Ok(At::End { x, top: false, sector: 8, index: 9 })
                } else {
                    Ok(At::End { x, top: false, sector: 7, index: 1 })
                }
            }

            Fuse::Global { index, fuse } =>
                global_location(index, fuse, device),

            Fuse::IOCell { x, y, n, fuse } =>
                match device.io_cell(x, y, n) {
                    Some(DeviceIOCell::Bottom { left, strip }) =>
                        io_cell_column(x, false, left, n, strip, fuse),

                    Some(DeviceIOCell::Top { strip }) =>
                        io_cell_column(x, true, false, n, strip, fuse),

                    Some(DeviceIOCell::Left { strip }) =>
                        io_cell_row(x, true, y, n, strip, false, fuse),

                    Some(DeviceIOCell::Right { strip, pci_compliance: pci }) =>
                        io_cell_row(x, false, y, n, strip, pci, fuse),

                    None =>
                        Err(FuseOutOfRange::IO),
                },

            Fuse::IOInterconnect { x, y, i, fuse } =>
                match device.io_block(x, y) {
                    Some(DeviceIOBlock::Bottom) =>
                        io_interconnect_column(x, false, i, fuse),

                    Some(DeviceIOBlock::Top) =>
                        io_interconnect_column(x, true, i, fuse),

                    _ =>
                        Err(FuseOutOfRange::Unimplemented),
                }

            Fuse::JTAG { .. } =>
                Err(FuseOutOfRange::Unimplemented),

            Fuse::LogicBlock { x, y, fuse } =>
                logic_block(x, y, fuse),

            Fuse::LogicCell { x, y, n, fuse } =>
                logic_cell(x, y, n, fuse),

            Fuse::LogicInterconnect { x, y, i, fuse } =>
                logic_interconnect(x, y, i, fuse),

            Fuse::R4Interconnect { .. } =>
                Err(FuseOutOfRange::Unimplemented),

            Fuse::UserCode { bit } =>
                if device.has_grow {
                    Ok(user_code_location_grow(bit, device.grow))
                } else {
                    Ok(user_code_location(bit))
                },

            Fuse::UFM { .. } =>
                Err(FuseOutOfRange::Unimplemented),

            Fuse::UFMInterconnect { .. } =>
                Err(FuseOutOfRange::Unimplemented),
        }
    }
}

fn global_location(global: Global, fuse: GlobalFuse, device: &Device)
    -> Result<FuseLocation, FuseOutOfRange>
{
    use Global::*;
    use FuseLocation as At;

    match fuse {
        GlobalFuse::ColumnOff(x) if x == device.left =>
            match global {
                Global0 => Ok(At::Global { x, sector: 5 }),
                Global1 => Ok(At::Global { x, sector: 6 }),
                Global2 => Ok(At::Global { x, sector: 7 }),
                Global3 => Ok(At::Global { x, sector: 8 }),
            },

        GlobalFuse::ColumnOff(x) if x > device.left && x <= device.right =>
            match global {
                Global0 if x == device.left + 1 =>
                    Ok(At::Global { x: x - 1, sector: 12 }),

                Global0 =>
                    Ok(At::Global { x: x - 1, sector: 27 }),

                //Global1 if x == device.right =>
                //    Ok(At::Global { x, sector: 12 }),

                Global1 =>
                    Ok(At::Global { x, sector: 0 }),

                //Global2 if x == device.right =>
                //    Ok(At::Global { x, sector: 10 }),

                Global2 =>
                    Ok(At::Global { x, sector: 2 }),

                //Global3 if x == device.right =>
                //    Ok(At::Global { x, sector: 9 }),

                Global3 =>
                    Ok(At::Global { x, sector: 3 }),
            },

        GlobalFuse::ColumnOff(_) =>
            Err(FuseOutOfRange::X),

        GlobalFuse::Internal =>
            match global {
                Global0 =>
                    if device.has_grow {
                        Ok(At::Global { x: device.grow + 1, sector: 4 })
                    } else {
                        Ok(At::Global { x: 2, sector: 4 })
                    },

                Global1 =>
                    if device.has_grow {
                        Ok(At::Global { x: device.grow, sector: 26 })
                    } else {
                        Ok(At::Global { x: 1, sector: 11 })
                    },

                Global2 =>
                    if device.has_grow {
                        Ok(At::Global { x: device.grow, sector: 24 })
                    } else {
                        Ok(At::Global { x: 1, sector: 3 })
                    },

                Global3 =>
                    if device.has_grow {
                        Ok(At::Global { x: device.grow, sector: 22 })
                    } else {
                        Ok(At::Global { x: 1, sector: 9 })
                    },
            },

        GlobalFuse::Invert =>
            Err(FuseOutOfRange::Unimplemented),

        GlobalFuse::RowOff =>
            match global {
                Global0 =>
                    if device.has_grow {
                        Ok(At::Global { x: device.grow + 1, sector: 5 })
                    } else {
                        Ok(At::Global { x: 2, sector: 5 })
                    },

                Global1 =>
                    if device.has_grow {
                        Ok(At::Global { x: device.grow + 1, sector: 1 })
                    } else {
                        Ok(At::Global { x: 2, sector: 1 })
                    },

                Global2 =>
                    if device.has_grow {
                        Ok(At::Global { x: device.grow, sector: 25 })
                    } else {
                        Ok(At::Global { x: 1, sector: 4 })
                    },

                Global3 =>
                    if device.has_grow {
                        Ok(At::Global { x: device.grow, sector: 23 })
                    } else {
                        Ok(At::Global { x: 1, sector: 10 })
                    },
            },

        GlobalFuse::Source3(_select) =>
            Err(FuseOutOfRange::Unimplemented),

        GlobalFuse::Source4(_select) =>
            Err(FuseOutOfRange::Unimplemented),

        GlobalFuse::Source6(_select) =>
            Err(FuseOutOfRange::Unimplemented),
    }
}

fn io_cell_column(
    x: u8,
    top: bool,
    left: bool,
    n: IOCellNumber,
    strip: Option<u16>,
    fuse: IOCellFuse,
) -> Result<FuseLocation, FuseOutOfRange> {
    use io_cell_column_end as end;
    use IOCellFuse as Fuse;
    use Select3::*;
    use Select4::*;

    match fuse {
        Fuse::BusHold =>
            io_cell_strip(strip, top || left, 2, false),
        Fuse::EnableInvert =>
            Ok(end(x, top, n, [(13, 6), (13, 8), (9, 6), (9, 8)])),
        Fuse::Enable3(Select3_0) =>
            Ok(end(x, top, n, [(15, 5), (15, 7), (8, 5), (8, 7)])),
        Fuse::Enable3(Select3_1) =>
            Ok(end(x, top, n, [(13, 5), (13, 7), (9, 5), (9, 7)])),
        Fuse::Enable3(Select3_2) =>
            Ok(end(x, top, n, [(15, 6), (15, 8), (8, 6), (8, 8)])),
        Fuse::Enable4(Select4_0) =>
            Ok(end(x, top, n, [(16, 5), (16, 7), (7, 5), (7, 7)])),
        Fuse::Enable4(Select4_1) =>
            Ok(end(x, top, n, [(16, 6), (16, 8), (7, 6), (7, 8)])),
        Fuse::Enable4(Select4_2) =>
            Ok(end(x, top, n, [(17, 5), (17, 7), (6, 5), (6, 7)])),
        Fuse::Enable4(Select4_3) =>
            Ok(end(x, top, n, [(17, 6), (17, 8), (6, 6), (6, 8)])),
        Fuse::FastSlewRate =>
            io_cell_strip(strip, top || left, 3, false),
        Fuse::InputDelay =>
            Ok(end(x, top, n, [(19, 2), (19, 4), (19, 6), (19, 8)])),
        Fuse::InputOff =>
            Ok(end(x, top, n, [(24, 0), (15, 0), (8, 0), (6, 0)])),
        Fuse::LowCurrent0 =>
            io_cell_strip(strip, top || left, 5, false),
        Fuse::LowCurrent1 =>
            io_cell_strip(strip, top || left, 6, false),
        Fuse::OpenDrain =>
            io_cell_strip(strip, top || left, 1, false),
        Fuse::OutputInvert =>
            Ok(end(x, top, n, [(13, 2), (13, 4), (9, 2), (9, 4)])),
        Fuse::OutputFast =>
            Ok(end(x, top, n, [(12, 2), (12, 4), (10, 2), (10, 4)])),
        Fuse::Output3(Select3_0) =>
            Ok(end(x, top, n, [(15, 1), (15, 3), (8, 1), (8, 3)])),
        Fuse::Output3(Select3_1) =>
            Ok(end(x, top, n, [(13, 1), (13, 3), (9, 1), (9, 3)])),
        Fuse::Output3(Select3_2) =>
            Ok(end(x, top, n, [(15, 2), (15, 4), (8, 2), (8, 4)])),
        Fuse::Output4(Select4_0) =>
            Ok(end(x, top, n, [(16, 1), (16, 3), (7, 1), (7, 3)])),
        Fuse::Output4(Select4_1) =>
            Ok(end(x, top, n, [(16, 2), (16, 4), (7, 2), (7, 4)])),
        Fuse::Output4(Select4_2) =>
            Ok(end(x, top, n, [(17, 1), (17, 3), (6, 1), (6, 3)])),
        Fuse::Output4(Select4_3) =>
            Ok(end(x, top, n, [(17, 2), (17, 4), (6, 2), (6, 4)])),
        Fuse::SchmittTrigger =>
            Ok(end(x, top, n, [(25, 0), (16, 0), (9, 0), (7, 0)])),
        Fuse::WeakPullUp =>
            io_cell_strip(strip, top || left, 4, false),
        _ =>
            Err(FuseOutOfRange::Unimplemented),
    }
}

fn io_cell_column_end(
    x: u8,
    top: bool,
    n: IOCellNumber,
    cell: [(u8, u8); 4],
) -> FuseLocation {
    let (sector, index) = cell[n.index()];
    FuseLocation::End { x, top, sector, index }
}

fn io_cell_row(
    x: u8,
    left: bool,
    y: u8,
    n: IOCellNumber,
    strip: Option<u16>,
    pci_compliance: bool,
    fuse: IOCellFuse,
) -> Result<FuseLocation, FuseOutOfRange> {
    use IOCellFuse as Fuse;
    use Select3::*;
    use Select6::*;

    match fuse {
        Fuse::BusHold =>
            io_cell_strip(strip, left, 2, pci_compliance),
        Fuse::EnableInvert =>
            io_cell_row_side(x, left, y, 2, n, 2),
        Fuse::Enable3(Select3_0) =>
            io_cell_row_side(x, left, y, 3, n, 3),
        Fuse::Enable3(Select3_1) =>
            io_cell_row_side(x, left, y, 2, n, 3),
        Fuse::Enable3(Select3_2) =>
            io_cell_row_side(x, left, y, 3, n, 2),
        Fuse::Enable6(Select6_0) =>
            io_cell_row_side(x, left, y, 4, n, 3),
        Fuse::Enable6(Select6_1) =>
            io_cell_row_side(x, left, y, 4, n, 2),
        Fuse::Enable6(Select6_2) =>
            io_cell_row_side(x, left, y, 5, n, 3),
        Fuse::Enable6(Select6_3) =>
            io_cell_row_side(x, left, y, 5, n, 2),
        Fuse::Enable6(Select6_4) =>
            io_cell_row_side(x, left, y, 6, n, 3),
        Fuse::Enable6(Select6_5) =>
            io_cell_row_side(x, left, y, 6, n, 2),
        Fuse::FastSlewRate =>
            io_cell_strip(strip, left, 3, pci_compliance),
        //Fuse::InputDelay,
        //Fuse::InputOff,
        Fuse::LowCurrent0 =>
            io_cell_strip(strip, left, 5, pci_compliance),
        Fuse::LowCurrent1 =>
            io_cell_strip(strip, left, 6, pci_compliance),
        Fuse::OpenDrain =>
            io_cell_strip(strip, left, 1, pci_compliance),
        Fuse::OutputInvert =>
            io_cell_row_side(x, left, y, 2, n, 1),
        Fuse::OutputFast =>
            io_cell_row_side(x, left, y, 1, n, 1),
        Fuse::Output3(Select3_0) =>
            io_cell_row_side(x, left, y, 3, n, 0),
        Fuse::Output3(Select3_1) =>
            io_cell_row_side(x, left, y, 2, n, 0),
        Fuse::Output3(Select3_2) =>
            io_cell_row_side(x, left, y, 3, n, 1),
        Fuse::Output6(Select6_0) =>
            io_cell_row_side(x, left, y, 4, n, 0),
        Fuse::Output6(Select6_1) =>
            io_cell_row_side(x, left, y, 4, n, 1),
        Fuse::Output6(Select6_2) =>
            io_cell_row_side(x, left, y, 5, n, 0),
        Fuse::Output6(Select6_3) =>
            io_cell_row_side(x, left, y, 5, n, 1),
        Fuse::Output6(Select6_4) =>
            io_cell_row_side(x, left, y, 6, n, 0),
        Fuse::Output6(Select6_5) =>
            io_cell_row_side(x, left, y, 6, n, 1),
        Fuse::PCICompliance =>
            io_cell_strip(strip, left, 0, pci_compliance),
        //Fuse::SchmittTrigger,
        Fuse::WeakPullUp =>
            io_cell_strip(strip, left, 4, pci_compliance),
        _ =>
            Err(FuseOutOfRange::Unimplemented),
    }
}

fn io_cell_strip(
    strip: Option<u16>,
    top_left: bool,
    index: u16,
    pci_compliance: bool,
) -> Result<FuseLocation, FuseOutOfRange> {
    if let Some(strip) = strip {
        if top_left {
            Ok(FuseLocation::Strip { strip: strip + index })
        } else if pci_compliance {
            Ok(FuseLocation::Strip { strip: strip + 6 - index })
        } else {
            Ok(FuseLocation::Strip { strip: strip + 7 - index })
        }
    } else {
        Err(FuseOutOfRange::IO)
    }
}

fn io_cell_row_side(
    x: u8,
    left: bool,
    y: u8,
    mut sector: u8,
    n: IOCellNumber,
    index: u8,
) -> Result<FuseLocation, FuseOutOfRange> {
    use FuseLocation as At;
    use IOCellNumber::*;
    use LogicCellNumber::*;

    if !left {
        sector = 12 - sector;
    }
    match n {
        IOCell0 => Ok(At::Cell { x, y, sector, n: LogicCell2, index }),
        IOCell1 => Ok(At::Cell { x, y, sector, n: LogicCell3, index }),
        IOCell2 => Ok(At::Cell { x, y, sector, n: LogicCell4, index }),
        IOCell3 => Ok(At::Cell { x, y, sector, n: LogicCell9, index }),
        IOCell4 => Ok(At::Cell { x, y, sector, n: LogicCell8, index }),
        IOCell5 => Ok(At::Cell { x, y, sector, n: LogicCell7, index }),
        IOCell6 => Ok(At::Cell { x, y, sector, n: LogicCell6, index }),
    }
}

fn io_interconnect_column(
    x: u8,
    top: bool,
    i: IOInterconnectIndex,
    fuse: IOInterconnectFuse,
) -> Result<FuseLocation, FuseOutOfRange> {
    use IOInterconnectFuse as Fuse;
    use Select3::*;
    use Select4::*;

    match fuse {
        Fuse::Source3(Select3_0) =>
            io_interconnect_column_end(x, top, i, 4, 1),
        Fuse::Source3(Select3_1) =>
            io_interconnect_column_end(x, top, i, 5, 1),
        Fuse::Source3(Select3_2) =>
            io_interconnect_column_end(x, top, i, 5, 2),
        Fuse::Source4(Select4_0) =>
            io_interconnect_column_end(x, top, i, 2, 1),
        Fuse::Source4(Select4_1) =>
            io_interconnect_column_end(x, top, i, 2, 2),
        Fuse::Source4(Select4_2) =>
            io_interconnect_column_end(x, top, i, 3, 1),
        Fuse::Source4(Select4_3) =>
            io_interconnect_column_end(x, top, i, 3, 2),
        _ =>
            Err(FuseOutOfRange::Unimplemented),
    }
}

fn io_interconnect_column_end(
    x: u8,
    top: bool,
    i: IOInterconnectIndex,
    sector: u8,
    index: u8,
) -> Result<FuseLocation, FuseOutOfRange> {
    use FuseLocation as At;
    use IOInterconnectIndex::*;

    let s = 23 - sector;
    match i {
        IOInterconnect0 => Ok(At::End { x, top, sector: s, index: index + 0 }),
        IOInterconnect1 => Ok(At::End { x, top, sector: s, index: index + 2 }),
        IOInterconnect2 => Ok(At::End { x, top, sector: s, index: index + 4 }),
        IOInterconnect3 => Ok(At::End { x, top, sector: s, index: index + 6 }),
        IOInterconnect4 => Ok(At::End { x, top, sector: s, index: index + 8 }),
        IOInterconnect5 => Ok(At::End { x, top, sector, index: index + 0 }),
        IOInterconnect6 => Ok(At::End { x, top, sector, index: index + 2 }),
        IOInterconnect7 => Ok(At::End { x, top, sector, index: index + 4 }),
        IOInterconnect8 => Ok(At::End { x, top, sector, index: index + 6 }),
        IOInterconnect9 => Ok(At::End { x, top, sector, index: index + 8 }),
        _ =>
            Err(FuseOutOfRange::Unimplemented),
    }
}

fn logic_block(
    x: u8,
    y: u8,
    fuse: LogicBlockFuse,
) -> Result<FuseLocation, FuseOutOfRange> {
    use Control::*;
    use Global::*;
    use LogicCellNumber::*;
    use LogicBlockControlFuse::*;
    use LogicBlockFuse as Fuse;
    use FuseLocation as At;
    use Select3::*;
    use Select6::*;

    match fuse {
        Fuse::AClearGlobal(Global0) =>
            Ok(At::Cell { x, y, sector: 5, n: LogicCell4, index: 2 }),
        Fuse::AClearGlobal(Global1) =>
            Ok(At::Cell { x, y, sector: 5, n: LogicCell4, index: 3 }),
        Fuse::AClearGlobal(Global2) =>
            Ok(At::Cell { x, y, sector: 5, n: LogicCell9, index: 2 }),
        Fuse::AClearGlobal(Global3) =>
            Ok(At::Cell { x, y, sector: 5, n: LogicCell9, index: 3 }),
        Fuse::AClear1Control5Not4 =>
            Ok(At::Block { x, y, sector: 5, index: 5 }),
        Fuse::AClear1Global =>
            Ok(At::Block { x, y, sector: 20, index: 1 }),
        Fuse::AClear1Invert =>
            Ok(At::Block { x, y, sector: 21, index: 3 }),
        Fuse::AClear1Off =>
            Ok(At::Block { x, y, sector: 20, index: 5 }),
        Fuse::AClear2Control5Not4 =>
            Ok(At::Block { x, y, sector: 5, index: 0 }),
        Fuse::AClear2Global =>
            Ok(At::Block { x, y, sector: 15, index: 5 }),
        Fuse::AClear2Invert =>
            Ok(At::Block { x, y, sector: 21, index: 4 }),
        Fuse::AClear2Off =>
            Ok(At::Block { x, y, sector: 21, index: 5 }),
        Fuse::ALoadControl =>
            Ok(At::Block { x, y, sector: 21, index: 2 }),
        Fuse::ALoadInvert =>
            Ok(At::Block { x, y, sector: 19, index: 5 }),
        Fuse::CarryInInvertA =>
            Ok(At::Block { x, y, sector: 22, index: 4 }),
        Fuse::Clock1Control =>
            Ok(At::Block { x, y, sector: 15, index: 0 }),
        Fuse::Clock1Control0Not1 =>
            Ok(At::Block { x, y, sector: 16, index: 0 }),
        Fuse::Clock1Global(Global0) =>
            Ok(At::Cell { x, y, sector: 3, n: LogicCell4, index: 2 }),
        Fuse::Clock1Global(Global1) =>
            Ok(At::Cell { x, y, sector: 3, n: LogicCell4, index: 3 }),
        Fuse::Clock1Global(Global2) =>
            Ok(At::Cell { x, y, sector: 3, n: LogicCell9, index: 2 }),
        Fuse::Clock1Global(Global3) =>
            Ok(At::Cell { x, y, sector: 3, n: LogicCell9, index: 3 }),
        Fuse::Clock1Invert =>
            Ok(At::Block { x, y, sector: 15, index: 1 }),
        Fuse::Clock2ALoadControl3Not2 =>
            Ok(At::Block { x, y, sector: 15, index: 2 }),
        Fuse::Clock2Control =>
            Ok(At::Block { x, y, sector: 16, index: 3 }),
        Fuse::Clock2Global(Global0) =>
            Ok(At::Cell { x, y, sector: 4, n: LogicCell4, index: 2 }),
        Fuse::Clock2Global(Global1) =>
            Ok(At::Cell { x, y, sector: 4, n: LogicCell4, index: 3 }),
        Fuse::Clock2Global(Global2) =>
            Ok(At::Cell { x, y, sector: 4, n: LogicCell9, index: 2 }),
        Fuse::Clock2Global(Global3) =>
            Ok(At::Cell { x, y, sector: 4, n: LogicCell9, index: 3 }),
        Fuse::Clock2Invert =>
            Ok(At::Block { x, y, sector: 19, index: 2 }),
        Fuse::Control { control: Control0, fuse: Source3(Select3_0) } =>
            Ok(At::Block { x, y, sector: 12, index: 0 }),
        Fuse::Control { control: Control0, fuse: Source3(Select3_1) } =>
            Ok(At::Block { x, y, sector: 13, index: 0 }),
        Fuse::Control { control: Control0, fuse: Source3(Select3_2) } =>
            Ok(At::Block { x, y, sector: 14, index: 0 }),
        Fuse::Control { control: Control0, fuse: Source6(Select6_0) } =>
            Ok(At::Block { x, y, sector: 6, index: 0 }),
        Fuse::Control { control: Control0, fuse: Source6(Select6_1) } =>
            Ok(At::Block { x, y, sector: 8, index: 0 }),
        Fuse::Control { control: Control0, fuse: Source6(Select6_2) } =>
            Ok(At::Block { x, y, sector: 6, index: 1 }),
        Fuse::Control { control: Control0, fuse: Source6(Select6_3) } =>
            Ok(At::Block { x, y, sector: 7, index: 0 }),
        Fuse::Control { control: Control0, fuse: Source6(Select6_4) } =>
            Ok(At::Block { x, y, sector: 8, index: 1 }),
        Fuse::Control { control: Control0, fuse: Source6(Select6_5) } =>
            Ok(At::Block { x, y, sector: 7, index: 1 }),
        Fuse::Control { control: Control1, fuse: Source3(Select3_0) } =>
            Ok(At::Block { x, y, sector: 14, index: 1 }),
        Fuse::Control { control: Control1, fuse: Source3(Select3_1) } =>
            Ok(At::Block { x, y, sector: 13, index: 1 }),
        Fuse::Control { control: Control1, fuse: Source3(Select3_2) } =>
            Ok(At::Block { x, y, sector: 12, index: 1 }),
        Fuse::Control { control: Control1, fuse: Source6(Select6_0) } =>
            Ok(At::Block { x, y, sector: 11, index: 0 }),
        Fuse::Control { control: Control1, fuse: Source6(Select6_1) } =>
            Ok(At::Block { x, y, sector: 9, index: 0 }),
        Fuse::Control { control: Control1, fuse: Source6(Select6_2) } =>
            Ok(At::Block { x, y, sector: 9, index: 1 }),
        Fuse::Control { control: Control1, fuse: Source6(Select6_3) } =>
            Ok(At::Block { x, y, sector: 10, index: 0 }),
        Fuse::Control { control: Control1, fuse: Source6(Select6_4) } =>
            Ok(At::Block { x, y, sector: 11, index: 1 }),
        Fuse::Control { control: Control1, fuse: Source6(Select6_5) } =>
            Ok(At::Block { x, y, sector: 10, index: 1 }),
        Fuse::Control { control: Control2, fuse: Source3(Select3_0) } =>
            Ok(At::Block { x, y, sector: 12, index: 2 }),
        Fuse::Control { control: Control2, fuse: Source3(Select3_1) } =>
            Ok(At::Block { x, y, sector: 13, index: 2 }),
        Fuse::Control { control: Control2, fuse: Source3(Select3_2) } =>
            Ok(At::Block { x, y, sector: 14, index: 2 }),
        Fuse::Control { control: Control2, fuse: Source6(Select6_0) } =>
            Ok(At::Block { x, y, sector: 6, index: 2 }),
        Fuse::Control { control: Control2, fuse: Source6(Select6_1) } =>
            Ok(At::Block { x, y, sector: 8, index: 2 }),
        Fuse::Control { control: Control2, fuse: Source6(Select6_2) } =>
            Ok(At::Block { x, y, sector: 6, index: 3 }),
        Fuse::Control { control: Control2, fuse: Source6(Select6_3) } =>
            Ok(At::Block { x, y, sector: 7, index: 2 }),
        Fuse::Control { control: Control2, fuse: Source6(Select6_4) } =>
            Ok(At::Block { x, y, sector: 8, index: 3 }),
        Fuse::Control { control: Control2, fuse: Source6(Select6_5) } =>
            Ok(At::Block { x, y, sector: 7, index: 3 }),
        Fuse::Control { control: Control3, fuse: Source3(Select3_0) } =>
            Ok(At::Block { x, y, sector: 14, index: 3 }),
        Fuse::Control { control: Control3, fuse: Source3(Select3_1) } =>
            Ok(At::Block { x, y, sector: 13, index: 3 }),
        Fuse::Control { control: Control3, fuse: Source3(Select3_2) } =>
            Ok(At::Block { x, y, sector: 12, index: 3 }),
        Fuse::Control { control: Control3, fuse: Source6(Select6_0) } =>
            Ok(At::Block { x, y, sector: 11, index: 2 }),
        Fuse::Control { control: Control3, fuse: Source6(Select6_1) } =>
            Ok(At::Block { x, y, sector: 9, index: 2 }),
        Fuse::Control { control: Control3, fuse: Source6(Select6_2) } =>
            Ok(At::Block { x, y, sector: 9, index: 3 }),
        Fuse::Control { control: Control3, fuse: Source6(Select6_3) } =>
            Ok(At::Block { x, y, sector: 10, index: 2 }),
        Fuse::Control { control: Control3, fuse: Source6(Select6_4) } =>
            Ok(At::Block { x, y, sector: 11, index: 3 }),
        Fuse::Control { control: Control3, fuse: Source6(Select6_5) } =>
            Ok(At::Block { x, y, sector: 10, index: 3 }),
        Fuse::Control { control: Control4, fuse: Source3(Select3_0) } =>
            Ok(At::Block { x, y, sector: 12, index: 4 }),
        Fuse::Control { control: Control4, fuse: Source3(Select3_1) } =>
            Ok(At::Block { x, y, sector: 13, index: 4 }),
        Fuse::Control { control: Control4, fuse: Source3(Select3_2) } =>
            Ok(At::Block { x, y, sector: 14, index: 4 }),
        Fuse::Control { control: Control4, fuse: Source6(Select6_0) } =>
            Ok(At::Block { x, y, sector: 6, index: 4 }),
        Fuse::Control { control: Control4, fuse: Source6(Select6_1) } =>
            Ok(At::Block { x, y, sector: 8, index: 4 }),
        Fuse::Control { control: Control4, fuse: Source6(Select6_2) } =>
            Ok(At::Block { x, y, sector: 6, index: 5 }),
        Fuse::Control { control: Control4, fuse: Source6(Select6_3) } =>
            Ok(At::Block { x, y, sector: 7, index: 4 }),
        Fuse::Control { control: Control4, fuse: Source6(Select6_4) } =>
            Ok(At::Block { x, y, sector: 8, index: 5 }),
        Fuse::Control { control: Control4, fuse: Source6(Select6_5) } =>
            Ok(At::Block { x, y, sector: 7, index: 5 }),
        Fuse::Control { control: Control5, fuse: Source3(Select3_0) } =>
            Ok(At::Block { x, y, sector: 14, index: 5 }),
        Fuse::Control { control: Control5, fuse: Source3(Select3_1) } =>
            Ok(At::Block { x, y, sector: 13, index: 5 }),
        Fuse::Control { control: Control5, fuse: Source3(Select3_2) } =>
            Ok(At::Block { x, y, sector: 12, index: 5 }),
        Fuse::Control { control: Control5, fuse: Source6(Select6_0) } =>
            Ok(At::Block { x, y, sector: 11, index: 4 }),
        Fuse::Control { control: Control5, fuse: Source6(Select6_1) } =>
            Ok(At::Block { x, y, sector: 9, index: 4 }),
        Fuse::Control { control: Control5, fuse: Source6(Select6_2) } =>
            Ok(At::Block { x, y, sector: 9, index: 5 }),
        Fuse::Control { control: Control5, fuse: Source6(Select6_3) } =>
            Ok(At::Block { x, y, sector: 10, index: 4 }),
        Fuse::Control { control: Control5, fuse: Source6(Select6_4) } =>
            Ok(At::Block { x, y, sector: 11, index: 5 }),
        Fuse::Control { control: Control5, fuse: Source6(Select6_5) } =>
            Ok(At::Block { x, y, sector: 10, index: 5 }),
        Fuse::Enable1Off =>
            Ok(At::Block { x, y, sector: 16, index: 4 }),
        Fuse::Enable1Control3Not2 =>
            Ok(At::Block { x, y, sector: 16, index: 2 }),
        Fuse::Enable1Invert =>
            Ok(At::Block { x, y, sector: 16, index: 1 }),
        Fuse::Enable2Off =>
            Ok(At::Block { x, y, sector: 19, index: 0 }),
        Fuse::Enable2SLoadControl0Not1 =>
            Ok(At::Block { x, y, sector: 17, index: 0 }),
        Fuse::Enable2SLoadInvert =>
            Ok(At::Block { x, y, sector: 20, index: 4 }),
        Fuse::InvertA =>
            Ok(At::Block { x, y, sector: 20, index: 2 }),
        Fuse::InvertAControl4Not3 =>
            Ok(At::Block { x, y, sector: 15, index: 3 }),
        Fuse::SClearControl =>
            Ok(At::Block { x, y, sector: 21, index: 0 }),
        Fuse::SClearControl5Not4 =>
            Ok(At::Block { x, y, sector: 15, index: 4 }),
        Fuse::SClearInvert =>
            Ok(At::Block { x, y, sector: 21, index: 1 }),
        Fuse::SLoadControl =>
            Ok(At::Block { x, y, sector: 19, index: 1 }),
        Fuse::SLoadNotAlways =>
            Ok(At::Block { x, y, sector: 20, index: 0 }),
    }
}

fn logic_cell(
    x: u8,
    y: u8,
    n: LogicCellNumber,
    fuse: LogicCellFuse,
) -> Result<FuseLocation, FuseOutOfRange> {
    use LogicCellFuse as Fuse;
    use LogicCellInput::*;
    use LogicCellInputFuse::*;
    use FuseLocation as At;
    use LUTBit::*;
    use Select3::*;
    use Select6::*;

    match fuse {
        Fuse::AClear1 =>
            Ok(At::Cell { x, y, sector: 20, n, index: 3 }),
        Fuse::CarryIn =>
            Ok(At::Cell { x, y, sector: 20, n, index: 0 }),
        Fuse::Clock2 =>
            Ok(At::Cell { x, y, sector: 19, n, index: 2 }),
        Fuse::Feedback =>
            Ok(At::Cell { x, y, sector: 19, n, index: 0 }),
        Fuse::Input { input: LogicCellInputA, fuse: Source3(Select3_0) } =>
            Ok(At::Cell { x, y, sector:12, n, index: 0 }),
        Fuse::Input { input: LogicCellInputA, fuse: Source3(Select3_1) } =>
            Ok(At::Cell { x, y, sector:13, n, index: 0 }),
        Fuse::Input { input: LogicCellInputA, fuse: Source3(Select3_2) } =>
            Ok(At::Cell { x, y, sector:14, n, index: 0 }),
        Fuse::Input { input: LogicCellInputA, fuse: Source6(Select6_0) } =>
            Ok(At::Cell { x, y, sector: 6, n, index: 0 }),
        Fuse::Input { input: LogicCellInputA, fuse: Source6(Select6_1) } =>
            Ok(At::Cell { x, y, sector:11, n, index: 0 }),
        Fuse::Input { input: LogicCellInputA, fuse: Source6(Select6_2) } =>
            Ok(At::Cell { x, y, sector: 6, n, index: 1 }),
        Fuse::Input { input: LogicCellInputA, fuse: Source6(Select6_3) } =>
            Ok(At::Cell { x, y, sector: 7, n, index: 0 }),
        Fuse::Input { input: LogicCellInputA, fuse: Source6(Select6_4) } =>
            Ok(At::Cell { x, y, sector:11, n, index: 1 }),
        Fuse::Input { input: LogicCellInputA, fuse: Source6(Select6_5) } =>
            Ok(At::Cell { x, y, sector: 7, n, index: 1 }),
        Fuse::Input { input: LogicCellInputB, fuse: Source3(Select3_0) } =>
            Ok(At::Cell { x, y, sector:14, n, index: 1 }),
        Fuse::Input { input: LogicCellInputB, fuse: Source3(Select3_1) } =>
            Ok(At::Cell { x, y, sector:13, n, index: 1 }),
        Fuse::Input { input: LogicCellInputB, fuse: Source3(Select3_2) } =>
            Ok(At::Cell { x, y, sector:12, n, index: 1 }),
        Fuse::Input { input: LogicCellInputB, fuse: Source6(Select6_0) } =>
            Ok(At::Cell { x, y, sector: 8, n, index: 0 }),
        Fuse::Input { input: LogicCellInputB, fuse: Source6(Select6_1) } =>
            Ok(At::Cell { x, y, sector: 9, n, index: 0 }),
        Fuse::Input { input: LogicCellInputB, fuse: Source6(Select6_2) } =>
            Ok(At::Cell { x, y, sector: 9, n, index: 1 }),
        Fuse::Input { input: LogicCellInputB, fuse: Source6(Select6_3) } =>
            Ok(At::Cell { x, y, sector:10, n, index: 0 }),
        Fuse::Input { input: LogicCellInputB, fuse: Source6(Select6_4) } =>
            Ok(At::Cell { x, y, sector: 8, n, index: 1 }),
        Fuse::Input { input: LogicCellInputB, fuse: Source6(Select6_5) } =>
            Ok(At::Cell { x, y, sector:10, n, index: 1 }),
        Fuse::Input { input: LogicCellInputC, fuse: Source3(Select3_0) } =>
            Ok(At::Cell { x, y, sector:12, n, index: 2 }),
        Fuse::Input { input: LogicCellInputC, fuse: Source3(Select3_1) } =>
            Ok(At::Cell { x, y, sector:13, n, index: 2 }),
        Fuse::Input { input: LogicCellInputC, fuse: Source3(Select3_2) } =>
            Ok(At::Cell { x, y, sector:14, n, index: 2 }),
        Fuse::Input { input: LogicCellInputC, fuse: Source6(Select6_0) } =>
            Ok(At::Cell { x, y, sector: 6, n, index: 2 }),
        Fuse::Input { input: LogicCellInputC, fuse: Source6(Select6_1) } =>
            Ok(At::Cell { x, y, sector: 8, n, index: 2 }),
        Fuse::Input { input: LogicCellInputC, fuse: Source6(Select6_2) } =>
            Ok(At::Cell { x, y, sector: 6, n, index: 3 }),
        Fuse::Input { input: LogicCellInputC, fuse: Source6(Select6_3) } =>
            Ok(At::Cell { x, y, sector: 7, n, index: 2 }),
        Fuse::Input { input: LogicCellInputC, fuse: Source6(Select6_4) } =>
            Ok(At::Cell { x, y, sector: 8, n, index: 3 }),
        Fuse::Input { input: LogicCellInputC, fuse: Source6(Select6_5) } =>
            Ok(At::Cell { x, y, sector: 7, n, index: 3 }),
        Fuse::Input { input: LogicCellInputD, fuse: Source3(Select3_0) } =>
            Ok(At::Cell { x, y, sector:14, n, index: 3 }),
        Fuse::Input { input: LogicCellInputD, fuse: Source3(Select3_1) } =>
            Ok(At::Cell { x, y, sector:13, n, index: 3 }),
        Fuse::Input { input: LogicCellInputD, fuse: Source3(Select3_2) } =>
            Ok(At::Cell { x, y, sector:12, n, index: 3 }),
        Fuse::Input { input: LogicCellInputD, fuse: Source6(Select6_0) } =>
            Ok(At::Cell { x, y, sector:11, n, index: 2 }),
        Fuse::Input { input: LogicCellInputD, fuse: Source6(Select6_1) } =>
            Ok(At::Cell { x, y, sector: 9, n, index: 2 }),
        Fuse::Input { input: LogicCellInputD, fuse: Source6(Select6_2) } =>
            Ok(At::Cell { x, y, sector: 9, n, index: 3 }),
        Fuse::Input { input: LogicCellInputD, fuse: Source6(Select6_3) } =>
            Ok(At::Cell { x, y, sector:10, n, index: 2 }),
        Fuse::Input { input: LogicCellInputD, fuse: Source6(Select6_4) } =>
            Ok(At::Cell { x, y, sector:11, n, index: 3 }),
        Fuse::Input { input: LogicCellInputD, fuse: Source6(Select6_5) } =>
            Ok(At::Cell { x, y, sector:10, n, index: 3 }),
        Fuse::LUTBit(LUTBit0000) =>
            Ok(At::Cell { x, y, sector: 16, n, index: 3 }),
        Fuse::LUTBit(LUTBit0001) =>
            Ok(At::Cell { x, y, sector: 16, n, index: 1 }),
        Fuse::LUTBit(LUTBit0010) =>
            Ok(At::Cell { x, y, sector: 18, n, index: 2 }),
        Fuse::LUTBit(LUTBit0011) =>
            Ok(At::Cell { x, y, sector: 18, n, index: 1 }),
        Fuse::LUTBit(LUTBit0100) =>
            Ok(At::Cell { x, y, sector: 16, n, index: 2 }),
        Fuse::LUTBit(LUTBit0101) =>
            Ok(At::Cell { x, y, sector: 16, n, index: 0 }),
        Fuse::LUTBit(LUTBit0110) =>
            Ok(At::Cell { x, y, sector: 18, n, index: 3 }),
        Fuse::LUTBit(LUTBit0111) =>
            Ok(At::Cell { x, y, sector: 18, n, index: 0 }),
        Fuse::LUTBit(LUTBit1000) =>
            Ok(At::Cell { x, y, sector: 15, n, index: 3 }),
        Fuse::LUTBit(LUTBit1001) =>
            Ok(At::Cell { x, y, sector: 15, n, index: 1 }),
        Fuse::LUTBit(LUTBit1010) =>
            Ok(At::Cell { x, y, sector: 17, n, index: 2 }),
        Fuse::LUTBit(LUTBit1011) =>
            Ok(At::Cell { x, y, sector: 17, n, index: 1 }),
        Fuse::LUTBit(LUTBit1100) =>
            Ok(At::Cell { x, y, sector: 15, n, index: 2 }),
        Fuse::LUTBit(LUTBit1101) =>
            Ok(At::Cell { x, y, sector: 15, n, index: 0 }),
        Fuse::LUTBit(LUTBit1110) =>
            Ok(At::Cell { x, y, sector: 17, n, index: 3 }),
        Fuse::LUTBit(LUTBit1111) =>
            Ok(At::Cell { x, y, sector: 17, n, index: 0 }),
        Fuse::LUTChainOff =>
            Ok(At::Cell { x, y, sector: 19, n, index: 3 }),
        Fuse::OutputLeftLUT =>
            Ok(At::Cell { x, y, sector: 20, n, index: 1 }),
        Fuse::OutputLocalLUT =>
            Ok(At::Cell { x, y, sector: 21, n, index: 3 }),
        Fuse::RegisterChainOff =>
            Ok(At::Cell { x, y, sector: 20, n, index: 2 }),
        Fuse::Syncronous =>
            Ok(At::Cell { x, y, sector: 21, n, index: 0 }),
        _ =>
            Err(FuseOutOfRange::Unimplemented),
    }
}

fn logic_interconnect(
    x: u8,
    y: u8,
    i: LogicInterconnectIndex,
    fuse: LogicInterconnectFuse,
) -> Result<FuseLocation, FuseOutOfRange> {
    use FuseLocation as At;
    use LogicCellNumber::*;
    use LogicInterconnectFuse as Fuse;
    use LogicInterconnectIndex::*;
    use Select3::*;
    use Select4::*;

    match (i, fuse) {
        (LogicInterconnect0, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell0, index: 3 }),
        (LogicInterconnect0, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell0, index: 2 }),
        (LogicInterconnect0, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell0, index: 3 }),
        (LogicInterconnect0, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell0, index: 2 }),
        (LogicInterconnect0, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell0, index: 2 }),
        (LogicInterconnect0, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell0, index: 3 }),
        (LogicInterconnect0, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell0, index: 2 }),
        (LogicInterconnect0, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell0, index: 3 }),
        (LogicInterconnect1, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell1, index: 3 }),
        (LogicInterconnect1, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell1, index: 2 }),
        (LogicInterconnect1, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell1, index: 3 }),
        (LogicInterconnect1, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell1, index: 2 }),
        (LogicInterconnect1, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell1, index: 2 }),
        (LogicInterconnect1, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell1, index: 3 }),
        (LogicInterconnect1, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell1, index: 2 }),
        (LogicInterconnect1, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell1, index: 3 }),
        (LogicInterconnect2, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell2, index: 3 }),
        (LogicInterconnect2, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell2, index: 2 }),
        (LogicInterconnect2, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell2, index: 3 }),
        (LogicInterconnect2, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell2, index: 2 }),
        (LogicInterconnect2, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell2, index: 2 }),
        (LogicInterconnect2, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell2, index: 3 }),
        (LogicInterconnect2, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell2, index: 2 }),
        (LogicInterconnect2, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell2, index: 3 }),
        (LogicInterconnect3, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell3, index: 3 }),
        (LogicInterconnect3, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell3, index: 2 }),
        (LogicInterconnect3, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell3, index: 3 }),
        (LogicInterconnect3, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell3, index: 2 }),
        (LogicInterconnect3, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell3, index: 2 }),
        (LogicInterconnect3, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell3, index: 3 }),
        (LogicInterconnect3, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell3, index: 2 }),
        (LogicInterconnect3, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell3, index: 3 }),
        (LogicInterconnect4, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell4, index: 3 }),
        (LogicInterconnect4, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell4, index: 2 }),
        (LogicInterconnect4, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell4, index: 3 }),
        (LogicInterconnect4, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell4, index: 2 }),
        (LogicInterconnect4, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell4, index: 2 }),
        (LogicInterconnect4, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell4, index: 3 }),
        (LogicInterconnect4, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell4, index: 2 }),
        (LogicInterconnect4, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell4, index: 3 }),
        (LogicInterconnect5, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell0, index: 1 }),
        (LogicInterconnect5, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell0, index: 0 }),
        (LogicInterconnect5, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell0, index: 1 }),
        (LogicInterconnect5, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell0, index: 0 }),
        (LogicInterconnect5, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell0, index: 0 }),
        (LogicInterconnect5, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell0, index: 1 }),
        (LogicInterconnect5, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell0, index: 0 }),
        (LogicInterconnect5, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell0, index: 1 }),
        (LogicInterconnect6, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell0, index: 3 }),
        (LogicInterconnect6, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell0, index: 2 }),
        (LogicInterconnect6, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell0, index: 3 }),
        (LogicInterconnect6, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell0, index: 2 }),
        (LogicInterconnect6, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell0, index: 2 }),
        (LogicInterconnect6, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell0, index: 3 }),
        (LogicInterconnect6, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell0, index: 2 }),
        (LogicInterconnect6, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell0, index: 3 }),
        (LogicInterconnect7, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell1, index: 1 }),
        (LogicInterconnect7, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell1, index: 0 }),
        (LogicInterconnect7, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell1, index: 1 }),
        (LogicInterconnect7, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell1, index: 0 }),
        (LogicInterconnect7, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell1, index: 0 }),
        (LogicInterconnect7, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell1, index: 1 }),
        (LogicInterconnect7, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell1, index: 0 }),
        (LogicInterconnect7, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell1, index: 1 }),
        (LogicInterconnect8, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell1, index: 3 }),
        (LogicInterconnect8, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell1, index: 2 }),
        (LogicInterconnect8, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell1, index: 3 }),
        (LogicInterconnect8, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell1, index: 2 }),
        (LogicInterconnect8, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell1, index: 2 }),
        (LogicInterconnect8, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell1, index: 3 }),
        (LogicInterconnect8, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell1, index: 2 }),
        (LogicInterconnect8, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell1, index: 3 }),
        (LogicInterconnect9, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell2, index: 1 }),
        (LogicInterconnect9, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell2, index: 0 }),
        (LogicInterconnect9, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell2, index: 1 }),
        (LogicInterconnect9, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell2, index: 0 }),
        (LogicInterconnect9, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell2, index: 0 }),
        (LogicInterconnect9, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell2, index: 1 }),
        (LogicInterconnect9, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell2, index: 0 }),
        (LogicInterconnect9, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell2, index: 1 }),
        (LogicInterconnect10, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell2, index: 3 }),
        (LogicInterconnect10, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell2, index: 2 }),
        (LogicInterconnect10, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell2, index: 3 }),
        (LogicInterconnect10, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell2, index: 2 }),
        (LogicInterconnect10, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell2, index: 2 }),
        (LogicInterconnect10, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell2, index: 3 }),
        (LogicInterconnect10, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell2, index: 2 }),
        (LogicInterconnect10, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell2, index: 3 }),
        (LogicInterconnect11, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell3, index: 1 }),
        (LogicInterconnect11, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell3, index: 0 }),
        (LogicInterconnect11, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell3, index: 1 }),
        (LogicInterconnect11, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell3, index: 0 }),
        (LogicInterconnect11, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell3, index: 0 }),
        (LogicInterconnect11, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell3, index: 1 }),
        (LogicInterconnect11, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell3, index: 0 }),
        (LogicInterconnect11, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell3, index: 1 }),
        (LogicInterconnect12, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell3, index: 3 }),
        (LogicInterconnect12, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell3, index: 2 }),
        (LogicInterconnect12, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell3, index: 3 }),
        (LogicInterconnect12, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell3, index: 2 }),
        (LogicInterconnect12, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell3, index: 2 }),
        (LogicInterconnect12, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell3, index: 3 }),
        (LogicInterconnect12, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell3, index: 2 }),
        (LogicInterconnect12, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell3, index: 3 }),
        (LogicInterconnect12, Fuse::SourceGlobal) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell4, index: 0 }),
        (LogicInterconnect13, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell5, index: 3 }),
        (LogicInterconnect13, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell5, index: 2 }),
        (LogicInterconnect13, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell5, index: 3 }),
        (LogicInterconnect13, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell5, index: 2 }),
        (LogicInterconnect13, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell5, index: 2 }),
        (LogicInterconnect13, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell5, index: 3 }),
        (LogicInterconnect13, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell5, index: 2 }),
        (LogicInterconnect13, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell5, index: 3 }),
        (LogicInterconnect14, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell6, index: 3 }),
        (LogicInterconnect14, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell6, index: 2 }),
        (LogicInterconnect14, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell6, index: 3 }),
        (LogicInterconnect14, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell6, index: 2 }),
        (LogicInterconnect14, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell6, index: 2 }),
        (LogicInterconnect14, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell6, index: 3 }),
        (LogicInterconnect14, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell6, index: 2 }),
        (LogicInterconnect14, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell6, index: 3 }),
        (LogicInterconnect15, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell7, index: 3 }),
        (LogicInterconnect15, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell7, index: 2 }),
        (LogicInterconnect15, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell7, index: 3 }),
        (LogicInterconnect15, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell7, index: 2 }),
        (LogicInterconnect15, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell7, index: 2 }),
        (LogicInterconnect15, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell7, index: 3 }),
        (LogicInterconnect15, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell7, index: 2 }),
        (LogicInterconnect15, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell7, index: 3 }),
        (LogicInterconnect16, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell8, index: 3 }),
        (LogicInterconnect16, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell8, index: 2 }),
        (LogicInterconnect16, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell8, index: 3 }),
        (LogicInterconnect16, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell8, index: 2 }),
        (LogicInterconnect16, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell8, index: 2 }),
        (LogicInterconnect16, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell8, index: 3 }),
        (LogicInterconnect16, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell8, index: 2 }),
        (LogicInterconnect16, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell8, index: 3 }),
        (LogicInterconnect17, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell9, index: 3 }),
        (LogicInterconnect17, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell9, index: 2 }),
        (LogicInterconnect17, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 22, n: LogicCell9, index: 3 }),
        (LogicInterconnect17, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 23, n: LogicCell9, index: 2 }),
        (LogicInterconnect17, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell9, index: 2 }),
        (LogicInterconnect17, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 24, n: LogicCell9, index: 3 }),
        (LogicInterconnect17, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell9, index: 2 }),
        (LogicInterconnect17, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 25, n: LogicCell9, index: 3 }),
        (LogicInterconnect18, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell5, index: 1 }),
        (LogicInterconnect18, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell5, index: 0 }),
        (LogicInterconnect18, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell5, index: 1 }),
        (LogicInterconnect18, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell5, index: 0 }),
        (LogicInterconnect18, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell5, index: 0 }),
        (LogicInterconnect18, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell5, index: 1 }),
        (LogicInterconnect18, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell5, index: 0 }),
        (LogicInterconnect18, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell5, index: 1 }),
        (LogicInterconnect19, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell5, index: 3 }),
        (LogicInterconnect19, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell5, index: 2 }),
        (LogicInterconnect19, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell5, index: 3 }),
        (LogicInterconnect19, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell5, index: 2 }),
        (LogicInterconnect19, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell5, index: 2 }),
        (LogicInterconnect19, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell5, index: 3 }),
        (LogicInterconnect19, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell5, index: 2 }),
        (LogicInterconnect19, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell5, index: 3 }),
        (LogicInterconnect20, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell6, index: 1 }),
        (LogicInterconnect20, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell6, index: 0 }),
        (LogicInterconnect20, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell6, index: 1 }),
        (LogicInterconnect20, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell6, index: 0 }),
        (LogicInterconnect20, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell6, index: 0 }),
        (LogicInterconnect20, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell6, index: 1 }),
        (LogicInterconnect20, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell6, index: 0 }),
        (LogicInterconnect20, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell6, index: 1 }),
        (LogicInterconnect21, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell6, index: 3 }),
        (LogicInterconnect21, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell6, index: 2 }),
        (LogicInterconnect21, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell6, index: 3 }),
        (LogicInterconnect21, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell6, index: 2 }),
        (LogicInterconnect21, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell6, index: 2 }),
        (LogicInterconnect21, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell6, index: 3 }),
        (LogicInterconnect21, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell6, index: 2 }),
        (LogicInterconnect21, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell6, index: 3 }),
        (LogicInterconnect22, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell7, index: 1 }),
        (LogicInterconnect22, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell7, index: 0 }),
        (LogicInterconnect22, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell7, index: 1 }),
        (LogicInterconnect22, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell7, index: 0 }),
        (LogicInterconnect22, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell7, index: 0 }),
        (LogicInterconnect22, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell7, index: 1 }),
        (LogicInterconnect22, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell7, index: 0 }),
        (LogicInterconnect22, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell7, index: 1 }),
        (LogicInterconnect23, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell7, index: 3 }),
        (LogicInterconnect23, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell7, index: 2 }),
        (LogicInterconnect23, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell7, index: 3 }),
        (LogicInterconnect23, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell7, index: 2 }),
        (LogicInterconnect23, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell7, index: 2 }),
        (LogicInterconnect23, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell7, index: 3 }),
        (LogicInterconnect23, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell7, index: 2 }),
        (LogicInterconnect23, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell7, index: 3 }),
        (LogicInterconnect24, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell8, index: 1 }),
        (LogicInterconnect24, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell8, index: 0 }),
        (LogicInterconnect24, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell8, index: 1 }),
        (LogicInterconnect24, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell8, index: 0 }),
        (LogicInterconnect24, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell8, index: 0 }),
        (LogicInterconnect24, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell8, index: 1 }),
        (LogicInterconnect24, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell8, index: 0 }),
        (LogicInterconnect24, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell8, index: 1 }),
        (LogicInterconnect25, Fuse::DirectLink) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell8, index: 3 }),
        (LogicInterconnect25, Fuse::Source3(Select3_0)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell8, index: 2 }),
        (LogicInterconnect25, Fuse::Source3(Select3_1)) =>
            Ok(At::Cell { x, y, sector: 5,  n: LogicCell8, index: 3 }),
        (LogicInterconnect25, Fuse::Source3(Select3_2)) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell8, index: 2 }),
        (LogicInterconnect25, Fuse::Source4(Select4_0)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell8, index: 2 }),
        (LogicInterconnect25, Fuse::Source4(Select4_1)) =>
            Ok(At::Cell { x, y, sector: 3,  n: LogicCell8, index: 3 }),
        (LogicInterconnect25, Fuse::Source4(Select4_2)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell8, index: 2 }),
        (LogicInterconnect25, Fuse::Source4(Select4_3)) =>
            Ok(At::Cell { x, y, sector: 2,  n: LogicCell8, index: 3 }),
        (LogicInterconnect25, Fuse::SourceGlobal) =>
            Ok(At::Cell { x, y, sector: 4,  n: LogicCell9, index: 0 }),
        (_, Fuse::SourceGlobal) =>
            Err(FuseOutOfRange::SourceGlobal),
    }
}

fn user_code_location(bit: UserCodeBit) -> FuseLocation {
    use FuseLocation as At;
    use UserCodeBit::*;

    match bit {
        UserCodeBit0  => At::End { x: 1, top: false, sector: 1,  index: 4 },
        UserCodeBit1  => At::End { x: 1, top: false, sector: 2,  index: 4 },
        UserCodeBit2  => At::End { x: 1, top: false, sector: 3,  index: 4 },
        UserCodeBit3  => At::End { x: 1, top: false, sector: 4,  index: 4 },
        UserCodeBit4  => At::End { x: 1, top: false, sector: 5,  index: 4 },
        UserCodeBit5  => At::End { x: 1, top: false, sector: 6,  index: 4 },
        UserCodeBit6  => At::End { x: 1, top: false, sector: 7,  index: 4 },
        UserCodeBit7  => At::End { x: 1, top: false, sector: 8,  index: 4 },
        UserCodeBit8  => At::End { x: 1, top: false, sector: 9,  index: 4 },
        UserCodeBit9  => At::End { x: 1, top: false, sector: 10, index: 4 },
        UserCodeBit10 => At::End { x: 1, top: false, sector: 1,  index: 3 },
        UserCodeBit11 => At::End { x: 1, top: false, sector: 2,  index: 3 },
        UserCodeBit12 => At::End { x: 1, top: false, sector: 3,  index: 3 },
        UserCodeBit13 => At::End { x: 1, top: false, sector: 4,  index: 3 },
        UserCodeBit14 => At::End { x: 1, top: false, sector: 5,  index: 3 },
        UserCodeBit15 => At::End { x: 1, top: false, sector: 6,  index: 3 },
        UserCodeBit16 => At::End { x: 1, top: false, sector: 7,  index: 3 },
        UserCodeBit17 => At::End { x: 1, top: false, sector: 8,  index: 3 },
        UserCodeBit18 => At::End { x: 1, top: false, sector: 9,  index: 3 },
        UserCodeBit19 => At::End { x: 1, top: false, sector: 10, index: 3 },
        UserCodeBit20 => At::End { x: 1, top: false, sector: 1,  index: 2 },
        UserCodeBit21 => At::End { x: 1, top: false, sector: 2,  index: 2 },
        UserCodeBit22 => At::End { x: 1, top: false, sector: 3,  index: 2 },
        UserCodeBit23 => At::End { x: 1, top: false, sector: 4,  index: 2 },
        UserCodeBit24 => At::End { x: 1, top: false, sector: 5,  index: 2 },
        UserCodeBit25 => At::End { x: 1, top: false, sector: 6,  index: 2 },
        UserCodeBit26 => At::End { x: 1, top: false, sector: 7,  index: 2 },
        UserCodeBit27 => At::End { x: 1, top: false, sector: 8,  index: 2 },
        UserCodeBit28 => At::End { x: 1, top: false, sector: 9,  index: 2 },
        UserCodeBit29 => At::End { x: 1, top: false, sector: 10, index: 2 },
        UserCodeBit30 => At::End { x: 1, top: false, sector: 9,  index: 1 },
        UserCodeBit31 => At::End { x: 1, top: false, sector: 10, index: 1 },
    }
}

fn user_code_location_grow(bit: UserCodeBit, x: u8) -> FuseLocation {
    use FuseLocation as At;
    use UserCodeBit::*;

    match bit {
        UserCodeBit0  => At::End { x, top: false, sector: 19, index: 9 },
        UserCodeBit1  => At::End { x, top: false, sector: 18, index: 9 },
        UserCodeBit2  => At::End { x, top: false, sector: 17, index: 9 },
        UserCodeBit3  => At::End { x, top: false, sector: 16, index: 9 },
        UserCodeBit4  => At::End { x, top: false, sector: 15, index: 9 },
        UserCodeBit5  => At::End { x, top: false, sector: 14, index: 9 },
        UserCodeBit6  => At::End { x, top: false, sector: 19, index: 10 },
        UserCodeBit7  => At::End { x, top: false, sector: 18, index: 10 },
        UserCodeBit8  => At::End { x, top: false, sector: 17, index: 10 },
        UserCodeBit9  => At::End { x, top: false, sector: 16, index: 10 },
        UserCodeBit10 => At::End { x, top: false, sector: 15, index: 10 },
        UserCodeBit11 => At::End { x, top: false, sector: 14, index: 10 },
        UserCodeBit12 => At::End { x, top: false, sector: 13, index: 10 },
        UserCodeBit13 => At::End { x, top: false, sector: 12, index: 10 },
        UserCodeBit14 => At::End { x, top: false, sector: 11, index: 10 },
        UserCodeBit15 => At::End { x, top: false, sector: 10, index: 10 },
        UserCodeBit16 => At::End { x, top: false, sector: 9,  index: 10 },
        UserCodeBit17 => At::End { x, top: false, sector: 8,  index: 10 },
        UserCodeBit18 => At::End { x, top: false, sector: 7,  index: 10 },
        UserCodeBit19 => At::End { x, top: false, sector: 6,  index: 10 },
        UserCodeBit20 => At::End { x, top: false, sector: 5,  index: 10 },
        UserCodeBit21 => At::End { x, top: false, sector: 4,  index: 10 },
        UserCodeBit22 => At::End { x, top: false, sector: 3,  index: 10 },
        UserCodeBit23 => At::End { x, top: false, sector: 2,  index: 10 },
        UserCodeBit24 => At::End { x, top: false, sector: 1,  index: 10 },
        UserCodeBit25 => At::End { x, top: false, sector: 0,  index: 10 },
        UserCodeBit26 =>
            At::End { x: x - 1, top: false, sector: 27, index: 10 },
        UserCodeBit27 =>
            At::End { x: x - 1, top: false, sector: 26, index: 10 },
        UserCodeBit28 => At::End { x, top: false, sector: 13, index: 9 },
        UserCodeBit29 => At::End { x, top: false, sector: 12, index: 9 },
        UserCodeBit30 => At::End { x, top: false, sector: 11, index: 9 },
        UserCodeBit31 => At::End { x, top: false, sector: 10, index: 9 },
    }
}

