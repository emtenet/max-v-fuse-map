use crate::*;

mod at;

use at::*;

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
    IOColumnCell {
        x: u8,
        y: u8,
        n: IOColumnCellNumber,
        fuse: IOCellFuse,
    },
    IOColumnInterconnect {
        x: u8,
        y: u8,
        i: IOColumnInterconnectIndex,
        fuse: IOInterconnectFuse,
    },
    IORowCell {
        x: u8,
        y: u8,
        n: IORowCellNumber,
        fuse: IOCellFuse,
    },
    IORowInterconnect {
        x: u8,
        y: u8,
        i: IORowInterconnectIndex,
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
    Fuse,
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
    pub fn to_index(self, density: &Density) -> Result<usize, FuseOutOfRange> {
        self.to_location(density)?.to_index(density)
    }

    fn to_location(self, density: &Density)
        -> Result<FuseAt, FuseOutOfRange>
    {
        use FuseAt as At;

        match self {
            Fuse::C4Interconnect { x, y, i, fuse } =>
                match density.c4_block(x, y) {
                    Some(DensityC4Block::TopLeft) =>
                        c4_interconnect_top_left(x, i, fuse),

                    Some(DensityC4Block::Top) =>
                        c4_interconnect_top(x, i, fuse),

                    Some(DensityC4Block::RowLeft) =>
                        c4_interconnect_row_left(x, y, i, fuse),

                    Some(DensityC4Block::Row) =>
                        c4_interconnect_row(x, y, i, fuse),

                    Some(DensityC4Block::RowRight) =>
                        c4_interconnect_row_right(x, y, i, fuse),

                    Some(DensityC4Block::BottomLeft) =>
                        c4_interconnect_bottom_left(x, i, fuse),

                    Some(DensityC4Block::Bottom) =>
                        c4_interconnect_bottom(x, i, fuse),

                    _ =>
                        Err(FuseOutOfRange::Unimplemented),
                },

            Fuse::DeviceOutputEnable => {
                let x = density.grow;
                if density.has_grow {
                    Ok(At::End { x, top: false, sector: 9, index: 9 })
                } else {
                    Ok(At::End { x, top: false, sector: 8, index: 1 })
                }
            }

            Fuse::DeviceReset => {
                let x = density.grow;
                if density.has_grow {
                    Ok(At::End { x, top: false, sector: 8, index: 9 })
                } else {
                    Ok(At::End { x, top: false, sector: 7, index: 1 })
                }
            }

            Fuse::Global { index, fuse } =>
                global_location(index, fuse, density),

            Fuse::IOColumnCell { x, y, n, fuse } =>
                match density.io_column_cell(x, y, n) {
                    Some(DensityIOColumnCell::Bottom { left, strip }) =>
                        io_column_cell(x, false, left, n, strip, fuse),

                    Some(DensityIOColumnCell::Top { strip }) =>
                        io_column_cell(x, true, false, n, strip, fuse),

                    None =>
                        Err(FuseOutOfRange::IO),
                },

            Fuse::IOColumnInterconnect { x, y, i, fuse } =>
                match density.io_block(x, y) {
                    Some(DensityIOBlock::Bottom) =>
                        io_column_interconnect(x, false, i, fuse),

                    Some(DensityIOBlock::Top) =>
                        io_column_interconnect(x, true, i, fuse),

                    _ =>
                        Err(FuseOutOfRange::IO),
                }

            Fuse::IORowCell { x, y, n, fuse } =>
                match density.io_row_cell(x, y, n) {
                    Some(DensityIORowCell::Left { strip }) =>
                        io_row_cell(x, true, y, n, strip, false, fuse),

                    Some(DensityIORowCell::Right { strip, pci_compliance }) =>
                        io_row_cell(x, false, y, n, strip, pci_compliance, fuse),

                    None =>
                        Err(FuseOutOfRange::IO),
                },

            Fuse::IORowInterconnect { x, y, i, fuse } =>
                match density.io_block(x, y) {
                    Some(DensityIOBlock::Left) =>
                        io_row_interconnect(x, y, i, fuse),

                    Some(DensityIOBlock::Right) =>
                        io_row_interconnect(x, y, i, fuse),

                    _ =>
                        Err(FuseOutOfRange::IO),
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
                if density.has_grow {
                    user_code_location_grow(bit, density.grow)
                } else {
                    user_code_location(bit)
                },

            Fuse::UFM { .. } =>
                Err(FuseOutOfRange::Unimplemented),

            Fuse::UFMInterconnect { .. } =>
                Err(FuseOutOfRange::Unimplemented),
        }
    }
}

macro_rules! block {
    ($x:expr, $y:expr, $s:expr, $i:expr) => {
        Ok(FuseAt::Block {
            x: $x,
            y: $y,
            sector: $s,
            index: $i,
        })
    }
}

macro_rules! cell {
    ($x:expr, $y:expr, $s:expr, $n:expr, $i:expr) => {
        Ok(FuseAt::Cell {
            x: $x,
            y: $y,
            sector: $s,
            n: $n,
            index: $i,
        })
    }
}

macro_rules! end {
    ($x:expr, $top:expr, $s:expr, $i:expr) => {
        Ok(FuseAt::End {
            x: $x,
            top: $top,
            sector: $s,
            index: $i,
        })
    }
}

fn c4_interconnect_top_left(
    x: u8,
    i: C4InterconnectIndex,
    fuse: C4InterconnectFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use C4InterconnectIndex::*;
    use C4InterconnectFuse::*;

    match fuse {
        IODataIn0 =>
            match i {
                C4Interconnect9 =>  end!(x + 1, true, 0, 1),
                C4Interconnect10 => end!(x + 1, true, 0, 3),
                C4Interconnect11 => end!(x + 1, true, 0, 5),
                C4Interconnect12 => end!(x + 1, true, 0, 7),
                C4Interconnect13 => end!(x + 1, true, 0, 9),
                _ => Err(FuseOutOfRange::Fuse),
            },
        IODataIn1 =>
            match i {
                C4Interconnect9 =>  end!(x, true, 11, 2),
                C4Interconnect10 => end!(x, true, 11, 4),
                C4Interconnect11 => end!(x, true, 11, 6),
                C4Interconnect12 => end!(x, true, 11, 8),
                C4Interconnect13 => end!(x, true, 11, 10),
                _ => Err(FuseOutOfRange::Fuse),
            },
        _ =>
            Err(FuseOutOfRange::Fuse),
    }
}

fn c4_interconnect_top(
    x: u8,
    i: C4InterconnectIndex,
    fuse: C4InterconnectFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use C4InterconnectIndex::*;
    use C4InterconnectFuse::*;

    match fuse {
        IODataIn0 =>
            match i {
                C4Interconnect0 => end!(x, true, 23, 3),
                C4Interconnect1 => end!(x, true, 23, 7),
                C4Interconnect2 => end!(x, true, 23, 9),
                C4Interconnect7 => end!(x, true, 23, 1),
                C4Interconnect8 => end!(x, true, 23, 5),
                C4Interconnect9 => end!(x + 1, true, 0, 1),
                C4Interconnect10 => end!(x + 1, true, 0, 3),
                C4Interconnect11 => end!(x + 1, true, 0, 5),
                C4Interconnect12 => end!(x + 1, true, 0, 7),
                C4Interconnect13 => end!(x + 1, true, 0, 9),
                _ => Err(FuseOutOfRange::Fuse),
            },
        IODataIn1 =>
            match i {
                C4Interconnect0 => end!(x, true, 25, 4),
                C4Interconnect1 => end!(x, true, 25, 8),
                C4Interconnect2 => end!(x, true, 25, 10),
                C4Interconnect7 => end!(x, true, 25, 2),
                C4Interconnect8 => end!(x, true, 25, 6),
                C4Interconnect9 => end!(x, true, 26, 2),
                C4Interconnect10 => end!(x, true, 26, 4),
                C4Interconnect11 => end!(x, true, 26, 6),
                C4Interconnect12 => end!(x, true, 26, 8),
                C4Interconnect13 => end!(x, true, 26, 10),
                _ => Err(FuseOutOfRange::Fuse),
            },
        _ =>
            Err(FuseOutOfRange::Fuse),
    }
}

fn c4_interconnect_row_left(
    x: u8,
    y: u8,
    i: C4InterconnectIndex,
    fuse: C4InterconnectFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use C4InterconnectFuse::*;
    use Select3::*;
    use Select4::*;

    match fuse {
        DirectLink => c4_interconnect_fuse_1(x, y, i, 8, 0, 1),
        Source3(Select3_0) => c4_interconnect_fuse_1(x, y, i, 7, 1, 0),
        Source3(Select3_1) => c4_interconnect_fuse_1(x, y, i, 7, 1, 1),
        Source3(Select3_2) => c4_interconnect_fuse_1(x, y, i, 8, 0, 0),
        Source4(Select4_0) => c4_interconnect_fuse_0(x, y, i, 9, 11, 0),
        Source4(Select4_1) => c4_interconnect_fuse_0(x, y, i, 9, 11, 1),
        Source4(Select4_2) => c4_interconnect_fuse_0(x, y, i, 10, 12, 0),
        Source4(Select4_3) => c4_interconnect_fuse_0(x, y, i, 10, 12, 1),
        _ => Err(FuseOutOfRange::Fuse),
    }
}

fn c4_interconnect_row(
    x: u8,
    y: u8,
    i: C4InterconnectIndex,
    fuse: C4InterconnectFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use C4InterconnectFuse::*;
    use Select3::*;
    use Select4::*;

    match fuse {
        DirectLink => c4_interconnect_fuse_1(x, y, i, 23, 0, 1),
        Source3(Select3_0) => c4_interconnect_fuse_1(x, y, i, 22, 1, 0),
        Source3(Select3_1) => c4_interconnect_fuse_1(x, y, i, 22, 1, 1),
        Source3(Select3_2) => c4_interconnect_fuse_1(x, y, i, 23, 0, 0),
        Source4(Select4_0) => c4_interconnect_fuse_0(x, y, i, 24, 26, 0),
        Source4(Select4_1) => c4_interconnect_fuse_0(x, y, i, 24, 26, 1),
        Source4(Select4_2) => c4_interconnect_fuse_0(x, y, i, 25, 27, 0),
        Source4(Select4_3) => c4_interconnect_fuse_0(x, y, i, 25, 27, 1),
        _ => Err(FuseOutOfRange::Fuse),
    }
}

fn c4_interconnect_row_right(
    x: u8,
    y: u8,
    i: C4InterconnectIndex,
    fuse: C4InterconnectFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use C4InterconnectFuse::*;
    use Select3::*;
    use Select4::*;

    match fuse {
        DirectLink => c4_interconnect_fuse_0(x, y, i, 23, 27, 1),
        Source3(Select3_0) => c4_interconnect_fuse_0(x, y, i, 22, 26, 0),
        Source3(Select3_1) => c4_interconnect_fuse_0(x, y, i, 22, 26, 1),
        Source3(Select3_2) => c4_interconnect_fuse_0(x, y, i, 23, 27, 0),
        Source4(Select4_0) => c4_interconnect_fuse_1(x, y, i, 24, 11, 0),
        Source4(Select4_1) => c4_interconnect_fuse_1(x, y, i, 24, 11, 1),
        Source4(Select4_2) => c4_interconnect_fuse_1(x, y, i, 25, 12, 0),
        Source4(Select4_3) => c4_interconnect_fuse_1(x, y, i, 25, 12, 1),
        _ => Err(FuseOutOfRange::Fuse),
    }
}

fn c4_interconnect_fuse_0(
    x: u8,
    y: u8,
    i: C4InterconnectIndex,
    s0: u8,
    s1: u8,
    i0: u8,
) -> Result<FuseAt, FuseOutOfRange> {
    use C4InterconnectIndex::*;
    use LogicCellNumber::*;

    match i {
        C4Interconnect0 => cell!(x, y, s0, LogicCell0, i0 + 0),
        C4Interconnect1 => cell!(x, y, s1, LogicCell1, i0 + 0),
        C4Interconnect2 => cell!(x, y, s1, LogicCell2, i0 + 2),
        C4Interconnect3 => cell!(x, y, s1, LogicCell4, i0 + 0),
        C4Interconnect4 => cell!(x, y, s1, LogicCell5, i0 + 2),
        C4Interconnect5 => cell!(x, y, s1, LogicCell7, i0 + 0),
        C4Interconnect6 => cell!(x, y, s1, LogicCell8, i0 + 0),
        C4Interconnect7 => cell!(x, y, s1, LogicCell0, i0 + 2),
        C4Interconnect8 => cell!(x, y, s1, LogicCell2, i0 + 0),
        C4Interconnect9 => cell!(x, y, s1, LogicCell3, i0 + 0),
        C4Interconnect10 => cell!(x, y, s0, LogicCell5, i0 + 0),
        C4Interconnect11 => cell!(x, y, s1, LogicCell6, i0 + 0),
        C4Interconnect12 => cell!(x, y, s1, LogicCell7, i0 + 2),
        C4Interconnect13 => cell!(x, y, s1, LogicCell9, i0 + 0),
    }
}

fn c4_interconnect_fuse_1(
    x: u8,
    y: u8,
    i: C4InterconnectIndex,
    s0: u8,
    s1: u8,
    i0: u8,
) -> Result<FuseAt, FuseOutOfRange> {
    use C4InterconnectIndex::*;
    use LogicCellNumber::*;

    match i {
        C4Interconnect0 => cell!(x + 0, y, s0, LogicCell0, i0 + 0),
        C4Interconnect1 => cell!(x + 1, y, s1, LogicCell1, i0 + 0),
        C4Interconnect2 => cell!(x + 1, y, s1, LogicCell2, i0 + 2),
        C4Interconnect3 => cell!(x + 1, y, s1, LogicCell4, i0 + 0),
        C4Interconnect4 => cell!(x + 1, y, s1, LogicCell5, i0 + 2),
        C4Interconnect5 => cell!(x + 1, y, s1, LogicCell7, i0 + 0),
        C4Interconnect6 => cell!(x + 1, y, s1, LogicCell8, i0 + 0),
        C4Interconnect7 => cell!(x + 1, y, s1, LogicCell0, i0 + 2),
        C4Interconnect8 => cell!(x + 1, y, s1, LogicCell2, i0 + 0),
        C4Interconnect9 => cell!(x + 1, y, s1, LogicCell3, i0 + 0),
        C4Interconnect10 => cell!(x + 0, y, s0, LogicCell5, i0 + 0),
        C4Interconnect11 => cell!(x + 1, y, s1, LogicCell6, i0 + 0),
        C4Interconnect12 => cell!(x + 1, y, s1, LogicCell7, i0 + 2),
        C4Interconnect13 => cell!(x + 1, y, s1, LogicCell9, i0 + 0),
    }
}

fn c4_interconnect_bottom_left(
    x: u8,
    i: C4InterconnectIndex,
    fuse: C4InterconnectFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use C4InterconnectIndex::*;
    use C4InterconnectFuse::*;

    match fuse {
        IODataIn0 =>
            match i {
                C4Interconnect3 => end!(x + 1, false, 0, 9),
                C4Interconnect10 => end!(x + 1, false, 0, 1),
                C4Interconnect11 => end!(x + 1, false, 0, 3),
                C4Interconnect12 => end!(x + 1, false, 0, 5),
                C4Interconnect13 => end!(x + 1, false, 0, 7),
                _ => Err(FuseOutOfRange::Fuse),
            },
        IODataIn1 =>
            match i {
                C4Interconnect3 => end!(x, false, 11, 10),
                C4Interconnect10 => end!(x, false, 11, 2),
                C4Interconnect11 => end!(x, false, 11, 4),
                C4Interconnect12 => end!(x, false, 11, 6),
                C4Interconnect13 => end!(x, false, 11, 8),
                _ => Err(FuseOutOfRange::Fuse),
            },
        _ =>
            Err(FuseOutOfRange::Fuse),
    }
}

fn c4_interconnect_bottom(
    x: u8,
    i: C4InterconnectIndex,
    fuse: C4InterconnectFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use C4InterconnectIndex::*;
    use C4InterconnectFuse::*;

    match fuse {
        IODataIn0 =>
            match i {
                C4Interconnect0 => end!(x, false, 23, 7),
                C4Interconnect1 => end!(x, false, 23, 9),
                C4Interconnect3 => end!(x + 1, false, 0, 9),
                C4Interconnect7 => end!(x, false, 23, 1),
                C4Interconnect8 => end!(x, false, 23, 3),
                C4Interconnect9 => end!(x, false, 23, 5),
                C4Interconnect10 => end!(x + 1, false, 0, 1),
                C4Interconnect11 => end!(x + 1, false, 0, 3),
                C4Interconnect12 => end!(x + 1, false, 0, 5),
                C4Interconnect13 => end!(x + 1, false, 0, 7),
                _ => Err(FuseOutOfRange::Fuse),
            },
        IODataIn1 =>
            match i {
                C4Interconnect0 => end!(x, false, 25, 8),
                C4Interconnect1 => end!(x, false, 25, 10),
                C4Interconnect3 => end!(x, false, 26, 10),
                C4Interconnect7 => end!(x, false, 25, 2),
                C4Interconnect8 => end!(x, false, 25, 4),
                C4Interconnect9 => end!(x, false, 25, 6),
                C4Interconnect10 => end!(x, false, 26, 2),
                C4Interconnect11 => end!(x, false, 26, 4),
                C4Interconnect12 => end!(x, false, 26, 6),
                C4Interconnect13 => end!(x, false, 26, 8),
                _ => Err(FuseOutOfRange::Fuse),
            },
        _ =>
            Err(FuseOutOfRange::Fuse),
    }
}

fn global_location(global: Global, fuse: GlobalFuse, density: &Density)
    -> Result<FuseAt, FuseOutOfRange>
{
    use Global::*;
    use FuseAt as At;

    match fuse {
        GlobalFuse::ColumnOff(x) if x == density.left =>
            match global {
                Global0 => Ok(At::Global { x, sector: 5 }),
                Global1 => Ok(At::Global { x, sector: 6 }),
                Global2 => Ok(At::Global { x, sector: 7 }),
                Global3 => Ok(At::Global { x, sector: 8 }),
            },

        GlobalFuse::ColumnOff(x) if x > density.left && x <= density.right =>
            match global {
                Global0 if x == density.left + 1 =>
                    Ok(At::Global { x: x - 1, sector: 12 }),

                Global0 =>
                    Ok(At::Global { x: x - 1, sector: 27 }),

                //Global1 if x == density.right =>
                //    Ok(At::Global { x, sector: 12 }),

                Global1 =>
                    Ok(At::Global { x, sector: 0 }),

                //Global2 if x == density.right =>
                //    Ok(At::Global { x, sector: 10 }),

                Global2 =>
                    Ok(At::Global { x, sector: 2 }),

                //Global3 if x == density.right =>
                //    Ok(At::Global { x, sector: 9 }),

                Global3 =>
                    Ok(At::Global { x, sector: 3 }),
            },

        GlobalFuse::ColumnOff(_) =>
            Err(FuseOutOfRange::X),

        GlobalFuse::Internal =>
            match global {
                Global0 =>
                    if density.has_grow {
                        Ok(At::Global { x: density.grow + 1, sector: 4 })
                    } else {
                        Ok(At::Global { x: 2, sector: 4 })
                    },

                Global1 =>
                    if density.has_grow {
                        Ok(At::Global { x: density.grow, sector: 26 })
                    } else {
                        Ok(At::Global { x: 1, sector: 11 })
                    },

                Global2 =>
                    if density.has_grow {
                        Ok(At::Global { x: density.grow, sector: 24 })
                    } else {
                        Ok(At::Global { x: 1, sector: 3 })
                    },

                Global3 =>
                    if density.has_grow {
                        Ok(At::Global { x: density.grow, sector: 22 })
                    } else {
                        Ok(At::Global { x: 1, sector: 9 })
                    },
            },

        GlobalFuse::Invert =>
            Err(FuseOutOfRange::Unimplemented),

        GlobalFuse::RowOff =>
            match global {
                Global0 =>
                    if density.has_grow {
                        Ok(At::Global { x: density.grow + 1, sector: 5 })
                    } else {
                        Ok(At::Global { x: 2, sector: 5 })
                    },

                Global1 =>
                    if density.has_grow {
                        Ok(At::Global { x: density.grow + 1, sector: 1 })
                    } else {
                        Ok(At::Global { x: 2, sector: 1 })
                    },

                Global2 =>
                    if density.has_grow {
                        Ok(At::Global { x: density.grow, sector: 25 })
                    } else {
                        Ok(At::Global { x: 1, sector: 4 })
                    },

                Global3 =>
                    if density.has_grow {
                        Ok(At::Global { x: density.grow, sector: 23 })
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

fn io_column_cell(
    x: u8,
    top: bool,
    left: bool,
    n: IOColumnCellNumber,
    strip: Option<u16>,
    fuse: IOCellFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use io_column_cell_at as at;
    use IOCellFuse as Fuse;
    use Select3::*;
    use Select4::*;

    match fuse {
        Fuse::BusHold =>
            io_cell_strip(strip, top || left, 2, false),
        Fuse::EnableInvert =>
            Ok(at(x, top, n, [(13, 6), (13, 8), (9, 6), (9, 8)])),
        Fuse::Enable3(Select3_0) =>
            Ok(at(x, top, n, [(15, 5), (15, 7), (8, 5), (8, 7)])),
        Fuse::Enable3(Select3_1) =>
            Ok(at(x, top, n, [(13, 5), (13, 7), (9, 5), (9, 7)])),
        Fuse::Enable3(Select3_2) =>
            Ok(at(x, top, n, [(15, 6), (15, 8), (8, 6), (8, 8)])),
        Fuse::Enable4(Select4_0) =>
            Ok(at(x, top, n, [(16, 5), (16, 7), (7, 5), (7, 7)])),
        Fuse::Enable4(Select4_1) =>
            Ok(at(x, top, n, [(16, 6), (16, 8), (7, 6), (7, 8)])),
        Fuse::Enable4(Select4_2) =>
            Ok(at(x, top, n, [(17, 5), (17, 7), (6, 5), (6, 7)])),
        Fuse::Enable4(Select4_3) =>
            Ok(at(x, top, n, [(17, 6), (17, 8), (6, 6), (6, 8)])),
        Fuse::FastSlewRate =>
            io_cell_strip(strip, top || left, 3, false),
        Fuse::InputDelay =>
            Ok(at(x, top, n, [(19, 2), (19, 4), (19, 6), (19, 8)])),
        Fuse::InputOff =>
            Ok(at(x, top, n, [(24, 0), (15, 0), (8, 0), (6, 0)])),
        Fuse::LowCurrent0 =>
            io_cell_strip(strip, top || left, 5, false),
        Fuse::LowCurrent1 =>
            io_cell_strip(strip, top || left, 6, false),
        Fuse::OpenDrain =>
            io_cell_strip(strip, top || left, 1, false),
        Fuse::OutputInvert =>
            Ok(at(x, top, n, [(13, 2), (13, 4), (9, 2), (9, 4)])),
        Fuse::OutputFast =>
            Ok(at(x, top, n, [(12, 2), (12, 4), (10, 2), (10, 4)])),
        Fuse::Output3(Select3_0) =>
            Ok(at(x, top, n, [(15, 1), (15, 3), (8, 1), (8, 3)])),
        Fuse::Output3(Select3_1) =>
            Ok(at(x, top, n, [(13, 1), (13, 3), (9, 1), (9, 3)])),
        Fuse::Output3(Select3_2) =>
            Ok(at(x, top, n, [(15, 2), (15, 4), (8, 2), (8, 4)])),
        Fuse::Output4(Select4_0) =>
            Ok(at(x, top, n, [(16, 1), (16, 3), (7, 1), (7, 3)])),
        Fuse::Output4(Select4_1) =>
            Ok(at(x, top, n, [(16, 2), (16, 4), (7, 2), (7, 4)])),
        Fuse::Output4(Select4_2) =>
            Ok(at(x, top, n, [(17, 1), (17, 3), (6, 1), (6, 3)])),
        Fuse::Output4(Select4_3) =>
            Ok(at(x, top, n, [(17, 2), (17, 4), (6, 2), (6, 4)])),
        Fuse::SchmittTrigger =>
            Ok(at(x, top, n, [(25, 0), (16, 0), (9, 0), (7, 0)])),
        Fuse::WeakPullUp =>
            io_cell_strip(strip, top || left, 4, false),
        _ =>
            Err(FuseOutOfRange::Fuse),
    }
}

fn io_column_cell_at(
    x: u8,
    top: bool,
    n: IOColumnCellNumber,
    cell: [(u8, u8); 4],
) -> FuseAt {
    let (sector, index) = cell[n.index()];
    FuseAt::End { x, top, sector, index }
}

fn io_cell_strip(
    strip: Option<u16>,
    top_left: bool,
    index: u16,
    pci_compliance: bool,
) -> Result<FuseAt, FuseOutOfRange> {
    if let Some(strip) = strip {
        if top_left {
            Ok(FuseAt::Strip { strip: strip + index })
        } else if pci_compliance {
            Ok(FuseAt::Strip { strip: strip + 6 - index })
        } else {
            Ok(FuseAt::Strip { strip: strip + 7 - index })
        }
    } else {
        Err(FuseOutOfRange::IO)
    }
}

fn io_column_interconnect(
    x: u8,
    top: bool,
    i: IOColumnInterconnectIndex,
    fuse: IOInterconnectFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use io_column_interconnect_end as end;
    use IOInterconnectFuse as Fuse;
    use Select3::*;
    use Select4::*;

    match fuse {
        Fuse::Source3(Select3_0) => end(x, top, i, 4, 1),
        Fuse::Source3(Select3_1) => end(x, top, i, 5, 1),
        Fuse::Source3(Select3_2) => end(x, top, i, 5, 2),
        Fuse::Source4(Select4_0) => end(x, top, i, 2, 1),
        Fuse::Source4(Select4_1) => end(x, top, i, 2, 2),
        Fuse::Source4(Select4_2) => end(x, top, i, 3, 1),
        Fuse::Source4(Select4_3) => end(x, top, i, 3, 2),
        _ =>
            Err(FuseOutOfRange::Fuse),
    }
}

fn io_column_interconnect_end(
    x: u8,
    top: bool,
    i: IOColumnInterconnectIndex,
    sector: u8,
    index: u8,
) -> Result<FuseAt, FuseOutOfRange> {
    use IOColumnInterconnectIndex::*;

    match i {
        IOColumnInterconnect0 => end!(x, top, 23 - sector, index + 0),
        IOColumnInterconnect1 => end!(x, top, 23 - sector, index + 2),
        IOColumnInterconnect2 => end!(x, top, 23 - sector, index + 4),
        IOColumnInterconnect3 => end!(x, top, 23 - sector, index + 6),
        IOColumnInterconnect4 => end!(x, top, 23 - sector, index + 8),
        IOColumnInterconnect5 => end!(x, top, sector, index + 0),
        IOColumnInterconnect6 => end!(x, top, sector, index + 2),
        IOColumnInterconnect7 => end!(x, top, sector, index + 4),
        IOColumnInterconnect8 => end!(x, top, sector, index + 6),
        IOColumnInterconnect9 => end!(x, top, sector, index + 8),
    }
}

fn io_row_cell(
    x: u8,
    left: bool,
    y: u8,
    n: IORowCellNumber,
    strip: Option<u16>,
    pci_compliance: bool,
    fuse: IOCellFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use io_row_cell_at as at;
    use IOCellFuse as Fuse;
    use IORowCellNumber::*;
    use LogicCellNumber::*;
    use Select3::*;
    use Select6::*;

    match fuse {
        Fuse::BusHold => io_cell_strip(strip, left, 2, pci_compliance),
        Fuse::EnableInvert => at(x, y, 2, n, 2),
        Fuse::Enable3(Select3_0) => at(x, y, 3, n, 3),
        Fuse::Enable3(Select3_1) => at(x, y, 2, n, 3),
        Fuse::Enable3(Select3_2) => at(x, y, 3, n, 2),
        Fuse::Enable6(Select6_0) => at(x, y, 4, n, 3),
        Fuse::Enable6(Select6_1) => at(x, y, 4, n, 2),
        Fuse::Enable6(Select6_2) => at(x, y, 5, n, 3),
        Fuse::Enable6(Select6_3) => at(x, y, 5, n, 2),
        Fuse::Enable6(Select6_4) => at(x, y, 6, n, 3),
        Fuse::Enable6(Select6_5) => at(x, y, 6, n, 2),
        Fuse::FastSlewRate => io_cell_strip(strip, left, 3, pci_compliance),
        Fuse::InputDelay if left =>
            match n {
                IORowCell0 => cell!(x, y, 2, LogicCell1, 0),
                IORowCell1 => cell!(x, y, 2, LogicCell1, 1),
                IORowCell2 => cell!(x, y, 2, LogicCell1, 2),
                IORowCell3 => block!(x, y, 4, 1),
                IORowCell4 => block!(x, y, 4, 2),
                IORowCell5 => block!(x, y, 4, 3),
                IORowCell6 => cell!(x, y, 1, LogicCell5, 3),
            },
        Fuse::InputDelay =>
            match n {
                IORowCell0 => cell!(x, y, 4, LogicCell1, 1),
                IORowCell1 => cell!(x, y, 4, LogicCell1, 2),
                IORowCell2 => cell!(x, y, 4, LogicCell1, 3),
                IORowCell3 => block!(x, y, 2, 0),
                IORowCell4 => block!(x, y, 2, 1),
                IORowCell5 => cell!(x, y, 3, LogicCell5, 2),
                IORowCell6 =>
                    Err(FuseOutOfRange::IO),
            },
        Fuse::InputOff =>
            match n {
                IORowCell0 => cell!(x, y, 0, LogicCell0, 0),
                IORowCell1 => cell!(x, y, 0, LogicCell1, 3),
                IORowCell2 => cell!(x, y, 0, LogicCell3, 1),
                IORowCell3 => block!(x, y, 0, 0),
                IORowCell4 => cell!(x, y, 0, LogicCell9, 2),
                IORowCell5 => cell!(x, y, 0, LogicCell8, 0),
                IORowCell6 => cell!(x, y, 0, LogicCell6, 1),
            },
        Fuse::LowCurrent0 => io_cell_strip(strip, left, 5, pci_compliance),
        Fuse::LowCurrent1 => io_cell_strip(strip, left, 6, pci_compliance),
        Fuse::OpenDrain => io_cell_strip(strip, left, 1, pci_compliance),
        Fuse::OutputInvert => at(x, y, 2, n, 1),
        Fuse::OutputFast => at(x, y, 1, n, 1),
        Fuse::Output3(Select3_0) => at(x, y, 3, n, 0),
        Fuse::Output3(Select3_1) => at(x, y, 2, n, 0),
        Fuse::Output3(Select3_2) => at(x, y, 3, n, 1),
        Fuse::Output6(Select6_0) => at(x, y, 4, n, 0),
        Fuse::Output6(Select6_1) => at(x, y, 4, n, 1),
        Fuse::Output6(Select6_2) => at(x, y, 5, n, 0),
        Fuse::Output6(Select6_3) => at(x, y, 5, n, 1),
        Fuse::Output6(Select6_4) => at(x, y, 6, n, 0),
        Fuse::Output6(Select6_5) => at(x, y, 6, n, 1),
        Fuse::PCICompliance => io_cell_strip(strip, left, 0, pci_compliance),
        Fuse::SchmittTrigger =>
            match n {
                IORowCell0 => cell!(x, y, 0, LogicCell0, 1),
                IORowCell1 => cell!(x, y, 0, LogicCell2, 0),
                IORowCell2 => cell!(x, y, 0, LogicCell3, 2),
                IORowCell3 => block!(x, y, 0, 1),
                IORowCell4 => cell!(x, y, 0, LogicCell9, 1),
                IORowCell5 => cell!(x, y, 0, LogicCell7, 3),
                IORowCell6 => cell!(x, y, 0, LogicCell6, 0),
            },
        Fuse::WeakPullUp => io_cell_strip(strip, left, 4, pci_compliance),
        _ => Err(FuseOutOfRange::Fuse),
    }
}

fn io_row_cell_at(
    x: u8,
    y: u8,
    sector: u8,
    n: IORowCellNumber,
    index: u8,
) -> Result<FuseAt, FuseOutOfRange> {
    use IORowCellNumber::*;
    use LogicCellNumber::*;

    match n {
        IORowCell0 => cell!(x, y, sector, LogicCell2, index),
        IORowCell1 => cell!(x, y, sector, LogicCell3, index),
        IORowCell2 => cell!(x, y, sector, LogicCell4, index),
        IORowCell3 => cell!(x, y, sector, LogicCell9, index),
        IORowCell4 => cell!(x, y, sector, LogicCell8, index),
        IORowCell5 => cell!(x, y, sector, LogicCell7, index),
        IORowCell6 => cell!(x, y, sector, LogicCell6, index),
    }
}

fn io_row_interconnect(
    x: u8,
    y: u8,
    i: IORowInterconnectIndex,
    fuse: IOInterconnectFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use io_row_interconnect_at as at;
    use IORowInterconnectIndex::*;
    use IOInterconnectFuse as Fuse;
    use Select3::*;
    use Select4::*;

    match fuse {
        Fuse::DirectLink => at(x, y, i, 8, 1),
        Fuse::Source3(Select3_0) => at(x, y, i, 7, 0),
        Fuse::Source3(Select3_1) => at(x, y, i, 7, 1),
        Fuse::Source3(Select3_2) => at(x, y, i, 8, 0),
        Fuse::Source4(Select4_0) => at(x, y, i, 9, 0),
        Fuse::Source4(Select4_1) => at(x, y, i, 9, 1),
        Fuse::Source4(Select4_2) => at(x, y, i, 10, 0),
        Fuse::Source4(Select4_3) => at(x, y, i, 10, 1),
        Fuse::SourceGlobal =>
            match i {
                IORowInterconnect8 if x < 2 => block!(x, y, 10, 0),
                IORowInterconnect8 => block!(x, y, 7, 0),
                IORowInterconnect17 if x < 2 => block!(x, y, 10, 5),
                IORowInterconnect17 => block!(x, y, 7, 5),
                _ =>
                    Err(FuseOutOfRange::Fuse),
            },
    }
}

fn io_row_interconnect_at(
    x: u8,
    y: u8,
    i: IORowInterconnectIndex,
    sector: u8,
    index: u8,
) -> Result<FuseAt, FuseOutOfRange> {
    use IORowInterconnectIndex::*;
    use LogicCellNumber::*;

    match i {
        IORowInterconnect0 => cell!(x, y, sector, LogicCell0, index + 2),
        IORowInterconnect1 => cell!(x, y, sector, LogicCell1, index + 0),
        IORowInterconnect2 => cell!(x, y, sector, LogicCell1, index + 2),
        IORowInterconnect3 => cell!(x, y, sector, LogicCell2, index + 0),
        IORowInterconnect4 => cell!(x, y, sector, LogicCell2, index + 2),
        IORowInterconnect5 => cell!(x, y, sector, LogicCell3, index + 0),
        IORowInterconnect6 => cell!(x, y, sector, LogicCell3, index + 2),
        IORowInterconnect7 => cell!(x, y, sector, LogicCell4, index + 0),
        IORowInterconnect8 => cell!(x, y, sector, LogicCell4, index + 2),
        IORowInterconnect9 => cell!(x, y, sector, LogicCell5, index + 2),
        IORowInterconnect10 => cell!(x, y, sector, LogicCell6, index + 0),
        IORowInterconnect11 => cell!(x, y, sector, LogicCell6, index + 2),
        IORowInterconnect12 => cell!(x, y, sector, LogicCell7, index + 0),
        IORowInterconnect13 => cell!(x, y, sector, LogicCell7, index + 2),
        IORowInterconnect14 => cell!(x, y, sector, LogicCell8, index + 0),
        IORowInterconnect15 => cell!(x, y, sector, LogicCell8, index + 2),
        IORowInterconnect16 => cell!(x, y, sector, LogicCell9, index + 0),
        IORowInterconnect17 => cell!(x, y, sector, LogicCell9, index + 2),
    }
}

fn logic_block(
    x: u8,
    y: u8,
    fuse: LogicBlockFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use Control::*;
    use Global::*;
    use LogicCellNumber::*;
    use LogicBlockControlFuse::*;
    use LogicBlockFuse as Fuse;
    use Select3::*;
    use Select6::*;

    match fuse {
        Fuse::AClearGlobal(Global0) => cell!(x, y, 5, LogicCell4, 2),
        Fuse::AClearGlobal(Global1) => cell!(x, y, 5, LogicCell4, 3),
        Fuse::AClearGlobal(Global2) => cell!(x, y, 5, LogicCell9, 2),
        Fuse::AClearGlobal(Global3) => cell!(x, y, 5, LogicCell9, 3),
        Fuse::AClear1Control5Not4 => block!(x, y, 5, 5),
        Fuse::AClear1Global => block!(x, y, 20, 1),
        Fuse::AClear1Invert => block!(x, y, 21, 3),
        Fuse::AClear1Off => block!(x, y, 20, 5),
        Fuse::AClear2Control5Not4 => block!(x, y, 5, 0),
        Fuse::AClear2Global => block!(x, y, 15, 5),
        Fuse::AClear2Invert => block!(x, y, 21, 4),
        Fuse::AClear2Off => block!(x, y, 21, 5),
        Fuse::ALoadControl => block!(x, y, 21, 2),
        Fuse::ALoadInvert => block!(x, y, 19, 5),
        Fuse::CarryInInvertA => block!(x, y, 22, 4),
        Fuse::Clock1Control => block!(x, y, 15, 0),
        Fuse::Clock1Control0Not1 => block!(x, y, 16, 0),
        Fuse::Clock1Global(Global0) => cell!(x, y, 3, LogicCell4, 2),
        Fuse::Clock1Global(Global1) => cell!(x, y, 3, LogicCell4, 3),
        Fuse::Clock1Global(Global2) => cell!(x, y, 3, LogicCell9, 2),
        Fuse::Clock1Global(Global3) => cell!(x, y, 3, LogicCell9, 3),
        Fuse::Clock1Invert => block!(x, y, 15, 1),
        Fuse::Clock2ALoadControl3Not2 => block!(x, y, 15, 2),
        Fuse::Clock2Control => block!(x, y, 16, 3),
        Fuse::Clock2Global(Global0) => cell!(x, y, 4, LogicCell4, 2),
        Fuse::Clock2Global(Global1) => cell!(x, y, 4, LogicCell4, 3),
        Fuse::Clock2Global(Global2) => cell!(x, y, 4, LogicCell9, 2),
        Fuse::Clock2Global(Global3) => cell!(x, y, 4, LogicCell9, 3),
        Fuse::Clock2Invert => block!(x, y, 19, 2),
        Fuse::Control { control: Control0, fuse: Source3(Select3_0) } => block!(x, y, 12, 0),
        Fuse::Control { control: Control0, fuse: Source3(Select3_1) } => block!(x, y, 13, 0),
        Fuse::Control { control: Control0, fuse: Source3(Select3_2) } => block!(x, y, 14, 0),
        Fuse::Control { control: Control0, fuse: Source6(Select6_0) } => block!(x, y, 6, 0),
        Fuse::Control { control: Control0, fuse: Source6(Select6_1) } => block!(x, y, 8, 0),
        Fuse::Control { control: Control0, fuse: Source6(Select6_2) } => block!(x, y, 6, 1),
        Fuse::Control { control: Control0, fuse: Source6(Select6_3) } => block!(x, y, 7, 0),
        Fuse::Control { control: Control0, fuse: Source6(Select6_4) } => block!(x, y, 8, 1),
        Fuse::Control { control: Control0, fuse: Source6(Select6_5) } => block!(x, y, 7, 1),
        Fuse::Control { control: Control1, fuse: Source3(Select3_0) } => block!(x, y, 14, 1),
        Fuse::Control { control: Control1, fuse: Source3(Select3_1) } => block!(x, y, 13, 1),
        Fuse::Control { control: Control1, fuse: Source3(Select3_2) } => block!(x, y, 12, 1),
        Fuse::Control { control: Control1, fuse: Source6(Select6_0) } => block!(x, y, 11, 0),
        Fuse::Control { control: Control1, fuse: Source6(Select6_1) } => block!(x, y, 9, 0),
        Fuse::Control { control: Control1, fuse: Source6(Select6_2) } => block!(x, y, 9, 1),
        Fuse::Control { control: Control1, fuse: Source6(Select6_3) } => block!(x, y, 10, 0),
        Fuse::Control { control: Control1, fuse: Source6(Select6_4) } => block!(x, y, 11, 1),
        Fuse::Control { control: Control1, fuse: Source6(Select6_5) } => block!(x, y, 10, 1),
        Fuse::Control { control: Control2, fuse: Source3(Select3_0) } => block!(x, y, 12, 2),
        Fuse::Control { control: Control2, fuse: Source3(Select3_1) } => block!(x, y, 13, 2),
        Fuse::Control { control: Control2, fuse: Source3(Select3_2) } => block!(x, y, 14, 2),
        Fuse::Control { control: Control2, fuse: Source6(Select6_0) } => block!(x, y, 6, 2),
        Fuse::Control { control: Control2, fuse: Source6(Select6_1) } => block!(x, y, 8, 2),
        Fuse::Control { control: Control2, fuse: Source6(Select6_2) } => block!(x, y, 6, 3),
        Fuse::Control { control: Control2, fuse: Source6(Select6_3) } => block!(x, y, 7, 2),
        Fuse::Control { control: Control2, fuse: Source6(Select6_4) } => block!(x, y, 8, 3),
        Fuse::Control { control: Control2, fuse: Source6(Select6_5) } => block!(x, y, 7, 3),
        Fuse::Control { control: Control3, fuse: Source3(Select3_0) } => block!(x, y, 14, 3),
        Fuse::Control { control: Control3, fuse: Source3(Select3_1) } => block!(x, y, 13, 3),
        Fuse::Control { control: Control3, fuse: Source3(Select3_2) } => block!(x, y, 12, 3),
        Fuse::Control { control: Control3, fuse: Source6(Select6_0) } => block!(x, y, 11, 2),
        Fuse::Control { control: Control3, fuse: Source6(Select6_1) } => block!(x, y, 9, 2),
        Fuse::Control { control: Control3, fuse: Source6(Select6_2) } => block!(x, y, 9, 3),
        Fuse::Control { control: Control3, fuse: Source6(Select6_3) } => block!(x, y, 10, 2),
        Fuse::Control { control: Control3, fuse: Source6(Select6_4) } => block!(x, y, 11, 3),
        Fuse::Control { control: Control3, fuse: Source6(Select6_5) } => block!(x, y, 10, 3),
        Fuse::Control { control: Control4, fuse: Source3(Select3_0) } => block!(x, y, 12, 4),
        Fuse::Control { control: Control4, fuse: Source3(Select3_1) } => block!(x, y, 13, 4),
        Fuse::Control { control: Control4, fuse: Source3(Select3_2) } => block!(x, y, 14, 4),
        Fuse::Control { control: Control4, fuse: Source6(Select6_0) } => block!(x, y, 6, 4),
        Fuse::Control { control: Control4, fuse: Source6(Select6_1) } => block!(x, y, 8, 4),
        Fuse::Control { control: Control4, fuse: Source6(Select6_2) } => block!(x, y, 6, 5),
        Fuse::Control { control: Control4, fuse: Source6(Select6_3) } => block!(x, y, 7, 4),
        Fuse::Control { control: Control4, fuse: Source6(Select6_4) } => block!(x, y, 8, 5),
        Fuse::Control { control: Control4, fuse: Source6(Select6_5) } => block!(x, y, 7, 5),
        Fuse::Control { control: Control5, fuse: Source3(Select3_0) } => block!(x, y, 14, 5),
        Fuse::Control { control: Control5, fuse: Source3(Select3_1) } => block!(x, y, 13, 5),
        Fuse::Control { control: Control5, fuse: Source3(Select3_2) } => block!(x, y, 12, 5),
        Fuse::Control { control: Control5, fuse: Source6(Select6_0) } => block!(x, y, 11, 4),
        Fuse::Control { control: Control5, fuse: Source6(Select6_1) } => block!(x, y, 9, 4),
        Fuse::Control { control: Control5, fuse: Source6(Select6_2) } => block!(x, y, 9, 5),
        Fuse::Control { control: Control5, fuse: Source6(Select6_3) } => block!(x, y, 10, 4),
        Fuse::Control { control: Control5, fuse: Source6(Select6_4) } => block!(x, y, 11, 5),
        Fuse::Control { control: Control5, fuse: Source6(Select6_5) } => block!(x, y, 10, 5),
        Fuse::Enable1Off => block!(x, y, 16, 4),
        Fuse::Enable1Control3Not2 => block!(x, y, 16, 2),
        Fuse::Enable1Invert => block!(x, y, 16, 1),
        Fuse::Enable2Off => block!(x, y, 19, 0),
        Fuse::Enable2SLoadControl0Not1 => block!(x, y, 17, 0),
        Fuse::Enable2SLoadInvert => block!(x, y, 20, 4),
        Fuse::InvertA => block!(x, y, 20, 2),
        Fuse::InvertAControl4Not3 => block!(x, y, 15, 3),
        Fuse::SClearControl => block!(x, y, 21, 0),
        Fuse::SClearControl5Not4 => block!(x, y, 15, 4),
        Fuse::SClearInvert => block!(x, y, 21, 1),
        Fuse::SLoadControl => block!(x, y, 19, 1),
        Fuse::SLoadNotAlways => block!(x, y, 20, 0),
    }
}

fn logic_cell(
    x: u8,
    y: u8,
    n: LogicCellNumber,
    fuse: LogicCellFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use LogicCellFuse as Fuse;
    use LogicCellInput::*;
    use LogicCellInputFuse::*;
    use FuseAt as At;
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
        Fuse::OutputRightLUT =>
            Ok(At::Cell { x, y, sector: 19, n, index: 1 }),
        Fuse::RegisterChainOff =>
            Ok(At::Cell { x, y, sector: 20, n, index: 2 }),
        Fuse::Syncronous =>
            Ok(At::Cell { x, y, sector: 21, n, index: 0 }),
    }
}

fn logic_interconnect(
    x: u8,
    y: u8,
    i: LogicInterconnectIndex,
    fuse: LogicInterconnectFuse,
) -> Result<FuseAt, FuseOutOfRange> {
    use FuseAt as At;
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

fn user_code_location(bit: UserCodeBit) -> Result<FuseAt, FuseOutOfRange> {
    use UserCodeBit::*;

    match bit {
        UserCodeBit0  => end!(1, false, 1, 4),
        UserCodeBit1  => end!(1, false, 2, 4),
        UserCodeBit2  => end!(1, false, 3, 4),
        UserCodeBit3  => end!(1, false, 4, 4),
        UserCodeBit4  => end!(1, false, 5, 4),
        UserCodeBit5  => end!(1, false, 6, 4),
        UserCodeBit6  => end!(1, false, 7, 4),
        UserCodeBit7  => end!(1, false, 8, 4),
        UserCodeBit8  => end!(1, false, 9, 4),
        UserCodeBit9  => end!(1, false, 10, 4),
        UserCodeBit10 => end!(1, false, 1, 3),
        UserCodeBit11 => end!(1, false, 2, 3),
        UserCodeBit12 => end!(1, false, 3, 3),
        UserCodeBit13 => end!(1, false, 4, 3),
        UserCodeBit14 => end!(1, false, 5, 3),
        UserCodeBit15 => end!(1, false, 6, 3),
        UserCodeBit16 => end!(1, false, 7, 3),
        UserCodeBit17 => end!(1, false, 8, 3),
        UserCodeBit18 => end!(1, false, 9, 3),
        UserCodeBit19 => end!(1, false, 10, 3),
        UserCodeBit20 => end!(1, false, 1, 2),
        UserCodeBit21 => end!(1, false, 2, 2),
        UserCodeBit22 => end!(1, false, 3, 2),
        UserCodeBit23 => end!(1, false, 4, 2),
        UserCodeBit24 => end!(1, false, 5, 2),
        UserCodeBit25 => end!(1, false, 6, 2),
        UserCodeBit26 => end!(1, false, 7, 2),
        UserCodeBit27 => end!(1, false, 8, 2),
        UserCodeBit28 => end!(1, false, 9, 2),
        UserCodeBit29 => end!(1, false, 10, 2),
        UserCodeBit30 => end!(1, false, 9, 1),
        UserCodeBit31 => end!(1, false, 10, 1),
    }
}

fn user_code_location_grow(bit: UserCodeBit, x: u8)
    -> Result<FuseAt, FuseOutOfRange>
{
    use UserCodeBit::*;

    match bit {
        UserCodeBit0  => end!(x, false, 19, 9),
        UserCodeBit1  => end!(x, false, 18, 9),
        UserCodeBit2  => end!(x, false, 17, 9),
        UserCodeBit3  => end!(x, false, 16, 9),
        UserCodeBit4  => end!(x, false, 15, 9),
        UserCodeBit5  => end!(x, false, 14, 9),
        UserCodeBit6  => end!(x, false, 19, 10),
        UserCodeBit7  => end!(x, false, 18, 10),
        UserCodeBit8  => end!(x, false, 17, 10),
        UserCodeBit9  => end!(x, false, 16, 10),
        UserCodeBit10 => end!(x, false, 15, 10),
        UserCodeBit11 => end!(x, false, 14, 10),
        UserCodeBit12 => end!(x, false, 13, 10),
        UserCodeBit13 => end!(x, false, 12, 10),
        UserCodeBit14 => end!(x, false, 11, 10),
        UserCodeBit15 => end!(x, false, 10, 10),
        UserCodeBit16 => end!(x, false, 9, 10),
        UserCodeBit17 => end!(x, false, 8, 10),
        UserCodeBit18 => end!(x, false, 7, 10),
        UserCodeBit19 => end!(x, false, 6, 10),
        UserCodeBit20 => end!(x, false, 5, 10),
        UserCodeBit21 => end!(x, false, 4, 10),
        UserCodeBit22 => end!(x, false, 3, 10),
        UserCodeBit23 => end!(x, false, 2, 10),
        UserCodeBit24 => end!(x, false, 1, 10),
        UserCodeBit25 => end!(x, false, 0, 10),
        UserCodeBit26 => end!(x - 1, false, 27, 10),
        UserCodeBit27 => end!(x - 1, false, 26, 10),
        UserCodeBit28 => end!(x, false, 13, 9),
        UserCodeBit29 => end!(x, false, 12, 9),
        UserCodeBit30 => end!(x, false, 11, 9),
        UserCodeBit31 => end!(x, false, 10, 9),
    }
}

