use std::collections::HashMap;

use crate::{
    Control,
    Device,
    IOColumnCellNumber,
    IOColumnInterconnectIndex,
    IORowCellNumber,
    IORowInterconnectIndex,
    LogicCellInput,
    LogicCellNumber,
    LogicInterconnectIndex,
    PinName,
    Port,
    X,
    Y,
};

mod read;

pub struct DeviceSources {
    pub device: Device,
    blocks: [[Block; 15]; 22],
    //global: [Interconnect<18>; 4],
    //jtag: JTAG,
    pins: HashMap<PinName, PinSource>,
    //ufm: UFM,
}

impl DeviceSources {
    fn block(&self, x: X, y: Y) -> Option<&Block> {
        self.blocks.get(x.0 as usize)
            .and_then(|col| col.get(y.0 as usize))
    }

    pub fn io_column_cell(
        &self, x: X, y: Y,
        n: IOColumnCellNumber,
    ) -> Option<IOColumnCellSources> {
        if let Some(Block::Column(block)) = self.block(x, y) {
            if let Some(io_cell) = &block.io_cells[n.index()] {
                Some(IOColumnCellSources { io_cell })
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn io_column_interconnect(
        &self, x: X, y: Y,
        i: IOColumnInterconnectIndex,
    ) -> Option<InterconnectSources> {
        if let Some(Block::Column(block)) = self.block(x, y) {
            let interconnect = &block.io_interconnects[i.index()];
            Some(interconnect.sources())
        } else {
            None
        }
    }

    pub fn io_row_cell(
        &self, x: X, y: Y,
        n: IORowCellNumber,
    ) -> Option<IORowCellSources> {
        match self.block(x, y) {
            Some(Block::Left(block)) =>
                if let Some(io_cell) = &block.io_cells[n.index()] {
                    Some(IORowCellSources { io_cell })
                } else {
                    None
                },

            Some(Block::Right(block)) =>
                if let Some(io_cell) = &block.io_cells[n.index()] {
                    Some(IORowCellSources { io_cell })
                } else {
                    None
                },

            _ =>
                None,
        }
    }

    pub fn io_row_interconnect(
        &self, x: X, y: Y,
        i: IORowInterconnectIndex,
    )
        -> Option<InterconnectSources>
    {
        match self.block(x, y) {
            Some(Block::Left(block)) => {
                let interconnect = block.io_interconnects.get(i);
                Some(interconnect.sources())
            }

            Some(Block::Right(block)) => {
                let interconnect = block.io_interconnects.get(i);
                Some(interconnect.sources())
            }

            _ =>
                None,
        }
    }

    pub fn logic_cell(
        &self, x: X, y: Y,
        n: LogicCellNumber,
        input: LogicCellInput,
    ) -> Option<InterconnectSources> {
        if let Some(Block::Logic(block)) = self.block(x, y) {
            let cell = &block.logic_cells[n.index()];
            match input {
                LogicCellInput::A => Some(cell.input_a.sources()),
                LogicCellInput::B => Some(cell.input_b.sources()),
                LogicCellInput::C => Some(cell.input_c.sources()),
                LogicCellInput::D => Some(cell.input_d.sources()),
            }
        } else {
            None
        }
    }

    pub fn logic_control(&self, x: X, y: Y, i: Control)
        -> Option<InterconnectSources>
    {
        if let Some(Block::Logic(block)) = self.block(x, y) {
            let interconnect =  &block.logic_controls[i.index()];
            Some(interconnect.sources())
        } else {
            None
        }
    }

    pub fn logic_interconnect(&self, x: X, y: Y, i: LogicInterconnectIndex)
        -> Option<InterconnectSources>
    {
        if let Some(Block::Logic(block)) = self.block(x, y) {
            let interconnect = block.logic_interconnects.get(i);
            Some(interconnect.sources())
        } else {
            None
        }
    }

    pub fn pins(&self) -> impl Iterator<Item = (&PinName, &PinSource)> {
        self.pins.iter()
    }

    pub fn pin(&self, name: &str) -> Option<PinSource> {
        self.pins.get(name).cloned()
    }
}

#[derive(Copy, Clone)]
pub struct IOColumnCellSources<'d> {
    io_cell: &'d IOColumnCell,
}

impl<'d> IOColumnCellSources<'d> {
    pub fn enable(&self) -> InterconnectSources<'d> {
        self.io_cell.enable.sources()
    }

    pub fn pin_name(&self) -> PinName {
        self.io_cell.pin_name
    }

    pub fn output(&self) -> InterconnectSources<'d> {
        self.io_cell.output.sources()
    }
}

#[derive(Copy, Clone)]
pub struct IORowCellSources<'d> {
    io_cell: &'d IORowCell,
}

impl<'d> IORowCellSources<'d> {
    pub fn enable(&self) -> InterconnectSources<'d> {
        self.io_cell.enable.sources()
    }

    pub fn pin_name(&self) -> PinName {
        self.io_cell.pin_name
    }

    pub fn output(&self) -> InterconnectSources<'d> {
        self.io_cell.output.sources()
    }
}

#[derive(Copy, Clone)]
pub struct InterconnectSources<'d> {
    port: Port,
    sources: &'d [Source],
}

impl<'d> InterconnectSources<'d> {
    pub fn port(&self) -> Port {
        self.port
    }

    pub fn source(&self, index: usize) -> Option<Port> {
        self.sources.get(index)
            .map(|source| source.port)
    }

    pub fn sources(&self) -> InterconnectSourcesIter<'d> {
        InterconnectSourcesIter {
            iter: self.sources.iter().enumerate(),
        }
    }
}

pub struct InterconnectSourcesIter<'d> {
    iter: std::iter::Enumerate<std::slice::Iter<'d, Source>>,
}

impl<'d> Iterator for InterconnectSourcesIter<'d> {
    type Item = (usize, Port);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((index, source)) = self.iter.next() {
            Some((index, source.port))
        } else {
            None
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum PinSource {
    Column {
        x: X,
        y: Y,
        n: IOColumnCellNumber,
    },
    Row {
        x: X,
        y: Y,
        n: IORowCellNumber,
    },
}

//  Block
// =======

#[derive(Default)]
enum Block {
    #[default]
    Blank,
    //Corner {
    //    c4_interconnects: [Interconnect_; 14],
    //},
    Column(Box<ColumnBlock>),
    Left(Box<LeftBlock>),
    Right(Box<RightBlock>),
    Logic(Box<LogicBlock>),
    //UFM {
    //    c4_interconnects: [Interconnect_; 14],
    //    r4_interconnects: [Interconnect_; 16],
    //    ufm_interconnects: [Interconnect_; 10],
    //},
    //Grow {
    //    c4_interconnects: [Interconnect_; 14],
    //    r4_interconnects: [Interconnect_; 16],
    //},
}

//  Interconnect
// ==============

#[derive(Copy, Clone)]
#[derive(Debug)]
struct Interconnect<const N: usize> {
    port: Port,
    sources: [Source; N],
}

impl<const N: usize> Default for Interconnect<N>
where
    [Source; N]: Default,
{
    fn default() -> Self {
        Interconnect {
            port: Default::default(),
            sources: Default::default(),
        }
    }
}

impl<const N: usize> Interconnect<N> {
    fn sources(&self) -> InterconnectSources {
        InterconnectSources {
            port: self.port,
            sources: &self.sources,
        }
    }
}

enum InterconnectRef<'d> {
    Normal(&'d Interconnect<13>),
    Global(&'d Interconnect<16>),
}

impl<'d> InterconnectRef<'d> {
    fn sources(self) -> InterconnectSources<'d> {
        match self {
            Self::Normal(interconnect) => interconnect.sources(),
            Self::Global(interconnect) => interconnect.sources(),
        }
    }
}

enum InterconnectMut<'d> {
    Normal(&'d mut Interconnect<13>),
    Global(&'d mut Interconnect<16>),
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Default)]
struct Source {
    port: Port,
    fuse: [usize; 3],
}

//  IO
// ====

#[derive(Default)]
struct ColumnBlock {
    //c4_interconnects: [Interconnect_; 14],
    io_cells: [Option<IOColumnCell>; 4],
    io_interconnects: [Interconnect<12>; 10],
}

#[derive(Default)]
struct LeftBlock {
    //c4_interconnects: [Interconnect_; 14],
    io_cells: [Option<IORowCell>; 7],
    io_interconnects: IORowInterconnects,
    //r4_interconnects: [Interconnect_; 16],
}

#[derive(Default)]
struct RightBlock {
    io_cells: [Option<IORowCell>; 7],
    io_interconnects: IORowInterconnects,
    //r4_interconnects: [Interconnect_; 16],
}

#[derive(Default)]
struct IOColumnCell {
    enable: Interconnect<10>,
    pin_name: PinName,
    output: Interconnect<11>,
}

#[derive(Default)]
struct IORowCell {
    enable: Interconnect<18>,
    pin_name: PinName,
    output: Interconnect<19>,
}

#[derive(Default)]
struct IORowInterconnects(
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<16>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<16>,
);

impl IORowInterconnects {
    fn get(&self, i: IORowInterconnectIndex) -> InterconnectRef {
        use IORowInterconnectIndex::*;
        use InterconnectRef::*;

        match i {
            IORowInterconnect0 => Normal(&self.0),
            IORowInterconnect1 => Normal(&self.1),
            IORowInterconnect2 => Normal(&self.2),
            IORowInterconnect3 => Normal(&self.3),
            IORowInterconnect4 => Normal(&self.4),
            IORowInterconnect5 => Normal(&self.5),
            IORowInterconnect6 => Normal(&self.6),
            IORowInterconnect7 => Normal(&self.7),
            IORowInterconnect8 => Global(&self.8),
            IORowInterconnect9 => Normal(&self.9),
            IORowInterconnect10 => Normal(&self.10),
            IORowInterconnect11 => Normal(&self.11),
            IORowInterconnect12 => Normal(&self.12),
            IORowInterconnect13 => Normal(&self.13),
            IORowInterconnect14 => Normal(&self.14),
            IORowInterconnect15 => Normal(&self.15),
            IORowInterconnect16 => Normal(&self.16),
            IORowInterconnect17 => Global(&self.17),
        }
    }

    fn get_mut(&mut self, i: IORowInterconnectIndex) -> InterconnectMut {
        use IORowInterconnectIndex::*;
        use InterconnectMut::*;

        match i {
            IORowInterconnect0 => Normal(&mut self.0),
            IORowInterconnect1 => Normal(&mut self.1),
            IORowInterconnect2 => Normal(&mut self.2),
            IORowInterconnect3 => Normal(&mut self.3),
            IORowInterconnect4 => Normal(&mut self.4),
            IORowInterconnect5 => Normal(&mut self.5),
            IORowInterconnect6 => Normal(&mut self.6),
            IORowInterconnect7 => Normal(&mut self.7),
            IORowInterconnect8 => Global(&mut self.8),
            IORowInterconnect9 => Normal(&mut self.9),
            IORowInterconnect10 => Normal(&mut self.10),
            IORowInterconnect11 => Normal(&mut self.11),
            IORowInterconnect12 => Normal(&mut self.12),
            IORowInterconnect13 => Normal(&mut self.13),
            IORowInterconnect14 => Normal(&mut self.14),
            IORowInterconnect15 => Normal(&mut self.15),
            IORowInterconnect16 => Normal(&mut self.16),
            IORowInterconnect17 => Global(&mut self.17),
        }
    }
}

//  Logic
// =======

#[derive(Default)]
struct LogicBlock {
    //c4_interconnect: [Interconnect<13>; 14],
    logic_cells: [LogicCell; 10],
    logic_controls: [Interconnect<18>; 6],
    logic_interconnects: LogicInterconnects,
    //r4_interconnect: [Interconnect<13>; 16],
}

#[derive(Default)]
struct LogicCell {
    input_a: Interconnect<18>,
    input_b: Interconnect<18>,
    input_c: Interconnect<18>,
    input_d: Interconnect<18>,
}

#[derive(Default)]
struct LogicInterconnects(
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<16>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<13>,
    Interconnect<16>,
);

impl LogicInterconnects {
    fn get(&self, i: LogicInterconnectIndex) -> InterconnectRef {
        use LogicInterconnectIndex::*;
        use InterconnectRef::*;

        match i {
            LogicInterconnect0 => Normal(&self.0),
            LogicInterconnect1 => Normal(&self.1),
            LogicInterconnect2 => Normal(&self.2),
            LogicInterconnect3 => Normal(&self.3),
            LogicInterconnect4 => Normal(&self.4),
            LogicInterconnect5 => Normal(&self.5),
            LogicInterconnect6 => Normal(&self.6),
            LogicInterconnect7 => Normal(&self.7),
            LogicInterconnect8 => Normal(&self.8),
            LogicInterconnect9 => Normal(&self.9),
            LogicInterconnect10 => Normal(&self.10),
            LogicInterconnect11 => Normal(&self.11),
            LogicInterconnect12 => Global(&self.12),
            LogicInterconnect13 => Normal(&self.13),
            LogicInterconnect14 => Normal(&self.14),
            LogicInterconnect15 => Normal(&self.15),
            LogicInterconnect16 => Normal(&self.16),
            LogicInterconnect17 => Normal(&self.17),
            LogicInterconnect18 => Normal(&self.18),
            LogicInterconnect19 => Normal(&self.19),
            LogicInterconnect20 => Normal(&self.20),
            LogicInterconnect21 => Normal(&self.21),
            LogicInterconnect22 => Normal(&self.22),
            LogicInterconnect23 => Normal(&self.23),
            LogicInterconnect24 => Normal(&self.24),
            LogicInterconnect25 => Global(&self.25),
        }
    }

    fn get_mut(&mut self, i: LogicInterconnectIndex) -> InterconnectMut {
        use LogicInterconnectIndex::*;
        use InterconnectMut::*;

        match i {
            LogicInterconnect0 => Normal(&mut self.0),
            LogicInterconnect1 => Normal(&mut self.1),
            LogicInterconnect2 => Normal(&mut self.2),
            LogicInterconnect3 => Normal(&mut self.3),
            LogicInterconnect4 => Normal(&mut self.4),
            LogicInterconnect5 => Normal(&mut self.5),
            LogicInterconnect6 => Normal(&mut self.6),
            LogicInterconnect7 => Normal(&mut self.7),
            LogicInterconnect8 => Normal(&mut self.8),
            LogicInterconnect9 => Normal(&mut self.9),
            LogicInterconnect10 => Normal(&mut self.10),
            LogicInterconnect11 => Normal(&mut self.11),
            LogicInterconnect12 => Global(&mut self.12),
            LogicInterconnect13 => Normal(&mut self.13),
            LogicInterconnect14 => Normal(&mut self.14),
            LogicInterconnect15 => Normal(&mut self.15),
            LogicInterconnect16 => Normal(&mut self.16),
            LogicInterconnect17 => Normal(&mut self.17),
            LogicInterconnect18 => Normal(&mut self.18),
            LogicInterconnect19 => Normal(&mut self.19),
            LogicInterconnect20 => Normal(&mut self.20),
            LogicInterconnect21 => Normal(&mut self.21),
            LogicInterconnect22 => Normal(&mut self.22),
            LogicInterconnect23 => Normal(&mut self.23),
            LogicInterconnect24 => Normal(&mut self.24),
            LogicInterconnect25 => Global(&mut self.25),
        }
    }
}

//#[derive(Default)]
//struct JTAG {
//    tdo: Interconnect<18>,
//}

//#[derive(Default)]
//struct UFM {
//    ar_clk: Interconnect<18>,
//    ar_in: Interconnect<18>,
//    ar_shift: Interconnect<18>,
//    dr_clk: Interconnect<18>,
//    dr_in: Interconnect<18>,
//    dr_shift: Interconnect<18>,
//    erase: Interconnect<18>,
//    osc_ena: Interconnect<18>,
//    program: Interconnect<18>,
//}

