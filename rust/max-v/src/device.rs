use std::collections::HashMap;

use crate::{
    Control,
    C4InterconnectIndex,
    Device,
    DensityLayout,
    Global,
    IOColumnCellNumber,
    IOColumnInterconnectIndex,
    IORowCellNumber,
    IORowInterconnectIndex,
    JTAGInput,
    LogicCellInput,
    LogicCellNumber,
    LogicInterconnectIndex,
    PinName,
    Port,
    R4InterconnectIndex,
    UFMInput,
    X,
    Y,
};

mod read;

pub struct DeviceSources {
    device: Device,
    blocks: [[Block; 15]; 22],
    globals: GlobalInterconnects,
    jtag: JTAGInterconnects,
    pins: HashMap<PinName, PinSource>,
    ufm: UFMInterconnects,
}

impl DeviceSources {
    pub fn device(&self) -> Device {
        self.device
    }

    pub fn density_layout(&self) -> &'static DensityLayout {
        self.device.density().layout()
    }

    fn block(&self, x: X, y: Y) -> Option<&Block> {
        self.blocks.get(x.0 as usize)
            .and_then(|col| col.get(y.0 as usize))
    }

    pub fn c4_interconnect(&self, x: X, y: Y, i: C4InterconnectIndex)
        -> Option<InterconnectSources>
    {
        match self.block(x, y) {
            Some(Block::Column(block)) => {
                let interconnect = &block.c4_interconnects.0[i.index()];
                interconnect.sources()
            }

            Some(Block::Corner(block)) => {
                let interconnect = &block.c4_interconnects.0[i.index()];
                interconnect.sources()
            }

            Some(Block::Grow(block)) => {
                let interconnect = &block.c4_interconnects[i.index()];
                Some(interconnect.sources())
            }

            Some(Block::Left(block)) => {
                let interconnect = &block.c4_interconnects[i.index()];
                Some(interconnect.sources())
            }

            Some(Block::Logic(block)) => {
                let interconnect = &block.c4_interconnects[i.index()];
                Some(interconnect.sources())
            }

            Some(Block::UFM(block)) => {
                let interconnect = &block.c4_interconnects[i.index()];
                Some(interconnect.sources())
            }

            _ =>
                None,
        }
    }

    pub fn global_interconnect(&self, global: Global)
        -> InterconnectSources
    {
        match global {
            Global::Global0 => self.globals.0.interconnect.sources(),
            Global::Global1 => self.globals.1.interconnect.sources(),
            Global::Global2 => self.globals.2.interconnect.sources(),
            Global::Global3 => self.globals.3.interconnect.sources(),
        }
    }

    pub fn global_pin(&self, global: Global) -> Option<PinSource> {
        match global {
            Global::Global0 => self.globals.0.pin,
            Global::Global1 => self.globals.1.pin,
            Global::Global2 => self.globals.2.pin,
            Global::Global3 => self.globals.3.pin,
        }
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

    pub fn jtag_interconnect(&self, jtag: JTAGInput)
        -> InterconnectSources
    {
        match jtag {
            JTAGInput::TDO =>
                self.jtag.tdo.sources(),
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

    pub fn r4_interconnect(&self, x: X, y: Y, i: R4InterconnectIndex)
        -> Option<InterconnectSources>
    {
        match self.block(x, y) {
            Some(Block::Grow(block)) =>
                block.r4_interconnect(i).map(Interconnect::sources),

            Some(Block::Left(block)) =>
                block.r4_interconnect(i).map(Interconnect::sources),

            Some(Block::Logic(block)) =>
                block.r4_interconnect(i).map(Interconnect::sources),

            Some(Block::Right(block)) =>
                block.r4_interconnect(i).map(Interconnect::sources),

            Some(Block::UFM(block)) =>
                block.r4_interconnect(i).map(Interconnect::sources),

            _ =>
                None,
        }
    }

    pub fn ufm_interconnect(&self, ufm: UFMInput)
        -> InterconnectSources
    {
        match ufm {
            UFMInput::ArClk => self.ufm.ar_clk.sources(),
            UFMInput::ArIn => self.ufm.ar_in.sources(),
            UFMInput::ArShift => self.ufm.ar_shift.sources(),
            UFMInput::DrClk => self.ufm.dr_clk.sources(),
            UFMInput::DrIn => self.ufm.dr_in.sources(),
            UFMInput::DrShift => self.ufm.dr_shift.sources(),
            UFMInput::Erase => self.ufm.erase.sources(),
            UFMInput::OscEna => self.ufm.osc_ena.sources(),
            UFMInput::Program => self.ufm.program.sources(),
        }
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

    pub fn source_count(&self) -> usize {
        self.sources.len()
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
    Column(Box<ColumnBlock>),
    Corner(Box<CornerBlock>),
    Grow(Box<GrowBlock>),
    Left(Box<LeftBlock>),
    Right(Box<RightBlock>),
    Logic(Box<LogicBlock>),
    UFM(Box<UFMBlock>),
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

//  Block
// =======

#[derive(Default)]
struct CornerBlock {
    c4_interconnects: C4ColumnInterconnects,
}

#[derive(Default)]
struct GrowBlock {
    c4_interconnects: [Interconnect<13>; 14],
    r4_interconnects: [Interconnect<13>; 8],
}

impl GrowBlock {
    fn r4_interconnect(&self, i: R4InterconnectIndex)
        -> Option<&Interconnect<13>>
    {
        let i = i.index();

        if i >= 8 {
            Some(&self.r4_interconnects[i - 8])
        } else {
            None
        }
    }
}

#[derive(Default)]
struct UFMBlock {
    c4_interconnects: [Interconnect<13>; 14],
    r4_interconnects: [Interconnect<13>; 8],
    //ufm_interconnects: [Interconnect_; 10],
}

impl UFMBlock {
    fn r4_interconnect(&self, i: R4InterconnectIndex)
        -> Option<&Interconnect<13>>
    {
        let i = i.index();

        if i >= 8 {
            Some(&self.r4_interconnects[i - 8])
        } else {
            None
        }
    }
}

#[derive(Default)]
struct C4ColumnInterconnects([C4ColumnInterconnect; 14]);

#[derive(Default)]
enum C4ColumnInterconnect {
    #[default]
    None,
    One(Interconnect<1>),
    Two(Interconnect<2>),
}

impl C4ColumnInterconnect {
    fn is_some(&self) -> bool {
        if let C4ColumnInterconnect::None = self {
            false
        } else {
            true
        }
    }

    fn sources(&self) -> Option<InterconnectSources> {
        match self {
            C4ColumnInterconnect::None =>
                None,

            C4ColumnInterconnect::One(interconnect) =>
                Some(interconnect.sources()),

            C4ColumnInterconnect::Two(interconnect) =>
                Some(interconnect.sources()),
        }
    }
}

//  IO
// ====

#[derive(Default)]
struct ColumnBlock {
    c4_interconnects: C4ColumnInterconnects,
    io_cells: [Option<IOColumnCell>; 4],
    io_interconnects: [Interconnect<12>; 10],
}

#[derive(Default)]
struct LeftBlock {
    c4_interconnects: [Interconnect<13>; 14],
    io_cells: [Option<IORowCell>; 7],
    io_interconnects: IORowInterconnects,
    r4_interconnects: [Interconnect<2>; 8],
}

impl LeftBlock {
    fn r4_interconnect(&self, i: R4InterconnectIndex)
        -> Option<&Interconnect<2>>
    {
        self.r4_interconnects.get(i.index())
    }
}

#[derive(Default)]
struct RightBlock {
    io_cells: [Option<IORowCell>; 7],
    io_interconnects: IORowInterconnects,
    r4_interconnects: [Interconnect<13>; 8],
}

impl RightBlock {
    fn r4_interconnect(&self, i: R4InterconnectIndex)
        -> Option<&Interconnect<13>>
    {
        self.r4_interconnects.get(i.index())
    }
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
    c4_interconnects: [Interconnect<13>; 14],
    logic_cells: [LogicCell; 10],
    logic_controls: [Interconnect<18>; 6],
    logic_interconnects: LogicInterconnects,
    r4_interconnects: [Interconnect<13>; 16],
}

impl LogicBlock {
    fn r4_interconnect(&self, i: R4InterconnectIndex)
        -> Option<&Interconnect<13>>
    {
        self.r4_interconnects.get(i.index())
    }
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

struct GlobalInterconnects(
    GlobalInterconnect,
    GlobalInterconnect,
    GlobalInterconnect,
    GlobalInterconnect,
);

impl GlobalInterconnects {
    fn new(density: &DensityLayout) -> Self {
        GlobalInterconnects(
            GlobalInterconnect::new(density),
            GlobalInterconnect::new(density),
            GlobalInterconnect::new(density),
            GlobalInterconnect::new(density),
        )
    }
}

struct GlobalInterconnect {
    pin: Option<PinSource>,
    interconnect: DeviceInterconnect,
}

impl GlobalInterconnect {
    fn new(density: &DensityLayout) -> Self {
        GlobalInterconnect {
            pin: None,
            interconnect: DeviceInterconnect::new(density),
        }
    }
}

struct JTAGInterconnects {
    tdo: DeviceInterconnect,
}

impl JTAGInterconnects {
    fn new(density: &DensityLayout) -> Self {
        JTAGInterconnects {
            tdo: DeviceInterconnect::new(density),
        }
    }
}

struct UFMInterconnects {
    ar_clk: DeviceInterconnect,
    ar_in: DeviceInterconnect,
    ar_shift: DeviceInterconnect,
    dr_clk: DeviceInterconnect,
    dr_in: DeviceInterconnect,
    dr_shift: DeviceInterconnect,
    erase: DeviceInterconnect,
    osc_ena: DeviceInterconnect,
    program: DeviceInterconnect,
}

impl UFMInterconnects {
    fn new(density: &DensityLayout) -> Self {
        UFMInterconnects {
            ar_clk: DeviceInterconnect::new(density),
            ar_in: DeviceInterconnect::new(density),
            ar_shift: DeviceInterconnect::new(density),
            dr_clk: DeviceInterconnect::new(density),
            dr_in: DeviceInterconnect::new(density),
            dr_shift: DeviceInterconnect::new(density),
            erase: DeviceInterconnect::new(density),
            osc_ena: DeviceInterconnect::new(density),
            program: DeviceInterconnect::new(density),
        }
    }
}

enum DeviceInterconnect {
    Small(Interconnect<18>),
    Large(Interconnect<10>),
}

impl DeviceInterconnect {
    fn new(density: &DensityLayout) -> Self {
        if density.has_grow {
            DeviceInterconnect::Large(Default::default())
        } else {
            DeviceInterconnect::Small(Default::default())
        }
    }

    fn sources(&self) -> InterconnectSources {
        match self {
            DeviceInterconnect::Small(interconnect) =>
                interconnect.sources(),

            DeviceInterconnect::Large(interconnect) =>
                interconnect.sources(),
        }
    }
}

