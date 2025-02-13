use std::collections::HashMap;
use std::path::Path;

use crate::{
    Control,
    DensityLayout,
    Device,
    Fuse,
    IOColumnCellNumber,
    IOColumnInterconnectIndex,
    IORowCellNumber,
    IORowInterconnectIndex,
    LogicBlockFuse,
    LogicBlockControlFuse,
    LogicCellFuse,
    LogicCellInput,
    LogicCellSourceFuse,
    LogicCellNumber,
    LogicCellOutput,
    LogicInterconnectIndex,
    Port,
    Select3,
    Select6,
    X,
    Y,
};

mod de;

pub struct DeviceSources {
    pub device: Device,
    blocks: [[Block; 15]; 22],
    //global: [Interconnect<18>; 4],
    //jtag: JTAG,
    pins: HashMap<String, PinSource>,
    //ufm: UFM,
}

impl DeviceSources {
    pub fn read(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        let toml = std::fs::read_to_string(path)?;
        Ok(toml::from_str(&toml)?)
    }

    fn block(&self, x: X, y: Y) -> Option<&Block> {
        self.blocks.get(x.0 as usize)
            .and_then(|col| col.get(y.0 as usize))
    }

    pub fn io_column_interconnect(
        &self, x: X, y: Y,
        i: IOColumnInterconnectIndex,
    )
        -> Option<InterconnectSources>
    {
        if let Some(Block::Column(block)) = self.block(x, y) {
            let interconnect = &block.io_interconnects[i.index()];
            Some(interconnect.sources())
        } else {
            None
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
}

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

#[derive(Debug)]
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
#[derive(Default)]
struct Interconnect<T> {
    port: Port,
    sources: T,
}

impl<const N: usize> Interconnect<[Source; N]> {
    fn sources(&self) -> InterconnectSources {
        InterconnectSources {
            port: self.port,
            sources: &self.sources,
        }
    }
}

enum InterconnectRef<'d> {
    Normal(&'d Interconnect<[Source; 13]>),
    Global(&'d Interconnect<[Source; 16]>),
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
    Normal(&'d mut Interconnect<[Source; 13]>),
    Global(&'d mut Interconnect<[Source; 16]>),
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Default)]
struct Source {
    port: Port,
    fuse: [usize; 2],
}

//  IO
// ====

#[derive(Default)]
struct ColumnBlock {
    //c4_interconnects: [Interconnect_; 14],
    io_cells: [Option<IOCell>; 4],
    io_interconnects: [Interconnect<[Source; 12]>; 10],
}

#[derive(Default)]
struct LeftBlock {
    //c4_interconnects: [Interconnect_; 14],
    io_cells: [Option<IOCell>; 7],
    io_interconnects: IOInterconnects,
    //r4_interconnects: [Interconnect_; 16],
}

#[derive(Default)]
struct RightBlock {
    io_cells: [Option<IOCell>; 7],
    io_interconnects: IOInterconnects,
    //r4_interconnects: [Interconnect_; 16],
}

#[derive(Default)]
struct IOCell {
    enable: Interconnect<[Source; 18]>,
    name: String,
    output: Interconnect<[Source; 18]>,
}

#[derive(Default)]
struct IOInterconnects(
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 16]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 16]>,
);

impl IOInterconnects {
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
    logic_controls: [Interconnect<[Source; 18]>; 6],
    logic_interconnects: LogicInterconnects,
    //r4_interconnect: [Interconnect<13>; 16],
}

#[derive(Default)]
struct LogicCell {
    input_a: Interconnect<[Source; 18]>,
    input_b: Interconnect<[Source; 18]>,
    input_c: Interconnect<[Source; 18]>,
    input_d: Interconnect<[Source; 18]>,
}

#[derive(Default)]
struct LogicInterconnects(
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 16]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 13]>,
    Interconnect<[Source; 16]>,
);

impl LogicBlock {
    fn new(x: X, y: Y, density: &DensityLayout) -> Self {
        use Control::*;

        let mut block = LogicBlock::default();

        for n in LogicCellNumber::iter() {
            let cell = &mut block.logic_cells[n.index()];
            for input in LogicCellInput::iter() {
                let interconnect = match input {
                    LogicCellInput::A => &mut cell.input_a,
                    LogicCellInput::B => &mut cell.input_b,
                    LogicCellInput::C => &mut cell.input_c,
                    LogicCellInput::D => &mut cell.input_d,
                };
                interconnect.port = Port::LogicCellInput { x, y, n, input };
                for select6 in Select6::iter() {
                    for select3 in Select3::iter() {
                        let source = &mut interconnect.sources[
                            (select6.index() * 3) + select3.index()
                        ];
                        source.port = match input {
                            LogicCellInput::A =>
                                logic_input_a(x, y, select6, select3),
                            LogicCellInput::B =>
                                logic_input_b(x, y, select6, select3),
                            LogicCellInput::C =>
                                logic_input_c(x, y, select6, select3),
                            LogicCellInput::D =>
                                logic_input_d(x, y, select6, select3),
                        };
                        source.fuse[0] = Fuse::LogicCell {
                            x, y, n,
                            fuse: LogicCellFuse::Input {
                                input,
                                fuse: LogicCellSourceFuse::Source6(select6),
                            },
                        }.to_index(density).unwrap();
                        source.fuse[1] = Fuse::LogicCell {
                            x, y, n,
                            fuse: LogicCellFuse::Input {
                                input,
                                fuse: LogicCellSourceFuse::Source3(select3),
                            },
                        }.to_index(density).unwrap();
                    }
                }
            }
        }

        for control in Control::iter() {
            let interconnect = &mut block.logic_controls[control.index()];
            interconnect.port = Port::LogicControl { x, y, control };
            for select6 in Select6::iter() {
                for select3 in Select3::iter() {
                    let source = &mut interconnect.sources[
                        (select6.index() * 3) + select3.index()
                    ];
                    match control {
                        Control0 | Control2 | Control4 =>
                            source.port = logic_input_c(x, y, select6, select3),

                        Control1 | Control3 | Control5 =>
                            source.port = logic_input_d(x, y, select6, select3),
                    }
                    source.fuse[0] = Fuse::LogicBlock {
                        x, y,
                        fuse: LogicBlockFuse::Control {
                            control,
                            fuse: LogicBlockControlFuse::Source6(select6),
                        },
                    }.to_index(density).unwrap();
                    source.fuse[1] = Fuse::LogicBlock {
                        x, y,
                        fuse: LogicBlockFuse::Control {
                            control,
                            fuse: LogicBlockControlFuse::Source3(select3),
                        },
                    }.to_index(density).unwrap();
                }
            }
        }

        block
    }
}

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

//  Logic
// =======

fn logic_input_a(
    x: X,
    y: Y,
    select6: Select6,
    select3: Select3,
) -> Port {
    use LogicCellNumber::*;
    use LogicCellOutput::*;
    use LogicInterconnectIndex::*;
    use Select3::*;
    use Select6::*;

    match (select6, select3) {
        (Select6_0, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect0 },
        (Select6_0, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect3 },
        (Select6_0, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect8 },
        (Select6_1, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect1 },
        (Select6_1, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect6 },
        (Select6_1, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect15 },
        (Select6_2, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect9 },
        (Select6_2, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect11 },
        (Select6_2, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect14 },
        (Select6_3, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect18 },
        (Select6_3, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect22 },
        (Select6_3, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect25 },
        (Select6_4, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect19 },
        (Select6_4, Select3_1) =>
            Port::LogicCellOutput { x, y, n: LogicCell3, output: Local },
        (Select6_4, Select3_2) =>
            Port::LogicCellOutput { x, y, n: LogicCell8, output: Local },
        (Select6_5, Select3_0) =>
            Port::LogicCellOutput { x, y, n: LogicCell4, output: Local },
        (Select6_5, Select3_1) =>
            Port::LogicCellOutput { x, y, n: LogicCell5, output: Local },
        (Select6_5, Select3_2) =>
            Port::LogicCellOutput { x, y, n: LogicCell6, output: Local },
    }
}

fn logic_input_b(
    x: X,
    y: Y,
    select6: Select6,
    select3: Select3,
) -> Port {
    use LogicCellNumber::*;
    use LogicCellOutput::*;
    use LogicInterconnectIndex::*;
    use Select3::*;
    use Select6::*;

    match (select6, select3) {
        (Select6_0, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect2 },
        (Select6_0, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect7 },
        (Select6_0, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect17 },
        (Select6_1, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect4 },
        (Select6_1, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect5 },
        (Select6_1, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect10 },
        (Select6_2, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect12 },
        (Select6_2, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect13 },
        (Select6_2, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect16 },
        (Select6_3, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect20 },
        (Select6_3, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect23 },
        (Select6_3, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect24 },
        (Select6_4, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect21 },
        (Select6_4, Select3_1) =>
            Port::LogicCellOutput { x, y, n: LogicCell0, output: Local },
        (Select6_4, Select3_2) =>
            Port::LogicCellOutput { x, y, n: LogicCell7, output: Local },
        (Select6_5, Select3_0) =>
            Port::LogicCellOutput { x, y, n: LogicCell1, output: Local },
        (Select6_5, Select3_1) =>
            Port::LogicCellOutput { x, y, n: LogicCell2, output: Local },
        (Select6_5, Select3_2) =>
            Port::LogicCellOutput { x, y, n: LogicCell9, output: Local },
    }
}

fn logic_input_c(
    x: X,
    y: Y,
    select6: Select6,
    select3: Select3,
) -> Port {
    use LogicCellNumber::*;
    use LogicCellOutput::*;
    use LogicInterconnectIndex::*;
    use Select3::*;
    use Select6::*;

    match (select6, select3) {
        (Select6_0, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect0 },
        (Select6_0, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect3 },
        (Select6_0, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect8 },
        (Select6_1, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect2 },
        (Select6_1, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect7 },
        (Select6_1, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect17 },
        (Select6_2, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect9 },
        (Select6_2, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect11 },
        (Select6_2, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect14 },
        (Select6_3, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect18 },
        (Select6_3, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect22 },
        (Select6_3, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect25 },
        (Select6_4, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect21 },
        (Select6_4, Select3_1) =>
            Port::LogicCellOutput { x, y, n: LogicCell0, output: Local },
        (Select6_4, Select3_2) =>
            Port::LogicCellOutput { x, y, n: LogicCell7, output: Local },
        (Select6_5, Select3_0) =>
            Port::LogicCellOutput { x, y, n: LogicCell4, output: Local },
        (Select6_5, Select3_1) =>
            Port::LogicCellOutput { x, y, n: LogicCell5, output: Local },
        (Select6_5, Select3_2) =>
            Port::LogicCellOutput { x, y, n: LogicCell6, output: Local },
    }
}

fn logic_input_d(
    x: X,
    y: Y,
    select6: Select6,
    select3: Select3,
) -> Port {
    use LogicCellNumber::*;
    use LogicCellOutput::*;
    use LogicInterconnectIndex::*;
    use Select3::*;
    use Select6::*;

    match (select6, select3) {
        (Select6_0, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect1 },
        (Select6_0, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect6 },
        (Select6_0, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect15 },
        (Select6_1, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect4 },
        (Select6_1, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect5 },
        (Select6_1, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect10 },
        (Select6_2, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect12 },
        (Select6_2, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect13 },
        (Select6_2, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect16 },
        (Select6_3, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect20 },
        (Select6_3, Select3_1) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect23 },
        (Select6_3, Select3_2) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect24 },
        (Select6_4, Select3_0) =>
            Port::LogicInterconnect { x, y, i: LogicInterconnect19 },
        (Select6_4, Select3_1) =>
            Port::LogicCellOutput { x, y, n: LogicCell3, output: Local },
        (Select6_4, Select3_2) =>
            Port::LogicCellOutput { x, y, n: LogicCell8, output: Local },
        (Select6_5, Select3_0) =>
            Port::LogicCellOutput { x, y, n: LogicCell1, output: Local },
        (Select6_5, Select3_1) =>
            Port::LogicCellOutput { x, y, n: LogicCell2, output: Local },
        (Select6_5, Select3_2) =>
            Port::LogicCellOutput { x, y, n: LogicCell9, output: Local },
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

