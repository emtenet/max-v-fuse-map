use std::path::Path;

use crate::{
    Control,
    DensityLayout,
    Device,
    Fuse,
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
    block: [[Block; 15]; 22],
    //global: [Interconnect<18>; 4],
    //jtag: JTAG,
    //ufm: UFM,
}

impl DeviceSources {
    pub fn read(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        let toml = std::fs::read_to_string(path)?;
        Ok(toml::from_str(&toml)?)
    }

    fn block(&self, x: X, y: Y) -> Option<&Block> {
        self.block.get(x.0 as usize)
            .and_then(|col| col.get(y.0 as usize))
    }

    pub fn logic_control(&self, x: X, y: Y, i: Control)
        -> Option<InterconnectSources>
    {
        if let Some(Block::Logic(block)) = self.block(x, y) {
            let interconnect =  &block.logic_control[i.index()];
            Some(interconnect.sources())
        } else {
            None
        }
    }

    pub fn logic_cell(
        &self, x: X, y: Y,
        n: LogicCellNumber,
        input: LogicCellInput,
    ) -> Option<InterconnectSources> {
        if let Some(Block::Logic(block)) = self.block(x, y) {
            let cell = &block.logic_cell[n.index()];
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

    pub fn logic_interconnect(&self, x: X, y: Y, i: LogicInterconnectIndex)
        -> Option<InterconnectSources>
    {
        if let Some(Block::Logic(block)) = self.block(x, y) {
            let interconnect =  &block.logic_interconnects[i.index()];
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

//  Block
// =======

#[derive(Default)]
enum Block {
    #[default]
    Blank,
    //Corner {
    //    c4_interconnect: [Interconnect_; 14],
    //},
    //Column {
    //    c4_interconnect: [Interconnect_; 14],
    //    io_cell: [IOCell; 4],
    //    io_interconnect: [Interconnect_; 10],
    //},
    //Left {
    //    c4_interconnect: [Interconnect_; 14],
    //    io_cell: [IOCell; 7],
    //    io_interconnect: [Interconnect_; 18],
    //    r4_interconnect: [Interconnect_; 16],
    //},
    //Right {
    //    io_cell: [IOCell; 7],
    //    io_interconnect: [Interconnect_; 18],
    //    r4_interconnect: [Interconnect_; 16],
    //},
    Logic(Box<LogicBlock>),
    //UFM {
    //    c4_interconnect: [Interconnect_; 14],
    //    r4_interconnect: [Interconnect_; 16],
    //    ufm_interconnect: [Interconnect_; 10],
    //},
    //Grow {
    //    c4_interconnect: [Interconnect_; 14],
    //    r4_interconnect: [Interconnect_; 16],
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

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Default)]
struct Source {
    port: Port,
    fuse: [usize; 2],
}

//  Logic
// =======

struct LogicBlock {
    //c4_interconnect: [Interconnect<13>; 14],
    logic_cell: [LogicCell; 10],
    logic_control: [Interconnect<[Source; 18]>; 6],
    logic_interconnects:
        [Interconnect<LogicInterconnectSources>; 26],
    //r4_interconnect: [Interconnect<13>; 16],
}

#[derive(Default)]
struct LogicCell {
    input_a: Interconnect<[Source; 18]>,
    input_b: Interconnect<[Source; 18]>,
    input_c: Interconnect<[Source; 18]>,
    input_d: Interconnect<[Source; 18]>,
}

impl LogicBlock {
    fn new(x: X, y: Y, density: &DensityLayout) -> Self {
        use Control::*;

        let mut block = LogicBlock {
            logic_cell: Default::default(),
            logic_control: Default::default(),
            logic_interconnects: [
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_global(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_normal(),
                Interconnect::logic_interconnect_global(),
            ],
        };

        for n in LogicCellNumber::iter() {
            let cell = &mut block.logic_cell[n.index()];
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
            let interconnect = &mut block.logic_control[control.index()];
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

#[derive(Debug)]
enum LogicInterconnectSources {
    Normal {
        sources: [Source; 13],
    },
    Global {
        sources: [Source; 16],
    },
}

impl Interconnect<LogicInterconnectSources> {
    fn logic_interconnect_normal() -> Self {
        Interconnect {
            port: Port::Unknown,
            sources: LogicInterconnectSources::Normal {
                sources: Default::default(),
            },
        }
    }

    fn logic_interconnect_global() -> Self {
        Interconnect {
            port: Port::Unknown,
            sources: LogicInterconnectSources::Global {
                sources: Default::default(),
            },
        }
    }

    fn sources(&self) -> InterconnectSources {
        match &self.sources {
            LogicInterconnectSources::Normal { sources } =>
                InterconnectSources {
                    port: self.port,
                    sources: sources,
                },

            LogicInterconnectSources::Global { sources } =>
                InterconnectSources {
                    port: self.port,
                    sources: sources,
                },
        }
    }
}

//#[derive(Default)]
//struct IOCell {
//    enable: Interconnect<18>,
//    output: Interconnect<18>,
//}

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

