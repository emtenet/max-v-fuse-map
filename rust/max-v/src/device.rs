use std::path::Path;

use crate::{
    Device,
    LogicInterconnectIndex,
    Port,
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

    pub fn logic_interconnect(
        &self,
        x: X,
        y: Y,
        i: LogicInterconnectIndex,
    ) -> Option<InterconnectSources> {
        match self.block(x, y) {
            Some(Block::Logic(block)) => {
                let interconnect =  &block.logic_interconnects[i.index()];
                match &interconnect.sources {
                    LogicInterconnectSources::Normal { sources } =>
                        Some(InterconnectSources {
                            port: interconnect.port,
                            sources: sources,
                        }),

                    LogicInterconnectSources::Global { sources } =>
                        Some(InterconnectSources {
                            port: interconnect.port,
                            sources: sources,
                        }),
                }
            }

            _ =>
                None,
        }
    }
}

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

struct LogicBlock {
    //c4_interconnect: [Interconnect<13>; 14],
    //logic_cell: [LogicBlockCell; 10],
    //logic_control: [Interconnect<18>; 6],
    logic_interconnects:
        [Interconnect<LogicInterconnectSources>; 26],
    //r4_interconnect: [Interconnect<13>; 16],
}

impl Default for LogicBlock {
    fn default() -> Self {
        LogicBlock {
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
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Default)]
struct Interconnect<T> {
    port: Port,
    sources: T,
}

pub struct InterconnectSources<'d> {
    port: Port,
    sources: &'d [Source],
}

impl<'d> InterconnectSources<'d> {
    pub fn port(&self) -> Port {
        self.port
    }

    pub fn iter(&self) -> InterconnectSourcesIter<'d> {
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
#[derive(Default)]
struct Source {
    port: Port,
    fuse: [usize; 2],
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
}

//#[derive(Default)]
//struct IOCell {
//    enable: Interconnect<18>,
//    output: Interconnect<18>,
//}

//#[derive(Default)]
//struct LogicBlockCell {
//    input_a: Interconnect<18>,
//    input_b: Interconnect<18>,
//    input_c: Interconnect<18>,
//    input_d: Interconnect<18>,
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

