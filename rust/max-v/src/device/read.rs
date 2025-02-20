use anyhow::{
    bail,
    Context,
    Result,
};
use std::collections::HashMap;
use std::path::Path;
use std::str::FromStr;

use crate::{
    C4InterconnectIndex,
    Control,
    DensityBlockType,
    DensityLayout as Density,
    Device,
    Global,
    IOCellInput,
    IOColumnCellNumber,
    IOColumnInterconnectIndex,
    IORowCellNumber,
    IORowInterconnectIndex,
    JTAGInput,
    JTAGOutput,
    LogicCellInput,
    LogicCellNumber,
    LogicCellOutput,
    LogicInterconnectIndex,
    PinName,
    Port,
    R4InterconnectIndex,
    UFMInterconnectIndex,
    UFMInput,
    UFMOutput,
    X,
    Y,
};
use super::{
    Block,
    C4ColumnInterconnect,
    C4ColumnInterconnects,
    ColumnBlock,
    CornerBlock,
    DeviceInterconnect,
    DeviceSources,
    GlobalInterconnects,
    GlobalInterconnect,
    GrowBlock,
    Interconnect,
    InterconnectMut,
    IOColumnCell,
    IORowCell,
    JTAGInterconnects,
    LeftBlock,
    LogicBlock,
    PinSource,
    RightBlock,
    Source,
    UFMBlock,
    UFMInterconnects,
};

// device interconnects
const GLOBAL_INTERCONNECTS: u8 = 0xED;
const JTAG_INTERCONNECTS: u8 = 0xEE;
const UFM_INTERCONNECTS: u8 = 0xEF;

// block types
const CORNER_BLOCK: u8 = 0xF0;
const COLUMN_BLOCK: u8 = 0xF1;
const GROW_BLOCK: u8 = 0xF2;
const LEFT_BLOCK: u8 = 0xF3;
const LOGIC_BLOCK: u8 = 0xF4;
const RIGHT_BLOCK: u8 = 0xF5;
const UFM_BLOCK: u8 = 0xF6;

// sub-block types
const C4_INTERCONNECTS: u8 = 0xF9;
const IO_CELLS: u8 = 0xFA;
const IO_INTERCONNECTS: u8 = 0xFB;
const LOGIC_CELLS: u8 = 0xFC;
const LOGIC_CONTROLS: u8 = 0xFD;
const LOGIC_INTERCONNECTS: u8 = 0xFE;
const R4_INTERCONNECTS: u8 = 0xFF;

// ports
const C4_INTERCONNECT: u8 = 0xD0;
const GLOBAL: u8 = 0xD1;
const IO_COLUMN_CELL: u8 = 0xD2;
const IO_COLUMN_INTERCONNECT: u8 = 0xD3;
const IO_ROW_CELL: u8 = 0xD4;
const IO_ROW_INTERCONNECT: u8 = 0xD5;
const JTAG_TCK: u8 = 0xD6;
const JTAG_TDI: u8 = 0xD7;
const JTAG_TMS: u8 = 0xD8;
const LOGIC_CELL_LEFT: u8 = 0xD9;
const LOGIC_CELL_LOCAL: u8 = 0xDA;
const LOGIC_CELL_RIGHT: u8 = 0xDB;
const LOGIC_INTERCONNECT: u8 = 0xDC;
const R4_INTERCONNECT: u8 = 0xDD;
const UFM_AR_OUT: u8 = 0xDE;
const UFM_BUSY: u8 = 0xDF;
const UFM_DR_OUT: u8 = 0xE0;
const UFM_INTERCONNECT: u8 = 0xE1;
const UFM_ISP_BUSY: u8 = 0xE2;
const UFM_OSC: u8 = 0xE3;
const UNKNOWN: u8 = 0xE4;

impl DeviceSources {
    pub fn read(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();

        Self::read_device(path)
            .with_context(|| format!(
                "Failed to read device from {}",
                path.display(),
            ))
    }

    fn read_device(path: &Path) -> Result<Self> {
        let bytes = std::fs::read(path)?;

        let Some(bytes) = bytes.strip_prefix(b"MAXV") else {
            bail!("Missing header 'MAXV'")
        };

        let mut bytes = Bytes { bytes };

        let device = bytes.device_type()?;
        let density = device.density().layout();
        let mut device = DeviceSources {
            device,
            blocks: Default::default(),
            globals: GlobalInterconnects::new(density),
            jtag: JTAGInterconnects::new(density),
            pins: Default::default(),
            ufm: UFMInterconnects::new(density),
        };

        device.globals.read(density, &mut bytes)?;
        device.jtag.read(density, &mut bytes)?;
        device.ufm.read(density, &mut bytes)?;

        while !bytes.is_empty() {
            device.read_block(density, &mut bytes)?;
        }

        Ok(device)
    }
}

struct Bytes<'b> {
    bytes: &'b [u8],
}

impl<'b> Bytes<'b> {
    fn byte(&mut self, name: &'static str) -> Result<u8> {
        if let Some((&byte, bytes)) = self.bytes.split_first() {
            self.bytes = bytes;
            Ok(byte)
        } else {
            bail!("Missing {name}")
        }
    }

    fn device_type(&mut self) -> Result<Device> {
        if let Some((&len, bytes)) = self.bytes.split_first() {
            let len = usize::from(len);
            if let Some((name, bytes)) = bytes.split_at_checked(len) {
                if let Ok(name) = std::str::from_utf8(name) {
                    if let Ok(device) = Device::from_str(name) {
                        self.bytes = bytes;
                        Ok(device)
                    } else {
                        bail!("Invalid device, '{name}'")
                    }
                } else {
                    bail!("Device name is not utf8")
                }
            } else {
                bail!("Device name is longer than file")
            }
        } else {
            bail!("Device name missing")
        }
    }

    fn expect(&mut self, expect: u8, name: &'static str) -> Result<()> {
        let Some((&byte, bytes)) = self.bytes.split_first() else {
            bail!("Missing {name}")
        };
        if byte != expect {
            bail!("Unexpected {name}: 0x{byte:x}")
        }
        self.bytes = bytes;
        Ok(())
    }

    fn fuse(&mut self) -> Result<usize> {
        if let Some((&[a, b, c], bytes)) = self.bytes.split_at_checked(3) {
            self.bytes = bytes;
            Ok((usize::from(a) << 8) + (usize::from(b) << 8) + usize::from(c))
        } else {
            bail!("Missing Fuse")
        }
    }

    fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    fn n<T>(&mut self, name: &'static str) -> Result<T>
    where
        T: TryFrom<usize>,
    {
        let Some((&n, bytes)) = self.bytes.split_first() else {
            bail!("Missing {name}")
        };
        let Ok(n) = T::try_from(usize::from(n)) else {
            bail!("Invalid {name}: {n}")
        };
        self.bytes = bytes;
        Ok(n)
    }

    fn pin_name(&mut self) -> Result<PinName> {
        let (pin_name, bytes) = PinName::read(self.bytes)?;
        self.bytes = bytes;
        Ok(pin_name)
    }

    fn x(&mut self, density: &Density) -> Result<X> {
        let Some((&x, bytes)) = self.bytes.split_first() else {
            bail!("Missing X coordinate")
        };
        if density.left > x || density.right < x {
            bail!("X out of range {}..{}",
                usize::from(density.left),
                usize::from(density.right + 1),
            )
        }
        self.bytes = bytes;
        Ok(X(x))
    }

    fn y(&mut self, density: &Density) -> Result<Y> {
        let Some((&y, bytes)) = self.bytes.split_first() else {
            bail!("Missing Y coordinate")
        };
        if density.top < y {
            bail!("Y out of range 0..{}",
                usize::from(density.top + 1),
            )
        }
        self.bytes = bytes;
        Ok(Y(y))
    }
}

impl DeviceSources {
    fn read_block<'b>(&mut self, density: &Density, bytes: &mut Bytes<'b>)
        -> Result<()>
    {
        let t = bytes.byte("Block type")?;
        let x = bytes.x(density)?;
        let y = bytes.y(density)?;

        let block = &mut self.blocks[usize::from(x)][usize::from(y)];
        let Block::Blank = &block else {
            bail!("Block already defined {x},{y}")
        };

        match density.block_type(x, y) {
            Some(DensityBlockType::Column) => {
                *block = Block::Column(
                    ColumnBlock::read(density, &mut self.pins, x, y, t, bytes)
                        .with_context(|| format!("in Column Block: {x},{y}"))?
                );
            }

            Some(DensityBlockType::Corner) => {
                *block = Block::Corner(
                    CornerBlock::read(density, x, y, t, bytes)
                        .with_context(|| format!("in Corner Block: {x},{y}"))?
                );
            }

            Some(DensityBlockType::Grow) => {
                *block = Block::Grow(
                    GrowBlock::read(density, x, y, t, bytes)
                        .with_context(|| format!("in Grow Block: {x},{y}"))?
                );
            }

            Some(DensityBlockType::Left) => {
                *block = Block::Left(
                    LeftBlock::read(density, &mut self.pins, x, y, t, bytes)
                        .with_context(|| format!("in Left Block: {x},{y}"))?
                );
            }

            Some(DensityBlockType::Logic) => {
                *block = Block::Logic(
                    LogicBlock::read(density, x, y, t, bytes)
                        .with_context(|| format!("in Logic Block: {x},{y}"))?
                );
            }

            Some(DensityBlockType::Right) => {
                *block = Block::Right(
                    RightBlock::read(density, &mut self.pins, x, y, t, bytes)
                        .with_context(|| format!("in Right Block: {x},{y}"))?
                );
            }

            Some(DensityBlockType::UFM) => {
                *block = Block::UFM(
                    UFMBlock::read(density, x, y, t, bytes)
                        .with_context(|| format!("in UFM Block: {x},{y}"))?
                );
            }

            None =>
                bail!("Invalid Block coordinate {x},{y}"),
        }

        Ok(())
    }
}

impl ColumnBlock {
    fn read<'b>(
        density: &Density,
        pins: &mut HashMap<PinName, PinSource>,
        x: X, y: Y,
        t: u8,
        bytes: &mut Bytes<'b>,
    ) -> Result<Box<Self>> {
        if t != COLUMN_BLOCK {
            bail!("Invalid Block type 0x{t:x}")
        }

        let mut block = Self::default();

        block.c4_interconnects.read(density, x, y, bytes)?;

        bytes.expect(IO_CELLS, "IO Cells header")?;
        let count = bytes.byte("IO Cell count")?;
        for _ in 0..count {
            block.read_cell(density, pins, x, y, bytes)?;
        }

        bytes.expect(IO_INTERCONNECTS, "IO Interconnects header")?;
        bytes.expect(
            IOColumnInterconnectIndex::count() as u8,
            "IO Interconnects count",
        )?;
        for i in IOColumnInterconnectIndex::iter() {
            let interconnect = &mut block.io_interconnects[i.index()];
            interconnect.read(
                density,
                Port::IOColumnInterconnect { x, y, i, },
                bytes,
            )?;
        }

        Ok(Box::new(block))
    }

    fn read_cell<'b>(
        &mut self,
        density: &Density,
        pins: &mut HashMap<PinName, PinSource>,
        x: X, y: Y,
        bytes: &mut Bytes<'b>,
    ) -> Result<()> {
        let n: IOColumnCellNumber = bytes.n("IO Cell number")?;
        let cell = &mut self.io_cells[n.index()];
        if cell.is_some() {
            bail!("IO Cell already defined: {}", n.index())
        }
        *cell = Some(
            IOColumnCell::read(density, pins, x, y, n, bytes)
                .with_context(|| format!("in IO Cell {}", n.index()))?,
        );
        Ok(())
    }
}

impl CornerBlock {
    fn read<'b>(
        density: &Density,
        x: X, y: Y,
        t: u8,
        bytes: &mut Bytes<'b>,
    ) -> Result<Box<Self>> {
        if t != CORNER_BLOCK {
            bail!("Invalid Block type 0x{t:x}")
        }

        let mut block = Self::default();

        block.c4_interconnects.read(density, x, y, bytes)?;

        Ok(Box::new(block))
    }
}

impl GrowBlock {
    fn read<'b>(
        density: &Density,
        x: X, y: Y,
        t: u8,
        bytes: &mut Bytes<'b>,
    ) -> Result<Box<Self>> {
        if t != GROW_BLOCK {
            bail!("Invalid Block type 0x{t:x}")
        }

        let mut block = Self::default();

        bytes.expect(C4_INTERCONNECTS, "C4 Interconnects header")?;
        bytes.expect(
            C4InterconnectIndex::count() as u8,
            "C4 Interconnects count",
        )?;
        for i in C4InterconnectIndex::iter() {
            let interconnect = &mut block.c4_interconnects[i.index()];
            interconnect.read(
                density,
                Port::C4Interconnect { x, y, i },
                bytes,
            )?;
        }

        r4_interconnects(&mut block.r4_interconnects, density, x, y, bytes)?;

        Ok(Box::new(block))
    }
}

impl LeftBlock {
    fn read<'b>(
        density: &Density,
        pins: &mut HashMap<PinName, PinSource>,
        x: X, y: Y,
        t: u8,
        bytes: &mut Bytes<'b>,
    ) -> Result<Box<Self>> {
        if t != LEFT_BLOCK {
            bail!("Invalid Block type 0x{t:x}")
        }

        let mut block = Self::default();

        bytes.expect(C4_INTERCONNECTS, "C4 Interconnects header")?;
        bytes.expect(
            C4InterconnectIndex::count() as u8,
            "C4 Interconnects count",
        )?;
        for i in C4InterconnectIndex::iter() {
            let interconnect = &mut block.c4_interconnects[i.index()];
            interconnect.read(
                density,
                Port::C4Interconnect { x, y, i },
                bytes,
            )?;
        }

        bytes.expect(IO_CELLS, "IO Cells header")?;
        let count = bytes.byte("IO Cell count")?;
        for _ in 0..count {
            block.read_cell(density, pins, x, y, bytes)?;
        }

        bytes.expect(IO_INTERCONNECTS, "IO Interconnects header")?;
        bytes.expect(
            IORowInterconnectIndex::count() as u8,
            "IO Interconnects count",
        )?;
        for i in IORowInterconnectIndex::iter() {
            match block.io_interconnects.get_mut(i) {
                InterconnectMut::Normal(interconnect) => {
                    interconnect.read(
                        density,
                        Port::IORowInterconnect { x, y, i },
                        bytes,
                    )?;
                }

                InterconnectMut::Global(interconnect) => {
                    interconnect.read(
                        density,
                        Port::IORowInterconnect { x, y, i },
                        bytes,
                    )?;
                }
            }
        }

        r4_interconnects(&mut block.r4_interconnects, density, x, y, bytes)?;

        Ok(Box::new(block))
    }

    fn read_cell<'b>(
        &mut self,
        density: &Density,
        pins: &mut HashMap<PinName, PinSource>,
        x: X, y: Y,
        bytes: &mut Bytes<'b>,
    ) -> Result<()> {
        let n: IORowCellNumber = bytes.n("IO Cell number")?;
        let cell = &mut self.io_cells[n.index()];
        if cell.is_some() {
            bail!("IO Cell already defined: {}", n.index())
        }
        *cell = Some(
            IORowCell::read(density, pins, x, y, n, bytes)
                .with_context(|| format!("in IO Cell {}", n.index()))?,
        );
        Ok(())
    }
}

impl LogicBlock {
    fn read<'b>(density: &Density, x: X, y: Y, t: u8, bytes: &mut Bytes<'b>)
        -> Result<Box<Self>>
    {
        if t != LOGIC_BLOCK {
            bail!("Invalid Block type 0x{t:x}")
        }

        let mut block = Self::default();

        bytes.expect(C4_INTERCONNECTS, "C4 Interconnects header")?;
        bytes.expect(
            C4InterconnectIndex::count() as u8,
            "C4 Interconnects count",
        )?;
        for i in C4InterconnectIndex::iter() {
            let interconnect = &mut block.c4_interconnects[i.index()];
            interconnect.read(
                density,
                Port::C4Interconnect { x, y, i },
                bytes,
            )?;
        }

        bytes.expect(LOGIC_CELLS, "Logic Cells header")?;
        bytes.expect(LogicCellNumber::count() as u8, "Logic Cell count")?;
        for n in LogicCellNumber::iter() {
            let cell = &mut block.logic_cells[n.index()];
            cell.input_a.read(density,
                Port::LogicCellInput {
                    x, y, n, input: LogicCellInput::A,
                },
                bytes,
            ).with_context(||
                format!("in Logic Cell {} Input A", n.index())
            )?;
            cell.input_b.read(
                density,
                Port::LogicCellInput {
                    x, y, n, input: LogicCellInput::B,
                },
                bytes,
            ).with_context(||
                format!("in Logic Cell {} Input B", n.index())
            )?;
            cell.input_c.read(
                density,
                Port::LogicCellInput {
                    x, y, n, input: LogicCellInput::C,
                },
                bytes,
            ).with_context(||
                format!("in Logic Cell {} Input C", n.index())
            )?;
            cell.input_d.read(
                density,
                Port::LogicCellInput {
                    x, y, n, input: LogicCellInput::D,
                },
                bytes,
            ).with_context(||
                format!("in Logic Cell {} Input D", n.index())
            )?;
        }

        bytes.expect(LOGIC_CONTROLS, "Logic Controls header")?;
        bytes.expect(
            Control::count() as u8,
            "Logic Controls count",
        )?;
        for control in Control::iter() {
            let interconnect = &mut block.logic_controls[control.index()];
            interconnect.read(
                density,
                Port::LogicControl { x, y, control },
                bytes,
            )?;
        }

        bytes.expect(LOGIC_INTERCONNECTS, "Logic Interconnects header")?;
        bytes.expect(
            LogicInterconnectIndex::count() as u8,
            "Logic Interconnects count",
        )?;
        for i in LogicInterconnectIndex::iter() {
            match block.logic_interconnects.get_mut(i) {
                InterconnectMut::Normal(interconnect) => {
                    interconnect.read(
                        density,
                        Port::LogicInterconnect { x, y, i },
                        bytes,
                    )?;
                }

                InterconnectMut::Global(interconnect) => {
                    interconnect.read(
                        density,
                        Port::LogicInterconnect { x, y, i },
                        bytes,
                    )?;
                }
            }
        }

        r4_interconnects(&mut block.r4_interconnects, density, x, y, bytes)?;

        Ok(Box::new(block))
    }
}

impl RightBlock {
    fn read<'b>(
        density: &Density,
        pins: &mut HashMap<PinName, PinSource>,
        x: X, y: Y,
        t: u8, bytes:
        &mut Bytes<'b>,
    ) -> Result<Box<Self>> {
        if t != RIGHT_BLOCK {
            bail!("Invalid Block type 0x{t:x}")
        }

        let mut block = Self::default();

        bytes.expect(IO_CELLS, "IO Cells header")?;
        let count = bytes.byte("IO Cell count")?;
        for _ in 0..count {
            block.read_cell(density, pins, x, y, bytes)?;
        }

        bytes.expect(IO_INTERCONNECTS, "IO Interconnects header")?;
        bytes.expect(
            IORowInterconnectIndex::count() as u8,
            "IO Interconnects count",
        )?;
        for i in IORowInterconnectIndex::iter() {
            match block.io_interconnects.get_mut(i) {
                InterconnectMut::Normal(interconnect) => {
                    interconnect.read(
                        density,
                        Port::IORowInterconnect { x, y, i },
                        bytes,
                    )?;
                }

                InterconnectMut::Global(interconnect) => {
                    interconnect.read(
                        density,
                        Port::IORowInterconnect { x, y, i },
                        bytes,
                    )?;
                }
            }
        }

        r4_interconnects(&mut block.r4_interconnects, density, x, y, bytes)?;

        Ok(Box::new(block))
    }

    fn read_cell<'b>(
        &mut self,
        density: &Density,
        pins: &mut HashMap<PinName, PinSource>,
        x: X, y: Y,
        bytes: &mut Bytes<'b>,
    ) -> Result<()> {
        let n: IORowCellNumber = bytes.n("IO Cell number")?;
        let cell = &mut self.io_cells[n.index()];
        if cell.is_some() {
            bail!("IO Cell already defined: {}", n.index())
        }
        *cell = Some(
            IORowCell::read(density, pins, x, y, n, bytes)
                .with_context(|| format!("in IO Cell {}", n.index()))?,
        );
        Ok(())
    }
}

impl UFMBlock {
    fn read<'b>(
        density: &Density,
        x: X, y: Y,
        t: u8,
        bytes: &mut Bytes<'b>,
    ) -> Result<Box<Self>> {
        if t != UFM_BLOCK {
            bail!("Invalid Block type 0x{t:x}")
        }

        let mut block = Self::default();

        bytes.expect(C4_INTERCONNECTS, "C4 Interconnects header")?;
        bytes.expect(
            C4InterconnectIndex::count() as u8,
            "C4 Interconnects count",
        )?;
        for i in C4InterconnectIndex::iter() {
            let interconnect = &mut block.c4_interconnects[i.index()];
            interconnect.read(
                density,
                Port::C4Interconnect { x, y, i },
                bytes,
            )?;
        }

        r4_interconnects(&mut block.r4_interconnects, density, x, y, bytes)?;

        Ok(Box::new(block))
    }
}

impl C4ColumnInterconnects {
    fn read<'b>(
        &mut self,
        density: &Density,
        x: X, y: Y,
        bytes: &mut Bytes<'b>,
    ) -> Result<()> {
        bytes.expect(C4_INTERCONNECTS, "C4 Interconnects header")?;
        let count = bytes.byte("C4 Interconnects count")?;
        for _ in 0..count {
            let n: C4InterconnectIndex = bytes.n("C4 Interconnect index")?;
            let interconnect = &mut self.0[n.index()];
            if interconnect.is_some() {
                bail!("C4 Interconnect already defined: {}", n.index())
            }
            *interconnect = C4ColumnInterconnect::read(density, x, y, n, bytes)
                .with_context(|| format!("in C4 Interconnect {}", n.index()))?;
        }
        Ok(())
    }
}

impl C4ColumnInterconnect {
    fn read<'b>(
        density: &Density,
        x: X, y: Y, i: C4InterconnectIndex,
        bytes: &mut Bytes<'b>,
    ) -> Result<Self> {
        match bytes.byte("C4 Sources count")? {
            1 => {
                let mut interconnect = Interconnect::<1>{
                    port: Port::C4Interconnect { x, y, i },
                    sources: Default::default(),
                };
                interconnect.sources[0].read(density, bytes)
                    .context("in Source 0")?;
                Ok(C4ColumnInterconnect::One(interconnect))
            }

            2 => {
                let mut interconnect = Interconnect::<2>{
                    port: Port::C4Interconnect { x, y, i },
                    sources: Default::default(),
                };
                interconnect.sources[0].read(density, bytes)
                    .context("in Source 0")?;
                interconnect.sources[1].read(density, bytes)
                    .context("in Source 1")?;
                Ok(C4ColumnInterconnect::Two(interconnect))
            }

            count =>
                bail!("Invalid C4 Sources count {count}"),
        }
    }
}

impl DeviceInterconnect {
    fn read<'b>(
        &mut self,
        density: &Density,
        port: Port,
        bytes: &mut Bytes<'b>,
    ) -> Result<()> {
        match self {
            DeviceInterconnect::Small(interconnect) =>
                interconnect.read(density, port, bytes),

            DeviceInterconnect::Large(interconnect) =>
                interconnect.read(density, port, bytes),
        }
    }
}

impl GlobalInterconnects {
    fn read<'b>(
        &mut self,
        density: &Density,
        bytes: &mut Bytes<'b>,
    ) -> Result<()> {
        bytes.expect(GLOBAL_INTERCONNECTS, "Global Interconnects header")?;
        self.0.read(density, Global::Global0, bytes)?;
        self.1.read(density, Global::Global1, bytes)?;
        self.2.read(density, Global::Global2, bytes)?;
        self.3.read(density, Global::Global3, bytes)?;
        Ok(())
    }
}

impl GlobalInterconnect {
    fn read<'b>(
        &mut self,
        density: &Density,
        global: Global,
        bytes: &mut Bytes<'b>,
    ) -> Result<()> {
        match bytes.byte("Global Pin option")? {
            0 => {
            }

            1 => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let n: IORowCellNumber = bytes.n("Global IO number")?;
                self.pin = Some(PinSource::Row {
                    x, y, n,
                });
            }

            option =>
                bail!("Invalid Global Pin option {option}"),
        }

        self.interconnect.read(density, Port::GlobalInput { global }, bytes)
    }
}

impl IOColumnCell {
    fn read<'b>(
        density: &Density,
        pins: &mut HashMap<PinName, PinSource>,
        x: X, y: Y, n: IOColumnCellNumber,
        bytes: &mut Bytes<'b>,
    ) -> Result<Self> {
        let mut cell = IOColumnCell {
            enable: Default::default(),
            pin_name: bytes.pin_name()?,
            output: Default::default(),
        };
        cell.output.read(
            density,
            Port::IOColumnCellInput {
                x, y, n,
                input: IOCellInput::Output,
            },
            bytes,
        ).context("in Output")?;
        cell.enable.read(
            density,
            Port::IOColumnCellInput {
                x, y, n,
                input: IOCellInput::Enable,
            },
            bytes,
        ).context("in Enable")?;
        pins.insert(cell.pin_name, PinSource::Column { x, y, n });
        Ok(cell)
    }
}

impl IORowCell {
    fn read<'b>(
        density: &Density,
        pins: &mut HashMap<PinName, PinSource>,
        x: X, y: Y, n: IORowCellNumber,
        bytes: &mut Bytes<'b>,
    ) -> Result<Self> {
        let mut cell = IORowCell {
            enable: Default::default(),
            pin_name: bytes.pin_name()?,
            output: Default::default(),
        };
        cell.output.read(
            density,
            Port::IORowCellInput {
                x, y, n,
                input: IOCellInput::Output,
            },
            bytes,
        ).context("in Output")?;
        cell.enable.read(
            density,
            Port::IORowCellInput {
                x, y, n,
                input: IOCellInput::Enable,
            },
            bytes,
        ).context("in Enable")?;
        pins.insert(cell.pin_name, PinSource::Row { x, y, n });
        Ok(cell)
    }
}

impl JTAGInterconnects {
    fn read<'b>(
        &mut self,
        density: &Density,
        bytes: &mut Bytes<'b>,
    ) -> Result<()> {
        use JTAGInput::*;

        bytes.expect(JTAG_INTERCONNECTS, "JTAG Interconnects header")?;
        self.tdo.read(density, Port::JTAGInput { input: TDO }, bytes)?;
        Ok(())
    }
}

fn r4_interconnects<'b, const I: usize, const N: usize>(
    interconnects: &mut [Interconnect<N>; I],
    density: &Density,
    x: X,
    y: Y,
    bytes: &mut Bytes<'b>,
) -> Result<()> {
    bytes.expect(R4_INTERCONNECTS, "R4 Interconnects header")?;
    bytes.expect(I as u8, "R4 Interconnects count")?;
    for index in 0..I {
        let i: R4InterconnectIndex = bytes.n("R4 Interconnect index")?;
        let interconnect = &mut interconnects[index];
        interconnect.read(
            density,
            Port::R4Interconnect { x, y, i },
            bytes,
        ).with_context(|| format!("in R4 Interconnect {}", i.index()))?;
    }
    Ok(())
}

impl UFMInterconnects {
    fn read<'b>(
        &mut self,
        density: &Density,
        bytes: &mut Bytes<'b>,
    ) -> Result<()> {
        use UFMInput::*;

        bytes.expect(UFM_INTERCONNECTS, "UFM Interconnects header")?;
        self.ar_clk.read(density, Port::UFMInput { input: ArClk }, bytes)?;
        self.ar_in.read(density, Port::UFMInput { input: ArIn }, bytes)?;
        self.ar_shift.read(density, Port::UFMInput { input: ArShift }, bytes)?;
        self.dr_clk.read(density, Port::UFMInput { input: DrClk }, bytes)?;
        self.dr_in.read(density, Port::UFMInput { input: DrIn }, bytes)?;
        self.dr_shift.read(density, Port::UFMInput { input: DrShift }, bytes)?;
        self.erase.read(density, Port::UFMInput { input: Erase }, bytes)?;
        self.osc_ena.read(density, Port::UFMInput { input: OscEna }, bytes)?;
        self.program.read(density, Port::UFMInput { input: Program }, bytes)?;
        Ok(())
    }
}

impl<const N: usize> Interconnect<N> {
    fn read<'b>(
        &mut self,
        density: &Density,
        port: Port,
        bytes: &mut Bytes<'b>,
    ) -> Result<()> {
        bytes.expect(N as u8, "Sources count")?;
        for i in 0..N {
            self.sources[i].read(density, bytes)
                .with_context(|| format!("in Source {i}"))?;
        }
        self.port = port;
        Ok(())
    }
}

impl Source {
    fn read<'b>(&mut self, density: &Density, bytes: &mut Bytes<'b>)
        -> Result<()>
    {
        self.port = Port::read(density, bytes)?;
        match bytes.byte("Fuse count")? {
            1 => {
                self.fuse[0] = bytes.fuse()?;
            }
            2 => {
                self.fuse[0] = bytes.fuse()?;
                self.fuse[1] = bytes.fuse()?;
            }
            3 => {
                self.fuse[0] = bytes.fuse()?;
                self.fuse[1] = bytes.fuse()?;
                self.fuse[2] = bytes.fuse()?;
            }
            n =>
                bail!("Invalid Fuse count {n}"),
        }
        Ok(())
    }
}

impl Port {
    fn read<'b>(density: &Density, bytes: &mut Bytes<'b>)
        -> Result<Self>
    {
        match bytes.byte("Port type")? {
            C4_INTERCONNECT => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let i: C4InterconnectIndex = bytes.n("C4 Interconnect #")?;
                if density.c4_block(x, y).is_some() {
                    Ok(Port::C4Interconnect {
                        x, y, i,
                    })
                } else {
                    bail!("C4 Interconnect invalid coordinate {x},{y}")
                }
            }
            GLOBAL => {
                let global: Global = bytes.n("Global")?;
                Ok(Port::Global {
                    global,
                })
            }
            IO_COLUMN_CELL => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let n: IOColumnCellNumber = bytes.n("IO Column Cell #")?;
                if density.io_column_block(x, y) {
                    Ok(Port::IOColumnCellOutput {
                        x, y, n,
                    })
                } else {
                    bail!("IO Column Interconnect invalid coordinate {x},{y}")
                }
            }
            IO_COLUMN_INTERCONNECT => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let i: IOColumnInterconnectIndex = bytes
                    .n("IO Column Interconnect #")?;
                if density.io_column_block(x, y) {
                    Ok(Port::IOColumnInterconnect {
                        x, y, i,
                    })
                } else {
                    bail!("IO Column Interconnect invalid coordinate {x},{y}")
                }
            }
            IO_ROW_CELL => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let n: IORowCellNumber = bytes.n("IO Row Cell #")?;
                if density.io_row_block(x, y) {
                    Ok(Port::IORowCellOutput {
                        x, y, n,
                    })
                } else {
                    bail!("IO Row Interconnect invalid coordinate {x},{y}")
                }
            }
            IO_ROW_INTERCONNECT => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let i: IORowInterconnectIndex = bytes
                    .n("IO Row Interconnect #")?;
                if density.io_row_block(x, y) {
                    Ok(Port::IORowInterconnect {
                        x, y, i,
                    })
                } else {
                    bail!("IO Row Interconnect invalid coordinate {x},{y}")
                }
            }
            JTAG_TCK =>
                Ok(Port::JTAGOutput { output: JTAGOutput::TCK }),
            JTAG_TDI =>
                Ok(Port::JTAGOutput { output: JTAGOutput::TDI }),
            JTAG_TMS =>
                Ok(Port::JTAGOutput { output: JTAGOutput::TMS }),
            LOGIC_CELL_LEFT => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let n: LogicCellNumber = bytes.n("Cell number")?;
                if density.logic_block(x, y) {
                    Ok(Port::LogicCellOutput {
                        x, y, n,
                        output: LogicCellOutput::Left,
                    })
                } else {
                    bail!("Logic Cell Left invalid coordinate {x},{y}")
                }
            }
            LOGIC_CELL_LOCAL => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let n: LogicCellNumber = bytes.n("Cell number")?;
                if density.logic_block(x, y) {
                    Ok(Port::LogicCellOutput {
                        x, y, n,
                        output: LogicCellOutput::Local,
                    })
                } else {
                    bail!("Logic Cell Local invalid coordinate {x},{y}")
                }
            }
            LOGIC_CELL_RIGHT => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let n: LogicCellNumber = bytes.n("Cell number")?;
                if density.logic_block(x, y) {
                    Ok(Port::LogicCellOutput {
                        x, y, n,
                        output: LogicCellOutput::Right,
                    })
                } else {
                    bail!("Logic Cell Right invalid coordinate {x},{y}")
                }
            }
            LOGIC_INTERCONNECT => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let i: LogicInterconnectIndex = bytes
                    .n("Logic Interconnect #")?;
                if density.logic_block(x, y) {
                    Ok(Port::LogicInterconnect {
                        x, y, i,
                    })
                } else {
                    bail!("Logic Interconnect invalid coordinate {x},{y}")
                }
            }
            R4_INTERCONNECT => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let i: R4InterconnectIndex = bytes.n("R4 Interconnect #")?;
                if density.r4_block(x, y).is_some() {
                    Ok(Port::R4Interconnect {
                        x, y, i,
                    })
                } else {
                    bail!("R4 Interconnect invalid coordinate {x},{y}")
                }
            }
            UFM_AR_OUT =>
                Ok(Port::UFMOutput { output: UFMOutput::ArOut }),
            UFM_BUSY =>
                Ok(Port::UFMOutput { output: UFMOutput::Busy }),
            UFM_DR_OUT =>
                Ok(Port::UFMOutput { output: UFMOutput::DrOut }),
            UFM_INTERCONNECT => {
                let x = bytes.x(density)?;
                let y = bytes.y(density)?;
                let i: UFMInterconnectIndex = bytes.n("UFM Interconnect #")?;
                if density.ufm_block(x, y) {
                    Ok(Port::UFMInterconnect {
                        x, y, i,
                    })
                } else {
                    bail!("UFM Interconnect invalid coordinate {x},{y}")
                }
            }
            UFM_ISP_BUSY =>
                Ok(Port::UFMOutput { output: UFMOutput::IspBusy }),
            UFM_OSC =>
                Ok(Port::UFMOutput { output: UFMOutput::Osc }),
            UNKNOWN =>
                Ok(Port::Unknown),
            t =>
                bail!("Unknown Port type 0x{t:x}"),
        }
    }
}

// Device := <<"MAXV", NameSize, Name/binary>> | Blocks
// Block := <<Type, X, Y>> | Cells | Controls | Interconnects
// IO Cells := <<Type, Count>> | Count * IO Cell
// IO Cell := <<N, Len, Name:Len>> <<#Output>> Sources <<#Enable>> Sources
// Logic Cells := <<Type>> | 10 * Logic Cell
// Logic Cell := <<Number>> | InputA | InputB | InputC | InputD
// Inout? := 18 * Source
// Logic Controls := <<Type>> | 6 * Logic Controls
// Logic Control := 18 * Source
// Interconnects := <<Type, Count>> | Count * Interconnect
// Interconnect := <<Count>> | Count * Source
// Source := <<Port, 1, Fuse:24>>
//        or <<Port, 2, Fuse:24, Fuse:24>>
//        or <<Port, 3, Fuse:24, Fuse:24, Fuse:24>>
// Port := <<Type>> or <<Type, X, Y, I>>


