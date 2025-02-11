use serde::de::{
    Deserialize,
    DeserializeSeed,
    Deserializer,
    Error,
    MapAccess,
    SeqAccess,
    Visitor,
};
use std::fmt;

use crate::{
    indexed_str,
    DensityLayout,
    Fuse,
    LogicCellOutput,
    LogicInterconnectFuse,
    LogicInterconnectIndex,
    Select3,
    Select4,
    X,
    Y,
};
use super::*;

#[macro_use]
mod macros;

key! { DeviceKey = "device" }
key! { InterconnectKey = "interconnect" }
key! { TypeKey = "type" }
key! { XKey = "x" }
key! { YKey = "y" }

indexed_str! {
    enum SourcesDirect43 {
        DirectLink = "direct-link",
        Select00 = "select-0-0",
        Select01 = "select-0-1",
        Select02 = "select-0-2",
        Select10 = "select-1-0",
        Select11 = "select-1-1",
        Select12 = "select-1-2",
        Select20 = "select-2-0",
        Select21 = "select-2-1",
        Select22 = "select-2-2",
        Select30 = "select-3-0",
        Select31 = "select-3-1",
        Select32 = "select-3-2",
    }
    struct SourcesDirect43OutOfRange;
}

indexed_str! {
    enum SourcesDirect43Global {
        DirectLink = "direct-link",
        Select00 = "select-0-0",
        Select01 = "select-0-1",
        Select02 = "select-0-2",
        Select10 = "select-1-0",
        Select11 = "select-1-1",
        Select12 = "select-1-2",
        Select20 = "select-2-0",
        Select21 = "select-2-1",
        Select22 = "select-2-2",
        Select30 = "select-3-0",
        Select31 = "select-3-1",
        Select32 = "select-3-2",
        Global0 = "select-g-0",
        Global1 = "select-g-1",
        Global2 = "select-g-2",
    }
    struct SourcesDirect43GlobalOutOfRange;
}

struct DeviceVisitor;

impl<'de> Deserialize<'de> for DeviceSources {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(DeviceVisitor)
    }
}

field! {
    enum DeviceField {
        Block = "block",
    }
}

impl<'de> Visitor<'de> for DeviceVisitor {
    type Value = DeviceSources;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("device")
    }

    fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let Some(DeviceKey) = access.next_key()? else {
            return Err(de::Error::missing_field("device"));
        };
        let mut device = DeviceSources {
            device: access.next_value()?,
            block: Default::default(),
            //global: Default::default(),
            //jtag: Default::default(),
            //ufm: Default::default(),
        };
        let density = device.device.density().layout();
        while let Some(field) = access.next_key()? {
            match field {
                DeviceField::Block =>
                    access.next_value_seed(BlocksVisitor {
                        density,
                        device: &mut device,
                    })?,
            }
        }
        Ok(device)
    }
}

struct BlocksVisitor<'v> {
    density: &'v DensityLayout,
    device: &'v mut DeviceSources,
}

impl<'de, 'v> DeserializeSeed<'de> for BlocksVisitor<'v> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>
    {
        deserializer.deserialize_seq(self)
    }
}

impl<'de, 'v> Visitor<'de> for BlocksVisitor<'v> {
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("blocks")
    }

    fn visit_seq<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        while let Some(_) = access.next_element_seed(BlockVisitor {
            density: self.density,
            device: self.device,
        })? {
        }

        Ok(())
    }
}

struct BlockVisitor<'v> {
    density: &'v DensityLayout,
    device: &'v mut DeviceSources,
}

impl<'de, 'v> DeserializeSeed<'de> for BlockVisitor<'v> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>
    {
        deserializer.deserialize_map(self)
    }
}

field! {
    enum BlockType {
        Corner = "corner",
        Column = "column",
        Left = "left",
        Right = "right",
        Logic = "logic",
        UFM = "ufm",
        Grow = "grow",
    }
}

impl<'de, 'v> Visitor<'de> for BlockVisitor<'v> {
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("`x`, `y`, ...")
    }

    fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let Some(XKey) = access.next_key()? else {
            return Err(de::Error::missing_field("x"));
        };
        let x: usize = access.next_value()?;
        let Some(YKey) = access.next_key()? else {
            return Err(de::Error::missing_field("y"));
        };
        let y: usize = access.next_value()?;
        let Some(TypeKey) = access.next_key()? else {
            return Err(de::Error::missing_field("type"));
        };
        if x < usize::from(self.density.left) ||
           x > usize::from(self.density.right)
        {
            return Err(de::Error::custom(format!(
                "`x` out of range {}..{}",
                usize::from(self.density.left + 1),
                usize::from(self.density.right + 1),
            )));
        }
        if y > usize::from(self.density.top) {
            return Err(de::Error::custom(format!(
                "`y` out of range 0..{}",
                usize::from(self.density.top + 1),
            )));
        }
        let block = &mut self.device.block[x][y];
        let Block::Blank = &block else {
            return Err(de::Error::custom("block already defined"));
        };
        let x = X(x as u8);
        let y = Y(y as u8);
        match access.next_value()? {
            BlockType::Logic =>
                if self.density.logic_block(x, y) {
                    let mut logic = Box::new(LogicBlock::default());
                    logic_block(self.density, &mut logic, x, y, access)?;
                    *block = Block::Logic(logic);
                } else {
                    return Err(de::Error::custom("`x` & `y` are not `type`"));
                },

            t => {
                todo!("{x:?},{y:?} is {t:?}")
            }
        }

        Ok(())
    }
}

field! {
    enum LogicField {
        LogicInterconnect = "logic-interconnect",
    }
}

fn logic_block<'de, 'v, A>(
    density: &'v DensityLayout,
    block: &'v mut LogicBlock,
    x: X,
    y: Y,
    mut access: A,
) -> Result<(), A::Error>
where
    A: MapAccess<'de>,
{
    while let Some(field) = access.next_key()? {
        match field {
            LogicField::LogicInterconnect =>
                access.next_value_seed(LogicInterconnectsVisitor {
                    density, block, x, y,
                })?,
        }
    }

    Ok(())
}

struct LogicInterconnectsVisitor<'v> {
    density: &'v DensityLayout,
    block: &'v mut LogicBlock,
    x: X,
    y: Y,
}

impl<'de, 'v> DeserializeSeed<'de> for LogicInterconnectsVisitor<'v> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>
    {
        deserializer.deserialize_seq(self)
    }
}

impl<'de, 'v> Visitor<'de> for LogicInterconnectsVisitor<'v> {
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("logic-interconnects")
    }

    fn visit_seq<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        while let Some(_) = access.next_element_seed(LogicInterconnectVisitor {
            density: self.density,
            block: self.block,
            x: self.x,
            y: self.y,
        })? {
        }

        Ok(())
    }
}

struct LogicInterconnectVisitor<'v> {
    density: &'v DensityLayout,
    block: &'v mut LogicBlock,
    x: X,
    y: Y,
}

impl<'de, 'v> DeserializeSeed<'de> for LogicInterconnectVisitor<'v> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>
    {
        deserializer.deserialize_map(self)
    }
}

impl<'de, 'v> Visitor<'de> for LogicInterconnectVisitor<'v> {
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("`interconnect`, ...")
    }

    fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let Some(InterconnectKey) = access.next_key()? else {
            return Err(de::Error::missing_field("interconnect"));
        };
        let i: LogicInterconnectIndex = access.next_value()?;
        let interconnect = &mut self.block.logic_interconnects[i.index()];

        LogicInterconnect {
            density: self.density,
            i,
            x: self.x,
            y: self.y,
        }.visit(&mut access, interconnect)
    }
}

struct LogicInterconnect<'v> {
    density: &'v DensityLayout,
    i: LogicInterconnectIndex,
    x: X,
    y: Y,
}

impl<'v> LogicInterconnect<'v> {
    fn visit<'de, A>(
        &self,
        access: &mut A,
        interconnect: &'v mut Interconnect<LogicInterconnectSources>,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        use LogicInterconnectSources::*;

        interconnect.port = Port::LogicInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
        };
        match &mut interconnect.sources {
            Normal { sources } => self.visit_normal(access, sources)?,
            Global { sources } => self.visit_global(access, sources)?,
        }

        Ok(())
    }

    fn visit_normal<'de, A>(
        &self,
        access: &mut A,
        sources: &mut [Source],
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        use Select3::*;
        use Select4::*;
        use SourcesDirect43::*;

        while let Some(index) = access.next_key::<SourcesDirect43>()?
        {
            let source = &mut sources[index.index()];

            access.next_value_seed(SourceVisitor { source })?;

            match index {
                DirectLink => self.fuse_direct_link(source),
                Select00 => self.fuse_select(source, Select4_0, Select3_0),
                Select01 => self.fuse_select(source, Select4_0, Select3_1),
                Select02 => self.fuse_select(source, Select4_0, Select3_2),
                Select10 => self.fuse_select(source, Select4_1, Select3_0),
                Select11 => self.fuse_select(source, Select4_1, Select3_1),
                Select12 => self.fuse_select(source, Select4_1, Select3_2),
                Select20 => self.fuse_select(source, Select4_2, Select3_0),
                Select21 => self.fuse_select(source, Select4_2, Select3_1),
                Select22 => self.fuse_select(source, Select4_2, Select3_2),
                Select30 => self.fuse_select(source, Select4_3, Select3_0),
                Select31 => self.fuse_select(source, Select4_3, Select3_1),
                Select32 => self.fuse_select(source, Select4_3, Select3_2),
            }
        }

        Ok(())
    }

    fn visit_global<'de, A>(
        &self,
        access: &mut A,
        sources: &mut [Source],
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        use Select3::*;
        use Select4::*;
        use SourcesDirect43Global::*;

        while let Some(select) = access.next_key::<SourcesDirect43Global>()?
        {
            let source = &mut sources[select.index()];

            access.next_value_seed(SourceVisitor { source })?;

            match select {
                DirectLink => self.fuse_direct_link(source),
                Select00 => self.fuse_select(source, Select4_0, Select3_0),
                Select01 => self.fuse_select(source, Select4_0, Select3_1),
                Select02 => self.fuse_select(source, Select4_0, Select3_2),
                Select10 => self.fuse_select(source, Select4_1, Select3_0),
                Select11 => self.fuse_select(source, Select4_1, Select3_1),
                Select12 => self.fuse_select(source, Select4_1, Select3_2),
                Select20 => self.fuse_select(source, Select4_2, Select3_0),
                Select21 => self.fuse_select(source, Select4_2, Select3_1),
                Select22 => self.fuse_select(source, Select4_2, Select3_2),
                Select30 => self.fuse_select(source, Select4_3, Select3_0),
                Select31 => self.fuse_select(source, Select4_3, Select3_1),
                Select32 => self.fuse_select(source, Select4_3, Select3_2),
                Global0 => self.fuse_global(source, Select3_0),
                Global1 => self.fuse_global(source, Select3_1),
                Global2 => self.fuse_global(source, Select3_2),
            }
        }

        Ok(())
    }

    fn fuse_direct_link(&self, source: &mut Source) {
        source.fuse[0] = Fuse::LogicInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: LogicInterconnectFuse::DirectLink,
        }.to_index(self.density).unwrap();
    }

    fn fuse_select(
        &self,
        source: &mut Source,
        select4: Select4,
        select3: Select3,
    ) {
        source.fuse[0] = Fuse::LogicInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: LogicInterconnectFuse::Source4(select4),
        }.to_index(self.density).unwrap();
        source.fuse[1] = Fuse::LogicInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: LogicInterconnectFuse::Source3(select3),
        }.to_index(self.density).unwrap();
    }

    fn fuse_global(
        &self,
        source: &mut Source,
        select3: Select3,
    ) {
        source.fuse[0] = Fuse::LogicInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: LogicInterconnectFuse::SourceGlobal,
        }.to_index(self.density).unwrap();
        source.fuse[1] = Fuse::LogicInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: LogicInterconnectFuse::Source3(select3),
        }.to_index(self.density).unwrap();
    }
}

struct SourceVisitor<'v> {
    source: &'v mut Source,
}

impl<'de, 'v> DeserializeSeed<'de> for SourceVisitor<'v> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>
    {
        deserializer.deserialize_seq(self)
    }
}

field! {
    enum SourceField {
        C4 = "c4",
        Column = "column",
        Global = "global",
        JTAG = "jtag",
        Left = "left",
        R4 = "r4",
        Right = "right",
        Row = "row",
        UFM = "ufm",
    }
}

impl<'de, 'v> Visitor<'de> for SourceVisitor<'v> {
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("source")
    }

    fn visit_seq<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        match access.next_element()? {
            Some(SourceField::C4) => {
                let Some(x) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("x"));
                };
                let Some(y) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("y"));
                };
                let Some(i) = access.next_element()? else {
                    return Err(de::Error::missing_field("i"));
                };
                self.source.port = Port::C4Interconnect {
                    x: X(x),
                    y: Y(y),
                    i,
                };
            }

            Some(SourceField::Column) => {
                let Some(x) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("x"));
                };
                let Some(y) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("y"));
                };
                let Some(n) = access.next_element()? else {
                    return Err(de::Error::missing_field("n"));
                };
                self.source.port = Port::IOColumnCellOutput {
                    x: X(x),
                    y: Y(y),
                    n,
                };
            }

            Some(SourceField::Global) => {
                let Some(global) = access.next_element()? else {
                    return Err(de::Error::missing_field("global"));
                };
                self.source.port = Port::Global {
                    global,
                };
            }

            Some(SourceField::JTAG) => {
                let Some(output) = access.next_element()? else {
                    return Err(de::Error::missing_field("output"));
                };
                self.source.port = Port::JTAGOutput {
                    output,
                };
            }

            Some(SourceField::Left) => {
                let Some(x) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("x"));
                };
                let Some(y) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("y"));
                };
                let Some(n) = access.next_element()? else {
                    return Err(de::Error::missing_field("n"));
                };
                self.source.port = Port::LogicCellOutput {
                    x: X(x),
                    y: Y(y),
                    n,
                    output: LogicCellOutput::Left,
                };
            }

            Some(SourceField::R4) => {
                let Some(x) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("x"));
                };
                let Some(y) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("y"));
                };
                let Some(i) = access.next_element()? else {
                    return Err(de::Error::missing_field("i"));
                };
                self.source.port = Port::R4Interconnect {
                    x: X(x),
                    y: Y(y),
                    i,
                };
            }

            Some(SourceField::Right) => {
                let Some(x) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("x"));
                };
                let Some(y) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("y"));
                };
                let Some(n) = access.next_element()? else {
                    return Err(de::Error::missing_field("n"));
                };
                self.source.port = Port::LogicCellOutput {
                    x: X(x),
                    y: Y(y),
                    n,
                    output: LogicCellOutput::Right,
                };
            }

            Some(SourceField::Row) => {
                let Some(x) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("x"));
                };
                let Some(y) = access.next_element::<u8>()? else {
                    return Err(de::Error::missing_field("y"));
                };
                let Some(n) = access.next_element()? else {
                    return Err(de::Error::missing_field("n"));
                };
                self.source.port = Port::IORowCellOutput {
                    x: X(x),
                    y: Y(y),
                    n,
                };
            }

            Some(SourceField::UFM) => {
                let Some(output) = access.next_element()? else {
                    return Err(de::Error::missing_field("output"));
                };
                self.source.port = Port::UFMOutput {
                    output,
                };
            }

            None => {
                self.source.port = Port::Unknown;
            }
        }

        Ok(())
    }
}

