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
    field,
    DensityBlockType,
    DensityLayout,
    Fuse,
    IOColumnInterconnectIndex,
    IOInterconnectFuse,
    IORowInterconnectIndex,
    LogicCellOutput,
    LogicInterconnectFuse,
    LogicInterconnectIndex,
    Select3,
    Select4,
    X,
    Y,
};
use super::*;

mod column;
mod left;
mod logic;
mod right;
mod row;
mod source;
#[macro_use]
mod types;

use column::*;
use left::*;
use logic::*;
use right::*;
use row::*;
use source::*;
use types::*;

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
        access.next_key_seed(Key("device"))?;
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

impl<'de, 'v> Visitor<'de> for BlockVisitor<'v> {
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("`x`, `y`, ...")
    }

    fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        access.next_key_seed(Key("x"))?;
        let x = access.next_value_seed(DeviceX(self.density))?;

        access.next_key_seed(Key("y"))?;
        let y = access.next_value_seed(DeviceY(self.density))?;

        let block = &mut self.device.block[usize::from(x)][usize::from(y)];
        let Block::Blank = &block else {
            return Err(de::Error::custom("block already defined"));
        };

        access.next_key_seed(Key("type"))?;
        match self.density.block_type(x, y) {
            Some(DensityBlockType::Column) => {
                access.next_value_seed(BlockType("column"))?;

                let mut inner = Box::new(
                    ColumnBlock::default(),
                );
                column_block(self.density, &mut inner, x, y, access)?;
                *block = Block::Column(inner);
            }

            Some(DensityBlockType::Left) => {
                access.next_value_seed(BlockType("left"))?;

                let mut inner = Box::new(
                    LeftBlock::default(),
                );
                left_block(self.density, &mut inner, x, y, access)?;
                *block = Block::Left(inner);
            }

            Some(DensityBlockType::Logic) => {
                access.next_value_seed(BlockType("logic"))?;

                let mut inner = Box::new(
                    LogicBlock::new(x, y, self.density),
                );
                logic_block(self.density, &mut inner, x, y, access)?;
                *block = Block::Logic(inner);
            }

            Some(DensityBlockType::Right) => {
                access.next_value_seed(BlockType("right"))?;

                let mut inner = Box::new(
                    RightBlock::default(),
                );
                right_block(self.density, &mut inner, x, y, access)?;
                *block = Block::Right(inner);
            }

            Some(t) =>
                todo!("{x:?},{y:?} is {t:?}"),

            None =>
                return Err(de::Error::custom("invalid block coordinate")),
        }

        Ok(())
    }
}

