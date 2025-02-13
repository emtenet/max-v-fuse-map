use crate::field;
use super::*;

field! {
    enum LogicField {
        LogicInterconnect = "logic-interconnect",
    }
}

pub (super) fn logic_block<'de, 'v, A>(
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
        access.next_key_seed(Key("interconnect"))?;
        let i: LogicInterconnectIndex = access.next_value()?;
        let visit = LogicInterconnect {
            density: self.density,
            i,
            x: self.x,
            y: self.y,
        };

        match self.block.logic_interconnects.get_mut(i) {
            InterconnectMut::Normal(interconnect) =>
                visit.normal(&mut access, interconnect),

            InterconnectMut::Global(interconnect) =>
                visit.global(&mut access, interconnect),
        }
    }
}

struct LogicInterconnect<'v> {
    density: &'v DensityLayout,
    i: LogicInterconnectIndex,
    x: X,
    y: Y,
}

impl<'v> LogicInterconnect<'v> {
    fn normal<'de, A>(
        &self,
        a: &mut A,
        i: &mut Interconnect<[Source; 13]>,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        use Select3::*;
        use Select4::*;

        i.port = Port::LogicInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
        };

        self.direct_link(a, &mut i.sources[0])?;
        self.select(a, &mut i.sources[ 1], "select-0-0", Select4_0, Select3_0)?;
        self.select(a, &mut i.sources[ 2], "select-0-1", Select4_0, Select3_1)?;
        self.select(a, &mut i.sources[ 3], "select-0-2", Select4_0, Select3_2)?;
        self.select(a, &mut i.sources[ 4], "select-1-0", Select4_1, Select3_0)?;
        self.select(a, &mut i.sources[ 5], "select-1-1", Select4_1, Select3_1)?;
        self.select(a, &mut i.sources[ 6], "select-1-2", Select4_1, Select3_2)?;
        self.select(a, &mut i.sources[ 7], "select-2-0", Select4_2, Select3_0)?;
        self.select(a, &mut i.sources[ 8], "select-2-1", Select4_2, Select3_1)?;
        self.select(a, &mut i.sources[ 9], "select-2-2", Select4_2, Select3_2)?;
        self.select(a, &mut i.sources[10], "select-3-0", Select4_3, Select3_0)?;
        self.select(a, &mut i.sources[11], "select-3-1", Select4_3, Select3_1)?;
        self.select(a, &mut i.sources[12], "select-3-2", Select4_3, Select3_2)?;

        Ok(())
    }

    fn global<'de, A>(
        &self,
        a: &mut A,
        i: &'v mut Interconnect<[Source; 16]>,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        use Select3::*;
        use Select4::*;

        i.port = Port::LogicInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
        };

        self.direct_link(a, &mut i.sources[0])?;
        self.select(a, &mut i.sources[ 1], "select-0-0", Select4_0, Select3_0)?;
        self.select(a, &mut i.sources[ 2], "select-0-1", Select4_0, Select3_1)?;
        self.select(a, &mut i.sources[ 3], "select-0-2", Select4_0, Select3_2)?;
        self.select(a, &mut i.sources[ 4], "select-1-0", Select4_1, Select3_0)?;
        self.select(a, &mut i.sources[ 5], "select-1-1", Select4_1, Select3_1)?;
        self.select(a, &mut i.sources[ 6], "select-1-2", Select4_1, Select3_2)?;
        self.select(a, &mut i.sources[ 7], "select-2-0", Select4_2, Select3_0)?;
        self.select(a, &mut i.sources[ 8], "select-2-1", Select4_2, Select3_1)?;
        self.select(a, &mut i.sources[ 9], "select-2-2", Select4_2, Select3_2)?;
        self.select(a, &mut i.sources[10], "select-3-0", Select4_3, Select3_0)?;
        self.select(a, &mut i.sources[11], "select-3-1", Select4_3, Select3_1)?;
        self.select(a, &mut i.sources[12], "select-3-2", Select4_3, Select3_2)?;
        self.select_global(a, &mut i.sources[13], "select-g-0", Select3_0)?;
        self.select_global(a, &mut i.sources[14], "select-g-1", Select3_1)?;
        self.select_global(a, &mut i.sources[15], "select-g-2", Select3_2)?;

        Ok(())
    }

    fn direct_link<'de, A>(
        &self,
        access: &mut A,
        source: &mut Source,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        source.fuse[0] = Fuse::LogicInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: LogicInterconnectFuse::DirectLink,
        }.to_index(self.density).unwrap();

        access.next_key_seed(Key("direct-link"))?;
        access.next_value_seed(SourceVisitor { source })
    }

    fn select<'de, A>(
        &self,
        access: &mut A,
        source: &mut Source,
        key: &'static str,
        select4: Select4,
        select3: Select3,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
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

        access.next_key_seed(Key(key))?;
        access.next_value_seed(SourceVisitor { source })
    }

    fn select_global<'de, A>(
        &self,
        access: &mut A,
        source: &mut Source,
        key: &'static str,
        select3: Select3,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
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

        access.next_key_seed(Key(key))?;
        access.next_value_seed(SourceVisitor { source })
    }
}

