use crate::field;
use super::*;

field! {
    enum ColumnField {
        IOCell = "io-cell",
        IOInterconnect = "io-interconnect",
    }
}

pub (super) fn column_block<'de, 'v, A>(
    density: &'v DensityLayout,
    block: &'v mut ColumnBlock,
    x: X,
    y: Y,
    mut access: A,
) -> Result<(), A::Error>
where
    A: MapAccess<'de>,
{
    while let Some(field) = access.next_key()? {
        match field {
            ColumnField::IOCell =>
                access.next_value_seed(IOCellsVisitor {
                    density, cells: &mut block.io_cells, x, y,
                })?,

            ColumnField::IOInterconnect =>
                access.next_value_seed(ColumnInterconnectsVisitor {
                    density, block, x, y,
                })?,
        }
    }

    Ok(())
}

struct ColumnInterconnectsVisitor<'v> {
    density: &'v DensityLayout,
    block: &'v mut ColumnBlock,
    x: X,
    y: Y,
}

impl<'de, 'v> DeserializeSeed<'de> for ColumnInterconnectsVisitor<'v> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>
    {
        deserializer.deserialize_seq(self)
    }
}

impl<'de, 'v> Visitor<'de> for ColumnInterconnectsVisitor<'v> {
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("io-interconnects")
    }

    fn visit_seq<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        while let Some(_) = access.next_element_seed(ColumnInterconnectVisitor {
            density: self.density,
            block: self.block,
            x: self.x,
            y: self.y,
        })? {
        }

        Ok(())
    }
}

struct ColumnInterconnectVisitor<'v> {
    density: &'v DensityLayout,
    block: &'v mut ColumnBlock,
    x: X,
    y: Y,
}

impl<'de, 'v> DeserializeSeed<'de> for ColumnInterconnectVisitor<'v> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>
    {
        deserializer.deserialize_map(self)
    }
}

impl<'de, 'v> Visitor<'de> for ColumnInterconnectVisitor<'v> {
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("`interconnect`, ...")
    }

    fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        access.next_key_seed(Key("interconnect"))?;
        let i: IOColumnInterconnectIndex = access.next_value()?;

        let visit = ColumnInterconnect {
            density: self.density,
            x: self.x,
            y: self.y,
            i,
        };

        let interconnect = &mut self.block.io_interconnects[i.index()];
        visit.normal(&mut access, interconnect)
    }
}

pub (super) struct ColumnInterconnect<'v> {
    pub (super) density: &'v DensityLayout,
    pub (super) x: X,
    pub (super) y: Y,
    pub (super) i: IOColumnInterconnectIndex,
}

impl<'v> ColumnInterconnect<'v> {
    fn normal<'de, A>(
        &self,
        a: &mut A,
        i: &'v mut Interconnect<[Source; 12]>,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        use Select3::*;
        use Select4::*;

        i.port = Port::IOColumnInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
        };

        self.select(a, &mut i.sources[ 0], "select-0-0", Select4_0, Select3_0)?;
        self.select(a, &mut i.sources[ 1], "select-0-1", Select4_0, Select3_1)?;
        self.select(a, &mut i.sources[ 2], "select-0-2", Select4_0, Select3_2)?;
        self.select(a, &mut i.sources[ 3], "select-1-0", Select4_1, Select3_0)?;
        self.select(a, &mut i.sources[ 4], "select-1-1", Select4_1, Select3_1)?;
        self.select(a, &mut i.sources[ 5], "select-1-2", Select4_1, Select3_2)?;
        self.select(a, &mut i.sources[ 6], "select-2-0", Select4_2, Select3_0)?;
        self.select(a, &mut i.sources[ 7], "select-2-1", Select4_2, Select3_1)?;
        self.select(a, &mut i.sources[ 8], "select-2-2", Select4_2, Select3_2)?;
        self.select(a, &mut i.sources[ 9], "select-3-0", Select4_3, Select3_0)?;
        self.select(a, &mut i.sources[10], "select-3-1", Select4_3, Select3_1)?;
        self.select(a, &mut i.sources[11], "select-3-2", Select4_3, Select3_2)?;

        Ok(())
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
        source.fuse[0] = Fuse::IOColumnInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: IOInterconnectFuse::Source4(select4),
        }.to_index(self.density).unwrap();
        source.fuse[1] = Fuse::IOColumnInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: IOInterconnectFuse::Source3(select3),
        }.to_index(self.density).unwrap();

        access.next_key_seed(Key(key))?;
        access.next_value_seed(SourceVisitor { source })
    }
}

pub (super) struct IOCellsVisitor<'v> {
    pub (super) density: &'v DensityLayout,
    pub (super) cells: &'v mut [Option<IOCell>],
    pub (super) x: X,
    pub (super) y: Y,
}

impl<'de, 'v> DeserializeSeed<'de> for IOCellsVisitor<'v> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>
    {
        Ok(())
    }
}

