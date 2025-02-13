use crate::field;
use super::*;

field! {
    enum RightField {
        IOInterconnect = "io-interconnect",
    }
}

pub (super) fn right_block<'de, 'v, A>(
    density: &'v DensityLayout,
    block: &'v mut RightBlock,
    x: X,
    y: Y,
    mut access: A,
) -> Result<(), A::Error>
where
    A: MapAccess<'de>,
{
    while let Some(field) = access.next_key()? {
        match field {
            RightField::IOInterconnect =>
                access.next_value_seed(RightInterconnectsVisitor {
                    density, block, x, y,
                })?,
        }
    }

    Ok(())
}

struct RightInterconnectsVisitor<'v> {
    density: &'v DensityLayout,
    block: &'v mut RightBlock,
    x: X,
    y: Y,
}

impl<'de, 'v> DeserializeSeed<'de> for RightInterconnectsVisitor<'v> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>
    {
        deserializer.deserialize_seq(self)
    }
}

impl<'de, 'v> Visitor<'de> for RightInterconnectsVisitor<'v> {
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("io-interconnects")
    }

    fn visit_seq<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        while let Some(_) = access.next_element_seed(RightInterconnectVisitor {
            density: self.density,
            block: self.block,
            x: self.x,
            y: self.y,
        })? {
        }

        Ok(())
    }
}

struct RightInterconnectVisitor<'v> {
    density: &'v DensityLayout,
    block: &'v mut RightBlock,
    x: X,
    y: Y,
}

impl<'de, 'v> DeserializeSeed<'de> for RightInterconnectVisitor<'v> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>
    {
        deserializer.deserialize_map(self)
    }
}

impl<'de, 'v> Visitor<'de> for RightInterconnectVisitor<'v> {
    type Value = ();

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("`interconnect`, ...")
    }

    fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        access.next_key_seed(Key("interconnect"))?;
        let i: IORowInterconnectIndex = access.next_value()?;
        let visit = RowInterconnect {
            density: self.density,
            i,
            x: self.x,
            y: self.y,
        };

        match self.block.io_interconnects.get_mut(i) {
            InterconnectMut::Normal(interconnect) =>
                visit.normal(&mut access, interconnect),

            InterconnectMut::Global(interconnect) =>
                visit.global(&mut access, interconnect),
        }
    }
}

