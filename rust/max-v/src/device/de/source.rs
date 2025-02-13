use crate::field;
use super::*;

pub (super) struct SourceVisitor<'v> {
    pub (super) source: &'v mut Source,
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

