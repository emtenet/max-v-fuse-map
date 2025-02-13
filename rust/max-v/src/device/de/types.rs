
use crate::{
    DensityLayout,
    X,
    Y,
};

pub (super) struct BlockType(pub &'static str);

impl<'de> serde::de::Visitor<'de> for BlockType {
    type Value = ();

    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "`{}`", self.0)
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if value == self.0 {
            Ok(())
        } else {
            Err(E::invalid_value(serde::de::Unexpected::Str(value), &self.0))
        }
    }
}

impl<'de> serde::de::DeserializeSeed<'de> for BlockType {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_str(self)
    }
}

pub (super) struct Key(pub &'static str);

impl<'de> serde::de::Visitor<'de> for Key {
    type Value = ();

    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "`{}`", self.0)
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if value == self.0 {
            Ok(())
        } else {
            Err(E::missing_field(self.0))
        }
    }
}

impl<'de> serde::de::DeserializeSeed<'de> for Key {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_str(self)
    }
}

pub (super) struct DeviceX<'d>(pub &'d DensityLayout);

impl<'d, 'de> serde::de::Visitor<'de> for DeviceX<'d> {
    type Value = X;

    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("x coordinate")
    }

    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if value < i64::from(self.0.left) ||
           value > i64::from(self.0.right)
        {
            return Err(serde::de::Error::custom(format!(
                "`x` out of range {}..{}",
                usize::from(self.0.left + 1),
                usize::from(self.0.right + 1),
            )));
        }
        Ok(X(value as u8))
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if value < u64::from(self.0.left) ||
           value > u64::from(self.0.right)
        {
            return Err(serde::de::Error::custom(format!(
                "`x` out of range {}..{}",
                usize::from(self.0.left + 1),
                usize::from(self.0.right + 1),
            )));
        }
        Ok(X(value as u8))
    }
}

impl<'d, 'de> serde::de::DeserializeSeed<'de> for DeviceX<'d> {
    type Value = X;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_u8(self)
    }
}

pub (super) struct DeviceY<'d>(pub &'d DensityLayout);

impl<'d, 'de> serde::de::Visitor<'de> for DeviceY<'d> {
    type Value = Y;

    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("y coordinate")
    }

    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if value > i64::from(self.0.top) {
            return Err(serde::de::Error::custom(format!(
                "`y` out of range 0..{}",
                usize::from(self.0.top + 1),
            )));
        }
        Ok(Y(value as u8))
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if value > u64::from(self.0.top) {
            return Err(serde::de::Error::custom(format!(
                "`y` out of range 0..{}",
                usize::from(self.0.top + 1),
            )));
        }
        Ok(Y(value as u8))
    }
}

impl<'d, 'de> serde::de::DeserializeSeed<'de> for DeviceY<'d> {
    type Value = Y;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_u8(self)
    }
}

