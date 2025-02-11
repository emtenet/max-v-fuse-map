
macro_rules! field {
    (enum $enum:ident { $( $item:ident = $str:literal, )+ }) => {
        #[derive(Debug)]
        enum $enum {
            $( $item, )*
        }

        impl<'de> serde::de::Deserialize<'de> for $enum {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::de::Deserializer<'de>,
            {
                struct Fields;
                static FIELDS: &'static [&'static str] = &[
                    $( $str, )+
                ];

                impl<'de> serde::de::Visitor<'de> for Fields {
                    type Value = $enum;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter)
                        -> std::fmt::Result
                    {
                        formatter.write_str("fields")
                    }

                    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        field!(@match $enum value {
                            $( $item = $str, )*
                        } FIELDS)
                    }
                }

                deserializer.deserialize_str(Fields)
            }
        }
    };
    (@match $enum:ident $value:ident {
            $( $item:ident = $str:literal, )+
        } $FIELDS:ident
    ) => {
        match $value {
            $( $str => Ok($enum::$item), )*
            _ => Err(de::Error::unknown_field($value, $FIELDS)),
        }
    }
}

macro_rules! key {
    ($struct:ident = $str:literal) => {
        struct $struct;

        impl<'de> serde::de::Visitor<'de> for $struct {
            type Value = $struct;

            fn expecting(&self, formatter: &mut std::fmt::Formatter)
                -> std::fmt::Result
            {
                formatter.write_str(concat!("`", $str, "`"))
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match value {
                    $str => Ok($struct),
                    _ => Err(de::Error::unknown_field(value, &[$str])),
                }
            }
        }

        impl<'de> serde::de::Deserialize<'de> for $struct {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::de::Deserializer<'de>,
            {
                deserializer.deserialize_str($struct)
            }
        }
    };
}

