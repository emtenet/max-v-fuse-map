
#[macro_export]
macro_rules! enumerated {
    (
        enum $enum:ident {
            $( $item:ident ),*
        $(,)? }
        struct $out_of_range:ident ;
        struct $iterator:ident : Iterator ;
    ) => {
        #[derive(Copy, Clone)]
        #[derive(Debug)]
        #[derive(Eq, PartialEq)]
        #[derive(Hash)]
        #[derive(Ord, PartialOrd)]
        pub enum $enum {
            $( $item ),+
        }

        #[derive(Debug)]
        pub struct $out_of_range;

        impl std::convert::TryFrom<usize> for $enum {
            type Error = $out_of_range;

            fn try_from(from: usize) -> Result<Self, Self::Error> {
                enumerated!(@try_from
                    $enum from $out_of_range
                    $( $item, )* ;
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                    30, 31, 32, 33, 34, 35, 36, 37, 38, 39, ;
                )
            }
        }

        impl<'de> serde::de::Deserialize<'de> for $enum {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::de::Deserializer<'de>,
            {
                struct Index;

                impl<'de> serde::de::Visitor<'de> for Index {
                    type Value = $enum;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter)
                        -> std::fmt::Result
                    {
                        formatter.write_str(stringify!($enum))
                    }

                    fn visit_i64<E>(self, value: i64)
                        -> Result<Self::Value, E>
                    where
                        E: serde::de::Error,
                    {
                        use serde::de::Unexpected;

                        if let Ok(value) = usize::try_from(value) {
                            if let Ok(value) = $enum::try_from(value) {
                                return Ok(value);
                            }
                        }
                        Err(E::invalid_value(
                            Unexpected::Signed(value),
                            &stringify!($enum),
                        ))
                    }

                    fn visit_u64<E>(self, value: u64)
                        -> Result<Self::Value, E>
                    where
                        E: serde::de::Error,
                    {
                        use serde::de::Unexpected;

                        if let Ok(value) = usize::try_from(value) {
                            if let Ok(value) = $enum::try_from(value) {
                                return Ok(value);
                            }
                        }
                        Err(E::invalid_value(
                            Unexpected::Unsigned(value),
                            &stringify!($enum),
                        ))
                    }
                }

                deserializer.deserialize_u8(Index)
            }
        }

        pub struct $iterator(Option<$enum>);

        impl $enum {
            pub fn iter() -> $iterator {
                $iterator(Some(enumerated!(@iterate $enum $($item,)* )))
            }

            pub fn index(self) -> usize {
                enumerated!(@index
                    $enum self
                    $( $item, )* ;
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                    30, 31, 32, 33, 34, 35, 36, 37, 38, 39, ;
                )
            }
        }

        impl std::iter::Iterator for $iterator {
            type Item = $enum;

            fn next(&mut self) -> Option<Self::Item> {
                enumerated!(@iterator
                    $enum self
                    $( $item, )* ;
                )
            }
        }
    };
    // index
    (@index
        $enum:ident $self:ident
        ;
        $( $numbers:literal, )* ;
        $( $index:literal, $item:ident, )*
    ) => {
        match $self {
            $( $enum::$item => $index, )*
        }
    };
    (@index
        $enum:ident $self:ident
        $name:ident, $( $names:ident, )* ;
        $number:literal, $( $numbers:literal, )* ;
        $( $index:literal, $item:ident, )*
    ) => {
        enumerated!(@index
            $enum $self
            $( $names, )* ;
            $( $numbers, )* ;
            $( $index, $item, )* $number, $name,
        )
    };
    // iterator
    (@iterate $enum:ident $item:ident, $( $_items:ident, )* ) =>{
        $enum::$item
    };
    (@iterator
        $enum:ident $self:ident
        $last:ident, ;
        $( $prev:ident, $next:ident, )*
    ) => {
        match $self.0 {
            $(
            Some($enum::$prev) =>
                std::mem::replace(&mut $self.0, Some($enum::$next)),
            )*
            Some($enum::$last) =>
                std::mem::replace(&mut $self.0, None),
            None => None,
        }
    };
    (@iterator
        $enum:ident $self:ident
        $p:ident, $n:ident, $( $item:ident, )* ;
        $( $prev:ident, $next:ident, )*
    ) => {
        enumerated!(@iterator
            $enum $self
            $n, $( $item, )* ;
            $( $prev, $next, )* $p, $n,
        )
    };
    // try from
    (@try_from
        $enum:ident $from:ident $error:ident
        ;
        $( $numbers:literal, )* ;
        $( $index:literal, $item:ident, )*
    ) => {
        match $from {
            $( $index => { Ok($enum::$item) } )*
            _ => { Err($error) }
        }
    };
    (@try_from
        $enum:ident $from:ident $error:ident
        $name:ident, $( $names:ident, )* ;
        $number:literal, $( $numbers:literal, )* ;
        $( $index:literal, $item:ident, )*
    ) => {
        enumerated!(@try_from
            $enum $from $error
            $( $names, )* ;
            $( $numbers, )* ;
            $( $index, $item, )* $number, $name,
        )
    };
}

#[macro_export]
macro_rules! enumerated_str {
    (
        enum $enum:ident {
            $( $item:ident = $str:literal, )+
        }
        struct $out_of_range:ident ;
        struct $iterator:ident : Iterator ;
    ) => {
        #[derive(Copy, Clone)]
        #[derive(Debug)]
        #[derive(Eq, PartialEq)]
        #[derive(Hash)]
        #[derive(Ord, PartialOrd)]
        pub enum $enum {
            $( $item, )+
        }

        #[derive(Debug)]
        pub struct $out_of_range;

        impl std::str::FromStr for $enum {
            type Err = $out_of_range;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                enumerated_str!(@from_str
                    $enum s $out_of_range
                    $( $item = $str, )*
                )
            }
        }

        impl<'de> serde::de::Deserialize<'de> for $enum {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::de::Deserializer<'de>,
            {
                use std::str::FromStr;

                struct Visitor;

                impl<'de> serde::de::Visitor<'de> for Visitor {
                    type Value = $enum;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter)
                        -> std::fmt::Result
                    {
                        formatter.write_str("a device")
                    }

                    fn visit_str<E>(self, value: &str)
                        -> Result<Self::Value, E>
                    where
                        E: serde::de::Error,
                    {
                        $enum::from_str(value)
                            .map_err(|_|
                                E::invalid_value(
                                    serde::de::Unexpected::Str(value),
                                    &stringify!($enum),
                                )
                            )
                    }
                }

                deserializer.deserialize_str(Visitor)
            }
        }

        pub struct $iterator(Option<$enum>);

        impl $enum {
            pub fn iter() -> $iterator {
                $iterator(Some(enumerated_str!(@iterate $enum $($item,)* )))
            }

            pub fn index(self) -> usize {
                enumerated_str!(@index
                    $enum self
                    $( $item, )* ;
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                    30, 31, 32, 33, 34, 35, 36, 37, 38, 39, ;
                )
            }
        }

        impl std::iter::Iterator for $iterator {
            type Item = $enum;

            fn next(&mut self) -> Option<Self::Item> {
                enumerated_str!(@iterator
                    $enum self
                    $( $item, )* ;
                )
            }
        }
    };
    // from_str
    (@from_str
        $enum:ident $s:ident $error:ident
        $( $item:ident = $str:literal, )+
    ) => {
        match $s {
            $( $str => { Ok($enum::$item) } )*
            _ => { Err($error) }
        }
    };
    // index
    (@index
        $enum:ident $self:ident
        ;
        $( $numbers:literal, )* ;
        $( $index:literal, $item:ident, )*
    ) => {
        match $self {
            $( $enum::$item => $index, )*
        }
    };
    (@index
        $enum:ident $self:ident
        $name:ident, $( $names:ident, )* ;
        $number:literal, $( $numbers:literal, )* ;
        $( $index:literal, $item:ident, )*
    ) => {
        enumerated_str!(@index
            $enum $self
            $( $names, )* ;
            $( $numbers, )* ;
            $( $index, $item, )* $number, $name,
        )
    };
    // iterator
    (@iterate $enum:ident $item:ident, $( $_items:ident, )* ) =>{
        $enum::$item
    };
    (@iterator
        $enum:ident $self:ident
        $last:ident, ;
        $( $prev:ident, $next:ident, )*
    ) => {
        match $self.0 {
            $(
            Some($enum::$prev) =>
                std::mem::replace(&mut $self.0, Some($enum::$next)),
            )*
            Some($enum::$last) => None,
            None => None,
        }
    };
    (@iterator
        $enum:ident $self:ident
        $p:ident, $n:ident, $( $item:ident, )* ;
        $( $prev:ident, $next:ident, )*
    ) => {
        enumerated_str!(@iterator
            $enum $self
            $n, $( $item, )* ;
            $( $prev, $next, )* $p, $n,
        )
    };
}

#[macro_export]
macro_rules! indexed_str {
    (
        enum $enum:ident {
            $( $item:ident = $str:literal, )+
        }
        struct $out_of_range:ident ;
    ) => {
        #[derive(Copy, Clone)]
        #[derive(Debug)]
        #[derive(Eq, PartialEq)]
        #[derive(Hash)]
        #[derive(Ord, PartialOrd)]
        pub enum $enum {
            $( $item, )+
        }

        #[derive(Debug)]
        pub struct $out_of_range;

        impl std::str::FromStr for $enum {
            type Err = $out_of_range;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                indexed_str!(@from_str
                    $enum s $out_of_range
                    $( $item = $str, )*
                )
            }
        }

        impl<'de> serde::de::Deserialize<'de> for $enum {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::de::Deserializer<'de>,
            {
                use std::str::FromStr;

                struct Visitor;

                impl<'de> serde::de::Visitor<'de> for Visitor {
                    type Value = $enum;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter)
                        -> std::fmt::Result
                    {
                        formatter.write_str("a device")
                    }

                    fn visit_str<E>(self, value: &str)
                        -> Result<Self::Value, E>
                    where
                        E: serde::de::Error,
                    {
                        $enum::from_str(value)
                            .map_err(|_|
                                E::invalid_value(
                                    serde::de::Unexpected::Str(value),
                                    &stringify!($enum),
                                )
                            )
                    }
                }

                deserializer.deserialize_str(Visitor)
            }
        }

        impl $enum {
            pub fn index(self) -> usize {
                indexed_str!(@index
                    $enum self
                    $( $item, )* ;
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                    30, 31, 32, 33, 34, 35, 36, 37, 38, 39, ;
                )
            }
        }
    };
    // from_str
    (@from_str
        $enum:ident $s:ident $error:ident
        $( $item:ident = $str:literal, )+
    ) => {
        match $s {
            $( $str => { Ok($enum::$item) } )*
            _ => { Err($error) }
        }
    };
    // index
    (@index
        $enum:ident $self:ident
        ;
        $( $numbers:literal, )* ;
        $( $index:literal, $item:ident, )*
    ) => {
        match $self {
            $( $enum::$item => $index, )*
        }
    };
    (@index
        $enum:ident $self:ident
        $name:ident, $( $names:ident, )* ;
        $number:literal, $( $numbers:literal, )* ;
        $( $index:literal, $item:ident, )*
    ) => {
        indexed_str!(@index
            $enum $self
            $( $names, )* ;
            $( $numbers, )* ;
            $( $index, $item, )* $number, $name,
        )
    };
}

