
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

        impl std::fmt::Display for $out_of_range {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str(concat!(stringify!($enum), "out of range"))
            }
        }

        impl std::error::Error for $out_of_range {
        }

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

        impl std::str::FromStr for $enum {
            type Err = $out_of_range;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                if let Ok(i) = usize::from_str(s) {
                    $enum::try_from(i)
                } else {
                    Err($out_of_range)
                }
            }
        }

        pub struct $iterator(Option<$enum>);

        impl $enum {
            pub fn iter() -> $iterator {
                $iterator(Some(enumerated!(@iterate $enum $($item,)* )))
            }

            pub fn count() -> usize {
                enumerated!(@count
                    $( $item, )* ;
                    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                    10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                    20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                    30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
                )
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
    // count
    (@count
        ;
        $count:literal, $( $_counts:literal, )*
    ) => {
        $count
    };
    (@count
        $_name:ident, $( $names:ident, )* ;
        $_count:literal, $( $counts:literal, )*
    ) => {
        enumerated!(@count
            $( $names, )* ;
            $( $counts, )*
        )
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

        impl $enum {
            pub fn as_str(&self) -> &'static str {
                match self {
                    $( Self::$item => $str, )+
                }
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

        impl std::str::FromStr for $enum {
            type Err = $out_of_range;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                enumerated_str!(@from_str
                    $enum s $out_of_range
                    $( $item = $str, )*
                )
            }
        }

        impl std::fmt::Display for $enum {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str(self.as_str())
            }
        }

        pub struct $iterator(Option<$enum>);

        impl $enum {
            pub fn iter() -> $iterator {
                $iterator(Some(enumerated_str!(@iterate $enum $($item,)* )))
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

