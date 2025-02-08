
#[macro_export]
macro_rules! enumerated {
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
            Some($enum::$last) => None,
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
    // entry point
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
}

