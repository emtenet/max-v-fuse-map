use crate::{
    IOColumnCellNumber,
    IORowCellNumber,
    X,
    Y,
};

pub struct DensityLayout {
    pub (crate) has_grow: bool,
    // x
    pub (crate) grow: X,
    pub (crate) left: X,
    pub (crate) right: X,
    // y
    pub (crate) top: Y,
    pub (crate) short_bottom: Y,
    // io
    bottom_io: &'static [ColumnStrip<DensityIOStrip>],
    left_io: &'static [RowStrip<DensityIOStrip>],
    right_io: &'static [RowStrip<DensityIOStrip>],
    shelf_io: &'static [ColumnStrip<DensityIOStrip>],
    top_io: &'static [ColumnStrip<DensityIOStrip>],
    // sector
    pub (crate) global_row: u8,
    pub (crate) short_rows: u8,
    pub (crate) short_sector: usize,
    pub (crate) long_rows: u8,
    pub (crate) long_sector: usize,
    // sync
    pub (crate) sync_width: usize,
}

#[derive(Debug)]
pub enum DensityBlockType {
    Corner,
    Column,
    Left,
    Right,
    Logic,
    UFM,
    Grow,
}

macro_rules! bottom_io {
    (@cell $base:literal + no) => {
        DensityIOStrip::No {
            special_enable: false,
            special_output: false,
        }
    };
    (@cell $base:literal + { $strip:literal }) => {
        DensityIOStrip::Forward($strip)
    };
    (@cell $base:literal + $index:literal) => {
        DensityIOStrip::Reverse($base + (6 * $index))
    };
    ( $base:literal : $( $cell:tt ),* ) => {
        ( $( bottom_io!(@cell $base + $cell) ),* )
    };
}

macro_rules! left_io {
    (@cell $base:literal + no) => {
        DensityIOStrip::No {
            special_enable: false,
            special_output: false,
        }
    };
    (@cell $base:literal + e) => {
        DensityIOStrip::No {
            special_enable: true,
            special_output: false,
        }
    };
    (@cell $base:literal + eo) => {
        DensityIOStrip::No {
            special_enable: true,
            special_output: true,
        }
    };
    (@cell $base:literal + o) => {
        DensityIOStrip::No {
            special_enable: false,
            special_output: true,
        }
    };
    (@cell $base:literal + $index:literal) => {
        DensityIOStrip::Forward($base + (6 * $index))
    };
    ( $base:literal : $( $cell:tt ),* ) => {
        ( $( left_io!(@cell $base + $cell) ),* )
    };
}

macro_rules! right_io {
    (@cell $base:literal + no) => {
        DensityIOStrip::No {
            special_enable: false,
            special_output: false,
        }
    };
    (@cell $base:literal + $index:literal) => {
        DensityIOStrip::Reverse($base + (6 * $index))
    };
    ( $base:literal : $( $cell:tt ),* ) => {
        ( $( right_io!(@cell $base + $cell) ),* )
    };
}

macro_rules! right_pci_io {
    (@cell $base:literal + no) => {
        DensityIOStrip::No {
            special_enable: false,
            special_output: false,
        }
    };
    (@cell $base:literal + $index:literal) => {
        DensityIOStrip::PCICompliance($base + (7 * $index))
    };
    ( $base:literal : $( $cell:tt ),* ) => {
        ( $( right_pci_io!(@cell $base + $cell) ),* )
    };
}

macro_rules! top_io {
    (@cell $base:literal + no) => {
        DensityIOStrip::No {
            special_enable: false,
            special_output: false,
        }
    };
    (@cell $base:literal + $index:literal) => {
        DensityIOStrip::Forward($base + (6 * $index))
    };
    ( $base:literal : $( $cell:tt ),* ) => {
        ( $( top_io!(@cell $base + $cell) ),* )
    };
}

pub const MAX_V_240Z: DensityLayout = DensityLayout {
    has_grow: false,
    // x
    grow: X(1),
    left: X(1),
    right: X(8),
    // y
    top: Y(5),
    short_bottom: Y(0),
    // io
    bottom_io: &[
        bottom_io!(1428:  0,  1,  2, no),
        bottom_io!(1428:  3,  4,  5,  6),
        bottom_io!(1428:  7,  8,  9, 10),
        bottom_io!(1428: 11, 12, 13, no),
        bottom_io!(1428: 14, 15, 16, 17),
        bottom_io!(1428: 18, 19, 20, 21),
    ],
    left_io: &[
        left_io!(1080:  3,  2,  1,  0, eo, eo, eo),
        left_io!(1080:  7,  6,  5,  4, eo,  o, no),
        left_io!(1080: 11, 10,  9,  8, eo, eo, no),
        left_io!(1080: 15, 14, 13, 12, no, no,  e),
    ],
    right_io: &[
        right_io!(1314:  0,  1,  2,  3,  4, no, no),
        right_io!(1314:  5,  6,  7,  8,  9, no, no),
        right_io!(1314: 10, 11, 12, 13, no, no, no),
        right_io!(1314: 14, 15, 16, 17, 18, no, no),
    ],
    shelf_io: &[],
    top_io: &[
        top_io!(1176:  3,  2,  1,  0),
        top_io!(1176:  7,  6,  5,  4),
        top_io!(1176: 10,  9,  8, no),
        top_io!(1176: 14, 13, 12, 11),
        top_io!(1176: 18, 17, 16, 15),
        top_io!(1176: 22, 21, 20, 19),
    ],
    // sector
    global_row: 2,
    short_rows: 4,
    short_sector: 256,
    long_rows: 4,
    long_sector: 256,
    // sync
    sync_width: 32,
};

pub const MAX_V_570Z: DensityLayout = DensityLayout {
    has_grow: true,
    // x
    grow: X(9),
    left: X(0),
    right: X(13),
    // y
    top: Y(8),
    short_bottom: Y(3),
    // io
    bottom_io: &[
        bottom_io!(3088:  0,  1,  2,  3),
        bottom_io!(3088:  4,  5,  6,  7),
        bottom_io!(3088:  8,  9, 10, 11),
    ],
    left_io: &[
        left_io!(2368: 10,  9,  8,  7,  6,  5,  4),
        left_io!(2368: 17, 16, 15, 14, 13, 12, 11),
        left_io!(2368: 24, 23, 22, 21, 20, 19, 18),
        left_io!(2368: 31, 30, 29, 28, 27, 26, 25),
    ],
    right_io: &[
        right_io!(2836:  0,  1,  2,  3,  4,  5, no),
        right_io!(2836:  6,  7,  8,  9, 10, 11, no),
        right_io!(2836: 12, 13, 14, 15, 16, 17, no),
        right_io!(2836: 18, 19, 20, 21, 22, 23, no),
        right_io!(2836: 24, 25, 26, 27, 28, 29, no),
        right_io!(2836: 30, 31, 32, 33, 34, 35, no),
        right_io!(2836: 36, 37, 38, 39, 40, 41, no),
    ],
    shelf_io: &[
        bottom_io!(3160:  0,  1,  2,  3),
        bottom_io!(3160:  4,  5,  6,  7),
        bottom_io!(3160:  8,  9, 10, 11),
        bottom_io!(3160: 12, 13, 14, 15),
        bottom_io!(3160: 16, 17, 18, 19),
        bottom_io!(3160: 20, 21, 22, 23),
        bottom_io!(3160: 24, 25, 26, 27),
        // wrap around to start of left strips
        bottom_io!(3160: {2368}, {2374}, {2380}, {2386}),
    ],
    top_io: &[
        top_io!(2560:  3,  2,  1,  0),
        top_io!(2560:  7,  6,  5,  4),
        top_io!(2560: 11, 10,  9,  8),
        top_io!(2560: 15, 14, 13, 12),
        top_io!(2560: 18, 17, 16, no),
        top_io!(2560: 22, 21, 20, 19),
        top_io!(2560: 26, 25, 24, 23),
        top_io!(2560: 30, 29, 28, 27),
        top_io!(2560: 33, 32, 31, no),
        top_io!(2560: 37, 36, 35, 34),
        top_io!(2560: 41, 40, 39, 38),
        top_io!(2560: 45, 44, 43, 42),
    ],
    // sector
    global_row: 3,
    short_rows: 4,
    short_sector: 256,
    long_rows: 7,
    long_sector: 384,
    // sync
    sync_width: 32,
};

pub const MAX_V_1270Z: DensityLayout = DensityLayout {
    has_grow: true,
    // x
    grow: X(11),
    left: X(0),
    right: X(17),
    // y
    top: Y(11),
    short_bottom: Y(3),
    // io
    bottom_io: &[
        bottom_io!(2856:  0,  1,  2,  3),
        bottom_io!(2856:  4,  5,  6, no),
        bottom_io!(2856:  7,  8,  9, 10),
        bottom_io!(2856: 11, 12, 13, 14),
        bottom_io!(2856: 15, 16, 17, no),
    ],
    left_io: &[
        left_io!(1847:  8,  7,  6,  5,  4,  3,  2),
        left_io!(1847: 15, 14, 13, 12, 11, 10,  9),
        left_io!(1847: 22, 21, 20, 19, 18, 17, 16),
        left_io!(1847: 29, 28, 27, 26, 25, 24, 23),
        left_io!(1847: 36, 35, 34, 33, 32, 31, 30),
        left_io!(1847: 43, 42, 41, 40, 39, 38, 37),
        left_io!(1847: 50, 49, 48, 47, 46, 45, 44),
    ],
    right_io: &[
        right_pci_io!(2472:  0,  1,  2,  3,  4, no, no),
        right_pci_io!(2472:  5,  6,  7,  8,  9, no, no),
        right_pci_io!(2472: 10, 11, 12, 13, 14, 15, no),
        right_pci_io!(2472: 16, 17, 18, 19, 20, no, no),
        right_pci_io!(2472: 21, 22, 23, 24, 25, 26, no),
        right_pci_io!(2472: 27, 28, 29, 30, 31, 32, no),
        right_pci_io!(2472: 33, 34, 35, 36, 37, no, no),
        right_pci_io!(2472: 38, 39, 40, 41, 42, 43, no),
        right_pci_io!(2472: 44, 45, 46, 47, 48, no, no),
        right_pci_io!(2472: 49, 50, 51, 52, 53, 54, no),
    ],
    shelf_io: &[
        bottom_io!(2964:  0,  1,  2,  3),
        bottom_io!(2964:  4,  5,  6, no),
        bottom_io!(2964:  7,  8,  9, 10),
        bottom_io!(2964: 11, 12, 13, 14),
        bottom_io!(2964: 15, 16, 17, no),
        bottom_io!(2964: 18, 19, 20, 21),
        bottom_io!(2964: 22, 23, 24, 25),
        bottom_io!(2964: 26, 27, 28, no),
        bottom_io!(2964: 29, 30, 31, 32),
        // wrap around to start of left strips
        bottom_io!(2964: 33, 34, {1847}, {1853}),
    ],
    top_io: &[
        top_io!(2153:  2,  1,  0, no),
        top_io!(2153:  6,  5,  4,  3),
        top_io!(2153:  9,  8,  7, no),
        top_io!(2153: 12, 11, 10, no),
        top_io!(2153: 15, 14, 13, no),
        top_io!(2153: 19, 18, 17, 16),
        top_io!(2153: 22, 21, 20, no),
        top_io!(2153: 25, 24, 23, no),
        top_io!(2153: 29, 28, 27, 26),
        top_io!(2153: 32, 31, 30, no),
        top_io!(2153: 36, 35, 34, 33),
        top_io!(2153: 39, 38, 37, no),
        top_io!(2153: 42, 41, 40, no),
        top_io!(2153: 45, 44, 43, no),
        top_io!(2153: 49, 48, 47, 46),
        top_io!(2153: 52, 51, 50, no),
    ],
    // sector
    global_row: 5,
    short_rows: 7,
    short_sector: 384,
    long_rows: 10,
    long_sector: 512,
    // sync
    sync_width: 64,
};

pub const MAX_V_2210Z: DensityLayout = DensityLayout {
    has_grow: true,
    // x
    grow: X(13),
    left: X(0),
    right: X(21),
    // y
    top: Y(14),
    short_bottom: Y(3),
    // io
    bottom_io: &[
        bottom_io!(4954:  0,  1,  2, no),
        bottom_io!(4954:  3,  4,  5,  6),
        bottom_io!(4954:  7,  8,  9, no),
        bottom_io!(4954: 10, 11, 12, 13),
        bottom_io!(4954: 14, 15, 16, no),
        bottom_io!(4954: 17, 18, 19, 20),
        bottom_io!(4954: 21, 22, 23, no),
    ],
    left_io: &[
        left_io!(3646:  6,  5,  4,  3,  2,  1,  0),
        left_io!(3646: 13, 12, 11, 10,  9,  8,  7),
        left_io!(3646: 19, 18, 17, 16, 15, 14, no),
        left_io!(3646: 26, 25, 24, 23, 22, 21, 20),
        left_io!(3646: 33, 32, 31, 30, 29, 28, 27),
        left_io!(3646: 40, 39, 38, 37, 36, 35, 34),
        left_io!(3646: 47, 46, 45, 44, 43, 42, 41),
        left_io!(3646: 53, 52, 51, 50, 49, 48, no),
        left_io!(3646: 60, 59, 58, 57, 56, 55, 54),
        left_io!(3646: 67, 66, 65, 64, 63, 62, 61),
    ],
    right_io: &[
        right_pci_io!(4451:  0,  1,  2,  3,  4,  5, no),
        right_pci_io!(4451:  6,  7,  8,  9, 10, no, no),
        right_pci_io!(4451: 11, 12, 13, 14, 15, 16, no),
        right_pci_io!(4451: 17, 18, 19, 20, 21, no, no),
        right_pci_io!(4451: 22, 23, 24, 25, 26, no, no),
        right_pci_io!(4451: 27, 28, 29, 30, 31, 32, no),
        right_pci_io!(4451: 33, 34, 35, 36, 37, 38, no),
        right_pci_io!(4451: 39, 40, 41, 42, 43, no, no),
        right_pci_io!(4451: 44, 45, 46, 47, 48, 49, no),
        right_pci_io!(4451: 50, 51, 52, 53, 54, no, no),
        right_pci_io!(4451: 55, 56, 57, 58, 59, 60, no),
        right_pci_io!(4451: 61, 62, 63, 64, 65, no, no),
        right_pci_io!(4451: 66, 67, 68, 69, 70, 71, no),
    ],
    shelf_io: &[
        bottom_io!(5098:  0,  1,  2,  3),
        bottom_io!(5098:  4,  5,  6, no),
        bottom_io!(5098:  7,  8,  9, 10),
        bottom_io!(5098: 11, 12, 13, no),
        bottom_io!(5098: 14, 15, 16, 17),
        bottom_io!(5098: 18, 19, 20, no),
        bottom_io!(5098: 21, 22, 23, 24),
        bottom_io!(5098: 25, 26, 27, no),
        bottom_io!(5098: 28, 29, 30, 31),
        bottom_io!(5098: 32, 33, 34, no),
        bottom_io!(5098: 35, 36, 37, 38),
        bottom_io!(5098: 39, 40, 41, no),
    ],
    top_io: &[
        top_io!(4054:  3,  2,  1,  0),
        top_io!(4054:  6,  5,  4, no),
        top_io!(4054: 10,  9,  8,  7),
        top_io!(4054: 13, 12, 11, no),
        top_io!(4054: 16, 15, 14, no),
        top_io!(4054: 19, 18, 17, no),
        top_io!(4054: 22, 21, 20, no),
        top_io!(4054: 25, 24, 23, no),
        top_io!(4054: 29, 28, 27, 26),
        top_io!(4054: 32, 31, 30, no),
        top_io!(4054: 36, 35, 34, 33),
        top_io!(4054: 39, 38, 37, no),
        top_io!(4054: 42, 41, 40, no),
        top_io!(4054: 45, 44, 43, no),
        top_io!(4054: 48, 47, 46, no),
        top_io!(4054: 51, 50, 49, no),
        top_io!(4054: 55, 54, 53, 52),
        top_io!(4054: 58, 57, 56, no),
        top_io!(4054: 62, 61, 60, 59),
        top_io!(4054: 65, 64, 63, no),
    ],
    // sector
    global_row: 6,
    short_rows: 10,
    short_sector: 512,
    long_rows: 13,
    long_sector: 704,
    // sync
    sync_width: 64,
};

impl DensityLayout {
    pub fn large(&self) -> bool {
        self.has_grow
    }

    pub fn block_type(&self, x: X, y: Y) -> Option<DensityBlockType> {
        if y > self.top || x < self.left || x > self.right {
            None
        } else if y == self.top {
            if x == self.left {
                Some(DensityBlockType::Corner)
            } else if x == self.right {
                None
            } else {
                Some(DensityBlockType::Column)
            }
        } else if y == 0 {
            if x < self.grow {
                None
            } else if x == self.grow {
                Some(DensityBlockType::Corner)
            } else if x == self.right {
                None
            } else {
                Some(DensityBlockType::Column)
            }
        } else if x == self.right {
            Some(DensityBlockType::Right)
        } else if x < self.grow && y < self.short_bottom {
            None
        } else if x == self.left {
            if y == self.short_bottom {
                Some(DensityBlockType::Corner)
            } else {
                Some(DensityBlockType::Left)
            }
        } else if y == self.short_bottom && x < self.grow {
            Some(DensityBlockType::Column)
        } else if x == self.grow && y <= 3 {
            if y == 1 {
                Some(DensityBlockType::Grow)
            } else {
                Some(DensityBlockType::UFM)
            }
        } else {
            Some(DensityBlockType::Logic)
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum DensityC4Block {
    TopLeft,
    Top,
    TopRight,
    RowLeft,
    Row,
    RowRight,
    BottomLeft,
    Bottom,
    BottomRight,
}

impl DensityLayout {
    pub fn c4_block(&self, x: X, y: Y) -> Option<DensityC4Block> {
        if y > self.top || x < self.left || x >= self.right {
            None
        } else if x < self.grow && y < self.short_bottom {
            None
        } else if x == self.left {
            if y == self.top {
                Some(DensityC4Block::TopLeft)
            } else if y == self.short_bottom {
                Some(DensityC4Block::BottomLeft)
            } else {
                Some(DensityC4Block::RowLeft)
            }
        } else if y == self.top {
            if x + 1 == self.right {
                Some(DensityC4Block::TopRight)
            } else {
                Some(DensityC4Block::Top)
            }
        } else if x + 1 == self.grow && y == self.short_bottom {
            Some(DensityC4Block::BottomRight)
        } else if x < self.grow && y == self.short_bottom {
            Some(DensityC4Block::Bottom)
        } else if x + 1 == self.right && y == 0 {
            Some(DensityC4Block::BottomRight)
        } else if x >= self.grow && y == 0 {
            Some(DensityC4Block::Bottom)
        } else if x + 1 == self.right {
            Some(DensityC4Block::RowRight)
        } else {
            Some(DensityC4Block::Row)
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum DensityIOBlock {
    Bottom,
    Left,
    Right,
    Top,
}

impl DensityLayout {
    pub fn io_block(&self, x: X, y: Y)
        -> Option<DensityIOBlock>
    {
        if y == 0 {
            if x > self.grow && x < self.right {
                Some(DensityIOBlock::Bottom)
            } else {
                None
            }
        } else if y == self.top {
            if x > self.left && x < self.right {
                Some(DensityIOBlock::Top)
            } else {
                None
            }
        } else if y > self.top {
            None
        } else if x == self.left {
            if !self.has_grow || y > 3 {
                Some(DensityIOBlock::Left)
            } else {
                None
            }
        } else if x == self.right {
            Some(DensityIOBlock::Right)
        } else if y == self.short_bottom && x < self.grow {
            Some(DensityIOBlock::Bottom)
        } else {
            None
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum DensityIOStrip {
    No {
        special_enable: bool,
        special_output: bool,
    },
    Forward(u16),
    Reverse(u16),
    PCICompliance(u16),
}

impl DensityIOStrip {
    pub fn left(self) -> bool {
        match self {
            DensityIOStrip::Forward(_) =>
                true,

            _ =>
                false,
        }
    }

    pub fn pci_compliance(self) -> bool {
        match self {
            DensityIOStrip::PCICompliance(_) =>
                true,

            _ =>
                false,
        }
    }

    pub fn special_enable(self) -> bool {
        match self {
            DensityIOStrip::No { special_enable, .. } =>
                special_enable,

            _ =>
                false,
        }
    }

    pub fn special_output(self) -> bool {
        match self {
            DensityIOStrip::No { special_output, .. } =>
                special_output,

            _ =>
                false,
        }
    }
}

impl DensityLayout {
    pub fn io_column_cell(&self, x: X, y: Y, n: IOColumnCellNumber)
        -> Option<(bool, DensityIOStrip)>
    {
        if y == 0 && x > self.grow && x < self.right {
            Some((
                false,
                io_column_strip(self.bottom_io, self.right - x - 1, n),
            ))
        } else if y == self.top && x > self.left && x < self.right {
            Some((true, io_column_strip(self.top_io, x - self.left - 1, n)))
        } else if y == self.short_bottom && x > self.left && x < self.grow {
            Some((false, io_column_strip(self.shelf_io, self.grow - x - 1, n)))
        } else {
            None
        }
    }
}

type ColumnStrip<T> = (T, T, T, T);

fn io_column_strip<T>(
    strip: &'static[ColumnStrip<T>],
    i: X,
    n: IOColumnCellNumber,
) -> T
where
    T: Copy,
{
    use IOColumnCellNumber::*;

    let i = usize::from(i);
    match n {
        IOColumnCell0 => strip[i].0,
        IOColumnCell1 => strip[i].1,
        IOColumnCell2 => strip[i].2,
        IOColumnCell3 => strip[i].3,
    }
}

impl DensityLayout {
    pub fn io_row_cell(&self, x: X, y: Y, n: IORowCellNumber)
        -> Option<(bool, DensityIOStrip)>
    {
        if x == self.left && y < self.top {
            if !self.has_grow && y > 0 {
                Some((true, io_row_strip(self.left_io, y - 1, n)))
            } else if self.has_grow && y > 3 {
                Some((true, io_row_strip(self.left_io, y - 4, n)))
            } else {
                None
            }
        } else if x == self.right && y > 0 && y < self.top {
            Some((false, io_row_strip(self.right_io, self.top - y - 1, n)))
        } else {
            None
        }
    }
}

type RowStrip<T> = (T, T, T, T, T, T, T);

fn io_row_strip<T>(
    strip: &'static[RowStrip<T>],
    i: Y,
    n: IORowCellNumber,
) -> T
where
    T: Copy,
{
    use IORowCellNumber::*;

    let i = usize::from(i);
    match n {
        IORowCell0 => strip[i].0,
        IORowCell1 => strip[i].1,
        IORowCell2 => strip[i].2,
        IORowCell3 => strip[i].3,
        IORowCell4 => strip[i].4,
        IORowCell5 => strip[i].5,
        IORowCell6 => strip[i].6,
    }
}

impl DensityLayout {
    pub fn logic_block(&self, x: X, y: Y) -> bool {
        if y >= self.top || y == 0 {
            false
        } else if x <= self.left || x >= self.right {
            false
        } else if x <= self.grow && y <= self.short_bottom {
            false
        } else {
            true
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum DensityR4Block {
    Left,
    LeftLeft,
    Column,
    Grow,
    Right,
}

impl DensityLayout {
    pub fn r4_block(&self, x: X, y: Y) -> Option<DensityR4Block> {
        if y >= self.top || y == 0 || x < self.left || x > self.right {
            None
        } else if x < self.grow && y <= self.short_bottom {
            None
        } else if x == self.left {
            Some(DensityR4Block::Left)
        } else if x == self.left + 1 {
            Some(DensityR4Block::LeftLeft)
        } else if x == self.right {
            Some(DensityR4Block::Right)
        } else if x == self.grow && y <= 3 {
            Some(DensityR4Block::Grow)
        } else {
            Some(DensityR4Block::Column)
        }
    }

    pub fn ufm_block(&self, x: X, y: Y) -> bool {
        if self.has_grow {
            x == self.grow && y > 1 && y <= 3
        } else {
            false
        }
    }
}

