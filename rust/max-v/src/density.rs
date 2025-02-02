use crate::{
    IOColumnCellNumber,
    IORowCellNumber,
};

pub struct Density {
    pub (crate) has_grow: bool,
    // x
    pub (crate) grow: u8,
    pub (crate) left: u8,
    pub (crate) right: u8,
    // y
    pub (crate) top: u8,
    pub (crate) short_bottom: u8,
    // io
    pci_compliance: bool,
    bottom_io: &'static [ColumnStrip],
    bottom_io_base: u16,
    left_io: &'static [RowStrip],
    left_io_base: u16,
    right_io: &'static [RowStrip],
    right_io_base: u16,
    shelf_io: &'static [ColumnStrip],
    shelf_io_base: u16,
    shelf_io_wrap: u16,
    top_io: &'static [ColumnStrip],
    top_io_base: u16,
    // sector
    pub (crate) global_row: u8,
    pub (crate) short_rows: u8,
    pub (crate) short_sector: usize,
    pub (crate) long_rows: u8,
    pub (crate) long_sector: usize,
    // sync
    pub (crate) sync_width: usize,
}

pub const MAX_V_240Z: Density = Density {
    has_grow: false,
    // x
    grow: 1,
    left: 1,
    right: 8,
    // y
    top: 5,
    short_bottom: 0,
    // io
    pci_compliance: false,
    bottom_io: &[
        (Some( 0), Some( 1), Some( 2), None    ),
        (Some( 3), Some( 4), Some( 5), Some( 6)),
        (Some( 7), Some( 8), Some( 9), Some(10)),
        (Some(11), Some(12), Some(13), None    ),
        (Some(14), Some(15), Some(16), Some(17)),
        (Some(18), Some(19), Some(20), Some(21)),
    ],
    bottom_io_base: 1428,
    left_io: &[
        (Some( 3), Some( 2), Some( 1), Some( 0), None,     None,     None    ),
        (Some( 7), Some( 6), Some( 5), Some( 4), None,     None,     None    ),
        (Some(11), Some(10), Some( 9), Some( 8), None,     None,     None    ),
        (Some(15), Some(14), Some(13), Some(12), None,     None,     None    ),
    ],
    left_io_base: 1080,
    right_io: &[
        (Some( 0), Some( 1), Some( 2), Some( 3), Some( 4), None,     None    ),
        (Some( 5), Some( 6), Some( 7), Some( 8), Some( 9), None,     None    ),
        (Some(10), Some(11), Some(12), Some(13), None,     None,     None    ),
        (Some(14), Some(15), Some(16), Some(17), Some(18), None,     None    ),
    ],
    right_io_base: 1314,
    shelf_io: &[],
    shelf_io_base: 1428,
    shelf_io_wrap: 0,
    top_io: &[
        (Some( 3), Some( 2), Some( 1), Some( 0)),
        (Some( 7), Some( 6), Some( 5), Some( 4)),
        (Some(10), Some( 9), Some( 8), None    ),
        (Some(14), Some(13), Some(12), Some(11)),
        (Some(18), Some(17), Some(16), Some(15)),
        (Some(22), Some(21), Some(20), Some(19)),
    ],
    top_io_base: 1176,
    // sector
    global_row: 2,
    short_rows: 4,
    short_sector: 256,
    long_rows: 4,
    long_sector: 256,
    // sync
    sync_width: 32,
};

pub const MAX_V_570Z: Density = Density {
    has_grow: true,
    // x
    grow: 9,
    left: 0,
    right: 13,
    // y
    top: 8,
    short_bottom: 3,
    // io
    pci_compliance: false,
    bottom_io: &[
        (Some( 0), Some( 1), Some( 2), Some( 3)),
        (Some( 4), Some( 5), Some( 6), Some( 7)),
        (Some( 8), Some( 9), Some(10), Some(11)),
    ],
    bottom_io_base: 3088,
    left_io: &[
        (Some(10), Some( 9), Some( 8), Some( 7), Some( 6), Some( 5), Some( 4)),
        (Some(17), Some(16), Some(15), Some(14), Some(13), Some(12), Some(11)),
        (Some(24), Some(23), Some(22), Some(21), Some(20), Some(19), Some(18)),
        (Some(31), Some(30), Some(29), Some(28), Some(27), Some(26), Some(25)),
    ],
    left_io_base: 2368,
    right_io: &[
        (Some( 0), Some( 1), Some( 2), Some( 3), Some( 4), Some( 5), None    ),
        (Some( 6), Some( 7), Some( 8), Some( 9), Some(10), Some(11), None    ),
        (Some(12), Some(13), Some(14), Some(15), Some(16), Some(17), None    ),
        (Some(18), Some(19), Some(20), Some(21), Some(22), Some(23), None    ),
        (Some(24), Some(25), Some(26), Some(27), Some(28), Some(29), None    ),
        (Some(30), Some(31), Some(32), Some(33), Some(34), Some(35), None    ),
        (Some(36), Some(37), Some(38), Some(39), Some(40), Some(41), None    ),
    ],
    right_io_base: 2836,
    shelf_io: &[
        (Some( 0), Some( 1), Some( 2), Some( 3)),
        (Some( 4), Some( 5), Some( 6), Some( 7)),
        (Some( 8), Some( 9), Some(10), Some(11)),
        (Some(12), Some(13), Some(14), Some(15)),
        (Some(16), Some(17), Some(18), Some(19)),
        (Some(20), Some(21), Some(22), Some(23)),
        (Some(24), Some(25), Some(26), Some(27)),
        (Some(28), Some(29), Some(30), Some(31)),
    ],
    shelf_io_base: 3160,
    shelf_io_wrap: 28,
    top_io: &[
        (Some( 3), Some( 2), Some( 1), Some( 0)),
        (Some( 7), Some( 6), Some( 5), Some( 4)),
        (Some(11), Some(10), Some( 9), Some( 8)),
        (Some(15), Some(14), Some(13), Some(12)),
        (Some(18), Some(17), Some(16), None    ),
        (Some(22), Some(21), Some(20), Some(19)),
        (Some(26), Some(25), Some(24), Some(23)),
        (Some(30), Some(29), Some(28), Some(27)),
        (Some(33), Some(32), Some(31), None    ),
        (Some(37), Some(36), Some(35), Some(34)),
        (Some(41), Some(40), Some(39), Some(38)),
        (Some(45), Some(44), Some(43), Some(42)),
    ],
    top_io_base: 2560,
    // sector
    global_row: 3,
    short_rows: 4,
    short_sector: 256,
    long_rows: 7,
    long_sector: 384,
    // sync
    sync_width: 32,
};

pub const MAX_V_1270Z: Density = Density {
    has_grow: true,
    // x
    grow: 11,
    left: 0,
    right: 17,
    // y
    top: 11,
    short_bottom: 3,
    // io
    pci_compliance: true,
    bottom_io: &[
        (Some( 0), Some( 1), Some( 2), Some( 3)),
        (Some( 4), Some( 5), Some( 6), None    ),
        (Some( 7), Some( 8), Some( 9), Some(10)),
        (Some(11), Some(12), Some(13), Some(14)),
        (Some(15), Some(16), Some(17), None    ),
    ],
    bottom_io_base: 2856,
    left_io: &[
        (Some( 8), Some( 7), Some( 6), Some( 5), Some( 4), Some( 3), Some( 2)),
        (Some(15), Some(14), Some(13), Some(12), Some(11), Some(10), Some( 9)),
        (Some(22), Some(21), Some(20), Some(19), Some(18), Some(17), Some(16)),
        (Some(29), Some(28), Some(27), Some(26), Some(25), Some(24), Some(23)),
        (Some(36), Some(35), Some(34), Some(33), Some(32), Some(31), Some(30)),
        (Some(43), Some(42), Some(41), Some(40), Some(39), Some(38), Some(37)),
        (Some(50), Some(49), Some(48), Some(47), Some(46), Some(45), Some(44)),
    ],
    left_io_base: 1847,
    right_io: &[
        (Some( 0), Some( 1), Some( 2), Some( 3), Some (4), None,     None    ),
        (Some( 5), Some( 6), Some( 7), Some( 8), Some( 9), None,     None    ),
        (Some(10), Some(11), Some(12), Some(13), Some(14), Some(15), None    ),
        (Some(16), Some(17), Some(18), Some(19), Some(20), None,     None    ),
        (Some(21), Some(22), Some(23), Some(24), Some(25), Some(26), None    ),
        (Some(27), Some(28), Some(29), Some(30), Some(31), Some(32), None    ),
        (Some(33), Some(34), Some(35), Some(36), Some(37), None,     None    ),
        (Some(38), Some(39), Some(40), Some(41), Some(42), Some(43), None    ),
        (Some(44), Some(45), Some(46), Some(47), Some(48), None,     None    ),
        (Some(49), Some(50), Some(51), Some(52), Some(53), Some(54), None    ),
    ],
    right_io_base: 2472,
    shelf_io: &[
        (Some( 0), Some( 1), Some( 2), Some( 3)),
        (Some( 4), Some( 5), Some( 6), None    ),
        (Some( 7), Some( 8), Some( 9), Some(10)),
        (Some(11), Some(12), Some(13), Some(14)),
        (Some(15), Some(16), Some(17), None    ),
        (Some(18), Some(19), Some(20), Some(21)),
        (Some(22), Some(23), Some(24), Some(25)),
        (Some(26), Some(27), Some(28), None    ),
        (Some(29), Some(30), Some(31), Some(32)),
        (Some(33), Some(34), Some(35), Some(36)),
    ],
    shelf_io_base: 2964,
    shelf_io_wrap: 35,
    top_io: &[
        (Some( 2), Some( 1), Some( 0), None    ),
        (Some( 6), Some( 5), Some( 4), Some( 3)),
        (Some( 9), Some( 8), Some( 7), None    ),
        (Some(12), Some(11), Some(10), None    ),
        (Some(15), Some(14), Some(13), None    ),
        (Some(19), Some(18), Some(17), Some(16)),
        (Some(22), Some(21), Some(20), None    ),
        (Some(25), Some(24), Some(23), None    ),
        (Some(29), Some(28), Some(27), Some(26)),
        (Some(32), Some(31), Some(30), None    ),
        (Some(36), Some(35), Some(34), Some(33)),
        (Some(39), Some(38), Some(37), None    ),
        (Some(42), Some(41), Some(40), None    ),
        (Some(45), Some(44), Some(43), None    ),
        (Some(49), Some(48), Some(47), Some(46)),
        (Some(52), Some(51), Some(50), None    ),
    ],
    top_io_base: 2153,
    // sector
    global_row: 5,
    short_rows: 7,
    short_sector: 384,
    long_rows: 10,
    long_sector: 512,
    // sync
    sync_width: 64,
};

pub const MAX_V_2210Z: Density = Density {
    has_grow: true,
    // x
    grow: 13,
    left: 0,
    right: 21,
    // y
    top: 14,
    short_bottom: 3,
    // io
    pci_compliance: true,
    bottom_io: &[
        (Some( 0), Some( 1), Some( 2), None    ),
        (Some( 3), Some( 4), Some( 5), Some( 6)),
        (Some( 7), Some( 8), Some( 9), None    ),
        (Some(10), Some(11), Some(12), Some(13)),
        (Some(14), Some(15), Some(16), None    ),
        (Some(17), Some(18), Some(19), Some(20)),
        (Some(21), Some(22), Some(23), None    ),
    ],
    bottom_io_base: 4954,
    left_io: &[
        (Some( 6), Some( 5), Some( 4), Some( 3), Some( 2), Some( 1), Some( 0)),
        (Some(13), Some(12), Some(11), Some(10), Some( 9), Some( 8), Some( 7)),
        (Some(19), Some(18), Some(17), Some(16), Some(15), Some(14), None    ),
        (Some(26), Some(25), Some(24), Some(23), Some(22), Some(21), Some(20)),
        (Some(33), Some(32), Some(31), Some(30), Some(29), Some(28), Some(27)),
        (Some(40), Some(39), Some(38), Some(37), Some(36), Some(35), Some(34)),
        (Some(47), Some(46), Some(45), Some(44), Some(43), Some(42), Some(41)),
        (Some(53), Some(52), Some(51), Some(50), Some(49), Some(48), None    ),
        (Some(60), Some(59), Some(58), Some(57), Some(56), Some(55), Some(54)),
        (Some(67), Some(66), Some(65), Some(64), Some(63), Some(62), Some(61)),
    ],
    left_io_base: 3646,
    right_io: &[
        (Some( 0), Some( 1), Some( 2), Some( 3), Some( 4), Some( 5), None    ),
        (Some( 6), Some( 7), Some( 8), Some( 9), Some(10), None,     None    ),
        (Some(11), Some(12), Some(13), Some(14), Some(15), Some(16), None    ),
        (Some(17), Some(18), Some(19), Some(20), Some(21), None,     None    ),
        (Some(22), Some(23), Some(24), Some(25), Some(26), None,     None    ),
        (Some(27), Some(28), Some(29), Some(30), Some(31), Some(32), None    ),
        (Some(33), Some(34), Some(35), Some(36), Some(37), Some(38), None    ),
        (Some(39), Some(40), Some(41), Some(42), Some(43), None,     None    ),
        (Some(44), Some(45), Some(46), Some(47), Some(48), Some(49), None    ),
        (Some(50), Some(51), Some(52), Some(53), Some(54), None,     None    ),
        (Some(55), Some(56), Some(57), Some(58), Some(59), Some(60), None    ),
        (Some(61), Some(62), Some(63), Some(64), Some(65), None,     None    ),
        (Some(66), Some(67), Some(68), Some(69), Some(70), Some(71), None    ),
    ],
    right_io_base: 4451,
    shelf_io: &[
        (Some( 0), Some( 1), Some( 2), Some( 3)),
        (Some( 4), Some( 5), Some( 6), None    ),
        (Some( 7), Some( 8), Some( 9), Some(10)),
        (Some(11), Some(12), Some(13), None    ),
        (Some(14), Some(15), Some(16), Some(17)),
        (Some(18), Some(19), Some(20), None    ),
        (Some(21), Some(22), Some(23), Some(24)),
        (Some(25), Some(26), Some(27), None    ),
        (Some(28), Some(29), Some(30), Some(31)),
        (Some(32), Some(33), Some(34), None    ),
        (Some(35), Some(36), Some(37), Some(38)),
        (Some(39), Some(40), Some(41), None    ),
    ],
    shelf_io_base: 5098,
    shelf_io_wrap: 42,
    top_io: &[
        (Some( 3), Some( 2), Some( 1), Some( 0)),
        (Some( 6), Some( 5), Some( 4), None    ),
        (Some(10), Some( 9), Some( 8), Some( 7)),
        (Some(13), Some(12), Some(11), None    ),
        (Some(16), Some(15), Some(14), None    ),
        (Some(19), Some(18), Some(17), None    ),
        (Some(22), Some(21), Some(20), None    ),
        (Some(25), Some(24), Some(23), None    ),
        (Some(29), Some(28), Some(27), Some(26)),
        (Some(32), Some(31), Some(30), None    ),
        (Some(36), Some(35), Some(34), Some(33)),
        (Some(39), Some(38), Some(37), None    ),
        (Some(42), Some(41), Some(40), None    ),
        (Some(45), Some(44), Some(43), None    ),
        (Some(48), Some(47), Some(46), None    ),
        (Some(51), Some(50), Some(49), None    ),
        (Some(55), Some(54), Some(53), Some(52)),
        (Some(58), Some(57), Some(56), None    ),
        (Some(62), Some(61), Some(60), Some(59)),
        (Some(65), Some(64), Some(63), None    ),
    ],
    top_io_base: 4054,
    // sector
    global_row: 6,
    short_rows: 10,
    short_sector: 512,
    long_rows: 13,
    long_sector: 704,
    // sync
    sync_width: 64,
};

impl Density {
    pub fn large(&self) -> bool {
        self.has_grow
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum DensityC4Block {
    TopLeft,
    Top,
    RowLeft,
    Row,
    RowRight,
    BottomLeft,
    Bottom,
}

impl Density {
    pub fn c4_block(&self, x: u8, y: u8) -> Option<DensityC4Block> {
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
            Some(DensityC4Block::Top)
        } else if x < self.grow && y == self.short_bottom {
            Some(DensityC4Block::Bottom)
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

impl Density {
    pub fn io_block(&self, x: u8, y: u8)
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
        } else if self.has_grow && x < self.grow {
            Some(DensityIOBlock::Bottom)
        } else {
            None
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum DensityIOColumnCell {
    Bottom {
        left: bool,
        strip: Option<u16>,
    },
    Top {
        strip: Option<u16>,
    },
}

impl Density {
    pub fn io_column_cell(&self, x: u8, y: u8, n: IOColumnCellNumber)
        -> Option<DensityIOColumnCell>
    {
        if y == 0 && x > self.grow && x < self.right {
            io_column_strip(self.bottom_io, self.right - x - 1, n)
                .map(|strip| DensityIOColumnCell::Bottom {
                    left: false,
                    strip: Some(self.bottom_io_base + (6 * strip)),
                })
                .or_else(|| Some(DensityIOColumnCell::Bottom {
                    left: false,
                    strip: None,
                }))
        } else if y == self.top && x > self.left && x < self.right {
            io_column_strip(self.top_io, x - self.left - 1, n)
                .map(|strip| DensityIOColumnCell::Top {
                    strip: Some(self.top_io_base + (6 * strip)),
                })
                .or_else(|| Some(DensityIOColumnCell::Top {
                    strip: None,
                }))
        } else if self.has_grow && x > self.left && x < self.grow {
            io_column_strip(self.shelf_io, self.grow - x - 1, n)
                .map(|strip| if strip >= self.shelf_io_wrap {
                    let strip = strip - self.shelf_io_wrap;
                    DensityIOColumnCell::Bottom {
                        left: true,
                        strip: Some(self.left_io_base + (6 * strip)),
                    }
                } else {
                    DensityIOColumnCell::Bottom {
                        left: false,
                        strip: Some(self.shelf_io_base + (6 * strip)),
                    }
                })
                .or_else(|| Some(DensityIOColumnCell::Bottom {
                    left: false,
                    strip: None,
                }))
        } else {
            None
        }
    }
}

type ColumnStrip = (
    Option<u16>,
    Option<u16>,
    Option<u16>,
    Option<u16>,
);

fn io_column_strip(strip: &'static[ColumnStrip], i: u8, n: IOColumnCellNumber)
    -> Option<u16>
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

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum DensityIORowCell {
    Left {
        strip: Option<u16>,
    },
    Right {
        pci_compliance: bool,
        strip: Option<u16>,
    },
}

impl Density {
    pub fn io_row_cell(&self, x: u8, y: u8, n: IORowCellNumber)
        -> Option<DensityIORowCell>
    {
        if x == self.left && y < self.top {
            if !self.has_grow && y > 0 {
                io_row_strip(self.left_io, y - 1, n)
                    .map(|strip| DensityIORowCell::Left {
                        strip: Some(self.left_io_base + (6 * strip)),
                    })
                    .or_else(|| Some(DensityIORowCell::Left {
                        strip: None,
                    }))
            } else if self.has_grow && y > 3 {
                io_row_strip(self.left_io, y - 4, n)
                    .map(|strip| DensityIORowCell::Left {
                        strip: Some(self.left_io_base + (6 * strip)),
                    })
                    .or_else(|| Some(DensityIORowCell::Left {
                        strip: None,
                    }))
            } else {
                None
            }
        } else if x == self.right && y > 0 && y < self.top {
            let stride: u16 = if self.pci_compliance { 7 } else { 6 };
            io_row_strip(self.right_io, self.top - y - 1, n)
                .map(|strip| DensityIORowCell::Right {
                    strip: Some(self.right_io_base + (stride * strip)),
                    pci_compliance: self.pci_compliance,
                })
                .or_else(|| Some(DensityIORowCell::Right {
                    strip: None,
                    pci_compliance: false,
                }))
        } else {
            None
        }
    }
}

type RowStrip = (
    Option<u16>,
    Option<u16>,
    Option<u16>,
    Option<u16>,
    Option<u16>,
    Option<u16>,
    Option<u16>,
);

fn io_row_strip(strip: &'static[RowStrip], i: u8, n: IORowCellNumber)
    -> Option<u16>
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

impl Density {
    pub fn r4_block(&self, x: u8, y: u8) -> Option<DensityR4Block> {
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

    pub fn ufm_block(&self, x: u8, y: u8) -> bool {
        if self.has_grow {
            x == self.grow && y > 1 && y <= 3
        } else {
            false
        }
    }
}

