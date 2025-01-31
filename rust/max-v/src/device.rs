
use crate::IOCellNumber;

pub struct Device {
    pub (crate) has_grow: bool,
    // x
    pub (crate) grow: usize,
    pub (crate) left: usize,
    pub (crate) right: usize,
    // y
    pub (crate) top: usize,
    // io
    pci_compliance: bool,
    bottom_io: &'static [StripEnd],
    bottom_io_base: usize,
    left_io: &'static [StripSide],
    left_io_base: usize,
    right_io: &'static [StripSide],
    right_io_base: usize,
    shelf_io: &'static [StripEnd],
    shelf_io_base: usize,
    shelf_io_wrap: usize,
    top_io: &'static [StripEnd],
    top_io_base: usize,
    // sector
    pub (crate) global_row: usize,
    pub (crate) short_rows: usize,
    pub (crate) short_sector: usize,
    pub (crate) long_rows: usize,
    pub (crate) long_sector: usize,
    // sync
    pub (crate) sync_width: usize,
}

pub const MAX_V_240Z: Device = Device {
    has_grow: false,
    // x
    grow: 1,
    left: 1,
    right: 8,
    // y
    top: 5,
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

pub const MAX_V_570Z: Device = Device {
    has_grow: true,
    // x
    grow: 9,
    left: 0,
    right: 13,
    // y
    top: 8,
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

pub const MAX_V_1270Z: Device = Device {
    has_grow: true,
    // x
    grow: 11,
    left: 0,
    right: 17,
    // y
    top: 11,
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

pub const MAX_V_2210Z: Device = Device {
    has_grow: true,
    // x
    grow: 13,
    left: 0,
    right: 21,
    // y
    top: 14,
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


#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum DeviceIOBlock {
    Bottom,
    Left,
    Right,
    Top,
}

impl Device {
    pub fn io_block(&self, x: usize, y: usize)
        -> Option<DeviceIOBlock>
    {
        if y == 0 {
            if x > self.grow && x < self.right {
                Some(DeviceIOBlock::Bottom)
            } else {
                None
            }
        } else if y == self.top {
            if x > self.left && x < self.right {
                Some(DeviceIOBlock::Top)
            } else {
                None
            }
        } else if y > self.top {
            None
        } else if x == self.left {
            if !self.has_grow || y > 3 {
                Some(DeviceIOBlock::Left)
            } else {
                None
            }
        } else if x == self.right {
            Some(DeviceIOBlock::Right)
        } else if self.has_grow && x < self.grow {
            Some(DeviceIOBlock::Bottom)
        } else {
            None
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum DeviceIOCell {
    Bottom {
        strip: Option<usize>,
    },
    Left {
        strip: Option<usize>,
    },
    Right {
        pci_compliance: bool,
        strip: Option<usize>,
    },
    Top {
        strip: Option<usize>,
    },
}

impl Device {
    pub fn io_cell(&self, x: usize, y: usize, n: IOCellNumber)
        -> Option<DeviceIOCell>
    {
        if y == 0 {
            if x > self.grow && x < self.right {
                io_cell_end(self.bottom_io, self.right - x - 1, n)
                    .map(|strip| DeviceIOCell::Bottom {
                        strip: Some(self.bottom_io_base + (6 * strip)),
                    })
                    .or_else(|| Some(DeviceIOCell::Bottom {
                        strip: None,
                    }))
            } else {
                None
            }
        } else if y == self.top {
            if x > self.left && x < self.right {
                io_cell_end(self.top_io, x - self.left - 1, n)
                    .map(|strip| DeviceIOCell::Top {
                        strip: Some(self.top_io_base + (6 * strip)),
                    })
                    .or_else(|| Some(DeviceIOCell::Top {
                        strip: None,
                    }))
            } else {
                None
            }
        } else if y > self.top {
            None
        } else if x == self.left {
            if !self.has_grow {
                io_cell_side(self.left_io, y - 1, n)
                    .map(|strip| DeviceIOCell::Left {
                        strip: Some(self.left_io_base + (6 * strip)),
                    })
                    .or_else(|| Some(DeviceIOCell::Left {
                        strip: None,
                    }))
            } else if y > 3 {
                io_cell_side(self.left_io, y - 4, n)
                    .map(|strip| DeviceIOCell::Left {
                        strip: Some(self.left_io_base + (6 * strip)),
                    })
                    .or_else(|| Some(DeviceIOCell::Left {
                        strip: None,
                    }))
            } else {
                None
            }
        } else if x == self.right {
            let stride: usize = if self.pci_compliance { 7 } else { 6 };
            io_cell_side(self.right_io, self.top - y - 1, n)
                .map(|strip| DeviceIOCell::Right {
                    strip: Some(self.right_io_base + (stride * strip)),
                    pci_compliance: self.pci_compliance,
                })
                .or_else(|| Some(DeviceIOCell::Right {
                    strip: None,
                    pci_compliance: false,
                }))
        } else if self.has_grow && x < self.grow {
            io_cell_end(self.shelf_io, self.grow - x - 1, n)
                .map(|strip| if strip >= self.shelf_io_wrap {
                    let strip = strip - self.shelf_io_wrap;
                    DeviceIOCell::Left {
                        strip: Some(self.left_io_base + (6 * strip)),
                    }
                } else {
                    DeviceIOCell::Bottom {
                        strip: Some(self.shelf_io_base + (6 * strip)),
                    }
                })
                .or_else(|| Some(DeviceIOCell::Bottom {
                    strip: None,
                }))
        } else {
            None
        }
    }
}

type StripEnd = (
    Option<usize>,
    Option<usize>,
    Option<usize>,
    Option<usize>,
);

fn io_cell_end(strip: &'static[StripEnd], i: usize, n: IOCellNumber)
    -> Option<usize>
{
    use IOCellNumber::*;

    match n {
        IOCell0 => strip[i].0,
        IOCell1 => strip[i].1,
        IOCell2 => strip[i].2,
        IOCell3 => strip[i].3,
        _ => None,
    }
}

type StripSide = (
    Option<usize>,
    Option<usize>,
    Option<usize>,
    Option<usize>,
    Option<usize>,
    Option<usize>,
    Option<usize>,
);

fn io_cell_side(strip: &'static[StripSide], i: usize, n: IOCellNumber)
    -> Option<usize>
{
    use IOCellNumber::*;

    match n {
        IOCell0 => strip[i].0,
        IOCell1 => strip[i].1,
        IOCell2 => strip[i].2,
        IOCell3 => strip[i].3,
        IOCell4 => strip[i].4,
        IOCell5 => strip[i].5,
        IOCell6 => strip[i].6,
    }
}

