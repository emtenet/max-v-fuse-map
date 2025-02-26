use max_v::*;

pub mod block_grid;
pub mod block_table;
pub mod print;

pub use print::*;

pub trait BlockInterconnectIndex: Copy + std::fmt::Display {
    fn interconnect_block() -> &'static str;
    fn interconnect_iter() -> impl Iterator<Item = Self>;
    fn interconnect_sources() -> usize;
    fn interconnect_at(self, device: &DeviceSources, x: X, y: Y)
        -> Option<InterconnectSources>;
}

impl BlockInterconnectIndex for C4InterconnectIndex {
    fn interconnect_block() -> &'static str {
        "C4"
    }

    fn interconnect_iter() -> impl Iterator<Item = Self> {
        Self::iter()
    }

    fn interconnect_sources() -> usize {
        13
    }

    fn interconnect_at(self, device: &DeviceSources, x: X, y: Y)
        -> Option<InterconnectSources>
    {
        device.c4_interconnect(x, y, self)
    }
}

impl BlockInterconnectIndex for IOColumnInterconnectIndex {
    fn interconnect_block() -> &'static str {
        "IO Column"
    }

    fn interconnect_iter() -> impl Iterator<Item = Self> {
        Self::iter()
    }

    fn interconnect_sources() -> usize {
        12
    }

    fn interconnect_at(self, device: &DeviceSources, x: X, y: Y)
        -> Option<InterconnectSources>
    {
        device.io_column_interconnect(x, y, self)
    }
}

impl BlockInterconnectIndex for IORowInterconnectIndex {
    fn interconnect_block() -> &'static str {
        "IO Row"
    }

    fn interconnect_iter() -> impl Iterator<Item = Self> {
        Self::iter()
    }

    fn interconnect_sources() -> usize {
        16
    }

    fn interconnect_at(self, device: &DeviceSources, x: X, y: Y)
        -> Option<InterconnectSources>
    {
        device.io_row_interconnect(x, y, self)
    }
}

impl BlockInterconnectIndex for LogicInterconnectIndex {
    fn interconnect_block() -> &'static str {
        "Logic"
    }

    fn interconnect_iter() -> impl Iterator<Item = Self> {
        Self::iter()
    }

    fn interconnect_sources() -> usize {
        16
    }

    fn interconnect_at(self, device: &DeviceSources, x: X, y: Y)
        -> Option<InterconnectSources>
    {
        device.logic_interconnect(x, y, self)
    }
}

impl BlockInterconnectIndex for R4InterconnectIndex {
    fn interconnect_block() -> &'static str {
        "R4"
    }

    fn interconnect_iter() -> impl Iterator<Item = Self> {
        Self::iter()
    }

    fn interconnect_sources() -> usize {
        13
    }

    fn interconnect_at(self, device: &DeviceSources, x: X, y: Y)
        -> Option<InterconnectSources>
    {
        device.r4_interconnect(x, y, self)
    }
}

impl BlockInterconnectIndex for UFMInterconnectIndex {
    fn interconnect_block() -> &'static str {
        "UFM"
    }

    fn interconnect_iter() -> impl Iterator<Item = Self> {
        Self::iter()
    }

    fn interconnect_sources() -> usize {
        13
    }

    fn interconnect_at(self, device: &DeviceSources, x: X, y: Y)
        -> Option<InterconnectSources>
    {
        device.ufm_interconnect(x, y, self)
    }
}

