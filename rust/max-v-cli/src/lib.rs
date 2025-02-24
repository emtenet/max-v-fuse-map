use max_v::*;

pub mod block_grid;
pub mod block_table;
pub mod print;

pub use print::*;

pub trait BlockInterconnectIndex: Copy + std::fmt::Display {
    fn interconnect_block() -> &'static str;
    fn interconnect_iter() -> impl Iterator<Item = Self>;
    fn interconnect_sources() -> usize;
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
}

