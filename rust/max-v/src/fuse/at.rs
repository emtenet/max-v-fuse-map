use crate::*;

pub enum FuseAt {
    Strip {
        strip: u16,
    },
    End {
        x: u8,
        top: bool,
        sector: u8,
        index: u8, // 0..10
    },
    Global {
        x: u8,
        sector: u8,
    },
    Block {
        x: u8,
        y: u8,
        sector: u8,
        index: u8, // 0..5
    },
    Cell {
        x: u8,
        y: u8,
        sector: u8,
        n: LogicCellNumber,
        index: u8, // 0..3
    },
}

impl FuseAt {
    pub (crate) fn to_index(self, density: &Density)
        -> Result<usize, FuseOutOfRange>
    {
        match self {
            FuseAt::Block { x, y, sector, index: index @ 0..6 } => {
                let sector = sector_at(x, sector, density)?;
                let cell = line_at(y, sector.rows, density)?;
                let index = cell + 20 + usize::from(index);
                Ok(sector.base + sector_sync(index, density.sync_width))
            }

            FuseAt::Block { .. } =>
                Err(FuseOutOfRange::SectorBlock),

            FuseAt::Cell { x, y, sector, n, index: index @ 0..4 } => {
                let sector = sector_at(x, sector, density)?;
                let cell = line_at(y, sector.rows, density)?;
                let index = cell + cell_at(n, index);
                Ok(sector.base + sector_sync(index, density.sync_width))
            }

            FuseAt::Cell { .. } =>
                Err(FuseOutOfRange::SectorCell),

            FuseAt::End { x, top, sector, index: index @ 0..11 } => {
                let sector = sector_at(x, sector, density)?;
                let mut index = usize::from(index);
                if !top {
                    index = 11 + 1 +
                        (usize::from(sector.rows) * 46) +
                        (10 - index);
                }
                Ok(sector.base + sector_sync(index, density.sync_width))
            }

            FuseAt::End { .. } =>
                Err(FuseOutOfRange::SectorEnd),

            FuseAt::Global { x, sector } => {
                let sector = sector_at(x, sector, density)?;
                let index = 11 + (usize::from(density.global_row) * 46);
                Ok(sector.base + sector_sync(index, density.sync_width))
            }

            FuseAt::Strip { strip } => {
                Ok((usize::from(strip) * density.sync_width) + 2)
            }
        }
    }
}

struct Sector {
    base: usize,
    rows: u8,
}

fn cell_at(n: LogicCellNumber, index: u8) -> usize {
    match n.index() {
        n @ 0..5 => {
            (n * 4) + usize::from(index)
        }

        n => {
            65 - (n * 4) - usize::from(index)
        }
    }
}

fn line_at(y: u8, rows: u8, density: &Density)
    -> Result<usize, FuseOutOfRange>
{
    if y >= density.top {
        Err(FuseOutOfRange::SectorY)
    } else if y + rows < density.top {
        Err(FuseOutOfRange::SectorY)
    } else {
        let row = density.top - y - 1;
        if row >= density.global_row {
            Ok(11 + 1 + (usize::from(row) * 46))
        } else {
            Ok(11 + (usize::from(row) * 46))
        }
    }
}

fn sector_at(x: u8, sector: u8, density: &Density)
    -> Result<Sector, FuseOutOfRange>
{
    if x < density.left {
        return Err(FuseOutOfRange::SectorX);
    }
    if x > density.right {
        return Err(FuseOutOfRange::SectorX);
    }

    let mut base = density.sync_width;
    if x == density.left {
        if sector >= 13 {
            return Err(FuseOutOfRange::Sector { x, sector });
        }
        return Ok(Sector {
            base: base + (usize::from(sector) * density.short_sector),
            rows: density.short_rows,
        });
    }

    if x == density.right {
        if sector >= 13 {
            return Err(FuseOutOfRange::Sector { x, sector });
        }
    } else {
        if sector >= 28 {
            return Err(FuseOutOfRange::Sector { x, sector });
        }
    }

    // step past IO column
    base += 13 * density.short_sector;
    let mut column = x - density.left - 1;

    if density.has_grow {
        if x < density.grow {
            let base = base + (usize::from(column) * 28 * density.short_sector);
            return Ok(Sector {
                base: base + (usize::from(sector) * density.short_sector),
                rows: density.short_rows,
            });
        }

        // step past short columns
        let grow = density.grow - density.left - 1;
        base += usize::from(grow) * 28 * density.short_sector;

        if x == density.grow {
            if sector < 20 {
                return Ok(Sector {
                    base: base + (usize::from(sector) * density.short_sector),
                    rows: density.short_rows,
                });
            } else {
                let base = base + (20 * density.short_sector);
                let sector = sector - 20;
                return Ok(Sector {
                    base: base + (usize::from(sector) * density.long_sector),
                    rows: density.long_rows,
                });
            }
        }

        // step past grow column
        base += 20 * density.short_sector;
        base += 8 * density.long_sector;
        column = x - density.grow - 1;
    }

    // step past long columns
    base += usize::from(column) * 28 * density.long_sector;

    if x < density.right {
        return Ok(Sector {
            base: base + (usize::from(sector) * density.long_sector),
            rows: density.long_rows,
        });
    }

    Ok(Sector {
        //base: base + ((12 - sector) * density.long_sector),
        base: base + (usize::from(sector) * density.long_sector),
        rows: density.long_rows,
    })
}

fn sector_sync(index: usize, sync_width: usize) -> usize {
    let sync_count = 1 + index.div_euclid(sync_width - 3);
    index + (sync_count * 3)
}

