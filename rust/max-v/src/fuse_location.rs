
use crate::*;

pub enum FuseLocation {
    //Absolute {
    //    fuse: usize,
    //},
    Strip {
        strip: usize,
    },
    End {
        x: usize,
        top: bool,
        sector: usize,
        index: usize, // 0..10
    },
    Global {
        x: usize,
        sector: usize,
    },
    Block {
        x: usize,
        y: usize,
        sector: usize,
        index: usize, // 0..5
    },
    Cell {
        x: usize,
        y: usize,
        sector: usize,
        n: LogicCellNumber,
        index: usize, // 0..3
    },
}

impl FuseLocation {
    pub (crate) fn to_index(self, device: &Device)
        -> Result<usize, FuseOutOfRange>
    {
        match self {
            FuseLocation::Block { x, y, sector, index: index @ 0..6 } => {
                let sector = sector_at(x, sector, device)?;
                let cell = line_at(y, sector.rows, device)?;
                let index = cell + 20 + index;
                Ok(sector.base + sector_sync(index, device.sync_width))
            }

            FuseLocation::Block { .. } =>
                Err(FuseOutOfRange::SectorBlock),

            FuseLocation::Cell { x, y, sector, n, index: index @ 0..4 } => {
                let sector = sector_at(x, sector, device)?;
                let cell = line_at(y, sector.rows, device)?;
                let index = cell + cell_at(n, index);
                Ok(sector.base + sector_sync(index, device.sync_width))
            }

            FuseLocation::Cell { .. } =>
                Err(FuseOutOfRange::SectorCell),

            FuseLocation::End { x, top, sector, index: mut index @ 0..11 } => {
                let sector = sector_at(x, sector, device)?;
                if !top {
                    index = 11 + 1 + (sector.rows * 46) + 10 - index;
                }
                Ok(sector.base + sector_sync(index, device.sync_width))
            }

            FuseLocation::End { .. } =>
                Err(FuseOutOfRange::SectorEnd),

            FuseLocation::Global { x, sector } => {
                let sector = sector_at(x, sector, device)?;
                let index = 11 + (device.global_row * 46);
                Ok(sector.base + sector_sync(index, device.sync_width))
            }

            FuseLocation::Strip { strip } => {
                Ok((strip * device.sync_width) + 2)
            }
        }
    }
}

struct Sector {
    base: usize,
    rows: usize,
}

fn cell_at(n: LogicCellNumber, index: usize) -> usize {
    match n.index() {
        n @ 0..5 => {
            (n * 4) + index
        }

        n => {
            65 - (n * 4) - index
        }
    }
}

fn line_at(y: usize, rows: usize, device: &Device)
    -> Result<usize, FuseOutOfRange>
{
    if y >= device.top {
        Err(FuseOutOfRange::SectorY)
    } else if y + rows < device.top {
        Err(FuseOutOfRange::SectorY)
    } else {
        let row = device.top - y - 1;
        if row >= device.global_row {
            Ok(11 + 1 + (row * 46))
        } else {
            Ok(11 + (row * 46))
        }
    }
}

fn sector_at(x: usize, sector: usize, device: &Device)
    -> Result<Sector, FuseOutOfRange>
{
    if x < device.left {
        return Err(FuseOutOfRange::SectorX);
    }
    if x > device.right {
        return Err(FuseOutOfRange::SectorX);
    }

    let mut base = device.sync_width;
    if x == device.left {
        if sector >= 13 {
            return Err(FuseOutOfRange::Sector { x, sector });
        }
        return Ok(Sector {
            base: base + (sector * device.short_sector),
            rows: device.short_rows,
        });
    }

    if x == device.right {
        if sector >= 13 {
            return Err(FuseOutOfRange::Sector { x, sector });
        }
    } else {
        if sector >= 28 {
            return Err(FuseOutOfRange::Sector { x, sector });
        }
    }

    // step past IO column
    base += 13 * device.short_sector;
    let mut column = x - device.left - 1;

    if device.has_grow {
        if x < device.grow {
            let base = base + (column * 28 * device.short_sector);
            return Ok(Sector {
                base: base + (sector * device.short_sector),
                rows: device.short_rows,
            });
        }

        // step past short columns
        let grow = device.grow - device.left - 1;
        base += grow * 28 * device.short_sector;

        if x == device.grow {
            if sector < 20 {
                return Ok(Sector {
                    base: base + (sector * device.short_sector),
                    rows: device.short_rows,
                });
            } else {
                let base = base + (20 * device.short_sector);
                let sector = sector - 20;
                return Ok(Sector {
                    base: base + (sector * device.long_sector),
                    rows: device.long_rows,
                });
            }
        }

        // step past grow column
        base += 20 * device.short_sector;
        base += 8 * device.long_sector;
        column = x - device.grow - 1;
    }

    // step past long columns
    base += column * 28 * device.long_sector;

    if x < device.right {
        return Ok(Sector {
            base: base + (sector * device.long_sector),
            rows: device.long_rows,
        });
    }

    Ok(Sector {
        //base: base + ((12 - sector) * device.long_sector),
        base: base + (sector * device.long_sector),
        rows: device.long_rows,
    })
}

fn sector_sync(index: usize, sync_width: usize) -> usize {
    let sync_count = 1 + index.div_euclid(sync_width - 3);
    index + (sync_count * 3)
}

