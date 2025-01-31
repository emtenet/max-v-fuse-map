
use crate::*;

pub enum FuseLocation {
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

impl FuseLocation {
    pub (crate) fn to_index(self, device: &Device)
        -> Result<usize, FuseOutOfRange>
    {
        match self {
            FuseLocation::Block { x, y, sector, index: index @ 0..6 } => {
                let sector = sector_at(x, sector, device)?;
                let cell = line_at(y, sector.rows, device)?;
                let index = cell + 20 + usize::from(index);
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

            FuseLocation::End { x, top, sector, index: index @ 0..11 } => {
                let sector = sector_at(x, sector, device)?;
                let mut index = usize::from(index);
                if !top {
                    index = 11 + 1 +
                        (usize::from(sector.rows) * 46) +
                        (10 - index);
                }
                Ok(sector.base + sector_sync(index, device.sync_width))
            }

            FuseLocation::End { .. } =>
                Err(FuseOutOfRange::SectorEnd),

            FuseLocation::Global { x, sector } => {
                let sector = sector_at(x, sector, device)?;
                let index = 11 + (usize::from(device.global_row) * 46);
                Ok(sector.base + sector_sync(index, device.sync_width))
            }

            FuseLocation::Strip { strip } => {
                Ok((usize::from(strip) * device.sync_width) + 2)
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

fn line_at(y: u8, rows: u8, device: &Device)
    -> Result<usize, FuseOutOfRange>
{
    if y >= device.top {
        Err(FuseOutOfRange::SectorY)
    } else if y + rows < device.top {
        Err(FuseOutOfRange::SectorY)
    } else {
        let row = device.top - y - 1;
        if row >= device.global_row {
            Ok(11 + 1 + (usize::from(row) * 46))
        } else {
            Ok(11 + (usize::from(row) * 46))
        }
    }
}

fn sector_at(x: u8, sector: u8, device: &Device)
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
            base: base + (usize::from(sector) * device.short_sector),
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
            let base = base + (usize::from(column) * 28 * device.short_sector);
            return Ok(Sector {
                base: base + (usize::from(sector) * device.short_sector),
                rows: device.short_rows,
            });
        }

        // step past short columns
        let grow = device.grow - device.left - 1;
        base += usize::from(grow) * 28 * device.short_sector;

        if x == device.grow {
            if sector < 20 {
                return Ok(Sector {
                    base: base + (usize::from(sector) * device.short_sector),
                    rows: device.short_rows,
                });
            } else {
                let base = base + (20 * device.short_sector);
                let sector = sector - 20;
                return Ok(Sector {
                    base: base + (usize::from(sector) * device.long_sector),
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
    base += usize::from(column) * 28 * device.long_sector;

    if x < device.right {
        return Ok(Sector {
            base: base + (usize::from(sector) * device.long_sector),
            rows: device.long_rows,
        });
    }

    Ok(Sector {
        //base: base + ((12 - sector) * device.long_sector),
        base: base + (usize::from(sector) * device.long_sector),
        rows: device.long_rows,
    })
}

fn sector_sync(index: usize, sync_width: usize) -> usize {
    let sync_count = 1 + index.div_euclid(sync_width - 3);
    index + (sync_count * 3)
}

