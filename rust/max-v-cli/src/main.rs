use anyhow::Result;

mod c4;
mod io_col;
mod io_row;
mod logic;
mod r4;
mod ufm;

use max_v::*;
use max_v_cli::*;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let args: Vec<&str> = args.iter().skip(1).map(|s| s.as_str()).collect();
    let devices = [
        DeviceSources::read("device/max_v_240z_t100.sources")?,
        DeviceSources::read("device/max_v_570z_f256.sources")?,
        DeviceSources::read("device/max_v_1270z_f324.sources")?,
        DeviceSources::read("device/max_v_2210z_f324.sources")?,
    ];

    match args[..] {
        ["c4", "grid", i, "count"] =>
            c4::grid::run_count(&devices, i.parse()?),

        ["c4", "grid", i, s, "rel-x"] =>
            c4::grid::run_rel_x(&devices, i.parse()?, s.parse()?),

        ["c4", "grid", i, s, "rel-y"] =>
            c4::grid::run_rel_y(&devices, i.parse()?, s.parse()?),

        ["c4", "grid", i, s, a] =>
            c4::grid::run(&devices, a.parse()?, i.parse()?, s.parse()?),

        ["c4", "has-source", k] =>
            c4::has_source::run(&devices, k.parse()?),

        ["io-col", "grid", i, "count"] =>
            io_col::grid::run_count(&devices, i.parse()?),

        ["io-col", "grid", i, s, "rel-x"] =>
            io_col::grid::run_rel_x(&devices, i.parse()?, s.parse()?),

        ["io-col", "grid", i, s, "rel-y"] =>
            io_col::grid::run_rel_y(&devices, i.parse()?, s.parse()?),

        ["io-col", "grid", i, s, a] =>
            io_col::grid::run(&devices, a.parse()?, i.parse()?, s.parse()?),

        ["io-row", "grid", i, "count"] =>
            io_row::grid::run_count(&devices, i.parse()?),

        ["io-row", "grid", i, s, "rel-x"] =>
            io_row::grid::run_rel_x(&devices, i.parse()?, s.parse()?),

        ["io-row", "grid", i, s, "rel-y"] =>
            io_row::grid::run_rel_y(&devices, i.parse()?, s.parse()?),

        ["io-row", "grid", i, s, a] =>
            io_row::grid::run(&devices, a.parse()?, i.parse()?, s.parse()?),

        ["logic", "grid", i, "count"] =>
            logic::grid::run_count(&devices, i.parse()?),

        ["logic", "grid", i, s, "rel-x"] =>
            logic::grid::run_rel_x(&devices, i.parse()?, s.parse()?),

        ["logic", "grid", i, s, "rel-y"] =>
            logic::grid::run_rel_y(&devices, i.parse()?, s.parse()?),

        ["logic", "grid", i, s, a] =>
            logic::grid::run(&devices, a.parse()?, i.parse()?, s.parse()?),

        ["r4", "grid", i, "count"] =>
            r4::grid::run_count(&devices, i.parse()?),

        ["r4", "grid", i, s, "rel-x"] =>
            r4::grid::run_rel_x(&devices, i.parse()?, s.parse()?),

        ["r4", "grid", i, s, "rel-y"] =>
            r4::grid::run_rel_y(&devices, i.parse()?, s.parse()?),

        ["r4", "grid", i, s, a] =>
            r4::grid::run(&devices, a.parse()?, i.parse()?, s.parse()?),

        ["ufm", "grid", i, "count"] =>
            ufm::grid::run_count(&devices, i.parse()?),

        ["ufm", "grid", i, s, "rel-x"] =>
            ufm::grid::run_rel_x(&devices, i.parse()?, s.parse()?),

        ["ufm", "grid", i, s, "rel-y"] =>
            ufm::grid::run_rel_y(&devices, i.parse()?, s.parse()?),

        ["ufm", "grid", i, s, a] =>
            ufm::grid::run(&devices, a.parse()?, i.parse()?, s.parse()?),

        _ => {
            println!("Error:");
            println!("  {args:?}");
            println!("Usage:");
            println!("  max-v c4 grid <interconnect> <select> <axis>");
            println!("  max-v c4 has-source <kind>");
            println!("  max-v c4 one-source <axis>");
            println!("  max-v io-col grid <interconnect> <select> <axis>");
            println!("  max-v io-col has-source <kind>");
            println!("  max-v io-col one-source <axis>");
            println!("  max-v io-row grid <interconnect> <select> <axis>");
            println!("  max-v io-row has-source <kind>");
            println!("  max-v io-row one-source <axis>");
            println!("  max-v logic grid <interconnect> <select> <axis>");
            println!("  max-v logic has-source <kind>");
            println!("  max-v logic one-source <axis>");
            println!("  max-v r4 grid <interconnect> <select> <axis>");
            println!("  max-v r4 has-source <kind>");
            println!("  max-v r4 one-source <axis>");
            println!("  max-v ufm grid <interconnect> <select> <axis>");
            println!("  max-v ufm has-source <kind>");
            println!("  max-v ufm one-source <axis>");
        }
    }

    Ok(())
}

