use anyhow::Result;

mod c4;
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
        ["c4", "grid", i, "count"] => {
            let index: C4InterconnectIndex = i.parse()?;
            c4::grid::run_count(&devices, index);
        }

        ["c4", "grid", i, s, "rel-x"] => {
            let index: C4InterconnectIndex = i.parse()?;
            let source: usize = s.parse()?;
            c4::grid::run_rel_x(&devices, index, source);
        }

        ["c4", "grid", i, s, "rel-y"] => {
            let index: C4InterconnectIndex = i.parse()?;
            let source: usize = s.parse()?;
            c4::grid::run_rel_y(&devices, index, source);
        }

        ["c4", "grid", i, s, a] => {
            let index: C4InterconnectIndex = i.parse()?;
            let source: usize = s.parse()?;
            let axis: PortAxis = a.parse()?;
            c4::grid::run(&devices, axis, index, source);
        }

        ["r4", "grid", i, "count"] => {
            let index: R4InterconnectIndex = i.parse()?;
            r4::grid::run_count(&devices, index);
        }

        ["r4", "grid", i, s, "rel-x"] => {
            let index: R4InterconnectIndex = i.parse()?;
            let source: usize = s.parse()?;
            r4::grid::run_rel_x(&devices, index, source);
        }

        ["r4", "grid", i, s, "rel-y"] => {
            let index: R4InterconnectIndex = i.parse()?;
            let source: usize = s.parse()?;
            r4::grid::run_rel_y(&devices, index, source);
        }

        ["r4", "grid", i, s, a] => {
            let index: R4InterconnectIndex = i.parse()?;
            let source: usize = s.parse()?;
            let axis: PortAxis = a.parse()?;
            r4::grid::run(&devices, axis, index, source);
        }

        ["ufm", "grid", i, "count"] => {
            let index: UFMInterconnectIndex = i.parse()?;
            ufm::grid::run_count(&devices, index);
        }

        ["ufm", "grid", i, s, "rel-x"] => {
            let index: UFMInterconnectIndex = i.parse()?;
            let source: usize = s.parse()?;
            ufm::grid::run_rel_x(&devices, index, source);
        }

        ["ufm", "grid", i, s, "rel-y"] => {
            let index: UFMInterconnectIndex = i.parse()?;
            let source: usize = s.parse()?;
            ufm::grid::run_rel_y(&devices, index, source);
        }

        ["ufm", "grid", i, s, a] => {
            let index: UFMInterconnectIndex = i.parse()?;
            let source: usize = s.parse()?;
            let axis: PortAxis = a.parse()?;
            ufm::grid::run(&devices, axis, index, source);
        }

        _ => {
            println!("Error:");
            println!("  {args:?}");
            println!("Usage:");
            println!("  max-v c4 grid <interconnect> <select> <axis>");
            println!("  max-v c4 has-source <kind>");
            println!("  max-v c4 one-source <axis>");
            println!("  max-v iob grid <interconnect> <select> <axis>");
            println!("  max-v iob has-source <kind>");
            println!("  max-v iob one-source <axis>");
            println!("  max-v lab grid <interconnect> <select> <axis>");
            println!("  max-v lab has-source <kind>");
            println!("  max-v lab one-source <axis>");
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

