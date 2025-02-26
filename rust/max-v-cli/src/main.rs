use anyhow::Result;

mod interconnect;

use max_v::*;
use max_v_cli::*;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let args: Vec<&str> = args.iter().skip(1).map(|s| s.as_str()).collect();
    let args = &args[..];
    let devices = [
        DeviceSources::read("device/max_v_240z_t100.sources")?,
        DeviceSources::read("device/max_v_570z_f256.sources")?,
        DeviceSources::read("device/max_v_1270z_f324.sources")?,
        DeviceSources::read("device/max_v_2210z_f324.sources")?,
    ];

    match args {
        [name @ "c4", rest @ ..] =>
            interconnect::<C4InterconnectIndex>(&devices, name, &rest)?,

        [name @ "io-col", rest @ ..] =>
            interconnect::<IOColumnInterconnectIndex>(&devices, name, &rest)?,

        [name @ "io-row", rest @ ..] =>
            interconnect::<IORowInterconnectIndex>(&devices, name, &rest)?,

        [name @ "logic", rest @ ..] =>
            interconnect::<LogicInterconnectIndex>(&devices, name, &rest)?,

        [name @ "r4", rest @ ..] =>
            interconnect::<R4InterconnectIndex>(&devices, name, &rest)?,

        [name @ "ufm", rest @ ..] =>
            interconnect::<UFMInterconnectIndex>(&devices, name, &rest)?,

        _ => {
            println!("Error:");
            println!("  max-v {args:?}");
            println!("Usage:");
            println!("  max-v c4 ...");
            println!("  max-v io-col ...");
            println!("  max-v io-row ...");
            println!("  max-v logic ...");
            println!("  max-v r4 ...");
            println!("  max-v ufm ...");
        }
    }

    Ok(())
}

fn interconnect<I>(devices: &[DeviceSources; 4], name: &str, args: &[&str])
    -> Result<()>
where
    I: BlockInterconnectIndex + std::str::FromStr,
    <I as std::str::FromStr>::Err: std::error::Error + Sync + Send + 'static,
{
    use interconnect::*;

    match args {
        ["grid", i, "count"] =>
            grid::run_count::<I>(devices, i.parse()?),

        ["grid", i, s, "rel-x"] =>
            grid::run_rel_x::<I>(devices, i.parse()?, s.parse()?),

        ["grid", i, s, "rel-y"] =>
            grid::run_rel_y::<I>(devices, i.parse()?, s.parse()?),

        ["grid", i, s, a] =>
            grid::run::<I>(devices, a.parse()?, i.parse()?, s.parse()?),

        ["has-source", k] =>
            has_source::run::<I>(devices, k.parse()?),

        ["one-source"] =>
            one_source_kind::run::<I>(devices),

        ["main-source"] =>
            main_source_kind::run::<I>(devices),

        _ => {
            println!("Error:");
            println!("  max-v {name} {args:?}");
            println!("Usage:");
            println!("  max-v {name} grid <interconnect> <select> <axis>");
            println!("  max-v {name} has-source <kind>");
            println!("  max-v {name} one-source");
            println!("  max-v {name} main-source");
        }
    }

    Ok(())
}

