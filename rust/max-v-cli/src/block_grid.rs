
use max_v::{
    DeviceSources,
    X,
    Y,
};
use super::{
    Print,
};

pub fn print<W>(devices: &[DeviceSources], with: W)
where
    W: Fn(&DeviceSources, X, Y) -> Print,
{
    for device in devices {
        let density = device.density_layout();
        println!("");
        println!(" ==> {}", device.device());
        print!("    ");
        for x in density.x_iter() {
            print!(" {x:2} ");
        }
        println!("");
        print!("    ");
        for _ in density.x_iter() {
            print!(" ===");
        }
        println!("");
        for y in density.y_iter() {
            print!("{y:2} |");
            for x in density.x_iter() {
                with(device, x, y).print();
            }
            println!("");
        }
    }
}

