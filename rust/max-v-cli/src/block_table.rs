use super::{
    BlockInterconnectIndex,
    Print,
};

pub fn print<I, W>(with: W)
where
    I: BlockInterconnectIndex,
    W: Fn(I, usize) -> Print,
{
    println!(" ==> {} interconnect / select", I::interconnect_block());
    print!("    ");
    for s in 0..I::interconnect_sources() {
        print!(" {s:3}");
    }
    println!("");
    print!("    ");
    for _ in 0..I::interconnect_sources() {
        print!(" ===");
    }
    println!("");
    for i in I::interconnect_iter() {
        print!("{i:2} |");
        for s in 0..I::interconnect_sources() {
            with(i, s).print();
        }
        println!("");
    }
}

