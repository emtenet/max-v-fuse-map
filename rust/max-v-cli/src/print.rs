
use max_v::{
    PortKind,
};

pub enum Print {
    Blank,
    Dash,
    Dot,
    Has {
        max_v_240z: bool,
        max_v_570z: bool,
        max_v_1270z: bool,
        max_v_2210z: bool,
    },
    Hash,
    Kind(PortKind),
    Signed(isize),
    Str(&'static str),
    Unsigned(usize),
}

impl Print {
    pub fn print(&self) {
        match self {
            Print::Blank =>
                print!("    "),

            Print::Dash =>
                print!("   -"),

            Print::Dot =>
                print!("   ."),

            Print::Has {
                max_v_240z: false,
                max_v_570z: false,
                max_v_1270z: false,
                max_v_2210z: false,
            } =>
                print!("   ."),

            Print::Has {
                max_v_240z,
                max_v_570z,
                max_v_1270z,
                max_v_2210z,
            } =>
                print!("{}{}{}{}",
                    if *max_v_240z { '#' } else { '_' },
                    if *max_v_570z { '#' } else { '_' },
                    if *max_v_1270z { '#' } else { '_' },
                    if *max_v_2210z { '#' } else { '_' },
                ),

            Print::Hash =>
                print!("   #"),

            Print::Kind(kind) =>
                print!(" {kind:>3}"),

            Print::Signed(0) =>
                print!("   0"),

            Print::Signed(i) =>
                print!(" {i:+3}"),

            Print::Str(s) =>
                print!(" {s:>3}"),

            Print::Unsigned(u) =>
                print!(" {u:3}"),
        }
    }
}

