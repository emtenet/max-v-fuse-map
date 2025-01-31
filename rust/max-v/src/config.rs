
pub struct LAB {
    pub clock_1: LABClock,
    pub clock_1_control: LABControl01,
    pub clock_1_invert: bool,
    pub clock_2: LABClock,
    pub clock_2_control: LABControl23, // also a_load_control
    pub clock_2_invert: bool,
    pub enable_1_off: bool,
    pub enable_1_control: LABControl23,
    pub enable_1_invert: bool,
    pub enable_2_off: bool,
    pub enable_2_control: LABControl01, // also s_load_control
    pub enable_2_invert: bool, // also s_load_invert
    pub a_clear: LABAClearGlobal,
    pub a_clear_1: LABAClear,
    pub a_clear_1_control: LABControl45,
    pub a_clear_1_invert: bool,
    pub a_clear_2: LABAClear,
    pub a_clear_2_control: LABControl45,
    pub a_clear_2_invert: bool,
    pub a_load: LABALoad,
    pub a_load_invert: bool,
    pub s_clear: LABSClear,
    pub s_clear_control: LABControl45,
    pub s_clear_invert: bool,
    pub s_load: LABSLoad,
    pub invert_a: LABInvertA,
    pub invert_a_control: LABControl45,
    pub carry_in: LABCarryIn,
    pub control: [LCInput; 6],
    pub interconnect_0_11: [LABInput; 12],
    pub interconnect_12: LABInputGlobal,
    pub interconnect_13_24: [LABInput; 12],
    pub interconnect_25: LABInputGlobal,
    pub cell: [LC; 10],
}

pub enum LABClock {
    Global0,
    Global1,
    Global2,
    Global3,
    Control,
}

pub enum LABControl01 {
    Control0,
    Control1,
}

pub enum LABControl23 {
    Control2,
    Control3,
}

pub enum LABControl45 {
    Control4,
    Control5,
}

pub enum LABAClearGlobal {
    Global0,
    Global1,
    Global2,
    Global3,
}

pub enum LABAClear {
    Off,
    Control,
    Global,
}

pub enum LABALoad {
    Off,
    Control,
}

pub enum LABSClear {
    Off,
    Control,
}

pub enum LABSLoad {
    On,
    Off,
    Control,
}

pub enum LABInvertA {
    Off,
    Control,
}

pub enum LABCarryIn {
    BlockChain,
    InvertA,
}

pub enum LABInput {
    From00,
    From01,
    From02,
    From10,
    From11,
    From12,
    From20,
    From21,
    From22,
    From30,
    From31,
    From32,
    DirectLink,
}

pub enum LABInputGlobal {
    From00,
    From01,
    From02,
    From10,
    From11,
    From12,
    From20,
    From21,
    From22,
    From30,
    From31,
    From32,
    Global0,
    Global1,
    Global2,
    DirectLink,
}

pub struct LC {
    pub input_a: LCInput,
    pub input_b: LCInput,
    pub input_c: LCInput,
    pub input_d: LCInput,
    pub lut: u16,
    pub lut_input_c: LUTInputC,
    pub lut_input_d: LUTInputD,
    pub register_clock: RegisterClock,
    pub register_clear: RegisterClear,
    pub register_input: RegisterInput,
    pub register_sync: bool,
    pub output_left: LCOutput,
    pub output_local: LCOutput,
    pub output_right: LCOutput,
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum LCInput {
    From00,
    From01,
    From02,
    From10,
    From11,
    From12,
    From20,
    From21,
    From22,
    From30,
    From31,
    From32,
    From40,
    From41,
    From42,
    From50,
    From51,
    From52,
}

pub enum LCOutput {
    LUT,
    Register,
}

pub enum LUTInputC {
    CarryIn,
    RegisterFeedback,
    InputC,
}

pub enum LUTInputD {
    LUTChain,
    InputD,
}

pub enum RegisterClock {
    BlockClock1,
    BlockClock2,
}

pub enum RegisterClear {
    BlockClear1,
    BlockClear2,
}

pub enum RegisterInput {
    RegisterChain,
    LUT,
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum LCInputSource {
    Interconnect(usize),
    LocalLine(usize),
}

impl LCInput {
    pub fn a_source(&self) -> LCInputSource {
        match self {
            LCInput::From00 => LCInputSource::Interconnect(0),
            LCInput::From01 => LCInputSource::Interconnect(3),
            LCInput::From02 => LCInputSource::Interconnect(8),
            LCInput::From10 => LCInputSource::Interconnect(1),
            LCInput::From11 => LCInputSource::Interconnect(6),
            LCInput::From12 => LCInputSource::Interconnect(15),
            LCInput::From20 => LCInputSource::Interconnect(9),
            LCInput::From21 => LCInputSource::Interconnect(11),
            LCInput::From22 => LCInputSource::Interconnect(14),
            LCInput::From30 => LCInputSource::Interconnect(18),
            LCInput::From31 => LCInputSource::Interconnect(22),
            LCInput::From32 => LCInputSource::Interconnect(25),
            LCInput::From40 => LCInputSource::Interconnect(19),
            LCInput::From41 => LCInputSource::LocalLine(3),
            LCInput::From42 => LCInputSource::LocalLine(8),
            LCInput::From50 => LCInputSource::LocalLine(4),
            LCInput::From51 => LCInputSource::LocalLine(5),
            LCInput::From52 => LCInputSource::LocalLine(6),
        }
    }

    pub fn b_source(&self) -> LCInputSource {
        match self {
            LCInput::From00 => LCInputSource::Interconnect(2),
            LCInput::From01 => LCInputSource::Interconnect(7),
            LCInput::From02 => LCInputSource::Interconnect(17),
            LCInput::From10 => LCInputSource::Interconnect(4),
            LCInput::From11 => LCInputSource::Interconnect(5),
            LCInput::From12 => LCInputSource::Interconnect(10),
            LCInput::From20 => LCInputSource::Interconnect(12),
            LCInput::From21 => LCInputSource::Interconnect(13),
            LCInput::From22 => LCInputSource::Interconnect(16),
            LCInput::From30 => LCInputSource::Interconnect(20),
            LCInput::From31 => LCInputSource::Interconnect(23),
            LCInput::From32 => LCInputSource::Interconnect(24),
            LCInput::From40 => LCInputSource::Interconnect(21),
            LCInput::From41 => LCInputSource::LocalLine(0),
            LCInput::From42 => LCInputSource::LocalLine(7),
            LCInput::From50 => LCInputSource::LocalLine(1),
            LCInput::From51 => LCInputSource::LocalLine(2),
            LCInput::From52 => LCInputSource::LocalLine(9),
        }
    }

    pub fn c_source(&self) -> LCInputSource {
        match self {
            LCInput::From00 => LCInputSource::Interconnect(0),
            LCInput::From01 => LCInputSource::Interconnect(3),
            LCInput::From02 => LCInputSource::Interconnect(8),
            LCInput::From10 => LCInputSource::Interconnect(2),
            LCInput::From11 => LCInputSource::Interconnect(7),
            LCInput::From12 => LCInputSource::Interconnect(17),
            LCInput::From20 => LCInputSource::Interconnect(9),
            LCInput::From21 => LCInputSource::Interconnect(11),
            LCInput::From22 => LCInputSource::Interconnect(14),
            LCInput::From30 => LCInputSource::Interconnect(18),
            LCInput::From31 => LCInputSource::Interconnect(22),
            LCInput::From32 => LCInputSource::Interconnect(25),
            LCInput::From40 => LCInputSource::Interconnect(21),
            LCInput::From41 => LCInputSource::LocalLine(0),
            LCInput::From42 => LCInputSource::LocalLine(7),
            LCInput::From50 => LCInputSource::LocalLine(4),
            LCInput::From51 => LCInputSource::LocalLine(5),
            LCInput::From52 => LCInputSource::LocalLine(6),
        }
    }

    pub fn d_source(&self) -> LCInputSource {
        match self {
            LCInput::From00 => LCInputSource::Interconnect(1),
            LCInput::From01 => LCInputSource::Interconnect(6),
            LCInput::From02 => LCInputSource::Interconnect(15),
            LCInput::From10 => LCInputSource::Interconnect(4),
            LCInput::From11 => LCInputSource::Interconnect(5),
            LCInput::From12 => LCInputSource::Interconnect(10),
            LCInput::From20 => LCInputSource::Interconnect(12),
            LCInput::From21 => LCInputSource::Interconnect(13),
            LCInput::From22 => LCInputSource::Interconnect(16),
            LCInput::From30 => LCInputSource::Interconnect(20),
            LCInput::From31 => LCInputSource::Interconnect(23),
            LCInput::From32 => LCInputSource::Interconnect(24),
            LCInput::From40 => LCInputSource::Interconnect(19),
            LCInput::From41 => LCInputSource::LocalLine(3),
            LCInput::From42 => LCInputSource::LocalLine(8),
            LCInput::From50 => LCInputSource::LocalLine(1),
            LCInput::From51 => LCInputSource::LocalLine(2),
            LCInput::From52 => LCInputSource::LocalLine(9),
        }
    }

    pub fn iter() -> LCInputs {
        LCInputs(Some(LCInput::From00))
    }
}

pub struct LCInputs(Option<LCInput>);

impl Iterator for LCInputs {
    type Item = LCInput;

    fn next(&mut self) -> Option<Self::Item> {
        use LCInput::*;
        use std::mem::replace;

        match &self.0 {
            Some(From00) => replace(&mut self.0, Some(From01)),
            Some(From01) => replace(&mut self.0, Some(From02)),
            Some(From02) => replace(&mut self.0, Some(From10)),
            Some(From10) => replace(&mut self.0, Some(From11)),
            Some(From11) => replace(&mut self.0, Some(From12)),
            Some(From12) => replace(&mut self.0, Some(From20)),
            Some(From20) => replace(&mut self.0, Some(From21)),
            Some(From21) => replace(&mut self.0, Some(From22)),
            Some(From22) => replace(&mut self.0, Some(From30)),
            Some(From30) => replace(&mut self.0, Some(From31)),
            Some(From31) => replace(&mut self.0, Some(From32)),
            Some(From32) => replace(&mut self.0, Some(From40)),
            Some(From40) => replace(&mut self.0, Some(From41)),
            Some(From41) => replace(&mut self.0, Some(From42)),
            Some(From42) => replace(&mut self.0, Some(From50)),
            Some(From50) => replace(&mut self.0, Some(From51)),
            Some(From51) => replace(&mut self.0, Some(From52)),
            Some(From52) => replace(&mut self.0, None),
            None => None,
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum LCInputPort {
    A(LCInput),
    B(LCInput),
    C(LCInput),
    D(LCInput),
}

impl LCInputSource {
    pub fn input_ports(&self) -> (LCInputPort, LCInputPort) {
        use LCInput::*;
        use LCInputPort::*;
        use LCInputSource::*;

        match self {
            Interconnect(0) => (A(From00), C(From00)),
            Interconnect(1) => (A(From10), D(From00)),
            Interconnect(2) => (B(From00), C(From10)),
            Interconnect(3) => (A(From01), C(From01)),
            Interconnect(4) => (B(From10), D(From10)),
            Interconnect(5) => (B(From11), D(From11)),
            Interconnect(6) => (A(From11), D(From01)),
            Interconnect(7) => (B(From01), C(From11)),
            Interconnect(8) => (A(From02), C(From02)),
            Interconnect(9) => (A(From20), C(From20)),
            Interconnect(10) => (B(From12), D(From12)),
            Interconnect(11) => (A(From21), C(From21)),
            Interconnect(12) => (B(From20), D(From20)),
            Interconnect(13) => (B(From21), D(From21)),
            Interconnect(14) => (A(From22), C(From22)),
            Interconnect(15) => (A(From12), D(From02)),
            Interconnect(16) => (B(From22), D(From22)),
            Interconnect(17) => (B(From02), C(From12)),
            Interconnect(18) => (A(From30), C(From30)),
            Interconnect(19) => (A(From40), D(From40)),
            Interconnect(20) => (B(From30), D(From30)),
            Interconnect(21) => (B(From40), C(From40)),
            Interconnect(22) => (A(From31), C(From31)),
            Interconnect(23) => (B(From31), D(From31)),
            Interconnect(24) => (B(From32), D(From32)),
            Interconnect(25) => (A(From32), C(From32)),
            LocalLine(0) => (B(From41), C(From41)),
            LocalLine(1) => (B(From50), D(From50)),
            LocalLine(2) => (B(From51), D(From51)),
            LocalLine(3) => (A(From41), D(From41)),
            LocalLine(4) => (A(From50), C(From50)),
            LocalLine(5) => (A(From51), C(From51)),
            LocalLine(6) => (A(From52), C(From52)),
            LocalLine(7) => (B(From42), C(From42)),
            LocalLine(8) => (A(From42), D(From42)),
            LocalLine(9) => (B(From52), D(From52)),
            _ =>
                unreachable!(),
        }
    }

    pub fn iter() -> LCInputSources {
        LCInputSources(Some(LCInputSource::Interconnect(0)))
    }
}

pub struct LCInputSources(Option<LCInputSource>);

impl Iterator for LCInputSources {
    type Item = LCInputSource;

    fn next(&mut self) -> Option<Self::Item> {
        use LCInputSource::*;
        use std::mem::replace;

        match &self.0 {
            Some(Interconnect(i @ 0..25)) => {
                let i = i + 1;

                replace(&mut self.0, Some(Interconnect(i)))
            }

            Some(Interconnect(25)) => {
                replace(&mut self.0, Some(LocalLine(0)))
            }

            Some(LocalLine(i @ 0..9)) => {
                let i = i + 1;

                replace(&mut self.0, Some(LocalLine(i)))
            }

            Some(LocalLine(9)) => {
                replace(&mut self.0, None)
            }

            None =>
                None,

            _ =>
                unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lc_input_source_ports() {
        use LCInputPort::*;

        for source in LCInputSource::iter() {
            let (one, two) = source.input_ports();

            match one {
                A(a) => assert_eq!(source, a.a_source()),
                B(b) => assert_eq!(source, b.b_source()),
                C(c) => assert_eq!(source, c.c_source()),
                D(d) => assert_eq!(source, d.d_source()),
            }

            match two {
                A(a) => assert_eq!(source, a.a_source()),
                B(b) => assert_eq!(source, b.b_source()),
                C(c) => assert_eq!(source, c.c_source()),
                D(d) => assert_eq!(source, d.d_source()),
            }
        }

        for input in LCInput::iter() {
            assert_eq!(A(input), input.a_source().input_ports().0);
            assert_eq!(B(input), input.b_source().input_ports().0);
            assert_eq!(C(input), input.c_source().input_ports().1);
            assert_eq!(D(input), input.d_source().input_ports().1);
        }
    }
}

