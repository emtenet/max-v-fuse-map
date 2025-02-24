use crate::*;

#[derive(Copy, Clone)]
#[derive(Default)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
pub enum Port {
    C4Interconnect {
        x: X,
        y: Y,
        i: C4InterconnectIndex,
    },
    Global {
        global: Global,
    },
    GlobalInput {
        global: Global,
    },
    IOColumnCellInput {
        x: X,
        y: Y,
        n: IOColumnCellNumber,
        input: IOCellInput,
    },
    IOColumnCellOutput {
        x: X,
        y: Y,
        n: IOColumnCellNumber,
    },
    IOColumnInterconnect {
        x: X,
        y: Y,
        i: IOColumnInterconnectIndex,
    },
    IORowCellInput {
        x: X,
        y: Y,
        n: IORowCellNumber,
        input: IOCellInput,
    },
    IORowCellOutput {
        x: X,
        y: Y,
        n: IORowCellNumber,
    },
    IORowInterconnect {
        x: X,
        y: Y,
        i: IORowInterconnectIndex,
    },
    JTAGInput {
        input: JTAGInput,
    },
    JTAGOutput {
        output: JTAGOutput,
    },
    LogicCellInput {
        x: X,
        y: Y,
        n: LogicCellNumber,
        input: LogicCellInput,
    },
    LogicCellOutput {
        x: X,
        y: Y,
        n: LogicCellNumber,
        output: LogicCellOutput,
    },
    LogicControl {
        x: X,
        y: Y,
        control: Control,
    },
    LogicInterconnect {
        x: X,
        y: Y,
        i: LogicInterconnectIndex,
    },
    R4Interconnect {
        x: X,
        y: Y,
        i: R4InterconnectIndex,
    },
    UFMInput {
        input: UFMInput,
    },
    UFMInterconnect {
        x: X,
        y: Y,
        i: UFMInterconnectIndex,
    },
    UFMOutput {
        output: UFMOutput,
    },
    #[default]
    Unknown,
}

impl Port {
    pub fn kind(&self) -> Option<PortKind> {
        match self {
            Port::C4Interconnect { .. } => Some(PortKind::C4Interconnect),
            Port::Global { .. } => Some(PortKind::Global),
            Port::GlobalInput { .. } => Some(PortKind::GlobalInput),
            Port::IOColumnCellInput { .. } => Some(PortKind::IOCellInput),
            Port::IOColumnCellOutput { .. } => Some(PortKind::IOCellOutput),
            Port::IOColumnInterconnect { .. } => Some(PortKind::IOInterconnect),
            Port::IORowCellInput { .. } => Some(PortKind::IOCellInput),
            Port::IORowCellOutput { .. } => Some(PortKind::IOCellOutput),
            Port::IORowInterconnect { .. } => Some(PortKind::IOInterconnect),
            Port::JTAGInput { .. } => Some(PortKind::JTAGInput),
            Port::JTAGOutput { .. } => Some(PortKind::JTAGOutput),
            Port::LogicCellInput { .. } => Some(PortKind::LogicCellInput),
            Port::LogicCellOutput { .. } => Some(PortKind::LogicCellOutput),
            Port::LogicControl { .. } => Some(PortKind::LogicControl),
            Port::LogicInterconnect { .. } => Some(PortKind::LogicInterconnect),
            Port::R4Interconnect { .. } => Some(PortKind::R4Interconnect),
            Port::UFMInput { .. } => Some(PortKind::UFMInput),
            Port::UFMInterconnect { .. } => Some(PortKind::UFMInterconnect),
            Port::UFMOutput { .. } => Some(PortKind::UFMOutput),
            Port::Unknown => None,
        }
    }

    pub fn x(&self) -> Option<X> {
        match self {
            Port::C4Interconnect { x, .. } |
            Port::IOColumnCellInput { x, .. } |
            Port::IOColumnCellOutput { x, .. } |
            Port::IOColumnInterconnect { x, .. } |
            Port::IORowCellInput { x, .. } |
            Port::IORowCellOutput { x, .. } |
            Port::IORowInterconnect { x, .. } |
            Port::LogicCellInput { x, .. } |
            Port::LogicCellOutput { x, .. } |
            Port::LogicControl { x, .. } |
            Port::LogicInterconnect { x, .. } |
            Port::R4Interconnect { x, .. } |
            Port::UFMInterconnect { x, .. } =>
                Some(*x),
            _ =>
                None,
        }
    }

    pub fn y(&self) -> Option<Y> {
        match self {
            Port::C4Interconnect { y, .. } |
            Port::IOColumnCellInput { y, .. } |
            Port::IOColumnCellOutput { y, .. } |
            Port::IOColumnInterconnect { y, .. } |
            Port::IORowCellInput { y, .. } |
            Port::IORowCellOutput { y, .. } |
            Port::IORowInterconnect { y, .. } |
            Port::LogicCellInput { y, .. } |
            Port::LogicCellOutput { y, .. } |
            Port::LogicControl { y, .. } |
            Port::LogicInterconnect { y, .. } |
            Port::R4Interconnect { y, .. } |
            Port::UFMInterconnect { y, .. } =>
                Some(*y),
            _ =>
                None,
        }
    }

    pub fn i(&self) -> Option<usize> {
        match self {
            Port::C4Interconnect { i, .. } => Some(i.index()),
            Port::Global { global } => Some(global.index()),
            Port::GlobalInput { global } => Some(global.index()),
            Port::IOColumnCellInput { n, .. } => Some(n.index()),
            Port::IOColumnCellOutput { n, .. } => Some(n.index()),
            Port::IOColumnInterconnect { i, .. } => Some(i.index()),
            Port::IORowCellInput { n, .. } => Some(n.index()),
            Port::IORowCellOutput { n, .. } => Some(n.index()),
            Port::IORowInterconnect { i, .. } => Some(i.index()),
            Port::LogicCellInput { n, .. } => Some(n.index()),
            Port::LogicCellOutput { n, .. } => Some(n.index()),
            Port::LogicControl { control, .. } => Some(control.index()),
            Port::LogicInterconnect { i, .. } => Some(i.index()),
            Port::R4Interconnect { i, .. } => Some(i.index()),
            Port::UFMInterconnect { i, .. } => Some(i.index()),
            _ =>
                None,
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Eq, PartialEq)]
pub enum PortKind {
    C4Interconnect,
    Global,
    GlobalInput,
    IOCellInput,
    IOCellOutput,
    IOInterconnect,
    JTAGInput,
    JTAGOutput,
    LogicCellInput,
    LogicCellOutput,
    LogicControl,
    LogicInterconnect,
    R4Interconnect,
    UFMInput,
    UFMInterconnect,
    UFMOutput,
}

impl std::str::FromStr for PortKind {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        match s {
            "c4" => Ok(Self::C4Interconnect),
            "global" => Ok(Self::Global),
            "io" => Ok(Self::IOCellOutput),
            "jtag" => Ok(Self::JTAGOutput),
            "logic" => Ok(Self::LogicCellOutput),
            "r4" => Ok(Self::R4Interconnect),
            "ufm" => Ok(Self::UFMOutput),
            _ =>
                anyhow::bail!("invalid PortKind `{s}`"),
        }
    }
}

impl std::fmt::Display for PortKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::C4Interconnect => f.pad("C4"),
            Self::Global => f.pad("G"),
            Self::GlobalInput => f.pad("GlobalInput"),
            Self::IOCellInput => f.pad("IOCellInput"),
            Self::IOCellOutput => f.pad("IO"),
            Self::IOInterconnect => f.pad("I"),
            Self::JTAGInput => f.pad("JTAGInput"),
            Self::JTAGOutput => f.pad("J"),
            Self::LogicCellInput => f.pad("LogicCellInput"),
            Self::LogicCellOutput => f.pad("LC"),
            Self::LogicControl => f.pad("LogicControl"),
            Self::LogicInterconnect => f.pad("I"),
            Self::R4Interconnect => f.pad("R4"),
            Self::UFMInput => f.pad("UFMInput"),
            Self::UFMInterconnect => f.pad("I"),
            Self::UFMOutput => f.pad("UFM"),
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Eq, PartialEq)]
pub enum PortAxis {
    Kind,
    X,
    Y,
    I,
}

impl std::str::FromStr for PortAxis {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        match s {
            "kind" => Ok(Self::Kind),
            "x" => Ok(Self::X),
            "y" => Ok(Self::Y),
            "i" => Ok(Self::I),
            _ =>
                anyhow::bail!("invalid PortAxis `{s}`"),
        }
    }
}

