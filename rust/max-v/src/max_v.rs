use std::ops::{Add, Sub};

pub mod config;
mod density;
mod device;
mod enumerated;
mod enums;
mod fuse;

pub use density::*;
pub use device::*;
pub use enums::*;
pub use fuse::*;

enumerated_str! {
    enum Density {
        Density240 = "5M240Z",
        Density570 = "5M570Z",
        Density1270 = "5M1270Z",
        Density2210 = "5M2210Z",
    }
    struct DensityOutOfRange ;
    struct Densitys : Iterator ;
}

impl Density {
    pub fn layout(self) -> &'static DensityLayout {
        match self {
            Density::Density240 => &MAX_V_240Z,
            Density::Density570 => &MAX_V_570Z,
            Density::Density1270 => &MAX_V_1270Z,
            Density::Density2210 => &MAX_V_2210Z,
        }
    }
}

enumerated_str! {
    enum Device {
        Device40E64 = "5M40ZE64",
        Device40M64 = "5M40ZM64",
        Device80E64 = "5M80ZE64",
        Device80M64 = "5M80ZM64",
        Device80M68 = "5M80ZM68",
        Device80T100 = "5M80ZT100",
        Device160E64 = "5M160ZE64",
        Device160M68 = "5M160ZM68",
        Device160M100 = "5M160ZM100",
        Device160T100 = "5M160ZT100",
        Device240M68 = "5M240ZM68",
        Device240M100 = "5M240ZM100",
        Device240T100 = "5M240ZT100",
        Device240T144 = "5M240ZT144",
        Device570M100 = "5M570ZM100",
        Device570T100 = "5M570ZT100",
        Device570T144 = "5M570ZT144",
        Device570F256 = "5M570ZF256",
        Device1270T144 = "5M1270ZT144",
        Device1270F256 = "5M1270ZF256",
        Device1270F324 = "5M1270ZF324",
        Device2210F256 = "5M2210ZF256",
        Device2210F324 = "5M2210ZF324",
    }
    struct DeviceOutOfRange ;
    struct Devices : Iterator ;
}

impl Device {
    pub fn density(self) -> Density {
        match self {
            Device::Device40E64 => Density::Density240,
            Device::Device40M64 => Density::Density240,
            Device::Device80E64 => Density::Density240,
            Device::Device80M64 => Density::Density240,
            Device::Device80M68 => Density::Density240,
            Device::Device80T100 => Density::Density240,
            Device::Device160E64 => Density::Density240,
            Device::Device160M68 => Density::Density240,
            Device::Device160M100 => Density::Density240,
            Device::Device160T100 => Density::Density240,
            Device::Device240M68 => Density::Density240,
            Device::Device240M100 => Density::Density240,
            Device::Device240T100 => Density::Density240,
            Device::Device240T144 => Density::Density570,
            Device::Device570M100 => Density::Density570,
            Device::Device570T100 => Density::Density570,
            Device::Device570T144 => Density::Density570,
            Device::Device570F256 => Density::Density570,
            Device::Device1270T144 => Density::Density1270,
            Device::Device1270F256 => Density::Density1270,
            Device::Device1270F324 => Density::Density2210,
            Device::Device2210F256 => Density::Density2210,
            Device::Device2210F324 => Density::Density2210,
        }
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
#[derive(Hash)]
#[derive(Ord, PartialOrd)]
#[repr(transparent)]
pub struct X(pub u8);

impl std::fmt::Display for X {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<u8> for X {
    fn from(x: u8) -> X {
        X(x)
    }
}

impl From<X> for usize {
    fn from(x: X) -> usize {
        usize::from(x.0)
    }
}

impl From<X> for i64 {
    fn from(x: X) -> i64 {
        i64::from(x.0)
    }
}

impl From<X> for u64 {
    fn from(x: X) -> u64 {
        u64::from(x.0)
    }
}

impl Add<u8> for X {
    type Output = Self;

    fn add(self, with: u8) -> Self {
        X(self.0 + with)
    }
}

impl Sub<u8> for X {
    type Output = Self;

    fn sub(self, with: u8) -> Self {
        X(self.0 - with)
    }
}

impl Sub<X> for X {
    type Output = Self;

    fn sub(self, with: X) -> Self {
        X(self.0 - with.0)
    }
}

impl PartialEq<u8> for X {
    fn eq(&self, with: &u8) -> bool {
        self.0.eq(with)
    }
}

impl PartialOrd<u8> for X {
    fn partial_cmp(&self, with: &u8) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(with)
    }
}

#[derive(Copy, Clone)]
#[derive(Debug)]
#[derive(Eq, PartialEq)]
#[derive(Hash)]
#[derive(Ord, PartialOrd)]
#[repr(transparent)]
pub struct Y(pub u8);

impl std::fmt::Display for Y {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<u8> for Y {
    fn from(y: u8) -> Y {
        Y(y)
    }
}

impl From<Y> for usize {
    fn from(y: Y) -> usize {
        usize::from(y.0)
    }
}

impl From<Y> for i64 {
    fn from(y: Y) -> i64 {
        i64::from(y.0)
    }
}

impl From<Y> for u64 {
    fn from(y: Y) -> u64 {
        u64::from(y.0)
    }
}

impl Add<u8> for Y {
    type Output = Self;

    fn add(self, with: u8) -> Self {
        Y(self.0 + with)
    }
}

impl Sub<u8> for Y {
    type Output = Self;

    fn sub(self, with: u8) -> Self {
        Y(self.0 - with)
    }
}

impl Sub<Y> for Y {
    type Output = Self;

    fn sub(self, with: Y) -> Self {
        Y(self.0 - with.0)
    }
}

impl PartialEq<u8> for Y {
    fn eq(&self, with: &u8) -> bool {
        self.0.eq(with)
    }
}

impl PartialOrd<u8> for Y {
    fn partial_cmp(&self, with: &u8) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(with)
    }
}

#[derive(Copy, Clone)]
#[derive(Eq, PartialEq)]
#[repr(transparent)]
pub struct PinName([u8; 4]);

impl PinName {
    pub fn as_str(&self) -> &str {
        let to = usize::from(self.0[0] + 1);
        unsafe {
            std::str::from_utf8_unchecked(&self.0[1..to])
        }
    }

    fn read(bytes: &[u8]) -> anyhow::Result<(Self, &[u8])> {
        if let Some((&l, bytes)) = bytes.split_first() {
            if l < 1 || l > 3 {
                anyhow::bail!("Pin name length: {l}")
            }
            let len = usize::from(l);
            if let Some((name, bytes)) = bytes.split_at_checked(len) {
                if std::str::from_utf8(name).is_ok() {
                    match name {
                        &[a] => Ok((PinName([1, a, 0, 0]), bytes)),
                        &[a, b] => Ok((PinName([2, a, b, 0]), bytes)),
                        &[a, b, c] => Ok((PinName([3, a, b, c]), bytes)),
                        _ => unreachable!(),
                    }
                } else {
                    anyhow::bail!("Pin name is not utf8")
                }
            } else {
                anyhow::bail!("Pin name past EOF")
            }
        } else {
            anyhow::bail!("Pin name at EOF")
        }
    }
}

impl std::borrow::Borrow<str> for PinName {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Default for PinName {
    fn default() -> Self {
        PinName([1, b'?', 0, 0])
    }
}

impl std::fmt::Debug for PinName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "PinName({})", self.as_str())
    }
}

impl std::fmt::Display for PinName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl std::hash::Hash for PinName {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        self.as_str().hash(state)
    }
}

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

