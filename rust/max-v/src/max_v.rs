
pub mod config;
mod density;
mod enumerated;
mod fuse;

pub use density::*;
pub use fuse::*;

#[repr(transparent)]
pub struct X(pub u8);

#[repr(transparent)]
pub struct Y(pub u8);

enumerated! {
    enum C4InterconnectIndex {
        C4Interconnect0,
        C4Interconnect1,
        C4Interconnect2,
        C4Interconnect3,
        C4Interconnect4,
        C4Interconnect5,
        C4Interconnect6,
        C4Interconnect7,
        C4Interconnect8,
        C4Interconnect9,
        C4Interconnect10,
        C4Interconnect11,
        C4Interconnect12,
        C4Interconnect13,
    }
    struct C4InterconnectIndexOutOfRange ;
    struct C4InterconnectIndexs : Iterator ;
}

enumerated! {
    enum Control {
        Control0,
        Control1,
        Control2,
        Control3,
        Control4,
        Control5,
    }
    struct ControlOutOfRange ;
    struct Controls: Iterator ;
}

enumerated! {
    enum Global {
        Global0,
        Global1,
        Global2,
        Global3,
    }
    struct GlobalOutOfRange ;
    struct Globals: Iterator ;
}

enumerated! {
    enum IOColumnCellNumber {
        IOColumnCell0,
        IOColumnCell1,
        IOColumnCell2,
        IOColumnCell3,
    }
    struct IOColumnCellNumberOutOfRange ;
    struct IOColumnCellNumbers : Iterator ;
}

enumerated! {
    enum IOColumnInterconnectIndex {
        IOColumnInterconnect0,
        IOColumnInterconnect1,
        IOColumnInterconnect2,
        IOColumnInterconnect3,
        IOColumnInterconnect4,
        IOColumnInterconnect5,
        IOColumnInterconnect6,
        IOColumnInterconnect7,
        IOColumnInterconnect8,
        IOColumnInterconnect9,
    }
    struct IOColumnInterconnectIndexOutOfRange ;
    struct IOColumnInterconnectIndexs : Iterator ;
}

enumerated! {
    enum IORowCellNumber {
        IORowCell0,
        IORowCell1,
        IORowCell2,
        IORowCell3,
        IORowCell4,
        IORowCell5,
        IORowCell6,
    }
    struct IORowCellNumberOutOfRange ;
    struct IORowCellNumbers : Iterator ;
}

enumerated! {
    enum IORowInterconnectIndex {
        IORowInterconnect0,
        IORowInterconnect1,
        IORowInterconnect2,
        IORowInterconnect3,
        IORowInterconnect4,
        IORowInterconnect5,
        IORowInterconnect6,
        IORowInterconnect7,
        IORowInterconnect8,
        IORowInterconnect9,
        IORowInterconnect10,
        IORowInterconnect11,
        IORowInterconnect12,
        IORowInterconnect13,
        IORowInterconnect14,
        IORowInterconnect15,
        IORowInterconnect16,
        IORowInterconnect17,
    }
    struct IORowInterconnectIndexOutOfRange ;
    struct IORowInterconnectIndexs : Iterator ;
}

enumerated! {
    enum JTAGInput {
        TDO,
    }
    struct JTAGInputOutOfRange ;
    struct JTAGInputs : Iterator ;
}

enumerated! {
    enum JTAGOutput {
        TCK,
        TDI,
        TMS,
    }
    struct JTAGOutputOutOfRange ;
    struct JTAGOutputs : Iterator ;
}

enumerated! {
    enum LogicCellInput {
        LogicCellInputA,
        LogicCellInputB,
        LogicCellInputC,
        LogicCellInputD,
    }
    struct LogicCellInputOutOfRange ;
    struct LogicCellInputs : Iterator ;
}

enumerated! {
    enum LogicCellOutput {
        LogicCellLeft,
        LogicCellLocal,
        LogicCellRight,
    }
    struct LogicCellOutputOutOfRange ;
    struct LogicCellOutputs : Iterator ;
}

enumerated! {
    enum LogicCellNumber {
        LogicCell0,
        LogicCell1,
        LogicCell2,
        LogicCell3,
        LogicCell4,
        LogicCell5,
        LogicCell6,
        LogicCell7,
        LogicCell8,
        LogicCell9,
    }
    struct LogicCellNumberOutOfRange ;
    struct LogicCellNumbers : Iterator ;
}

enumerated! {
    enum LogicInterconnectIndex {
        LogicInterconnect0,
        LogicInterconnect1,
        LogicInterconnect2,
        LogicInterconnect3,
        LogicInterconnect4,
        LogicInterconnect5,
        LogicInterconnect6,
        LogicInterconnect7,
        LogicInterconnect8,
        LogicInterconnect9,
        LogicInterconnect10,
        LogicInterconnect11,
        LogicInterconnect12,
        LogicInterconnect13,
        LogicInterconnect14,
        LogicInterconnect15,
        LogicInterconnect16,
        LogicInterconnect17,
        LogicInterconnect18,
        LogicInterconnect19,
        LogicInterconnect20,
        LogicInterconnect21,
        LogicInterconnect22,
        LogicInterconnect23,
        LogicInterconnect24,
        LogicInterconnect25,
    }
    struct LogicInterconnectIndexOutOfRange ;
    struct LogicInterconnectIndexs : Iterator ;
}

enumerated! {
    enum LUTBit {
        LUTBit0000,
        LUTBit1000,
        LUTBit0100,
        LUTBit1100,
        LUTBit0010,
        LUTBit1010,
        LUTBit0110,
        LUTBit1110,
        LUTBit0001,
        LUTBit1001,
        LUTBit0101,
        LUTBit1101,
        LUTBit0011,
        LUTBit1011,
        LUTBit0111,
        LUTBit1111,
    }
    struct LUTBitOutOfRange ;
    struct LUTBits: Iterator ;
}

enumerated! {
    enum R4InterconnectIndex {
        R4Interconnect0,
        R4Interconnect1,
        R4Interconnect2,
        R4Interconnect3,
        R4Interconnect4,
        R4Interconnect5,
        R4Interconnect6,
        R4Interconnect7,
        R4Interconnect8,
        R4Interconnect9,
        R4Interconnect10,
        R4Interconnect11,
        R4Interconnect12,
        R4Interconnect13,
        R4Interconnect14,
        R4Interconnect15,
    }
    struct R4InterconnectIndexOutOfRange ;
    struct R4InterconnectIndexs : Iterator ;
}

enumerated! {
    enum Select3 {
        Select3_0,
        Select3_1,
        Select3_2,
    }
    struct Select3OutOfRange ;
    struct Select3s: Iterator ;
}

enumerated! {
    enum Select4 {
        Select4_0,
        Select4_1,
        Select4_2,
        Select4_3,
    }
    struct Select4OutOfRange ;
    struct Select4s: Iterator ;
}

pub struct Select4Global;

enumerated! {
    enum Select6 {
        Select6_0,
        Select6_1,
        Select6_2,
        Select6_3,
        Select6_4,
        Select6_5,
    }
    struct Select6OutOfRange ;
    struct Select6s: Iterator ;
}

enumerated! {
    enum UFMInput {
        ArClk,
        ArIn,
        ArShift,
        DrClk,
        DrIn,
        DrShift,
        Erase,
        OscEna,
        Program,
    }
    struct UFMInputOutOfRange ;
    struct UFMInputs : Iterator ;
}

enumerated! {
    enum UFMOutput {
        ArOut,
        Busy,
        DrOut,
        Osc,
    }
    struct UFMOutputOutOfRange ;
    struct UFMOutputs : Iterator ;
}

enumerated! {
    enum UFMInterconnectIndex {
        UFMInterconnect0,
        UFMInterconnect1,
        UFMInterconnect2,
        UFMInterconnect3,
        UFMInterconnect4,
        UFMInterconnect5,
        UFMInterconnect6,
        UFMInterconnect7,
        UFMInterconnect8,
        UFMInterconnect9,
    }
    struct UFMInterconnectIndexOutOfRange ;
    struct UFMInterconnectIndexs : Iterator ;
}
enumerated! {
    enum UserCodeBit {
        UserCodeBit0,
        UserCodeBit1,
        UserCodeBit2,
        UserCodeBit3,
        UserCodeBit4,
        UserCodeBit5,
        UserCodeBit6,
        UserCodeBit7,
        UserCodeBit8,
        UserCodeBit9,
        UserCodeBit10,
        UserCodeBit11,
        UserCodeBit12,
        UserCodeBit13,
        UserCodeBit14,
        UserCodeBit15,
        UserCodeBit16,
        UserCodeBit17,
        UserCodeBit18,
        UserCodeBit19,
        UserCodeBit20,
        UserCodeBit21,
        UserCodeBit22,
        UserCodeBit23,
        UserCodeBit24,
        UserCodeBit25,
        UserCodeBit26,
        UserCodeBit27,
        UserCodeBit28,
        UserCodeBit29,
        UserCodeBit30,
        UserCodeBit31,
    }
    struct UserCodeBitOutOfRange ;
    struct UserCodeBits : Iterator ;
}

pub enum Port {
    C4Interconnect {
        x: X,
        y: Y,
        i: C4InterconnectIndex,
    },
    Global {
        global: Global,
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
    UFMOutput {
        output: UFMOutput,
    },
    Unknown,
}

