
macro_rules! interconnect_port {
    ($interconnect:ident . source $ff:tt) => {
        $interconnect.source $ff .expect("source")
    };
    ($interconnect:ident.first()) => {
        $interconnect.sources().next().expect("first").1
    };
    ($interconnect:ident.last()) => {
        $interconnect.sources().last().expect("last").1
    };
}

macro_rules! assert_device {
    ($device:ident) => {};
    ($device:ident
        c4_interconnect($x:literal, $y:literal, $i:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.c4_interconnect(
                X($x), Y($y), $i,
            ).expect("c4_interconnect");
            assert_eq!(
                Port::C4Interconnect { x: X($x), y: Y($y), i: $i },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        global_input($global:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.global_input($global);
            assert_eq!(
                Port::GlobalInput { global: $global },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        global_pin($global:ident) => None;
        $($tail:tt)*
    ) => {
        assert_eq!(None, $device.global_pin($global));
        assert_device!($device $($tail)*);
    };
    ($device:ident
        global_pin($global:ident) => Some($x:literal, $y:literal, $n:ident);
        $($tail:tt)*
    ) => {
        assert_eq!(
            Some(PinSource::Row { x: X($x), y: Y($y), n: $n }),
            $device.global_pin($global),
        );
        assert_device!($device $($tail)*);
    };
    ($device:ident
        io_column_pin($x:literal, $y:literal, $n:ident) => $pin:literal;
        $($tail:tt)*
    ) => {
        {
            assert_eq!(
                $pin,
                $device.io_column_cell(X($x), Y($y), $n)
                    .expect("io_column_cell")
                    .pin_name().as_str(),
            );
            assert_eq!(
                Some(PinSource::Column {
                    x: X($x), y: Y($y), n: $n,
                }),
                $device.pin($pin),
            );
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        io_row_pin($x:literal, $y:literal, $n:ident) => $pin:literal;
        $($tail:tt)*
    ) => {
        {
            assert_eq!(
                $pin,
                $device.io_row_cell(X($x), Y($y), $n)
                    .expect("io_row_cell")
                    .pin_name().as_str(),
            );
            assert_eq!(
                Some(PinSource::Row {
                    x: X($x), y: Y($y), n: $n,
                }),
                $device.pin($pin),
            );
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        io_column_interconnect($x:literal, $y:literal, $i:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.io_column_interconnect(
                X($x), Y($y), $i,
            ).expect("io_column_interconnect");
            assert_eq!(
                Port::IOColumnInterconnect { x: X($x), y: Y($y), i: $i },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        io_row_interconnect($x:literal, $y:literal, $i:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.io_row_interconnect(
                X($x), Y($y), $i,
            ).expect("io_row_interconnect");
            assert_eq!(
                Port::IORowInterconnect { x: X($x), y: Y($y), i: $i },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        jtag_input($input:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.jtag_input(JTAGInput::$input);
            assert_eq!(
                Port::JTAGInput { input: JTAGInput::$input },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        logic_cell($x:literal, $y:literal, $n:ident, $input:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.logic_cell(
                X($x), Y($y), $n,
                LogicCellInput::$input,
            ).expect("logic_cell");
            assert_eq!(
                Port::LogicCellInput {
                    x: X($x), y: Y($y), n: $n, input: LogicCellInput::$input,
                },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        logic_control($x:literal, $y:literal, $control:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.logic_control(
                X($x), Y($y), $control,
            ).expect("logic_control");
            assert_eq!(
                Port::LogicControl {
                    x: X($x), y: Y($y), control: $control,
                },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        logic_interconnect($x:literal, $y:literal, $i:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.logic_interconnect(
                X($x), Y($y), $i,
            ).expect("logic_interconnect");
            assert_eq!(
                Port::LogicInterconnect { x: X($x), y: Y($y), i: $i },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        r4_interconnect($x:literal, $y:literal, $i:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.r4_interconnect(
                X($x), Y($y), $i,
            ).expect("r4_interconnect");
            assert_eq!(
                Port::R4Interconnect { x: X($x), y: Y($y), i: $i },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        ufm_input($input:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.ufm_input(UFMInput::$input);
            assert_eq!(
                Port::UFMInput { input: UFMInput::$input },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
    };
    ($device:ident
        ufm_interconnect($x:literal, $y:literal, $i:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.ufm_interconnect(
                X($x), Y($y), $i,
            ).expect("ufm_interconnect");
            assert_eq!(
                Port::UFMInterconnect { x: X($x), y: Y($y), i: $i },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
    };
}

macro_rules! c4_interconnect {
    ($x:literal, $y:literal, $i:ident) => {
        Port::C4Interconnect {
            x: X($x), y: Y($y), i: $i,
        }
    };
}

macro_rules! global {
    ($global:ident) => {
        Port::Global { global: Global::$global }
    };
}

macro_rules! io_column {
    ($x:literal, $y:literal, $n:ident) => {
        Port::IOColumnCellOutput { x: X($x), y: Y($y), n: $n }
    };
}

macro_rules! io_row {
    ($x:literal, $y:literal, $n:ident) => {
        Port::IORowCellOutput { x: X($x), y: Y($y), n: $n }
    };
}

macro_rules! io_row_interconnect {
    ($x:literal, $y:literal, $i:ident) => {
        Port::IORowInterconnect {
            x: X($x), y: Y($y), i: $i,
        }
    };
}

macro_rules! jtag {
    ($jtag:ident) => {
        Port::JTAGOutput { output: JTAGOutput::$jtag }
    };
}

macro_rules! logic_cell {
    ($x:literal, $y:literal, $n:ident, $output:ident) => {
        Port::LogicCellOutput {
            x: X($x), y: Y($y), n: $n, output: LogicCellOutput::$output
        }
    };
}

macro_rules! logic_interconnect {
    ($x:literal, $y:literal, $i:ident) => {
        Port::LogicInterconnect {
            x: X($x), y: Y($y), i: $i,
        }
    };
}

macro_rules! r4_interconnect {
    ($x:literal, $y:literal, $i:ident) => {
        Port::R4Interconnect {
            x: X($x), y: Y($y), i: $i,
        }
    };
}

macro_rules! ufm {
    ($ufm:ident) => {
        Port::UFMOutput { output: UFMOutput::$ufm }
    };
}

macro_rules! ufm_interconnect {
    ($x:literal, $y:literal, $i:ident) => {
        Port::UFMInterconnect {
            x: X($x), y: Y($y), i: $i,
        }
    };
}

