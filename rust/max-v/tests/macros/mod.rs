
macro_rules! interconnect_port {
    ($interconnect:ident . source $ff:tt) => {
        $interconnect.source $ff .unwrap()
    };
    ($interconnect:ident.first()) => {
        $interconnect.sources().next().unwrap().1
    };
    ($interconnect:ident.last()) => {
        $interconnect.sources().last().unwrap().1
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
            ).unwrap();
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
        io_column_pin($x:literal, $y:literal, $n:ident) => $pin:literal;
        $($tail:tt)*
    ) => {
        {
            assert_eq!(
                $pin,
                $device.io_column_cell(X($x), Y($y), $n).unwrap()
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
                $device.io_row_cell(X($x), Y($y), $n).unwrap()
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
        logic_cell($x:literal, $y:literal, $n:ident, $input:ident)
            . $f:ident $ff:tt
            => $port:expr;
        $($tail:tt)*
    ) => {
        {
            let interconnect = $device.logic_cell(
                X($x), Y($y), $n,
                LogicCellInput::$input,
            ).unwrap();
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
            ).unwrap();
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
            ).unwrap();
            assert_eq!(
                Port::LogicInterconnect { x: X($x), y: Y($y), i: $i },
                interconnect.port(),
            );
            let port = interconnect_port!(interconnect . $f $ff );
            assert_eq!($port, port);
        }
        assert_device!($device $($tail)*);
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

