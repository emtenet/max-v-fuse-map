-module(generate_pins).

-export([run/0]).

% Generate files:
%
%   src/pin.erl
%   src/<device>.erl
%
% with pin information extracted from BSDL files.
%
% Pins are annotated by their IOC with experiments and reading
% the RCF file.

-include("max_v.hrl").

%%====================================================================
%% run
%%====================================================================

run() ->
    Devices = lists:map(fun bsdl/1, device:list()),
    Pins = merge(Devices),
    pin_file(Pins),
    lists:foreach(fun device_file/1, Devices).

%%--------------------------------------------------------------------

merge(Devices) ->
    lists:usort(lists:flatten([
        Pins
        ||
        {_, Pins} <- Devices
    ])).

%%====================================================================
%% bsdl
%%====================================================================

bsdl(Device) ->
    Name0 = device:name(Device),
    Size = byte_size(Name0) - 2,
    <<Name:Size/binary, "C5">> = Name0,
    File = <<"../bsdl/", Name/binary, ".bsdl">>,
    {ok, Data} = file:read_file(File),
    Lines = binary:split(Data, <<"\r\n">>, [global]),
    {Device, bsdl(Lines, [])}.

%%--------------------------------------------------------------------

bsdl([], Pins) ->
    lists:sort(Pins);
bsdl([<<"  --BSC group ", Line/binary>> | Lines], Pins) ->
    %  --BSC group 4 for I/O pin G2
    %  --BSC group 144 for unused pad
    case binary:split(Line, <<" for ">>) of
        [_, <<"unused pad">>] ->
            bsdl(Lines, Pins);

        [_, <<"I/O pin ", Pin/binary>>] ->
            bsdl(Lines, [bsdl_pin(Pin) | Pins])
    end;
bsdl([_ | Lines], Pins) ->
    bsdl(Lines, Pins).

%%--------------------------------------------------------------------

bsdl_pin(Coord = <<U, Number0/binary>>) when U >= $A andalso U =< $Z ->
    Number = binary_to_integer(Number0),
    Sort = (1000 * (U + 1 - $A)) + Number,
    Name = <<"PIN_", Coord/binary>>,
    L = U + $a - $A,
    Enum = <<L, Number0/binary>>,
    {Sort, Enum, Name};
bsdl_pin(Number) ->
    Sort = binary_to_integer(Number),
    Name = <<"PIN_", Number/binary>>,
    Enum = <<"pin", Number/binary>>,
    {Sort, Enum, Name}.

%%====================================================================
%% pin_file
%%====================================================================

pin_file(Pins) ->
    Data = [<<
        "-module(pin).\n"
        "\n"
        "-export([name/1]).\n"
        "\n"
        "-export_type([pin/0]).\n"
        "\n"
        "-type pin() ::\n">>,
        pin_type(Pins, []), <<
        "\n"
        "-spec name(pin()) -> binary().\n"
        "\n">>,
        pin_func(Pins, []), <<
        "\n"
    >>],
    File = "src/pin.erl",
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------

pin_type([], [[Line, <<" |\n">>] | Lines]) ->
    lists:reverse(Lines, [Line, <<".\n">>]);
pin_type([{_, Enum, _} | Pins], Lines) ->
    Line = [
        <<"    ", Enum/binary>>,
        <<" |\n">>
    ],
    pin_type(Pins, [Line | Lines]).

%%--------------------------------------------------------------------

pin_func([], [[Line, <<";\n">>] | Lines]) ->
    lists:reverse(Lines, [Line, <<".\n">>]);
pin_func([{_, Enum, Name} | Pins], Lines) ->
    Line = [
        <<"name(", Enum/binary, ") -> <<\"", Name/binary, "\">>">>,
        <<";\n">>
    ],
    pin_func(Pins, [Line | Lines]).

%%====================================================================
%% device_file
%%====================================================================

device_file({Device, BSDLPins}) ->
    Density = device:density(Device),
    Metric = density:metric(Density),
    Pins = pins(Device, BSDLPins),
    Data = [<<
        "-module(">>, atom_to_binary(Device), <<").\n"
        "\n"
        "-export([iocs/0]).\n"
        "-export([iocs/1]).\n"
        "-export([pins/0]).\n"
        "-export([top_iocs/1]).\n"
        "-export([top_pins/1]).\n"
        "-export([left_iocs/1]).\n"
        "-export([left_pins/1]).\n"
        "-export([right_iocs/1]).\n"
        "-export([right_pins/1]).\n"
        "-export([bottom_iocs/1]).\n"
        "-export([bottom_pins/1]).\n"
        "\n"
        "-type iob() :: iob:iob().\n"
        "-type ioc() :: ioc:ioc().\n"
        "-type pin() :: pin:pin().\n"
        "-type x() :: max_v:x().\n"
        "-type y() :: max_v:y().\n"
        "\n"
        "-spec iocs() -> [{ioc(), pin()}].\n"
        "\n"
        "iocs() ->\n">>,
        device_iocs(Pins, []), <<
        "\n"
        "-spec iocs(iob()) -> [{ioc(), pin()}].\n"
        "\n">>,
        device_iobs(Density, Pins), <<
        "\n"
        "-spec pins() -> [pin()].\n"
        "\n"
        "pins() ->\n">>,
        device_pins(Pins, []), <<
        "\n">>,
        side_iocs(Pins, Metric, <<"top">>, <<"x()">>,
                  fun device_columns/1, fun device_top/3),
        side_pins(Pins, Metric, <<"top">>, <<"x()">>,
                  fun device_columns/1, fun device_top/3),
        side_iocs(Pins, Metric, <<"left">>, <<"y()">>,
                  fun device_rows/1, fun device_left/3),
        side_pins(Pins, Metric, <<"left">>, <<"y()">>,
                  fun device_rows/1, fun device_left/3),
        side_iocs(Pins, Metric, <<"right">>, <<"y()">>,
                  fun device_rows/1, fun device_right/3),
        side_pins(Pins, Metric, <<"right">>, <<"y()">>,
                  fun device_rows/1, fun device_right/3),
        side_iocs(Pins, Metric, <<"bottom">>, <<"x()">>,
                  fun device_columns/1, fun device_bottom/3),
        side_pins(Pins, Metric, <<"bottom">>, <<"x()">>,
                  fun device_columns/1, fun device_bottom/3)
    ],
    Name = lists:flatten(io_lib:format("~s.erl", [Device])),
    File = filename:join("src", Name),
    ok = file:write_file(File, Data).

%%--------------------------------------------------------------------

device_iobs(Density, Pins) ->
    IOBs = density:iobs(Density),
    device_iobs_clauses(IOBs, Pins, []).

%%--------------------------------------------------------------------

device_iobs_clauses([{IOB, _}], Pins, Clauses) ->
    Clause = device_iobs_clause(IOB, Pins, <<".\n">>),
    lists:reverse(Clauses, [Clause]);
device_iobs_clauses([{IOB, _} | IOBs], Pins, Clauses) ->
    Clause = device_iobs_clause(IOB, Pins, <<";\n">>),
    device_iobs_clauses(IOBs, Pins, [Clause | Clauses]).

%%--------------------------------------------------------------------

device_iobs_clause(IOB = {iob, XX, YY}, Pins0, End) ->
    Pins = lists:filter(fun ({{ioc, X, Y, _}, _}) ->
        X =:= XX andalso Y =:= YY
    end, Pins0),
    [
        io_lib:format("iocs(~p) ->~n", [IOB]),
        side_iocs_lines(Pins),
        End
    ].

%%--------------------------------------------------------------------

device_iocs([], [[Indent, Last, <<",\n">>] | Lines0]) ->
    Lines1 = lists:reverse(Lines0, [Indent, Last, <<"\n    ].\n">>]),
    [[<<"     ">>, First, <<",\n">>] | Lines] = Lines1,
    [<<"    [">>, First, <<",\n">> | Lines];
device_iocs([Pin | Pins], Lines) ->
    Line = device_ioc(<<"     ">>, Pin, <<",\n">>),
    device_iocs(Pins, [Line | Lines]).

%%--------------------------------------------------------------------

device_ioc(Head, {IOC, Enum}, Tail) ->
    [Head, io_lib:format("{~w,~s}", [IOC, Enum]), Tail].

%%--------------------------------------------------------------------

device_pins([], [[Indent, Last, <<",\n">>] | Lines0]) ->
    Lines1 = lists:reverse(Lines0, [Indent, Last, <<"\n    ].\n">>]),
    [[<<"     ">>, First, <<",\n">>] | Lines] = Lines1,
    [<<"    [">>, First, <<",\n">> | Lines];
device_pins([Pin | Pins], Lines) ->
    Line = device_pin(<<"     ">>, Pin, <<",\n">>),
    device_pins(Pins, [Line | Lines]).

%%--------------------------------------------------------------------

device_pin(Head, {_, Enum}, Tail) ->
    [Head, io_lib:format("~s", [Enum]), Tail].

%%--------------------------------------------------------------------

device_columns(#metric{left_lab = Left, right_lab = Right}) ->
    {Left, Right}.

%%--------------------------------------------------------------------

device_rows(#metric{bottom_lab = Bottom, top_lab = Top}) ->
    {Bottom, Top}.

%%--------------------------------------------------------------------

device_top(Pins, #metric{top_io = Top}, At) ->
    lists:filter(fun ({{ioc, X, Y, _}, _}) ->
        X =:= At andalso Y =:= Top
    end, Pins).

%%--------------------------------------------------------------------

device_left(Pins, #metric{left_io = Left, indent_left_io = Indent}, At) ->
    lists:filter(fun ({{ioc, X, Y, _}, _}) ->
        (X =:= Left orelse X =:= Indent) andalso Y =:= At
    end, Pins).

%%--------------------------------------------------------------------

device_right(Pins, #metric{right_io = Right}, At) ->
    lists:filter(fun ({{ioc, X, Y, _}, _}) ->
        X =:= Right andalso Y =:= At
    end, Pins).

%%--------------------------------------------------------------------

device_bottom(Pins, #metric{bottom_io = Bottom, indent_bottom_io = Indent}, At) ->
    lists:filter(fun ({{ioc, X, Y, _}, _}) ->
        X =:= At andalso (Y =:= Bottom orelse Y =:= Indent)
    end, Pins).

%%--------------------------------------------------------------------

side_iocs(Pins, Metric, Name, Type, Range, Filter) ->
    {Min, Max} = Range(Metric),
    [<<
        "-spec ", Name/binary, "_iocs(", Type/binary, ") -> [{ioc(), pin()}].\n"
        "\n">>,
        side_iocs_clauses(Pins, Metric, Name, Min, Max, Filter, []), <<
        "\n"
    >>].

%%--------------------------------------------------------------------

side_iocs_clauses(Pins, Metric, Name, At, At, Filter, Clauses) ->
    Clause = side_iocs_clause(Pins, Metric, Name, At, Filter, <<".\n">>),
    lists:reverse(Clauses, [Clause]);
side_iocs_clauses(Pins, Metric, Name, At, Max, Filter, Clauses) ->
    Clause = side_iocs_clause(Pins, Metric, Name, At, Filter, <<";\n">>),
    side_iocs_clauses(Pins, Metric, Name, At + 1, Max, Filter, [Clause | Clauses]).

%%--------------------------------------------------------------------

side_iocs_clause(Pins0, Metric, Name, At, Filter, End) ->
    Pins = Filter(Pins0, Metric, At),
    Value = integer_to_binary(At),
    [
        <<Name/binary, "_iocs(", Value/binary, ") ->\n">>,
        side_iocs_lines(Pins),
        End
    ].

%%--------------------------------------------------------------------

side_iocs_lines([]) ->
    <<"    []">>;
side_iocs_lines([Pin]) ->
    device_ioc(<<"    [">>, Pin, <<"]">>);
side_iocs_lines(Pins) ->
    side_iocs_lines(Pins, <<"    [">>, []).

%%--------------------------------------------------------------------

side_iocs_lines([Pin], Head, Lines) ->
    Line = device_ioc(Head, Pin, <<"]">>),
    lists:reverse(Lines, [Line]);
side_iocs_lines([Pin | Pins], Head, Lines) ->
    Line = device_ioc(Head, Pin, <<",\n">>),
    side_iocs_lines(Pins, <<"     ">>, [Line | Lines]).

%%--------------------------------------------------------------------

side_pins(Pins, Metric, Name, Type, Range, Filter) ->
    {Min, Max} = Range(Metric),
    [<<
        "-spec ", Name/binary, "_pins(", Type/binary, ") -> [pin()].\n"
        "\n">>,
        side_pins_clauses(Pins, Metric, Name, Min, Max, Filter, []), <<
        "\n"
    >>].

%%--------------------------------------------------------------------

side_pins_clauses(Pins, Metric, Name, At, At, Filter, Clauses) ->
    Clause = side_pins_clause(Pins, Metric, Name, At, Filter, <<".\n">>),
    lists:reverse(Clauses, [Clause]);
side_pins_clauses(Pins, Metric, Name, At, Max, Filter, Clauses) ->
    Clause = side_pins_clause(Pins, Metric, Name, At, Filter, <<";\n">>),
    side_pins_clauses(Pins, Metric, Name, At + 1, Max, Filter, [Clause | Clauses]).

%%--------------------------------------------------------------------

side_pins_clause(Pins0, Metric, Name, At, Filter, End) ->
    Pins = Filter(Pins0, Metric, At),
    Value = integer_to_binary(At),
    [
        <<Name/binary, "_pins(", Value/binary, ") ->\n">>,
        side_pins_lines(Pins),
        End
    ].

%%--------------------------------------------------------------------

side_pins_lines([]) ->
    <<"    []">>;
side_pins_lines([Pin0 | Pins]) ->
    Line = device_pin(<<"    [">>, Pin0, <<>>),
    Lines = lists:map(fun (Pin) ->
        device_pin(<<", ">>, Pin, <<>>)
    end, Pins),
    [Line, Lines, <<"]">>].

%%====================================================================
%% pins
%%====================================================================

pins(Device, BSDLPins) ->
    io:format(" => ~s pins~n", [Device]),
    Sources = [
        pin_source(Device, Enum, Name)
        ||
        {_, Enum, Name} <- BSDLPins
    ],
    {ok, Experiments} = experiment:compile_to_rcf(Sources),
    Pins = lists:map(fun pin/1, Experiments),
    Sorted = pins_sorted(device:density(Device), Pins),
    Count = length(Pins),
    Count = length(Sorted),
    Sorted.

%%--------------------------------------------------------------------

pin({Enum, RCF}) ->
    Pin = binary_to_atom(Enum),
    IOC = pin_ioc(RCF),
    {IOC, Pin}.

%%--------------------------------------------------------------------

pin_ioc(RCF) ->
    #{signals := #{lut := #{dests := [Dest]}}} = RCF,
    #{ioc := IOC, port := data_in} = Dest,
    IOC.

%%--------------------------------------------------------------------

pin_source(Device, Enum, Name) ->
    #{
        title => Enum,
        device => Device,
        settings => [
            % set_location_assignment LC_X12_Y4_N1 -to lut
            {raw, <<"set_location_assignment -to q ", Name/binary, "\n">>}
        ],
        verilog => <<
            "module experiment (\n"
            "  output wire q\n"
            ");\n"
            "  lcell lut (.in(0), .out(q));\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

pins_sorted(Density, Pins) ->
    IOBs = lists:reverse(density:iobs(Density)),
    pins_sorted(IOBs, Density, Pins, []).

%%--------------------------------------------------------------------

pins_sorted([], _, _, Sorted) ->
    Sorted;
pins_sorted([{{iob, XX, YY}, _} | IOBs], Density, Pins, Sorted) ->
    Block = lists:sort(lists:filter(fun ({{ioc, X, Y, _}, _}) ->
        X =:= XX andalso Y =:= YY
    end, Pins)),
    case is_reverse_iob(XX, YY, Density) of
        true ->
            pins_sorted(IOBs, Density, Pins, lists:reverse(Block, Sorted));

        false ->
            pins_sorted(IOBs, Density, Pins, Block ++ Sorted)
    end.

%%--------------------------------------------------------------------

is_reverse_iob(X, Y, Density) ->
    case density:is_right_iob(X, Y, Density) of
        true ->
            true;

        false ->
            density:is_bottom_iob(X, Y, Density)
    end.

