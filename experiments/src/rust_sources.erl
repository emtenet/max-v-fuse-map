-module(rust_sources).

-export([run/0]).

-include("max_v.hrl").

-record(db, {
    density :: atom(),
    device :: atom(),
    c4s :: c4_interconnect_mux_database:blocks(),
    iobs :: iob_interconnect_mux_database:blocks(),
    labs :: lab_interconnect_mux_database:blocks(),
    r4s :: r4_interconnect_mux_database:blocks(),
    metric :: #metric{}
}).

% device interconnects
-define(GLOBAL_INTERCONNECTS, 16#ED).
-define(JTAG_INTERCONNECTS, 16#EE).
-define(UFM_INTERCONNECTS, 16#EF).

% block types
-define(CORNER_BLOCK, 16#F0).
-define(COLUMN_BLOCK, 16#F1).
-define(GROW_BLOCK, 16#F2).
-define(LEFT_BLOCK, 16#F3).
-define(LOGIC_BLOCK, 16#F4).
-define(RIGHT_BLOCK, 16#F5).
-define(UFM_BLOCK, 16#F6).

% sub-block types
-define(C4_INTERCONNECTS, 16#F9).
-define(IO_CELLS, 16#FA).
-define(IO_INTERCONNECTS, 16#FB).
-define(LOGIC_CELLS, 16#FC).
-define(LOGIC_CONTROLS, 16#FD).
-define(LOGIC_INTERCONNECTS, 16#FE).
-define(R4_INTERCONNECTS, 16#FF).

% ports
-define(C4_INTERCONNECT, 16#D0).
-define(GLOBAL, 16#D1).
-define(IO_COLUMN_CELL, 16#D2).
-define(IO_COLUMN_INTERCONNECT, 16#D3).
-define(IO_ROW_CELL, 16#D4).
-define(IO_ROW_INTERCONNECT, 16#D5).
-define(JTAG_TCK, 16#D6).
-define(JTAG_TDI, 16#D7).
-define(JTAG_TMS, 16#D8).
-define(LOGIC_CELL_LEFT, 16#D9).
-define(LOGIC_CELL_LOCAL, 16#DA).
-define(LOGIC_CELL_RIGHT, 16#DB).
-define(LOGIC_INTERCONNECT, 16#DC).
-define(R4_INTERCONNECT, 16#DD).
-define(UFM_AR_OUT, 16#DE).
-define(UFM_BUSY, 16#DF).
-define(UFM_DR_OUT, 16#E0).
-define(UFM_INTERCONNECT, 16#E1).
-define(UFM_ISP_BUSY, 16#E2).
-define(UFM_OSC, 16#E3).
-define(UNKNOWN, 16#E4).

%%====================================================================
%% run
%%====================================================================

run() ->
    device(max_v_40z_e64),
    device(max_v_40z_m64),
    device(max_v_160z_t100),
    device(max_v_240z_m100),
    device(max_v_240z_t100),
    device(max_v_240z_t144),
    device(max_v_570z_t100),
    device(max_v_570z_t144),
    device(max_v_570z_f256),
    device(max_v_1270z_t144),
    device(max_v_1270z_f324),
    device(max_v_2210z_f324),
    ok.

%%--------------------------------------------------------------------

device(Device) ->
    io:format(" ==> ~p~n", [Device]),
    Db = db(Device),
    Source = source(Db),
    Path = io_lib:format("../rust/device/~s.sources", [Device]),
    file:write_file(Path, Source).

%%--------------------------------------------------------------------

db(Device) ->
    Density = device:density(Device),
    Metric = density:metric(Density),
    {ok, C4s} = c4_interconnect_mux_database:open(Density),
    {ok, LABs} = lab_interconnect_mux_database:open(Density),
    {ok, IOBs} = iob_interconnect_mux_database:open(Density),
    {ok, R4s} = r4_interconnect_mux_database:open(Density),
    #db{
        density = Density,
        device = Device,
        c4s = C4s,
        iobs = IOBs,
        labs = LABs,
        r4s = R4s,
        metric = Metric
    }.

%%====================================================================
%% blocks
%%====================================================================

source(Db) ->
    Metric = Db#db.metric,
    Name0 = device:name(Db#db.device),
    NameSize = byte_size(Name0) - 2,
    <<Name:NameSize/binary, "C5">> = Name0,
    [
        <<"MAXV", NameSize, Name/binary>>,
        global_interconnects(Db),
        jtag_interconnects(Db),
        ufm_interconnects(Db),
        [
            block(X, Y, Db)
            ||
            X <- lists:seq(Metric#metric.left_io, Metric#metric.right_io),
            Y <- lists:seq(Metric#metric.bottom_io, Metric#metric.top_io)
        ]
    ].

%%--------------------------------------------------------------------

block(X, Y, Db) ->
    Metric = Db#db.metric,
    case density:block_type(X, Y, Metric) of
        column ->
            [<<?COLUMN_BLOCK, X, Y>>,
             c4_io_interconnects(X, Y, Db),
             io_column_cells(X, Y, Db),
             io_column_interconnects(X, Y, Db)
            ];
        global ->
            [<<?UFM_BLOCK, X, Y>>,
             c4_interconnects(X, Y, Db),
             r4_grow_interconnects(X, Y, Db)
            ];
        logic ->
            [<<?LOGIC_BLOCK, X, Y>>,
             c4_interconnects(X, Y, Db),
             logic_cells(X, Y, Db),
             logic_controls(X, Y, Db),
             logic_interconnects(X, Y, Db),
             r4_interconnects(X, Y, Db)
            ];
        row when X < 2 ->
            [<<?LEFT_BLOCK, X, Y>>,
             c4_interconnects(X, Y, Db),
             io_row_cells(X, Y, Db),
             io_row_interconnects(X, Y, Db),
             r4_left_interconnects(X, Y, Db)
            ];
        row ->
            [<<?RIGHT_BLOCK, X, Y>>,
             io_row_cells(X, Y, Db),
             io_row_interconnects(X, Y, Db),
             r4_right_interconnects(X, Y, Db)
            ];
        ufm ->
            [<<?UFM_BLOCK, X, Y>>,
             c4_interconnects(X, Y, Db),
             r4_grow_interconnects(X, Y, Db)
            ];
        false when X =:= Metric#metric.left_io andalso
                   Y =:= Metric#metric.top_io ->
            [<<?CORNER_BLOCK, X, Y>>,
             c4_io_interconnects(X, Y, [], [9, 10, 11, 12, 13], Db)
            ];
        false when X =:= Metric#metric.left_io andalso
                   Y =:= Metric#metric.indent_bottom_io ->
            [<<?CORNER_BLOCK, X, Y>>,
             c4_io_interconnects(X, Y, [], [3, 10, 11, 12, 13], Db)
            ];
        false when X =:= Metric#metric.indent_left_io andalso
                   Y =:= Metric#metric.bottom_lab ->
            [<<?GROW_BLOCK, X, Y>>,
             c4_interconnects(X, Y, Db),
             r4_grow_interconnects(X, Y, Db)
            ];
        false when X =:= Metric#metric.indent_left_io andalso
                   Y =:= Metric#metric.bottom_io ->
            [<<?CORNER_BLOCK, X, Y>>,
             c4_io_interconnects(X, Y, [], [3, 10, 11, 12, 13], Db)
            ];
        false -> <<>>
    end.

%%====================================================================
%% c4 interconnects
%%====================================================================

c4_interconnects(X, Y, Db) ->
    Interconnects = [
        c4_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 13)
    ],
    [<<?C4_INTERCONNECTS, 14>> | Interconnects].

%%--------------------------------------------------------------------

c4_interconnect(X, Y, I, Db) ->
    Source = c4_interconnect_source(X, Y, I, direct_link, Db),
    Sources = [
        c4_interconnect_source(X, Y, I, Mux4, Mux3, Db)
        ||
        Mux4 <- [mux0, mux1, mux2, mux3],
        Mux3 <- [mux0, mux1, mux2]
    ],
    [<<13>>, Source, Sources].

%%--------------------------------------------------------------------

c4_io_interconnects(X, Y, Db = #db{metric = #metric{right_lab = Right}})
        when Y > 3 andalso X =:= Right ->
    c4_io_interconnects(X, Y, [9, 10, 11, 12, 13], [0, 1, 2, 7, 8], Db);
c4_io_interconnects(X, Y, Db) when Y > 3 ->
    c4_io_interconnects(X, Y, [], [0, 1, 2, 7, 8, 9, 10, 11, 12, 13], Db);
c4_io_interconnects(X, Y, Db = #db{metric = #metric{right_lab = Right}})
        when X =:= Right ->
    c4_io_interconnects(X, Y, [3, 10, 11, 12, 13], [0, 1, 7, 8, 9], Db);
c4_io_interconnects(X, Y, Db) ->
    c4_io_interconnects(X, Y, [], [0, 1, 3, 7, 8, 9, 10, 11, 12, 13], Db).

%%--------------------------------------------------------------------

c4_io_interconnects(X, Y, I1s, I2s, Db) ->
    Count = length(I1s) + length(I2s),
    Interconnect1s = [
        c4_io_1_interconnect(X, Y, I, Db)
        ||
        I <- I1s
    ],
    Interconnect2s = [
        c4_io_2_interconnect(X, Y, I, Db)
        ||
        I <- I2s
    ],
    [<<?C4_INTERCONNECTS, Count>>, Interconnect1s, Interconnect2s].

%%--------------------------------------------------------------------

c4_io_1_interconnect(X, Y, I, Db) ->
    [<<I, 1>>,
     c4_interconnect_source(X, Y, I, io_data_in1, Db)
    ].

%%--------------------------------------------------------------------

c4_io_2_interconnect(X, Y, I, Db) ->
    [<<I, 2>>,
     c4_interconnect_source(X, Y, I, io_data_in1, Db),
     c4_interconnect_source(X, Y, I, io_data_in0, Db)
    ].

%%--------------------------------------------------------------------

c4_interconnect_source(X, Y, I, DirectLink, Db) ->
    Port = c4_interconnect_port(X, Y, I, DirectLink, Db),
    {ok, Fuse} = fuse_map:from_name(
        {{c4, X, Y}, {mux, I}, DirectLink},
        Db#db.density
    ),
    port_fuse(Port, Fuse).

%%--------------------------------------------------------------------

c4_interconnect_source(X, Y, I, Mux4, Mux3, Db) ->
    Port = c4_interconnect_port(X, Y, I, {Mux4, Mux3}, Db),
    {ok, Fuse0} = fuse_map:from_name(
        {{c4, X, Y}, {mux, I}, from4, Mux4},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{c4, X, Y}, {mux, I}, from3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1).

%%--------------------------------------------------------------------

c4_interconnect_port(X, Y, I, Mux, Db) ->
    #{{c4, X, Y} := Block} = Db#db.c4s,
    case Block of
        #{I := Interconnect} ->
            case Interconnect of
                #{Mux := From} ->
                    source_port(From, Db);

                _ ->
                    source_port(unknown, Db)
            end;

        _ ->
            source_port(unknown, Db)
    end.

%%====================================================================
%% global interconnects
%%====================================================================

global_interconnects(Db = #db{device = Device}) ->
    [A, B, C, D] = device:gclk_pins(Device),
    IOCs = device:iocs(Device),
    [<<?GLOBAL_INTERCONNECTS>>,
     global_pin(A, IOCs),
     ufm_interconnect(global, 0, {3, 3}, Db),
     global_pin(B, IOCs),
     ufm_interconnect(global, 1, {3, 3}, Db),
     global_pin(C, IOCs),
     ufm_interconnect(global, 2, {3, 3}, Db),
     global_pin(D, IOCs),
     ufm_interconnect(global, 3, {3, 3}, Db)
    ].

%%--------------------------------------------------------------------

global_pin(undefined, _) ->
    <<0>>;
global_pin(PinName, IOCs) ->
    {{ioc, X, Y, N}, _} = lists:keyfind(PinName, 2, IOCs),
    <<1, X, Y, N>>.

%%====================================================================
%% jtag interconnects
%%====================================================================

jtag_interconnects(Db) ->
    [<<?JTAG_INTERCONNECTS>>,
     ufm_interconnect(jtag, tdo, {4, 2}, Db)
    ].

%%====================================================================
%% io cells
%%====================================================================

io_column_cells(X, Y, Db) ->
    Cells = [
        io_column_cell(IOC, Db)
        ||
        IOC <- device:iocs(Db#db.device, {iob, X, Y})
    ],
    C = length(Cells),
    [<<?IO_CELLS, C>> | Cells].

%%--------------------------------------------------------------------

io_column_cell({{ioc, X, Y, N}, Pin}, Db) ->
    <<"PIN_", Name/binary>> = pin:name(Pin),
    Size = byte_size(Name),
    Output = io_column_cell_source(X, Y, N, fast_out, Db),
    Outputs = [
        io_column_cell_source(X, Y, N, I, output4, output3, Db)
        ||
        I <- lists:seq(0, 9)
    ],
    Enable = [
        io_column_cell_source(X, Y, N, I, enable4, enable3, Db)
        ||
        I <- lists:seq(0, 9)
    ],
    [<<N, Size, Name/binary, 11>>, Output, Outputs, <<10>>, Enable].

%%--------------------------------------------------------------------

io_column_cell_source(X, Y, N, fast_out, Db) ->
    Port = source_port(ioc_output_mux_map:fast_out_column({ioc, X, Y, N}), Db),
    {ok, Fuse0} = fuse_map:from_name(
        {{ioc, X, Y, N}, fast_out},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{ioc, X, Y, N}, output4, mux2},
        Db#db.density
    ),
    {ok, Fuse2} = fuse_map:from_name(
        {{ioc, X, Y, N}, output3, mux0},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1, Fuse2).

%%--------------------------------------------------------------------

io_column_cell_source(X, Y, N, I, From4, From3, Db) ->
    Port = <<?IO_COLUMN_INTERCONNECT, X, Y, I>>,
    {ok, Mux4, Mux3} = ioc_output_mux_map:from_col_interconnect(
        {interconnect, I}
    ),
    {ok, Fuse0} = fuse_map:from_name(
        {{ioc, X, Y, N}, From4, Mux4},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{ioc, X, Y, N}, From3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1).

%%--------------------------------------------------------------------

io_row_cells(X, Y, Db) ->
    Cells = [
        io_row_cell(IOC, Db)
        ||
        IOC <- device:iocs(Db#db.device, {iob, X, Y})
    ],
    C = length(Cells),
    [<<?IO_CELLS, C>> | Cells].

%%--------------------------------------------------------------------

io_row_cell({{ioc, X, Y, N}, Pin}, Db) ->
    <<"PIN_", Name/binary>> = pin:name(Pin),
    Size = byte_size(Name),
    Output = io_row_cell_source(X, Y, N, fast_out, Db),
    Outputs = [
        io_row_cell_source(X, Y, N, output6, Mux6, output3, Mux3, Db)
        ||
        Mux6 <- [mux0, mux1, mux2, mux3, mux5, mux5],
        Mux3 <- [mux0, mux1, mux2]
    ],
    Enable = [
        io_row_cell_source(X, Y, N, enable6, Mux6, enable3, Mux3, Db)
        ||
        Mux6 <- [mux0, mux1, mux2, mux3, mux5, mux5],
        Mux3 <- [mux0, mux1, mux2]
    ],
    [<<N, Size, Name/binary, 19>>, Output, Outputs, <<18>>, Enable].

%%--------------------------------------------------------------------

io_row_cell_source(X, Y, N, fast_out, Db) ->
    Port = source_port(ioc_output_mux_map:fast_out_row({ioc, X, Y, N}), Db),
    {ok, Fuse0} = fuse_map:from_name(
        {{ioc, X, Y, N}, fast_out},
        Db#db.density
    ),
    port_fuse(Port, Fuse0).

%%--------------------------------------------------------------------

io_row_cell_source(X, Y, N, From6, Mux6, From3, Mux3, Db) ->
    {interconnect, I} = ioc_output_mux_map:to_row_interconnect(Mux6, Mux3),
    Port = <<?IO_ROW_INTERCONNECT, X, Y, I>>,
    {ok, Fuse0} = fuse_map:from_name(
        {{ioc, X, Y, N}, From6, Mux6},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{ioc, X, Y, N}, From3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1).

%%====================================================================
%% io interconnects
%%====================================================================

io_column_interconnects(X, Y, Db) ->
    Interconnects = [
        io_column_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 9)
    ],
    [<<?IO_INTERCONNECTS, 10>> | Interconnects].

%%--------------------------------------------------------------------

io_column_interconnect(X, Y, I, Db) ->
    Sources = [
        io_interconnect_source(X, Y, I, Mux4, Mux3, Db)
        ||
        Mux4 <- [mux0, mux1, mux2, mux3],
        Mux3 <- [mux0, mux1, mux2]
    ],
    [<<12>>, Sources].

%%--------------------------------------------------------------------

io_row_interconnects(X, Y, Db) ->
    Interconnects = [
        io_row_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 17)
    ],
    [<<?IO_INTERCONNECTS, 18>> | Interconnects].

%%--------------------------------------------------------------------

io_row_interconnect(X, Y, I, Db) ->
    C = case I of
        8 -> 16;
        17 -> 16;
        _ -> 13
    end,
    Source = io_interconnect_source(X, Y, I, direct_link, Db),
    G = case I of
        8 -> [gclk];
        17 -> [gclk];
        _ -> []
    end,
    Sources = [
        io_interconnect_source(X, Y, I, Mux4, Mux3, Db)
        ||
        Mux4 <- [mux0, mux1, mux2, mux3 | G],
        Mux3 <- [mux0, mux1, mux2]
    ],
    [<<C>>, Source, Sources].

%%--------------------------------------------------------------------

io_interconnect_source(X, Y, I, DirectLink, Db) ->
    Port = io_interconnect_port(X, Y, I, DirectLink, Db),
    {ok, Fuse} = fuse_map:from_name(
        {{iob, X, Y}, {interconnect, I}, direct_link},
        Db#db.density
    ),
    port_fuse(Port, Fuse).

%%--------------------------------------------------------------------

io_interconnect_source(X, Y, I, Mux4, Mux3, Db) ->
    Port = io_interconnect_port(X, Y, I, {Mux4, Mux3}, Db),
    {ok, Fuse0} = fuse_map:from_name(
        {{iob, X, Y}, {interconnect, I}, from4, Mux4},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{iob, X, Y}, {interconnect, I}, from3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1).

%%--------------------------------------------------------------------

io_interconnect_port(X, Y, I, Mux, Db) ->
    #{{iob, X, Y} := IOB} = Db#db.iobs,
    case IOB of
        #{I := Interconnect} ->
            case Interconnect of
                #{Mux := From} ->
                    source_port(From, Db);

                _ ->
                    source_port(unknown, Db)
            end;

        _ ->
            source_port(unknown, Db)
    end.

%%====================================================================
%% logic cells
%%====================================================================

logic_cells(X, Y, Db) ->
    Cells = [
        logic_cell(X, Y, N, Db)
        ||
        N <- lists:seq(0, 9)
    ],
    [<<?LOGIC_CELLS, 10>> | Cells].

%%--------------------------------------------------------------------

logic_cell(X, Y, N, Db) ->
    InputA = [
        logic_cell_input(X, Y, N, data_a, data_a6, Mux6, data_a3, Mux3, Db)
        ||
        Mux6 <- [mux0, mux1, mux2, mux3, mux4, mux5],
        Mux3 <- [mux0, mux1, mux2]
    ],
    InputB = [
        logic_cell_input(X, Y, N, data_b, data_b6, Mux6, data_b3, Mux3, Db)
        ||
        Mux6 <- [mux0, mux1, mux2, mux3, mux4, mux5],
        Mux3 <- [mux0, mux1, mux2]
    ],
    InputC = [
        logic_cell_input(X, Y, N, data_c, data_c6, Mux6, data_c3, Mux3, Db)
        ||
        Mux6 <- [mux0, mux1, mux2, mux3, mux4, mux5],
        Mux3 <- [mux0, mux1, mux2]
    ],
    InputD = [
        logic_cell_input(X, Y, N, data_d, data_d6, Mux6, data_d3, Mux3, Db)
        ||
        Mux6 <- [mux0, mux1, mux2, mux3, mux4, mux5],
        Mux3 <- [mux0, mux1, mux2]
    ],
    [<<18>>, InputA, <<18>>, InputB, <<18>>, InputC, <<18>>, InputD].

%%--------------------------------------------------------------------

logic_cell_input(X, Y, N, Input, From6, Mux6, From3, Mux3, Db) ->
    Port = case lc_data_mux_map:to_interconnect(Input, Mux6, Mux3) of
        {interconnect, I} ->
            <<?LOGIC_INTERCONNECT, X, Y, I>>;

        {local_line, I} ->
            <<?LOGIC_CELL_LOCAL, X, Y, I>>
    end,
    {ok, Fuse0} = fuse_map:from_name(
        {{lc, X, Y, N}, From6, Mux6},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{lc, X, Y, N}, From3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1).

%%====================================================================
%% logic controls
%%====================================================================

logic_controls(X, Y, Db) ->
    Controls = [
        logic_control(X, Y, N, Db)
        ||
        N <- lists:seq(0, 5)
    ],
    [<<?LOGIC_CONTROLS, 6>> | Controls].

%%--------------------------------------------------------------------

logic_control(X, Y, N, Db) when N rem 2 =:= 0 ->
    Sources = [
        logic_control_input(X, Y, N, data_c, Mux6, Mux3, Db)
        ||
        Mux6 <- [mux0, mux1, mux2, mux3, mux4, mux5],
        Mux3 <- [mux0, mux1, mux2]
    ],
    [<<18>>, Sources];
logic_control(X, Y, N, Db) ->
    Sources = [
        logic_control_input(X, Y, N, data_d, Mux6, Mux3, Db)
        ||
        Mux6 <- [mux0, mux1, mux2, mux3, mux4, mux5],
        Mux3 <- [mux0, mux1, mux2]
    ],
    [<<18>>, Sources].

%%--------------------------------------------------------------------

logic_control_input(X, Y, N, Input, Mux6, Mux3, Db) ->
    Port = case lc_data_mux_map:to_interconnect(Input, Mux6, Mux3) of
        {interconnect, I} ->
            <<?LOGIC_INTERCONNECT, X, Y, I>>;

        {local_line, I} ->
            <<?LOGIC_CELL_LOCAL, X, Y, I>>
    end,
    {ok, Fuse0} = fuse_map:from_name(
        {{lab, X, Y}, {control, N}, from6, Mux6},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{lab, X, Y}, {control, N}, from3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1).

%%====================================================================
%% logic interconnects
%%====================================================================

logic_interconnects(X, Y, Db) ->
    Interconnects = [
        logic_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 25)
    ],
    [<<?LOGIC_INTERCONNECTS, 26>> | Interconnects].

%%--------------------------------------------------------------------

logic_interconnect(X, Y, I, Db) ->
    C = case I of
        12 -> 16;
        25 -> 16;
        _ -> 13
    end,
    Source = logic_interconnect_source(X, Y, I, direct_link, Db),
    G = case I of
        12 -> [gclk];
        25 -> [gclk];
        _ -> []
    end,
    Sources = [
        logic_interconnect_source(X, Y, I, Mux4, Mux3, Db)
        ||
        Mux4 <- [mux0, mux1, mux2, mux3 | G],
        Mux3 <- [mux0, mux1, mux2]
    ],
    [<<C>>, Source, Sources].

%%--------------------------------------------------------------------

logic_interconnect_source(X, Y, I, DirectLink, Db) ->
    Port = logic_interconnect_port(X, Y, I, DirectLink, Db),
    {ok, Fuse} = fuse_map:from_name(
        {{lab, X, Y}, {interconnect, I}, direct_link},
        Db#db.density
    ),
    port_fuse(Port, Fuse).

%%--------------------------------------------------------------------

logic_interconnect_source(X, Y, I, Mux4, Mux3, Db) ->
    Port = logic_interconnect_port(X, Y, I, {Mux4, Mux3}, Db),
    {ok, Fuse0} = fuse_map:from_name(
        {{lab, X, Y}, {interconnect, I}, from4, Mux4},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{lab, X, Y}, {interconnect, I}, from3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1).

%%--------------------------------------------------------------------

logic_interconnect_port(X, Y, I, Mux, Db) ->
    #{{lab, X, Y} := LAB} = Db#db.labs,
    case LAB of
        #{I := Interconnect} ->
            case Interconnect of
                #{Mux := From} ->
                    source_port(From, Db);

                _ ->
                    source_port(unknown, Db)
            end;

        _ ->
            source_port(unknown, Db)
    end.

%%====================================================================
%% r4 interconnects
%%====================================================================

r4_interconnects(X, Y, Db) ->
    Interconnects = [
        r4_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 15)
    ],
    [<<?R4_INTERCONNECTS, 16>> | Interconnects].

%%--------------------------------------------------------------------

r4_left_interconnects(X, Y, Db) ->
    Interconnects = [
        r4_io_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 7)
    ],
    [<<?R4_INTERCONNECTS, 8>> | Interconnects].

%%--------------------------------------------------------------------

r4_right_interconnects(X, Y, Db) ->
    Interconnects = [
        r4_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 7)
    ],
    [<<?R4_INTERCONNECTS, 8>> | Interconnects].

%%--------------------------------------------------------------------

r4_grow_interconnects(X, Y, Db) ->
    Interconnects = [
        r4_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(8, 15)
    ],
    [<<?R4_INTERCONNECTS, 8>> | Interconnects].

%%--------------------------------------------------------------------

r4_interconnect(X, Y, I, Db) ->
    Source = r4_interconnect_source(X, Y, I, direct_link, Db),
    Sources = [
        r4_interconnect_source(X, Y, I, Mux4, Mux3, Db)
        ||
        Mux4 <- [mux0, mux1, mux2, mux3],
        Mux3 <- [mux0, mux1, mux2]
    ],
    [<<I, 13>>, Source, Sources].

%%--------------------------------------------------------------------

r4_io_interconnect(X, Y, I, Db) ->
    [<<I, 2>>,
     r4_interconnect_source(X, Y, I, io_data_in1, Db),
     r4_interconnect_source(X, Y, I, io_data_in0, Db)
    ].

%%--------------------------------------------------------------------

r4_interconnect_source(X, Y, I, DirectLink, Db) ->
    Port = r4_interconnect_port(X, Y, I, DirectLink, Db),
    {ok, Fuse} = fuse_map:from_name(
        {{r4, X, Y}, {mux, I}, DirectLink},
        Db#db.density
    ),
    port_fuse(Port, Fuse).

%%--------------------------------------------------------------------

r4_interconnect_source(X, Y, I, Mux4, Mux3, Db) ->
    Port = r4_interconnect_port(X, Y, I, {Mux4, Mux3}, Db),
    {ok, Fuse0} = fuse_map:from_name(
        {{r4, X, Y}, {mux, I}, from4, Mux4},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{r4, X, Y}, {mux, I}, from3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1).

%%--------------------------------------------------------------------

r4_interconnect_port(X, Y, I, Mux, Db) ->
    #{{r4, X, Y} := Block} = Db#db.r4s,
    case Block of
        #{I := Interconnect} ->
            case Interconnect of
                #{Mux := From} ->
                    source_port(From, Db);

                _ ->
                    source_port(unknown, Db)
            end;

        _ ->
            source_port(unknown, Db)
    end.

%%====================================================================
%% ufm interconnects
%%====================================================================

ufm_interconnects(Db) ->
    [<<?UFM_INTERCONNECTS>>,
     ufm_interconnect(ufm, ar_clk, {1, 2}, Db),
     ufm_interconnect(ufm, ar_in, {1, 3}, Db),
     ufm_interconnect(ufm, ar_shift, {1, 2}, Db),
     ufm_interconnect(ufm, dr_clk, {1, 3}, Db),
     ufm_interconnect(ufm, dr_in, {1, 3}, Db),
     ufm_interconnect(ufm, dr_shift, {1, 3}, Db),
     ufm_interconnect(ufm, erase, {2, 2}, Db),
     ufm_interconnect(ufm, osc_ena, {2, 2}, Db),
     ufm_interconnect(ufm, program, {2, 2}, Db)
    ].

%%====================================================================
%% global / jtag / ufm
%%====================================================================

ufm_interconnect(Block, N, {S, L}, Db = #db{metric = Metric}) ->
    case Metric#metric.indent_bottom_io of
        0 ->
            ufm_small(Block, Metric#metric.indent_left_io, S, N, Db);

        3 ->
            ufm_large(Block, Metric#metric.indent_left_io, L, N, Db)
    end.

%%--------------------------------------------------------------------

ufm_small(Block, X, Y, N, Db) ->
    Sources = [
        ufm_small_source(Block, X, Y, N, Mux6, Mux3, Db)
        ||
        Mux6 <- [mux0, mux1, mux2, mux3, mux5, mux5],
        Mux3 <- [mux0, mux1, mux2]
    ],
    [<<18>>, Sources].

%%--------------------------------------------------------------------

ufm_small_source(Block, X, Y, N, Mux6, Mux3, Db) ->
    {interconnect, I} = global_mux_map:to_small_interconnect(Mux6, Mux3),
    Port = <<?IO_ROW_INTERCONNECT, X, Y, I>>,
    {ok, Fuse0} = fuse_map:from_name(
        {{Block, X, Y}, N, from6, Mux6},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{Block, X, Y}, N, from3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1).

%%--------------------------------------------------------------------

ufm_large(Block, X, Y, N, Db) ->
    Sources = [
        ufm_large_source(Block, X, Y, N, I, Db)
        ||
        I <- lists:seq(0, 9)
    ],
    [<<10>>, Sources].

%%--------------------------------------------------------------------

ufm_large_source(Block, X, Y, N, I, Db) ->
    Port = <<?UFM_INTERCONNECT, X, Y, I>>,
    {ok, Mux4, Mux3} = global_mux_map:from_large_interconnect(
        {interconnect, I}
    ),
    {ok, Fuse0} = fuse_map:from_name(
        {{Block, X, Y}, N, from4, Mux4},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{Block, X, Y}, N, from3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1).

%%====================================================================
%% sources
%%====================================================================

source_port({c4, X, Y, mux, I}, _) ->
    <<?C4_INTERCONNECT, X, Y, I>>;
source_port({io_data_in, X, Y, I, 0}, #db{metric = Metric}) ->
    case density:block_type(X, Y, Metric) of
        column ->
            <<?IO_COLUMN_CELL, X, Y, I>>;
        row ->
            <<?IO_ROW_CELL, X, Y, I>>
    end;
source_port({jtag, _X, _Y, tck}, _) ->
    <<?JTAG_TCK>>;
source_port({jtag, _X, _Y, tdi}, _) ->
    <<?JTAG_TDI>>;
source_port({jtag, _X, _Y, tms}, _) ->
    <<?JTAG_TMS>>;
source_port({lab_clk, _, _, _, I}, _) ->
    <<?GLOBAL, I>>;
source_port({le_buffer, X, Y, 0, II}, _) when II rem 2 =:= 0 ->
    I = II div 2,
    <<?LOGIC_CELL_LEFT, X, Y, I>>;
source_port({le_buffer, X, Y, 0, II}, _) when II rem 2 =:= 1 ->
    I = II div 2,
    <<?LOGIC_CELL_RIGHT, X, Y, I>>;
source_port({r4, X, Y, mux, I}, _) ->
    <<?R4_INTERCONNECT, X, Y, I>>;
source_port({ufm, _X, _Y, ar_out}, _) ->
    <<?UFM_AR_OUT>>;
source_port({ufm, _X, _Y, busy}, _) ->
    <<?UFM_BUSY>>;
source_port({ufm, _X, _Y, dr_out}, _) ->
    <<?UFM_DR_OUT>>;
source_port({ufm, _X, _Y, isp_busy}, _) ->
    <<?UFM_ISP_BUSY>>;
source_port({ufm, _X, _Y, osc}, _) ->
    <<?UFM_OSC>>;
source_port(unknown, _) ->
    <<?UNKNOWN>>;
source_port(Error, _) ->
    throw(Error).

%%====================================================================
%% sources
%%====================================================================

port_fuse(Port, Fuse0) ->
    <<Port/binary,
      1,
      Fuse0:24/big-integer
    >>.

%%--------------------------------------------------------------------

port_fuse(Port, Fuse0, Fuse1) ->
    <<Port/binary,
      2,
      Fuse0:24/big-integer,
      Fuse1:24/big-integer
    >>.

%%--------------------------------------------------------------------

port_fuse(Port, Fuse0, Fuse1, Fuse2) ->
    <<Port/binary,
      3,
      Fuse0:24/big-integer,
      Fuse1:24/big-integer,
      Fuse2:24/big-integer
    >>.

