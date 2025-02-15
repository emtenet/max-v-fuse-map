-module(rust_sources).

-export([run/0]).

-include("max_v.hrl").

-record(db, {
    density :: atom(),
    device :: atom(),
    iobs :: iob_interconnect_mux_database:blocks(),
    labs :: lab_interconnect_mux_database:blocks(),
    metric :: #metric{}
}).

%%====================================================================
%% run
%%====================================================================

run() ->
    device(max_v_40z_e64),
    device(max_v_160z_t100),
    device(max_v_240z_t144),
    device(max_v_570z_t100),
    device(max_v_1270z_t144),
    ok.

%%--------------------------------------------------------------------

device(Device) ->
    Db = db(Device),
    Source = source(Db),
    Path = io_lib:format("../rust/device/~s.sources", [Device]),
    file:write_file(Path, Source).

%%--------------------------------------------------------------------

db(Device) ->
    Density = device:density(Device),
    Metric = density:metric(Density),
    {ok, LABs} = lab_interconnect_mux_database:open(Density),
    {ok, IOBs} = iob_interconnect_mux_database:open(Density),
    #db{
        density = Density,
        device = Device,
        iobs = IOBs,
        labs = LABs,
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
            block_type(X, Y, <<"IC">>, [
                io_column_cells(X, Y, Db),
                io_column_interconnects(X, Y, Db)
            ]);
        global -> <<>>;
        logic ->
            block_type(X, Y, <<"LC">>, [
                logic_cells(X, Y, Db),
                logic_controls(X, Y, Db),
                logic_interconnects(X, Y, Db)
            ]);
        row when X < 2 ->
            block_type(X, Y, <<"IL">>, [
                io_row_cells(X, Y, Db),
                io_row_interconnects(X, Y, Db)
            ]);
        row ->
            block_type(X, Y, <<"IR">>, [
                io_row_cells(X, Y, Db),
                io_row_interconnects(X, Y, Db)
            ]);
        ufm -> <<>>;
        false -> <<>>
    end.

%%--------------------------------------------------------------------

block_type(X, Y, Type, Block) ->
    [<< Type/binary, X, Y>>,
        Block
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
    [<<"CE", C>> | Cells].

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
    Port = <<"LI", X, Y, I>>,
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
    port_fuse(Port, Fuse0, Fuse1, 0).

%%--------------------------------------------------------------------

io_row_cells(X, Y, Db) ->
    Cells = [
        io_row_cell(IOC, Db)
        ||
        IOC <- device:iocs(Db#db.device, {iob, X, Y})
    ],
    C = length(Cells),
    [<<"CE", C>> | Cells].

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
    port_fuse(Port, Fuse0, 0, 0).

%%--------------------------------------------------------------------

io_row_cell_source(X, Y, N, From6, Mux6, From3, Mux3, Db) ->
    {interconnect, I} = ioc_output_mux_map:to_row_interconnect(Mux6, Mux3),
    Port = <<"LI", X, Y, I>>,
    {ok, Fuse0} = fuse_map:from_name(
        {{ioc, X, Y, N}, From6, Mux6},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{ioc, X, Y, N}, From3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1, 0).

%%====================================================================
%% io interconnects
%%====================================================================

io_column_interconnects(X, Y, Db) ->
    Interconnects = [
        io_column_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 9)
    ],
    [<<"IN", 10>> | Interconnects].

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
    [<<"IN", 18>> | Interconnects].

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
    port_fuse(Port, Fuse, 0, 0).

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
    port_fuse(Port, Fuse0, Fuse1, 0).

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
    [<<"CE", 10>> | Cells].

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
    [<<N>>, InputA, InputB, InputC, InputD].

%%--------------------------------------------------------------------

logic_cell_input(X, Y, N, Input, From6, Mux6, From3, Mux3, Db) ->
    Port = case lc_data_mux_map:to_interconnect(Input, Mux6, Mux3) of
        {interconnect, I} ->
            <<"LI", X, Y, I>>;

        {local_line, I} ->
            <<"LC", X, Y, I>>
    end,
    {ok, Fuse0} = fuse_map:from_name(
        {{lc, X, Y, N}, From6, Mux6},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{lc, X, Y, N}, From3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1, 0).

%%====================================================================
%% logic controls
%%====================================================================

logic_controls(X, Y, Db) ->
    Controls = [
        logic_control(X, Y, N, Db)
        ||
        N <- lists:seq(0, 5)
    ],
    [<<"CO", 6>> | Controls].

%%--------------------------------------------------------------------

logic_control(X, Y, N, Db) when N rem 2 =:= 0 ->
    [
        logic_control_input(X, Y, N, data_c, Mux6, Mux3, Db)
        ||
        Mux6 <- [mux0, mux1, mux2, mux3, mux4, mux5],
        Mux3 <- [mux0, mux1, mux2]
    ];
logic_control(X, Y, N, Db) ->
    [
        logic_control_input(X, Y, N, data_d, Mux6, Mux3, Db)
        ||
        Mux6 <- [mux0, mux1, mux2, mux3, mux4, mux5],
        Mux3 <- [mux0, mux1, mux2]
    ].

%%--------------------------------------------------------------------

logic_control_input(X, Y, N, Input, Mux6, Mux3, Db) ->
    Port = case lc_data_mux_map:to_interconnect(Input, Mux6, Mux3) of
        {interconnect, I} ->
            <<"LI", X, Y, I>>;

        {local_line, I} ->
            <<"LC", X, Y, I>>
    end,
    {ok, Fuse0} = fuse_map:from_name(
        {{lab, X, Y}, {control, N}, from6, Mux6},
        Db#db.density
    ),
    {ok, Fuse1} = fuse_map:from_name(
        {{lab, X, Y}, {control, N}, from3, Mux3},
        Db#db.density
    ),
    port_fuse(Port, Fuse0, Fuse1, 0).

%%====================================================================
%% logic interconnects
%%====================================================================

logic_interconnects(X, Y, Db) ->
    Interconnects = [
        logic_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 25)
    ],
    [<<"IN", 26>> | Interconnects].

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
    port_fuse(Port, Fuse, 0, 0).

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
    port_fuse(Port, Fuse0, Fuse1, 0).

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
%% sources
%%====================================================================

source_port({c4, X, Y, mux, I}, _) ->
    <<"C4", X, Y, I>>;
source_port({io_data_in, X, Y, I, 0}, #db{metric = Metric}) ->
    case density:block_type(X, Y, Metric) of
        column ->
            <<"IC", X, Y, I>>;
        row ->
            <<"IR", X, Y, I>>
    end;
source_port({jtag, _X, _Y, tck}, _) ->
    <<"J-tck">>;
source_port({jtag, _X, _Y, tdi}, _) ->
    <<"J-tdi">>;
source_port({jtag, _X, _Y, tms}, _) ->
    <<"J-tms">>;
source_port({lab_clk, _, _, _, I}, _) ->
    <<"GLOB", I>>;
source_port({le_buffer, X, Y, 0, II}, _) when II rem 2 =:= 0 ->
    I = II div 2,
    <<"LL", X, Y, I>>;
source_port({le_buffer, X, Y, 0, II}, _) when II rem 2 =:= 1 ->
    I = II div 2,
    <<"LR", X, Y, I>>;
source_port({r4, X, Y, mux, I}, _) ->
    <<"R4", X, Y, I>>;
source_port({ufm, _X, _Y, ar_out}, _) ->
    <<"U-aro">>;
source_port({ufm, _X, _Y, busy}, _) ->
    <<"U-bus">>;
source_port({ufm, _X, _Y, dr_out}, _) ->
    <<"U-dro">>;
source_port({ufm, _X, _Y, osc}, _) ->
    <<"U-osc">>;
source_port(unknown, _) ->
    <<"XX",0,0,0>>;
source_port(Error, _) ->
    throw(Error).

%%====================================================================
%% sources
%%====================================================================

port_fuse(Port, Fuse0, 0, 0) ->
    <<Port/binary,
      1,
      Fuse0:24/big-integer
    >>;
port_fuse(Port, Fuse0, Fuse1, 0) ->
    <<Port/binary,
      2,
      Fuse0:24/big-integer,
      Fuse1:24/big-integer
    >>;
port_fuse(Port, Fuse0, Fuse1, Fuse2) ->
    <<Port/binary,
      3,
      Fuse0:24/big-integer,
      Fuse1:24/big-integer,
      Fuse2:24/big-integer
    >>.

