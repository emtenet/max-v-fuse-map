-module(rust_devices).

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
    Path = io_lib:format("../rust/device/~s.toml", [Device]),
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
    [<< "device = \"">>, Name, <<"\"\n">>,
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
            block_type(X, Y, <<"column">>, [
                io_cells(X, Y, Db),
                io_column_interconnects(X, Y, Db)
            ]);
        global -> <<>>;
        logic ->
            block_type(X, Y, <<"logic">>, logic_interconnects(X, Y, Db));
        row when X < 2 ->
            block_type(X, Y, <<"left">>, [
                io_cells(X, Y, Db),
                io_row_interconnects(X, Y, Db)
            ]);
        row ->
            block_type(X, Y, <<"right">>, [
                io_cells(X, Y, Db),
                io_row_interconnects(X, Y, Db)
            ]);
        ufm -> <<>>;
        false -> <<>>
    end.

%%--------------------------------------------------------------------

block_type(X, Y, Type, Block) ->
    [<< "\n"
        "[[block]]\n"
        "x = ">>, s(X), <<"\n"
        "y = ">>, s(Y), <<"\n"
        "type = \"">>, Type, <<"\"\n">>,
        Block
    ].

%%====================================================================
%% io cells
%%====================================================================

io_cells(X, Y, Db) ->
    [
        io_cell(IOC)
        ||
        IOC <- device:iocs(Db#db.device, {iob, X, Y})
    ].

%%--------------------------------------------------------------------

io_cell({{ioc, _X, _Y, N}, Pin}) ->
    <<"PIN_", Name/binary>> = pin:name(Pin),
    [<< "\n"
        "[[block.io-cell]]\n"
        "n = ">>, s(N), <<"\n"
        "pin = \"">>, Name, <<"\"\n"
    >>].

%%====================================================================
%% io interconnects
%%====================================================================

io_column_interconnects(X, Y, Db) ->
    [
        io_column_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 9)
    ].

%%--------------------------------------------------------------------

io_column_interconnect(X, Y, I, Db) ->
    Sources = [
        io_interconnect_source(X, Y, I, Mux4, Mux3, Db)
        ||
        Mux4 <- [mux0, mux1, mux2, mux3],
        Mux3 <- [mux0, mux1, mux2]
    ],
    [<< "\n"
        "[[block.io-interconnect]]\n"
        "interconnect = ">>, s(I), <<"\n">>,
        Sources
    ].

%%--------------------------------------------------------------------

io_row_interconnects(X, Y, Db) ->
    [
        io_row_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 17)
    ].

%%--------------------------------------------------------------------

io_row_interconnect(X, Y, I, Db) ->
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
    [<< "\n"
        "[[block.io-interconnect]]\n"
        "interconnect = ">>, s(I), <<"\n">>,
        Source,
        Sources
    ].


%%--------------------------------------------------------------------

io_interconnect_source(X, Y, I, DirectLink, Db) ->
    Port = io_interconnect_port(X, Y, I, DirectLink, Db),
    [s(DirectLink), <<" = ">>, Port, <<"\n">>].

%%--------------------------------------------------------------------

io_interconnect_source(X, Y, I, Mux4, Mux3, Db) ->
    Port = io_interconnect_port(X, Y, I, {Mux4, Mux3}, Db),
    [s(Mux4, Mux3), <<" = ">>, Port, <<"\n">>].

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
%% logic
%%====================================================================

logic_interconnects(X, Y, Db) ->
    [
        logic_interconnect(X, Y, I, Db)
        ||
        I <- lists:seq(0, 25)
    ].

%%--------------------------------------------------------------------

logic_interconnect(X, Y, I, Db) ->
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
    [<< "\n"
        "[[block.logic-interconnect]]\n"
        "interconnect = ">>, s(I), <<"\n">>,
        Source,
        Sources
    ].

%%--------------------------------------------------------------------

logic_interconnect_source(X, Y, I, DirectLink, Db) ->
    Port = logic_interconnect_port(X, Y, I, DirectLink, Db),
    [s(DirectLink), <<" = ">>, Port, <<"\n">>].

%%--------------------------------------------------------------------

logic_interconnect_source(X, Y, I, Mux4, Mux3, Db) ->
    Port = logic_interconnect_port(X, Y, I, {Mux4, Mux3}, Db),
    [s(Mux4, Mux3), <<" = ">>, Port, <<"\n">>].

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
    [<<"[\"c4\", ">>, s(X), <<", ">>, s(Y), <<", ">>, s(I), <<"]">>];
source_port({io_data_in, X, Y, I, 0}, #db{metric = Metric}) ->
    case density:block_type(X, Y, Metric) of
        column ->
            [<<"[\"column\", ">>, s(X), <<", ">>, s(Y), <<", ">>, s(I), <<"]">>];
        row ->
            [<<"[\"row\", ">>, s(X), <<", ">>, s(Y), <<", ">>, s(I), <<"]">>]
    end;
source_port({jtag, _X, _Y, tck}, _) ->
    <<"[\"jtag\", \"tck\"]">>;
source_port({jtag, _X, _Y, tdi}, _) ->
    <<"[\"jtag\", \"tdi\"]">>;
source_port({jtag, _X, _Y, tms}, _) ->
    <<"[\"jtag\", \"tms\"]">>;
source_port({lab_clk, _, _, _, I}, _) ->
    [<<"[\"global\", ">>, s(I), <<"]">>];
source_port({le_buffer, X, Y, 0, II}, _) when II rem 2 =:= 0 ->
    I = II div 2,
    [<<"[\"left\", ">>, s(X), <<", ">>, s(Y), <<", ">>, s(I), <<"]">>];
source_port({le_buffer, X, Y, 0, II}, _) when II rem 2 =:= 1 ->
    I = II div 2,
    [<<"[\"right\", ">>, s(X), <<", ">>, s(Y), <<", ">>, s(I), <<"]">>];
source_port({r4, X, Y, mux, I}, _) ->
    [<<"[\"r4\", ">>, s(X), <<", ">>, s(Y), <<", ">>, s(I), <<"]">>];
source_port({ufm, _X, _Y, ar_out}, _) ->
    <<"[\"ufm\", \"ar-out\"]">>;
source_port({ufm, _X, _Y, busy}, _) ->
    <<"[\"ufm\", \"busy\"]">>;
source_port({ufm, _X, _Y, dr_out}, _) ->
    <<"[\"ufm\", \"dr-out\"]">>;
source_port({ufm, _X, _Y, osc}, _) ->
    <<"[\"ufm\", \"osc\"]">>;
source_port(unknown, _) ->
    <<"[]">>;
source_port(Error, _) ->
    throw(Error).

%%====================================================================
%% helpers
%%====================================================================

s(direct_link) -> <<"direct-link">>;
s(I) -> integer_to_binary(I).

%%--------------------------------------------------------------------

s(mux0, mux0) -> <<"select-0-0">>;
s(mux0, mux1) -> <<"select-0-1">>;
s(mux0, mux2) -> <<"select-0-2">>;
s(mux1, mux0) -> <<"select-1-0">>;
s(mux1, mux1) -> <<"select-1-1">>;
s(mux1, mux2) -> <<"select-1-2">>;
s(mux2, mux0) -> <<"select-2-0">>;
s(mux2, mux1) -> <<"select-2-1">>;
s(mux2, mux2) -> <<"select-2-2">>;
s(mux3, mux0) -> <<"select-3-0">>;
s(mux3, mux1) -> <<"select-3-1">>;
s(mux3, mux2) -> <<"select-3-2">>;
s(mux4, mux0) -> <<"select-4-0">>;
s(mux4, mux1) -> <<"select-4-1">>;
s(mux4, mux2) -> <<"select-4-2">>;
s(mux5, mux0) -> <<"select-5-0">>;
s(mux5, mux1) -> <<"select-5-1">>;
s(mux5, mux2) -> <<"select-5-2">>;
s(gclk, mux0) -> <<"select-g-0">>;
s(gclk, mux1) -> <<"select-g-1">>;
s(gclk, mux2) -> <<"select-g-2">>.

