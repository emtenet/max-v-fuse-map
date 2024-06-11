-module(gh_pages).

-export([run/0]).

-export_type([device/0]).

-include("max_v.hrl").

% {{global,0},from3,mux0}
% {{global,0},from4,mux0}

% 240z
% {{global,1},from3,mux0}
% {{global,1},from6,mux0}
%
% {{global,0},fromX,muxX} -> {{ioc,1,3,4},outputX,muxX}}
% {{global,1},fromX,muxX} -> {{ioc,1,3,4},enableX,muxX}}
% {{global,2},fromX,muxX} -> {{ioc,1,3,5},outputX,muxX}}
% {{global,3},fromX,muxX} -> {{ioc,1,3,5},enableX,muxX}}

% {{c4,9,3},{mux,0},from3,mux0}
%
% {{r4,9,3},{mux,8},from3,mux0}
%
% {{iob,9,3},{interconnect,0},from3,mux1}
% {{iob,9,3},{interconnect,0},from4,mux2}

% {{lab,9,5},{interconnect,0},from3,mux0}
% {{lab,9,5},{interconnect,0},from4,mux0}

-type device() :: #{
    width := integer(),
    height := integer(),
    top := integer(),
    left := integer(),
    title := string(),
    global := {integer(), integer()},
    blocks := #{xy() := block()}
}.

-type xy() :: {integer(), integer()}.

-type block() :: #{
    type := global | iob | lab | other,
    globals := list(any()),
    c4s := #{integer() := any()},
    ics := #{integer() := any()},
    ios := #{integer() := any()},
    lcs := #{integer() := any()},
    r4s := #{integer() := any()}
}.

%%====================================================================
%% run
%%====================================================================

run() ->
    Devices = [device(Density) || Density <- density:list()],
    Json = json(Devices),
    file:write_file("/home/local/static/max-v/data.js", Json).

%%--------------------------------------------------------------------

device(Density) ->
    Device = density:largest_device(Density),
    Metric = density:metric(Density),
    Blocks0 = block_x(Metric#metric.left_io, Density, Metric, #{}),
    Blocks1 = c4_blocks(Density, Blocks0),
    Blocks2 = iob_blocks(Density, Blocks1),
    Blocks3 = lab_blocks(Density, Blocks2),
    Blocks = r4_blocks(Density, Blocks3),
    #{
        width => 1 + Metric#metric.right_io - Metric#metric.left_io,
        height => 1 + Metric#metric.top_io - Metric#metric.bottom_io,
        top => Metric#metric.top_io,
        left => Metric#metric.left_io,
        title => device:title(Device),
        global => case density:global_block(Density) of
            false ->
                {1, 3};

            Block ->
                Block
        end,
        blocks => Blocks
    }.

%%--------------------------------------------------------------------

block_x(X, _, Metric, Acc) when X > Metric#metric.right_io ->
    Acc;
block_x(X, Density, Metric, Acc0) ->
    Acc = block_xy(X, Metric#metric.bottom_io, Density, Metric, Acc0),
    block_x(X + 1, Density, Metric, Acc).

%%--------------------------------------------------------------------

block_xy(_, Y, _, Metric, Acc) when Y > Metric#metric.top_io ->
    Acc;
block_xy(X, Y, Density, Metric, Acc0) ->
    case density:block_type(X, Y, Density) of
        false ->
            block_xy(X, Y + 1, Density, Metric, Acc0);

        Type ->
            Acc = Acc0#{
                {X, Y} => #{
                    type => Type
                }
            },
            block_xy(X, Y + 1, Density, Metric, Acc)
    end.

%%====================================================================
%% c4
%%====================================================================

c4_blocks(Density, Acc0) ->
    {ok, Blocks} = c4_interconnect_mux_database:open(Density),
    maps:fold(fun c4_block/3, Acc0, Blocks).

%%--------------------------------------------------------------------

c4_block({c4, X, Y}, Indexes, Acc0) ->
    maps:fold(fun (Index, Muxes, Acc) ->
        c4_index(X, Y, Index, Muxes, Acc)
    end, Acc0, Indexes).

%%--------------------------------------------------------------------

c4_index(X, Y, I, Muxes, Acc0) ->
    maps:fold(fun (_, From, Acc) ->
        c4_from(X, Y, I, From, Acc)
    end, Acc0, Muxes).

%%--------------------------------------------------------------------

c4_from(X, Y, I, {c4, XX, YY, mux, II}, Acc0) ->
    Acc1 = blocks_from({X, Y}, c4, I, {XX, YY, c4, II}, Acc0),
    blocks_thru({XX, YY}, c4, II, {X, Y, c4, I}, Acc1);
c4_from(X, Y, I, {r4, XX, YY, mux, II}, Acc0) ->
    Acc1 = blocks_from({X, Y}, c4, I, {XX, YY, r4, II}, Acc0),
    blocks_thru({XX, YY}, r4, II, {X, Y, c4, I}, Acc1);
c4_from(X, Y, I, {io_data_in, XX, YY, II, 0}, Acc0) ->
    Acc1 = blocks_from({X, Y}, c4, I, {XX, YY, io, II}, Acc0),
    blocks_thru({XX, YY}, io, II, {X, Y, c4, I}, Acc1);
c4_from(X, Y, I, {le_buffer, XX, YY, 0, II}, Acc0) ->
    Acc1 = blocks_from({X, Y}, c4, I, {XX, YY, lc, II}, Acc0),
    blocks_thru({XX, YY}, lc, II, {X, Y, c4, I}, Acc1);
c4_from(_, _, _, {jtag_tdi_tap, _, _, _, 0}, Acc0) ->
    Acc0;
c4_from(_, _, _, {jtag_tms_tap, _, _, _, 0}, Acc0) ->
    Acc0;
c4_from(X, Y, I, From, _) ->
    throw({X, Y, I, From}).

%%====================================================================
%% iob
%%====================================================================

iob_blocks(Density, Acc0) ->
    {ok, Blocks} = iob_interconnect_mux_database:open(Density),
    maps:fold(fun iob_block/3, Acc0, Blocks).

%%--------------------------------------------------------------------

iob_block({iob, X, Y}, Indexes, Acc0) ->
    maps:fold(fun (Index, Muxes, Acc) ->
        iob_index(X, Y, Index, Muxes, Acc)
    end, Acc0, Indexes).

%%--------------------------------------------------------------------

iob_index(X, Y, I, Muxes, Acc0) ->
    maps:fold(fun (_, From, Acc) ->
        iob_from(X, Y, I, From, Acc)
    end, Acc0, Muxes).

%%--------------------------------------------------------------------

iob_from(X, Y, I, {c4, XX, YY, mux, II}, Acc0) ->
     blocks_from({X, Y}, ic, I, {XX, YY, c4, II}, Acc0);
iob_from(X, Y, I, {r4, XX, YY, mux, II}, Acc0) ->
     blocks_from({X, Y}, ic, I, {XX, YY, r4, II}, Acc0);
iob_from(X, Y, I, {io_data_in, XX, YY, II, 0}, Acc0) ->
     blocks_from({X, Y}, ic, I, {XX, YY, io, II}, Acc0);
iob_from(X, Y, I, {le_buffer, XX, YY, 0, II}, Acc0) ->
     blocks_from({X, Y}, ic, I, {XX, YY, lc, II}, Acc0);
iob_from(_, _, _, {jtag_tck_tap, _, _, _, 0}, Acc0) ->
    Acc0;
iob_from(_, _, _, {jtag_tdi_tap, _, _, _, 0}, Acc0) ->
    Acc0;
iob_from(_, _, _, {jtag_tms_tap, _, _, _, 0}, Acc0) ->
    Acc0;
iob_from(_, _, _, {lab_clk, _, _, 0, _}, Acc0) ->
    Acc0;
iob_from(X, Y, I, From, _) ->
    throw({X, Y, I, From}).

%%====================================================================
%% lab
%%====================================================================

lab_blocks(Density, Acc0) ->
    {ok, Blocks} = lab_interconnect_mux_database:open(Density),
    maps:fold(fun lab_block/3, Acc0, Blocks).

%%--------------------------------------------------------------------

lab_block({lab, X, Y}, Indexes, Acc0) ->
    maps:fold(fun (Index, Muxes, Acc) ->
        lab_index(X, Y, Index, Muxes, Acc)
    end, Acc0, Indexes).

%%--------------------------------------------------------------------

lab_index(X, Y, I, Muxes, Acc0) ->
    maps:fold(fun (_, From, Acc) ->
        lab_from(X, Y, I, From, Acc)
    end, Acc0, Muxes).

%%--------------------------------------------------------------------

lab_from(X, Y, I, {c4, XX, YY, mux, II}, Acc0) ->
     blocks_from({X, Y}, ic, I, {XX, YY, c4, II}, Acc0);
lab_from(X, Y, I, {r4, XX, YY, mux, II}, Acc0) ->
     blocks_from({X, Y}, ic, I, {XX, YY, r4, II}, Acc0);
lab_from(X, Y, I, {io_data_in, XX, YY, II, 0}, Acc0) ->
     blocks_from({X, Y}, ic, I, {XX, YY, io, II}, Acc0);
lab_from(X, Y, I, {le_buffer, XX, YY, 0, II}, Acc0) ->
     blocks_from({X, Y}, ic, I, {XX, YY, lc, II}, Acc0);
lab_from(_, _, _, {jtag_tdi_tap, _, _, _, 0}, Acc0) ->
    Acc0;
lab_from(_, _, _, {jtag_tms_tap, _, _, _, 0}, Acc0) ->
    Acc0;
lab_from(_, _, _, {lab_clk, _, _, 0, _}, Acc0) ->
    Acc0;
lab_from(X, Y, I, From, _) ->
    throw({X, Y, I, From}).

%%====================================================================
%% r4
%%====================================================================

r4_blocks(Density, Acc0) ->
    {ok, Blocks} = r4_interconnect_mux_database:open(Density),
    maps:fold(fun r4_block/3, Acc0, Blocks).

%%--------------------------------------------------------------------

r4_block({r4, X, Y}, Indexes, Acc0) ->
    maps:fold(fun (Index, Muxes, Acc) ->
        r4_index(X, Y, Index, Muxes, Acc)
    end, Acc0, Indexes).

%%--------------------------------------------------------------------

r4_index(X, Y, I, Muxes, Acc0) ->
    maps:fold(fun (_, From, Acc) ->
        r4_from(X, Y, I, From, Acc)
    end, Acc0, Muxes).

%%--------------------------------------------------------------------

r4_from(X, Y, I, {c4, XX, YY, mux, II}, Acc0) ->
    Acc1 = blocks_from({X, Y}, r4, I, {XX, YY, c4, II}, Acc0),
    blocks_thru({XX, YY}, c4, II, {X, Y, r4, I}, Acc1);
r4_from(X, Y, I, {r4, XX, YY, mux, II}, Acc0) ->
    Acc1 = blocks_from({X, Y}, r4, I, {XX, YY, r4, II}, Acc0),
    blocks_thru({XX, YY}, r4, II, {X, Y, r4, I}, Acc1);
r4_from(X, Y, I, {io_data_in, XX, YY, II, 0}, Acc0) ->
    Acc1 = blocks_from({X, Y}, r4, I, {XX, YY, io, II}, Acc0),
    blocks_thru({XX, YY}, io, II, {X, Y, r4, I}, Acc1);
r4_from(X, Y, I, {le_buffer, XX, YY, 0, II}, Acc0) ->
    Acc1 = blocks_from({X, Y}, r4, I, {XX, YY, lc, II}, Acc0),
    blocks_thru({XX, YY}, lc, II, {X, Y, r4, I}, Acc1);
r4_from(_, _, _, {jtag_tck_tap, _, _, _, 0}, Acc0) ->
    Acc0;
r4_from(_, _, _, {jtag_tdi_tap, _, _, _, 0}, Acc0) ->
    Acc0;
r4_from(_, _, _, {jtag_tms_tap, _, _, _, 0}, Acc0) ->
    Acc0;
r4_from(X, Y, I, From, _) ->
    throw({X, Y, I, From}).

%%====================================================================
%% blocks
%%====================================================================

blocks_from(At, T, I, From, Blocks) ->
    case Blocks of
        #{At := Block = #{T := Is = #{I := {Froms, Thrus}}}} ->
            Blocks#{At => Block#{T => Is#{I => {[From | Froms], Thrus}}}};

        #{At := Block = #{T := Is}} ->
            Blocks#{At => Block#{T => Is#{I => {[From], []}}}};

        #{At := Block} ->
            Blocks#{At => Block#{T => #{I => {[From], []}}}};

        _ ->
            Blocks#{At => #{type => other, T => #{I => {[From], []}}}}
    end.

%%--------------------------------------------------------------------

blocks_thru(At, T, I, Thru, Blocks) ->
    case Blocks of
        #{At := Block = #{T := Is = #{I := {Froms, Thrus}}}} ->
            Blocks#{At => Block#{T => Is#{I => {Froms, [Thru | Thrus]}}}};

        #{At := Block = #{T := Is}} ->
            Blocks#{At => Block#{T => Is#{I => {[], [Thru]}}}};

        #{At := Block} ->
            Blocks#{At => Block#{T => #{I => {[], [Thru]}}}};

        _ ->
            Blocks#{At => #{type => other, T => #{I => {[], [Thru]}}}}
    end.

%%====================================================================
%% run
%%====================================================================

json(Devices) ->
    [_ | Json] = lists:foldr(
        fun json_device/2,
        [<<"\n">>, <<"];\n">>],
        Devices
    ),
    [<<"DATA = [\n">> | Json].

%%--------------------------------------------------------------------

json_device(Device, Acc) ->
    #{
        width := Width,
        height := Height,
        top := Top,
        left := Left,
        title := Title,
        global := {GlobalX, GlobalY},
        blocks := Blocks0
    } = Device,
    [_ | Blocks] = maps:fold(
        fun json_block/3,
        [<<"\n">>],
        Blocks0
    ),
    Open = io_lib:format(
        "  {width: ~b, height: ~b, top: ~b, left: ~b,\n"
        "   global: [~b, ~b],\n"
        "   title: \"~s\",\n"
        "   blocks: [\n", [
        Width,
        Height,
        Top,
        Left,
        GlobalX, GlobalY,
        Title
    ]),
    Close = <<"  ]}">>,
    [<<",\n">>, Open, Blocks, Close | Acc].

%%--------------------------------------------------------------------

json_block({X, Y}, Block = #{type := Type}, Acc) ->
    [
        <<",\n">>,
        <<"    {x: ">>, integer_to_binary(X),
        <<", y: ">>, integer_to_binary(Y),
        <<", t: \"">>, atom_to_binary(Type), <<"\", c4s: [">>,
        json_c4s(maps:get(c4, Block, #{})),
        <<"], ics: [">>,
        json_c4s(maps:get(ic, Block, #{})),
        <<"], ios: [">>,
        json_c4s(maps:get(io, Block, #{})),
        <<"], lcs: [">>,
        json_c4s(maps:get(lc, Block, #{})),
        <<"], r4s: [">>,
        json_c4s(maps:get(r4, Block, #{})),
        <<"]}">>
        |
        Acc
    ].

%%--------------------------------------------------------------------

json_c4s(Is) when map_size(Is) =:= 0 ->
    [];
json_c4s(Is) ->
    [_ | Json] = maps:fold(
        fun json_c4/3,
        [<<"\n    ">>],
        Is
    ),
    [<<"\n">> | Json].

%%--------------------------------------------------------------------

json_c4(I, {Froms, Thrus}, Acc) ->
    [
        <<",\n">>,
        <<"      {i: ">>, integer_to_binary(I), <<", froms: [">>,
        json_froms(Froms),
        <<"], thrus: [">>,
        json_froms(Thrus),
        <<"]}">>
        |
        Acc
    ].

%%--------------------------------------------------------------------

json_froms([]) ->
    [];
json_froms(Froms) ->
    [_ | Json] = lists:foldr(fun json_from/2, [<<"\n      ">>], Froms),
    [<<"\n">> | Json].

%%--------------------------------------------------------------------

json_from({X, Y, T, I}, Acc) ->
    [
        <<",\n">>,
        <<"        {x: ">>,
        integer_to_binary(X),
        <<", y: ">>,
        integer_to_binary(Y),
        <<", t: \"">>,
        atom_to_binary(T),
        <<"\", i: ">>,
        integer_to_binary(I),
        <<"}">>
        |
        Acc
    ].

