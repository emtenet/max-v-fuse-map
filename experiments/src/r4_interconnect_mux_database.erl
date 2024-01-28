-module(r4_interconnect_mux_database).

-export([run/0]).

-export([open/1]).
-export([save/2]).
-export([find_key/4]).
-export([add/5]).

-export_type([block/0]).
-export_type([blocks/0]).
-export_type([indexes/0]).
-export_type([index/0]).
-export_type([mux/0]).
-export_type([mux_key/0]).
-export_type([from/0]).

-type density() :: density:density().

-type blocks() :: #{block() => indexes()}.
-type block() :: r4:r4().

-type indexes() :: #{index() => mux()}.
-type index() :: 0..15.

-type mux() :: #{mux_key() => from()}.
-type mux_key() :: direct_link | io_data_in0 | io_data_in1 | {mux4(), mux3()}.
-type mux4() :: max_v:mux4().
-type mux3() :: max_v:mux3().
-type from() :: max_v:r4() | max_v:le_buffer() | max_v:r4().

%%====================================================================
%% run
%%====================================================================

run() ->
    %fold(max_v_240z),
    lists:foreach(fun fold/1, density:list()),
    ok.

%%--------------------------------------------------------------------

fold(Density) ->
    {ok, Db0} = open(Density),
    {ok, Cache} = route_cache:open(Density),
    Db1 = route_cache:fold_blocks(
        r4,
        fun (Block, Indexes, Acc) ->
            fold_block(Density, Block, Indexes, Acc)
        end,
        Db0,
        Cache
    ),
    save(Density, Db1),
    ok.

%%--------------------------------------------------------------------

fold_block(Density, Block = {r4, _, _}, Indexes, Db0) ->
    io:format(" ==> ~s ~p~n", [Density, Block]),
    route_cache:fold_indexes(
        fun (Index, Froms, Acc) ->
            fold_index(Density, Block, Index, Froms, Acc)
        end,
        Db0,
        Indexes
    ).

%%--------------------------------------------------------------------

fold_index(Density, Block, Index, Froms, Db0) ->
    route_cache:fold_froms(
        fun (From, Cached, Acc) ->
            fold_from_check(Density, Block, Index, From, Cached, Acc)
        end,
        Db0,
        Froms
    ).

%%--------------------------------------------------------------------

fold_from_check(Density, R4, I, From0, Cached, Db) ->
    Name = {R4, {interconnect, I}, dummy},
    {ok, {Block, {mux, Index}, _}} = r4_fuse_map:from_name(Name, Density),
    From = fold_from(Density, From0),
    case find_key(Block, Index, From, Db) of
        {ok, _} ->
            Db;

        false ->
            fold_from(Density, Block, Index, From, Cached, Db)
    end.

%%--------------------------------------------------------------------

fold_from(Density, {c4, X, Y, 0, I}) ->
    Interconnect = {{c4, X, Y}, {interconnect, I}, dummy},
    {ok, Mux} = c4_fuse_map:from_name(Interconnect, Density),
    {{c4, XX, YY}, {mux, MM}, _} = Mux,
    {c4, XX, YY, mux, MM};
fold_from(Density, {r4, X, Y, 0, I}) ->
    Interconnect = {{r4, X, Y}, {interconnect, I}, dummy},
    {ok, Mux} = r4_fuse_map:from_name(Interconnect, Density),
    {{r4, XX, YY}, {mux, MM}, _} = Mux,
    {r4, XX, YY, mux, MM};
fold_from(_, From = {lab_clk, _, _, 0, _}) ->
    From;
fold_from(_, From = {le_buffer, _, _, 0, _}) ->
    From;
fold_from(_, From = {io_data_in, _, _, _, 0}) ->
    From.

%%--------------------------------------------------------------------

fold_from(Density, Block, Index, From, Cached, Db) ->
    case fold_reduce(Density, Cached) of
        false ->
            Db;

        {ok, Fuses} ->
            fold_fuses(Density, Block, Index, From, Fuses, Db)
    end.

%%--------------------------------------------------------------------

fold_reduce(Density, Cached) ->
    route_cache:fold_cached(
        fun fold_reduce_zero/2,
        Density,
        fun fold_reduce_rest/2,
        Cached,
        #{limit => 50}
    ).

%%--------------------------------------------------------------------

fold_reduce_zero(Experiment, Density) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    fuses:subtract(Fuses, density:minimal_fuses(Density)).

%%--------------------------------------------------------------------

fold_reduce_rest(Experiment, Fuses0) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    fuses:intersect(Fuses0, Fuses).

%%--------------------------------------------------------------------

fold_fuses(Density, Block, Index, From, Fuses, Db) ->
    case fold_mux(Density, Block, Index, Fuses, undefined) of
        {ok, Mux} ->
            add(Block, Index, Mux, From, Db);

        false ->
            Db
    end.

%%--------------------------------------------------------------------

fold_mux(_, _, _, [], Acc) ->
    undefined = Acc,
    false;
fold_mux(Density, Block, Index, [Fuse | Fuses], Acc) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {Block, {mux, Index}, from3, From3}} ->
            case Acc of
                {from4, From4} ->
                    {ok, {From4, From3}};

                undefined ->
                    fold_mux(Density, Block, Index, Fuses, {from3, From3})
            end;

        {ok, {Block, {mux, Index}, From, From4}} ->
            from4 = From,
            case Acc of
                {from3, From3} ->
                    {ok, {From4, From3}};

                undefined ->
                    fold_mux(Density, Block, Index, Fuses, {from4, From4})
            end;

        {ok, {Block, {mux, Index}, Direct}} ->
            undefined = Acc,
            case Direct of
                direct_link ->
                    {ok, direct_link};

                io_data_in0 ->
                    {ok, io_data_in0};

                io_data_in1 ->
                    {ok, io_data_in1}
            end;

        _ ->
            fold_mux(Density, Block, Index, Fuses, Acc)
    end.

%%====================================================================
%% open
%%====================================================================

-spec open(density()) -> {ok, blocks()}.

open(Density) ->
    File = database_file(Density),
    case file:consult(File) of
        {ok, []} ->
            {ok, #{}};

        {ok, [{Block = {r4, _, _}, {mux, Index}} | Lines]} ->
            Blocks = open(Lines, #{}, Block, Index, #{}),
            {ok, Blocks};

        {error, enoent} ->
            {ok, #{}}
    end.

%%--------------------------------------------------------------------

open([], Blocks, Block, Index, Mux) ->
    case Blocks of
        #{Block := Indexes} ->
            Blocks#{Block => Indexes#{Index => Mux}};

        _ ->
            Blocks#{Block => #{Index => Mux}}
    end;
open([{NextBlock = {r4, _, _}, {mux, NextIndex}} | Lines],
     Blocks, Block, Index, Mux) ->
    case Blocks of
        #{Block := Indexes} ->
            open(
                Lines, Blocks#{Block => Indexes#{Index => Mux}},
                NextBlock, NextIndex, #{}
            );

        _ ->
            open(
                Lines, Blocks#{Block => #{Index => Mux}},
                NextBlock, NextIndex, #{}
            )
    end;
open([{Key, From} | Lines], Blocks, Block, Index, Mux) ->
    open(Lines, Blocks, Block, Index, Mux#{Key => From}).

%%====================================================================
%% save
%%====================================================================

-spec save(density(), blocks()) -> ok.

save(Density, Blocks) ->
    File = database_file(Density),
    ok = file:write_file(File, [
        save_block(Block, Blocks)
        ||
        Block <- lists:sort(maps:keys(Blocks))
    ]).

%%--------------------------------------------------------------------

save_block(Block, Blocks) ->
    #{Block := Indexes} = Blocks,
    [
        save_index(Block, Index, Indexes)
        ||
        Index <- lists:sort(maps:keys(Indexes))
    ].

%%--------------------------------------------------------------------

save_index(Block, Index, Indexes) ->
    #{Index := Mux} = Indexes,
    [
        iolist_to_binary(io_lib:format(
            "{~p,~p}.~n",
            [Block, {mux, Index}]
        ))
        |
        [
            iolist_to_binary(io_lib:format("~w.~n", [Entry]))
            ||
            Entry <- lists:sort(maps:to_list(Mux))
        ]
    ].

%%====================================================================
%% add
%%====================================================================

-spec add(block(), index(), mux_key(), from(), blocks())
    -> blocks().

add(Block, Index, Key, From, Blocks) ->
    case Blocks of
        #{Block := #{Index := #{Key := From}}} ->
            Blocks;

        #{Block := #{Index := #{Key := Existing}}} ->
            throw({
                r4_interconnect, Block, Index, Key,
                add, From, existing, Existing
            });

        #{Block := Indexes = #{Index := Mux}} ->
            Blocks#{Block => Indexes#{Index => Mux#{Key => From}}};

        #{Block := Indexes} ->
            Blocks#{Block => Indexes#{Index => #{Key => From}}};

        _ ->
            Blocks#{Block => #{Index => #{Key => From}}}
    end.

%%====================================================================
%% find_key
%%====================================================================

-spec find_key(block(), index(), from(), blocks())
    -> {ok, mux_key()} | false.

find_key(Block, Index, From, Blocks) ->
    case Blocks of
        #{Block := #{Index := Keys}} ->
            find_key(From, maps:next(maps:iterator(Keys)));

        _ ->
            false
    end.

%%--------------------------------------------------------------------

find_key(_, none) ->
    false;
find_key(From, {Key, From, _}) ->
    {ok, Key};
find_key(From, {_, _, Iterator}) ->
    find_key(From, maps:next(Iterator)).

%%====================================================================
%% database
%%====================================================================

database_file(Density) ->
    lists:flatten(io_lib:format("../database/~s.r4-interconnect", [Density])).

