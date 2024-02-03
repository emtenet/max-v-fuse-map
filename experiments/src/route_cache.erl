-module(route_cache).

% Cache a list of experiments (cache dirs) that contain a route segment.
%
% For example, route segment:
%   {le_buffer, 3, 10, 0, 18} -> {r4, 0, 10, 0, 53}
% would be cached as:
%   {route_cache,
%    epm240,
%    #{0 => "cache/D3/--V_pLoF0A47cHkSUqXNWubGTYrRCaI264C8u6HWI"},
%    #{{r4,0,10} => #{53 => #{{le_buffer,3,10,0,18} => 0}}}
%   }

-export([run/0]).

-export([open/1]).
-export([block_types/1]).
-export([blocks/2]).
-export([index_max/2]).
-export([froms/2]).
-export([froms/3]).
-export([tos/2]).
-export([tos/3]).
-export([experiments/3]).
-export([experiments/4]).

-export([fold_blocks/4]).
-export([fold_block/2]).
-export([fold_indexes/3]).
-export([fold_froms/3]).
-export([fold_cached/4]).
-export([fold_cached/5]).

-export([matrix/2]).
-export([matrix/3]).

-type density() :: density:density().

-type cache() :: {route_cache, density(), keys(), blocks()}.

-type fold_indexes() :: {fold_indexes, indexes(), keys()}.
-type fold_froms() :: {fold_froms, froms(), keys()}.
-type fold_cached() :: {fold_cached, [key_index()], keys()}.

-type keys() :: #{key_index() => file:name()}.
-type key_index() :: non_neg_integer().

-type blocks() :: #{block() => indexes()}.
-type block() :: {atom(), max_ii:x(), max_ii:y()}.

-type indexes() :: #{index() => froms()}.
-type index() :: non_neg_integer().

-type froms() :: #{from() => [key_index()]}.
-type from() :: {atom(), max_ii:x(), max_ii:y(), non_neg_integer(), non_neg_integer()}.

-type experiment() :: {experiment:title(), experiment:fuses(), rcf_file:rcf()}.

-define(INCREMENTAL, true).

%%====================================================================
%% run
%%====================================================================

-spec run() -> ok.

run() ->
    Collectors = maps:from_list([
        {Density, {ready, collect_start(Density)}}
        ||
        Density <- density:list()
    ]),
    iterate(experiment_cache:iterate(), Collectors).

%%--------------------------------------------------------------------

iterate(false, Collectors) ->
    iterate_finish(Collectors);
iterate({Key, {Device, _POF, RCF}, Iterator}, Collectors0) ->
    Density = device:density(Device),
    {Collector, Collectors} = iterate_collector(Density, Collectors0),
    Collector ! {collect, self(), Key, RCF},
    iterate(experiment_cache:iterate(Iterator), Collectors).

%%--------------------------------------------------------------------

iterate_collector(Density, Collectors) ->
    case Collectors of
        #{Density := {ready, Pid}} ->
            {Pid, Collectors#{Density => {collecting, Pid}}};

        #{Density := {collecting, _}} ->
            iterate_collector_wait(Density, Collectors)
    end.

%%--------------------------------------------------------------------

iterate_collector_wait(WaitFor, Collectors) ->
    receive
        {collected, Density} when WaitFor =:= Density ->
            #{Density := {collecting, Pid}} = Collectors,
            {Pid, Collectors};

        {collected, Density} ->
            #{Density := {collecting, Pid}} = Collectors,
            iterate_collector_wait(WaitFor, Collectors#{
                Density => {ready, Pid}
            })
    end.

%%--------------------------------------------------------------------

iterate_finish(Collectors0) ->
    Collecting = maps:fold(fun
            (_, {ready, _}, Count) -> Count;
            (_, {collecting, _}, Count) -> Count + 1
    end, 0, Collectors0),
    Collectors = iterate_finish_wait(Collecting, Collectors0),
    maps:foreach(fun iterate_finish_collector/2, Collectors).

%%--------------------------------------------------------------------

iterate_finish_wait(0, Collectors) ->
    Collectors;
iterate_finish_wait(Collecting, Collectors) when Collecting > 0 ->
    receive
        {collected, Density} ->
            #{Density := {collecting, Pid}} = Collectors,
            iterate_finish_wait(Collecting - 1, Collectors#{
                Density => {ready, Pid}
            })
    end.

%%--------------------------------------------------------------------

iterate_finish_collector(_Density, {ready, Pid}) ->
    Pid ! save.

%%====================================================================
%% worker
%%====================================================================

collect_start(Density) ->
    erlang:spawn_link(fun () -> collect_init(Density) end).

%%--------------------------------------------------------------------

-ifdef(INCREMENTAL).

collect_init(Density) ->
    {ok, {route_cache, Density, Keys, Blocks}} = open(Density),
    {Seen, KeyCount} = maps:fold(fun collect_init/3, {#{}, 0}, Keys),
    io:format(" ==> ~p INCREMENTAL~n", [Density]),
    collect_loop(Density, Seen, KeyCount, Keys, Blocks).

%%--------------------------------------------------------------------

collect_init(KeyIndex, Key, {Seen, KeyCount}) ->
    {Seen#{Key => seen}, max(KeyIndex, KeyCount)}.

-else.

collect_init(Density) ->
    io:format(" ==> ~p EMPTY~n", [Density]),
    Seen = #{},
    KeyCount = 0,
    Keys = #{},
    Blocks = #{},
    collect_loop(Density, Seen, KeyCount, Keys, Blocks).

-endif.

%%--------------------------------------------------------------------

collect_loop(Density, Seen, KeyCount, Keys, Blocks) ->
    receive
        {collect, From, Key, RCF} ->
            collect(Key, RCF, From, Density, Seen, KeyCount, Keys, Blocks);

        save ->
            save(Density, Keys, Blocks)
    end.

%%--------------------------------------------------------------------

collect(Key, RCF, From, Density, Seen, KeyCount0, Keys0, Blocks0) ->
    case Seen of
        #{Key := seen} ->
            From ! {collected, Density},
            collect_loop(Density, Seen, KeyCount0, Keys0, Blocks0);

        _ ->
            KeyCount = KeyCount0 + 1,
            Keys = Keys0#{KeyCount => Key},
            {ok, #{signals := Signals}} = rcf_file:decode(RCF),
            Blocks = collect_signals(Signals, KeyCount, Blocks0),
            From ! {collected, Density},
            collect_loop(Density, Seen, KeyCount, Keys, Blocks)
    end.

%%--------------------------------------------------------------------

collect_signals(Signals, Key, Blocks0) ->
    maps:fold(fun (_, Signal, Blocks) ->
        collect_signal(Signal, Key, Blocks)
    end, Blocks0, Signals).

%%--------------------------------------------------------------------

collect_signal(#{dests := Dests}, Key, Blocks0) ->
    lists:foldl(fun (Dest, Blocks) ->
        collect_dest(Dest, Key, Blocks)
    end, Blocks0, Dests).

%%--------------------------------------------------------------------

collect_dest(Dest = #{route := Route}, Key, Blocks0) ->
    Blocks = collect_route(Route, Key, Blocks0),
    case Dest of
        #{lc := {lc, X, Y, N}, port := Type, route := [From | _]} ->
            Block = {Type, X, Y},
            Index = N,
            collect_block(Block, Index, From, Key, Blocks);

        #{ioc := {ioc, X, Y, N}, port := Type, route := []} ->
            Block = {Type, X, Y},
            Index = N,
            From = internal,
            collect_block(Block, Index, From, Key, Blocks);

        #{ioc := {ioc, X, Y, N}, port := Type, route := [From | _]} ->
            Block = {Type, X, Y},
            Index = N,
            collect_block(Block, Index, From, Key, Blocks);

        #{jtag := {jtag, X, Y, N}, port := Type, route := []} ->
            Block = {Type, X, Y},
            Index = N,
            From = internal,
            collect_block(Block, Index, From, Key, Blocks);

        #{jtag := {jtag, X, Y, N}, port := Type, route := [From | _]} ->
            Block = {Type, X, Y},
            Index = N,
            collect_block(Block, Index, From, Key, Blocks)
    end.

%%--------------------------------------------------------------------

collect_route([], _, Blocks) ->
    Blocks;
collect_route([_], _, Blocks) ->
    Blocks;
collect_route([Thru | Route = [From | _]], Key, Blocks0) ->
    {Block, Index} = collect_thru(Thru),
    Blocks = collect_block(Block, Index, From, Key, Blocks0),
    collect_route(Route, Key, Blocks).

%%--------------------------------------------------------------------

collect_thru({Type = io_bypass_out, X, Y, Index, 0}) ->
    {{Type, X, Y}, Index};
collect_thru({Type = io_data_out, X, Y, Index, 0}) ->
    {{Type, X, Y}, Index};
collect_thru({Type = io_oe, X, Y, Index, 0}) ->
    {{Type, X, Y}, Index};
collect_thru({Type = jtag_tdo_user, X, Y, Index, 0}) ->
    {{Type, X, Y}, Index};
collect_thru({Type, X, Y, 0, Index}) ->
    {{Type, X, Y}, Index}.

%%--------------------------------------------------------------------

collect_block(Block, Index, From, Key, Blocks) ->
    case Blocks of
        #{Block := Indexes = #{Index := Froms = #{From := Keys}}} ->
            Blocks#{Block => Indexes#{Index => Froms#{From => [Key | Keys]}}};

        #{Block := Indexes = #{Index := Froms}} ->
            Blocks#{Block => Indexes#{Index => Froms#{From => [Key]}}};

        #{Block := Indexes} ->
            Blocks#{Block => Indexes#{Index => #{From => [Key]}}};

        _ ->
            Blocks#{Block => #{Index => #{From => [Key]}}}
    end.

%%====================================================================
%% open
%%====================================================================

-spec open(density()) -> {ok, cache()}.

open(Density) ->
    File = density_file(Density),
    case file:read_file(File) of
        {ok, Binary} ->
            {Keys, Blocks} = erlang:binary_to_term(Binary), %, [safe]),
            {ok, {route_cache, Density, Keys, Blocks}};

        {error, enoent} ->
            {ok, {route_cache, Density, #{}, #{}}}
    end.

%%====================================================================
%% save
%%====================================================================

save(Density, Keys, Blocks) ->
    io:format(" ==> ~s SAVE~n", [Density]),
    File = density_file(Density),
    Binary = erlang:term_to_binary({Keys, Blocks}, [compressed]),
    ok = file:write_file(File, Binary).

%%====================================================================
%% block_types
%%====================================================================

-spec block_types(cache()) -> [atom()].

block_types({route_cache, _, _, Blocks}) ->
    maps:keys(maps:fold(fun block_types/3, #{}, Blocks)).

%%--------------------------------------------------------------------

block_types({Type, _X, _Y}, _Indexes, Types) ->
    case Types of
        #{Type := _} ->
            Types;

        _ ->
            Types#{Type => true}
    end.

%%====================================================================
%% blocks
%%====================================================================

-spec blocks(atom(), cache()) -> [block()].

blocks(Type, {route_cache, _, _, Blocks}) ->
    lists:sort(
        lists:filter(fun ({T, _, _}) -> T =:= Type end,
            maps:keys(Blocks)
        )
    ).

%%====================================================================
%% index_max
%%====================================================================

-spec index_max(atom(), cache()) -> index().

index_max(Type, {route_cache, _, _, Blocks}) ->
    maps:fold(fun (Block, Indexes, Max) ->
        index_max(Type, Block, Indexes, Max)
    end, 0, Blocks).

%%--------------------------------------------------------------------

index_max(Type, {Type, _, _}, Indexes, Max) ->
    max(Max, lists:max(maps:keys(Indexes)));
index_max(_, _, _, Max) ->
    Max.

%%====================================================================
%% froms
%%====================================================================

-spec froms(from(), cache())
    -> {ok, [from()]} | false.

froms({Type, X, Y, 0, Index}, Cache = {route_cache, _, _, _}) ->
    froms({Type, X, Y}, Index, Cache).

%%--------------------------------------------------------------------

-spec froms(block(), index(), cache())
    -> {ok, [from()]} | false.

froms(Block, Index, {route_cache, _, _, Blocks}) ->
    case Blocks of
        #{Block := #{Index := Froms}} ->
            {ok, lists:sort(maps:keys(Froms))};

        _ ->
            false
    end.

%%====================================================================
%% tos
%%====================================================================

-spec tos(block(), index(), cache()) -> {ok, [from()]}.

tos({Type, X, Y}, Index, Cache = {route_cache, _, _, _}) ->
    tos({Type, X, Y, 0, Index}, Cache).

%%--------------------------------------------------------------------

-spec tos(from(), cache()) -> {ok, [from()]}.

tos(From = {_, _, _, 0, _}, {route_cache, _, _, Blocks}) ->
    Tos = maps:fold(fun (Block, Indexes, Acc) ->
        tos_blocks(From, Block, Indexes, Acc)
    end, [], Blocks),
    {ok, lists:sort(Tos)}.

%%--------------------------------------------------------------------

tos_blocks(From, Block, Indexes, Acc0) ->
    maps:fold(fun (Index, Froms, Acc) ->
        tos_froms(From, Block, Index, Froms, Acc)
    end, Acc0, Indexes).

%%--------------------------------------------------------------------

tos_froms(From, Block, Index, Froms, Acc) ->
    case Froms of
        #{From := _} ->
            {Type, X, Y} = Block,
            [{Type, X, Y, 0, Index} | Acc];

        _ ->
            Acc
    end.

%%====================================================================
%% experiments
%%====================================================================

-spec experiments(from(), from(), cache())
    -> {ok, [experiment()]} | false.

experiments({Type, X, Y, 0, Index}, From, Cache = {route_cache, _, _, _}) ->
    experiments({Type, X, Y}, Index, From, Cache).

%%--------------------------------------------------------------------

-spec experiments(block(), index(), from(), cache())
    -> {ok, [experiment()]} | false.

experiments(Block, Index, From, {route_cache, _, Keys, Blocks}) ->
    case Blocks of
        #{Block := #{Index := #{From := KeyIndexes}}} ->
            {ok, [
                experiment(KeyIndex, Keys)
                ||
                KeyIndex <- KeyIndexes
            ]};

        _ ->
            false
    end.

%%--------------------------------------------------------------------

experiment(KeyIndex, Keys) ->
    #{KeyIndex := Key} = Keys,
    {ok, Result} = experiment_cache:iterate_read(Key),
    {Device, POFBinary, RCFBinary} = Result,
    Density = device:density(Device),
    {ok, POF} = pof:decode(POFBinary),
    Fuses = pof_file:fuses(Density, POF),
    {ok, RCF} = rcf_file:decode(RCFBinary),
    {KeyIndex, Fuses, RCF}.

%%====================================================================
%% fold_blocks
%%====================================================================

-spec fold_blocks(Type, Fold, Acc, cache()) -> Acc when
    Type :: atom(),
    Fold :: fun((block(), fold_indexes(), Acc) -> Acc),
    Acc :: term().

fold_blocks(Type, Fold, Init, Cache) ->
    Blocks = route_cache:blocks(Type, Cache),
    lists:foldl(
        fun (Block, Acc) ->
            fold_block(Fold, Block, Cache, Acc)
        end,
        Init,
        Blocks
    ).

%%--------------------------------------------------------------------

fold_block(Fold, Block, {route_cache, _, Keys, Blocks}, Acc) ->
    #{Block := Indexes} = Blocks,
    Fold(Block, {fold_indexes, Indexes, Keys}, Acc).

%%====================================================================
%% fold_block
%%====================================================================

-spec fold_block(block(), cache()) -> {ok, fold_indexes()} | false.

fold_block(Block, {route_cache, _, Keys, Blocks}) ->
    case Blocks of
        #{Block := Indexes} ->
            {ok, {fold_indexes, Indexes, Keys}};

        _ ->
            false
    end.

%%====================================================================
%% fold_indexes
%%====================================================================

-spec fold_indexes(Fold, Acc, fold_indexes()) -> Acc when
    Fold :: fun((index(), fold_froms(), Acc) -> Acc),
    Acc :: term().

fold_indexes(Fold, Init, Cache) ->
    Indexes = fold_indexes(Cache),
    lists:foldl(
        fun (Index, Acc) ->
            fold_index(Fold, Index, Cache, Acc)
        end,
        Init,
        Indexes
    ).

%%--------------------------------------------------------------------

fold_indexes({fold_indexes, Indexes, _}) ->
    lists:sort(maps:keys(Indexes)).

%%--------------------------------------------------------------------

fold_index(Fold, Index, {fold_indexes, Indexes, Keys}, Acc) ->
    #{Index := Froms} = Indexes,
    Fold(Index, {fold_froms, Froms, Keys}, Acc).

%%====================================================================
%% fold_froms
%%====================================================================

-spec fold_froms(Fold, Acc, fold_froms()) -> Acc when
    Fold :: fun((from(), fold_cached(), Acc) -> Acc),
    Acc :: term().

fold_froms(Fold, Init, {fold_froms, Froms, Keys}) ->
    maps:fold(
        fun (From, KeyIndexes, Acc) ->
            Fold(From, {fold_cached, KeyIndexes, Keys}, Acc)
        end,
        Init,
        Froms
    ).

%%====================================================================
%% fold_cached
%%====================================================================

-spec fold_cached(Init, Seed, Fold, fold_cached())
    -> {ok, Acc} | false
    when
        Init :: fun((experiment:result(), Seed) -> Acc),
        Seed :: term(),
        Fold :: fun((experiment:result(), Acc) -> Acc),
        Acc :: term().

-spec fold_cached(Init, Fold, Seed, fold_cached(), Options)
    -> {ok, Acc} | false
    when
        Init :: fun((experiment:result(), Seed) -> Acc),
        Seed :: term(),
        Fold :: fun((experiment:result(), Acc) -> Acc),
        Acc :: term(),
        Options :: #{limit => pos_integer()}.

fold_cached(Init, Seed, Fold, Cached) ->
    fold_cached(Init, Seed, Fold, Cached, #{}).

%%--------------------------------------------------------------------

fold_cached(Init, Seed, Fold, {fold_cached, KeyIndexes0, Keys}, Options) ->
    case limit(KeyIndexes0, Options) of
        [] ->
            false;

        [KeyIndex0 | KeyIndexes] ->
            {ok, lists:foldl(
                fun (KeyIndex, Acc) ->
                    fold_cached_fold(Fold, KeyIndex, Keys, Acc)
                end,
                fold_cached_fold(Init, KeyIndex0, Keys, Seed),
                KeyIndexes
            )}
    end.

%%--------------------------------------------------------------------

fold_cached_fold(Fold, KeyIndex, Keys, Acc) ->
    #{KeyIndex := Key} = Keys,
    {ok, Result} = experiment_cache:iterate_read(Key),
    Fold(Result, Acc).

%%--------------------------------------------------------------------

limit(List, #{limit := Limit}) ->
    case length(List) of
        Total when Total < Limit ->
            List;

        Total ->
            lists:nthtail(Total - Limit, List)
    end;
limit(List, _) ->
    List.

%%====================================================================
%% matrix
%%====================================================================

-spec matrix(block(), cache()) -> ok | false.

matrix(Block, {route_cache, Density, Keys, Blocks}) ->
    case Blocks of
        #{Block := Indexes} ->
            matrix_indexes(Density, Block, {fold_indexes, Indexes, Keys});

        _ ->
            false
    end.

%%--------------------------------------------------------------------

-spec matrix(block(), index(), cache()) -> ok | false.

matrix(Block, Index, {route_cache, Density, Keys, Blocks}) ->
    case Blocks of
        #{Block := #{Index := Froms}} ->
            matrix_froms(Density, Block, Index, {fold_froms, Froms, Keys});

        _ ->
            false
    end.

%%--------------------------------------------------------------------

matrix_indexes(Density, Block, Indexes) ->
    io:format(" ==> ~w ~w~n", [Density, Block]),
    Minimal = density:minimal_fuses(Density),
    Experiments0 = route_cache:fold_indexes(
        fun (Index, Froms, Acc0) ->
            route_cache:fold_froms(
                fun (From, Cached, Acc) ->
                    matrix_from({Index, From}, Cached, Acc)
                end,
                Acc0,
                Froms
            )
        end,
        [],
        Indexes
    ),
    Experiments = [{minimal, Minimal} | Experiments0],
    Matrix0 = matrix:build(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{iob, _, _}, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{lab, _, _}, _, _}) -> true;
        ({{lc, _, _, _}, _}) -> true;
        ({{lc, _, _, _}, _, _}) -> true;
        (_) -> false
    end),
    matrix:print(Matrix),
    ok.

%%--------------------------------------------------------------------

matrix_froms(Density, Block, Index, Froms) ->
    io:format(" ==> ~w ~w ~w~n", [Density, Block, Index]),
    Minimal = density:minimal_fuses(Density),
    Experiments0 = route_cache:fold_froms(
        fun (From, Cached, Acc) ->
            matrix_from(From, Cached, Acc)
        end,
        [],
        Froms
    ),
    Experiments = [{minimal, Minimal} | Experiments0],
    Matrix0 = matrix:build(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{iob, _, _}, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{lab, _, _}, _, _}) -> true;
        ({{lc, _, _, _}, _}) -> true;
        ({{lc, _, _, _}, _, _}) -> true;
        (_) -> false
    end),
    matrix:print(Matrix),
    ok.

%%--------------------------------------------------------------------

matrix_from(From, Cached, Experiments) ->
    case matrix_reduce(Cached) of
        {ok, Fuses} ->
            Experiment = {From, Fuses},
            [Experiment | Experiments];

        false ->
            Experiments
    end.

%%--------------------------------------------------------------------

matrix_reduce(Cached) ->
    route_cache:fold_cached(
        fun matrix_reduce_zero/2,
        zero,
        fun matrix_reduce_rest/2,
        Cached,
        #{limit => 50}
    ).

%%--------------------------------------------------------------------

matrix_reduce_zero(Experiment, zero) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    Fuses.

%%--------------------------------------------------------------------

matrix_reduce_rest(Experiment, Fuses0) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    fuses:intersect(Fuses0, Fuses).

%%====================================================================
%% utility
%%====================================================================

density_file(Density) ->
    Name = lists:flatten(io_lib:format("~s.routes", [Density])),
    filename:join("cache", Name).

