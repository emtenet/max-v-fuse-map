-module(experiment_cache).

-export([init/0]).

-export([load/1]).
-export([store/2]).
-export([flush/1]).

-export([iterate/0]).
-export([iterate/1]).
-export([iterate_read/1]).

-export_type([slot/0]).
-export_type([iterator/0]).

-type slot() :: {slot, device(), signature(), file:filename_all()}.

-opaque iterator() ::
    {cache,
     [file:filename_all()],
     [file:filename_all()],
     [file:filename_all()]
    }.

-type base64() :: binary().
-type device() :: device:device().
-type files() :: experiment_compile:files().
-type source() :: experiment_compile:source().
-type signature() :: binary().

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% init
%%====================================================================

-spec init() -> ok.

init() ->
    case file:make_dir("cache") of
        ok ->
            init(0);

        {error, eexist} ->
            init(0)
    end.

%%--------------------------------------------------------------------

init(4096) ->
    ok;
init(N) ->
    <<Head:2/binary, _>> = base64url:encode(<<N:12/integer, 0:4/integer>>),
    Dir = filename:join("cache", Head),
    case file:make_dir(Dir) of
        ok ->
            init(N + 1);

        {error, eexist} ->
            init(N + 1)
    end.

%%====================================================================
%% load
%%====================================================================

-spec load(source()) -> {hit, experiment:result()} | {miss, slot()}.

load(Source = #{device := Device}) ->
    Signature = signature(Source),
    File = cache_file(Signature),
    case file:read_file(File) of
        {ok, Zipped} ->
            {hit, gunzip(Zipped)};

        {error, enoent} ->
            {miss, {slot, Device, Signature, File}}
    end.

%%====================================================================
%% store
%%====================================================================

-spec store(slot(), files()) -> ok.

store({slot, _, Signature, File}, #{pof := POF, rcf := RCF}) ->
    Zipped = gzip(Signature, POF, RCF),
    ok = file:write_file(File, Zipped).

%%====================================================================
%% flush
%%====================================================================

-spec flush(source()) -> ok.

flush(Source) ->
    Signature = signature(Source),
    File = cache_file(Signature),
    case file:delete(File) of
        ok ->
            ok;

        {error, enoent} ->
            ok
    end.

%%====================================================================
%% iterate
%%====================================================================

-spec iterate()
    -> {base64(), experiment:result(), iterator()} | false.

iterate() ->
    {ok, Outers} = file:list_dir("cache"),
    iterate([], undefined, lists:sort(Outers)).

%%--------------------------------------------------------------------

-spec iterate(iterator())
    -> {base64(), experiment:result(), iterator()} | false.

iterate({cache, Inners, Outer, Outers}) ->
    iterate(Inners, Outer, Outers).

%%--------------------------------------------------------------------

iterate([], _, []) ->
    io:format("                           \r", []),
    false;
iterate([], _, [Outer = [_, _] | Outers]) ->
    {ok, Inners} = file:list_dir(filename:join("cache", Outer)),
    io:format("cache ~s\r", [Outer]),
    iterate(Inners, Outer, Outers);
iterate([], Outer, [_ | Outers]) ->
    iterate([], Outer, Outers);
iterate([Inner | Inners], Outer, Outers) ->
    Next = {cache, Inners, Outer, Outers},
    File = list_to_binary(filename:join(["cache", Outer, Inner])),
    case file:read_file(File) of
        {ok, Zipped} ->
            {base64(Outer, Inner), gunzip(Zipped), Next};

        {error, enoent} ->
            iterate(Inners, Outer, Outers)
    end.

%%====================================================================
%% iterate_read
%%====================================================================

-spec iterate_read(base64()) -> {ok, experiment:result()} | false.

iterate_read(Key) ->
    <<Head:2/binary, Tail/binary>> = Key,
    File = filename:join(["cache", Head, <<Tail/binary, ".gz">>]),
    case file:read_file(File) of
        {ok, Zipped} ->
            {ok, gunzip(Zipped)};

        {error, enoent} ->
            false
    end.

%%====================================================================
%% internal
%%====================================================================

signature(#{device := Device, settings := Settings, verilog := Verilog}) ->
    iolist_to_binary([
        Device,
        <<"\n">>,
        Settings,
        <<"\n====================\n">>,
        Verilog
    ]).

%%--------------------------------------------------------------------

cache_file(Signature) ->
    Hash = crypto:hash(sha256, Signature),
    Base64 = base64url:encode(Hash),
    <<Head:2/binary, Tail/binary>> = Base64,
    filename:join(["cache", Head, <<Tail/binary, ".gz">>]).

%%--------------------------------------------------------------------

base64([A, B], Tail) ->
    <<Base64:43/binary, ".gz">> = list_to_binary([A, B | Tail]),
    Base64.

%%--------------------------------------------------------------------

gzip(Signature, POF, RCF) ->
    SignatureSize = byte_size(Signature),
    POFSize = byte_size(POF),
    RCFSize = byte_size(RCF),
    Packed = <<
        SignatureSize:32/integer,
        POFSize:32/integer,
        RCFSize:32/integer,
        Signature/binary,
        POF/binary,
        RCF/binary
    >>,
    zlib:gzip(Packed).

%%--------------------------------------------------------------------

gunzip(Zipped) ->
    Packed = zlib:gunzip(Zipped),
    <<
        SignatureSize:32/integer,
        POFSize:32/integer,
        RCFSize:32/integer,
        Rest/binary
    >> = Packed,
    <<
        Signature:SignatureSize/binary,
        POF:POFSize/binary,
        RCF:RCFSize/binary
    >> = Rest,
    [Device0, _] = binary:split(Signature, <<"\n">>),
    Device = device:from_name(Device0),
    {Device, POF, RCF}.

