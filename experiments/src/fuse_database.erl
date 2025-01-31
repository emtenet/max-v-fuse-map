-module(fuse_database).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun export/1, density:list()),
    ok.

%%--------------------------------------------------------------------

export(Density) ->
    Count = density:fuse_count(Density),
    Lines = export_fuse(Count, Density, []),
    File = database_file(Density),
    ok = file:write_file(File, Lines).

%%--------------------------------------------------------------------

export_fuse(0, _, Lines) ->
    Lines;
export_fuse(Prev, Density, Lines) ->
    Fuse = Prev - 1,
    case fuse_map:to_name(Fuse, Density) of
        {ok, Name} ->
            Line = io_lib:format("~b=~w~n", [Fuse, Name]),
            export_fuse(Fuse, Density, [Line | Lines]);

        {error, _} ->
            export_fuse(Fuse, Density, Lines)
    end.

%%====================================================================
%% database
%%====================================================================

database_file(Density) ->
    lists:flatten(io_lib:format("../database/~s.fuses", [Density])).

