-module(jtag_mux_playground).

-export([run/0]).

% This experiment looks for pairs of fuses that could form the
% JTAG mux's.
%
% MAX V 240Z has its fuses in a different place than the other
% densities so process it separately.

% Another related fuse?
%  * {9,2,7,1,cell,21}
%  * {11,2,7,1,cell,21}
%  * {13,2,7,1,cell,21}

%%====================================================================
%% run
%%====================================================================

run() ->
    Opts = #{
        block_type => tdo,
        fuse_key => fun fuse_key/1,
        remove_fuse => fun remove_fuse/2
    },
    [max_v_240z | Densitys] = density:list(),
    generic_mux_playground:run([max_v_240z], Opts),
    generic_mux_playground:run(Densitys, Opts),
    ok.

%%--------------------------------------------------------------------

fuse_key({{jtag, X, Y}, N, Key, Value}) when is_integer(N) ->
    {X, Y, N, Key, Value};
fuse_key({{jtag, _, _}, _, Key, Value}) ->
    {Key, Value};
fuse_key(Fuse) ->
    Fuse.

%%--------------------------------------------------------------------

remove_fuse({{jtag, _, _}, T, _, _}, #{block_type := T}) ->
    false;
%remove_fuse({{ufm, _, _}, N, _, _}, _) when is_integer(N) ->
%    false;
remove_fuse({{ioc, 1, _, N}, _}, _) when N > 3 ->
    false;
remove_fuse({{ioc, 1, _, N}, _, _}, _) when N > 3 ->
    false;
remove_fuse({_, _, _, _, _}, _) ->
    false;
remove_fuse({_, _, _, _, _, _}, _) ->
    false;
remove_fuse(_, _) ->
    true.

