-module(r4_fuse_map).

-export([to_name/2]).

%%====================================================================
%% to_name
%%====================================================================

to_name({{r4, XX, YY}, {mux, MM}, Value}, Density) ->
    case to_name(XX, YY, MM, Density) of
        {x, _} ->
            false;

        {_, x} ->
            false;

        {X, I} ->
            {ok, {{r4, X, YY}, {interconnect, I}, Value}}
    end;
to_name({{r4, XX, YY}, {mux, MM}, Key, Value}, Density) ->
    case to_name(XX, YY, MM, Density) of
        {x, _} ->
            false;

        {_, x} ->
            false;

        {X, I} ->
            {ok, {{r4, X, YY}, {interconnect, I}, Key, Value}}
    end;
to_name(_, _) ->
    false.

%%--------------------------------------------------------------------

to_name(XX, YY, _, _)
        when not is_integer(XX) orelse XX < 0 orelse
             not is_integer(YY) orelse YY < 0 ->
    {x, x};
to_name(XX, YY, MM, max_v_240z) ->
    to_max_v_240z(XX, YY, MM);
to_name(XX, YY, MM, max_v_570z) ->
    to_max_v_570z(XX, YY, MM);
to_name(XX, YY, MM, max_v_1270z) ->
    to_max_v_1270z(XX, YY, MM);
to_name(XX, YY, MM, max_v_2210z) ->
    to_max_v_2210z(XX, YY, MM);
to_name(_, _, _, _) ->
    {x, x}.

