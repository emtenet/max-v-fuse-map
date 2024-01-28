-module(c4_fuse_map).

-export([run/0]).

-export([from_name/2]).
-export([to_name/2]).

%%====================================================================
%% run
%%====================================================================

run() ->
    run(max_v_240z, 7, 5),
    run(max_v_570z, 12, 8),
    run(max_v_1270z, 16, 11),
    run(max_v_2210z, 20, 14),
    ok.

%%--------------------------------------------------------------------

run(Density, MaxX, MaxY) ->
    io:format(" => ~s~n", [Density]),
    [
        run_at(Density, X, Y)
        ||
        Y <- lists:seq(0, MaxY),
        X <- lists:seq(0, MaxX)
    ],
    ok.

%%--------------------------------------------------------------------

run_at(Density, X, Y) ->
    [
        run_from(Density, X, Y, I)
        ||
        I <- lists:seq(0, 34)
    ],
    [
        run_to(Density, X, Y, M)
        ||
        M <- lists:seq(0, 13)
    ],
    ok.

%%--------------------------------------------------------------------

run_from(Density, X, Y, I) ->
    Interconnect = {{c4, X, Y}, {interconnect, I}, direct_link},
    case from_name(Interconnect, Density) of
        {ok, Mux} ->
            case to_name(Mux, Density) of
                {ok, Interconnect} ->
                    ok;

                {ok, Different} ->
                    throw({Density, Interconnect, to, Mux, from, Different});

                false ->
                    throw({Density, Interconnect, to, Mux, from, missing})
            end;

        false ->
            ok
    end.

%%--------------------------------------------------------------------

run_to(Density, XX, YY, MM) ->
    Mux = {{c4, XX, YY}, {mux, MM}, direct_link},
    case to_name(Mux, Density) of
        {ok, Interconnect} ->
            case from_name(Interconnect, Density) of
                {ok, Mux} ->
                    ok;

                {ok, Different} ->
                    throw({Density, Mux, to, Interconnect, from, Different});

                false ->
                    throw({Density, Mux, to, Interconnect, from, missing})
            end;

        false ->
            ok
    end.

%%====================================================================
%% from_name
%%====================================================================

from_name({{c4, X, Y}, {interconnect, I}, Value}, Density) ->
    case from_name(X, Y, I, Density) of
        {x, _} ->
            false;

        {_, x} ->
            false;

        {YY, MM} ->
            {ok, {{c4, X, YY}, {mux, MM}, Value}}
    end;
from_name({{c4, X, Y}, {interconnect, I}, Key, Value}, Density) ->
    case from_name(X, Y, I, Density) of
        {x, _} ->
            false;

        {_, x} ->
            false;

        {YY, MM} ->
            {ok, {{c4, X, YY}, {mux, MM}, Key, Value}}
    end;
from_name(_, _) ->
    false.

%%--------------------------------------------------------------------

from_name(X, Y, _, _)
        when not is_integer(X) orelse X < 0 orelse
             not is_integer(Y) orelse Y < 0 ->
    {x, x};
from_name(X, Y, M, max_v_240z) ->
    from_max_v_240z(X, Y, M);
from_name(X, Y, M, max_v_570z) ->
    from_max_v_570z(X, Y, M);
from_name(X, Y, M, max_v_1270z) ->
    from_max_v_1270z(X, Y, M);
from_name(X, Y, M, max_v_2210z) ->
    from_max_v_2210z(X, Y, M);
from_name(_, _, _, _) ->
    {x, x}.

%%====================================================================
%% to_name
%%====================================================================

to_name({{c4, XX, YY}, {mux, MM}, Value}, Density) ->
    case to_name(XX, YY, MM, Density) of
        {x, _} ->
            false;

        {_, x} ->
            false;

        {Y, I} ->
            {ok, {{c4, XX, Y}, {interconnect, I}, Value}}
    end;
to_name({{c4, XX, YY}, {mux, MM}, Key, Value}, Density) ->
    case to_name(XX, YY, MM, Density) of
        {x, _} ->
            false;

        {_, x} ->
            false;

        {Y, I} ->
            {ok, {{c4, XX, Y}, {interconnect, I}, Key, Value}}
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

%%====================================================================
%% from_max_v_240z
%%====================================================================

from_max_v_240z(X, Y, _)
        when X > 7 orelse Y > 5 ->
    {x, x};
from_max_v_240z(X, Y, 0) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, 0, 0, x, x, 0, 0}, %  1
        { x, 1, 1, 1, 1, 1, 1, 1}, %  2
        { x, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, 3, 3, 3, 3, 3, 3}, %  4
        { x, 4, 4, 4, 4, 4, 4, 4}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 7, 7, 7, 7, 7, 7, 7}, %  0
        { x, x, 0, 7, x, x, 0, 7}, %  1
        { x, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 0, 0, 0, 0, 0, 0, 0}, %  3
        { x, 0, 0, 0, 0, 0, 0, 0}, %  4
        { x, 0, 0, 0, 0, 0, 0, 0}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 1) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, 0, x, 0, x, 0, x}, %  1
        { x, 1, 1, 1, 1, 1, 1, 1}, %  2
        { x, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, 3, 3, 3, 3, 3, 3}, %  4
        { x, 4, 4, 4, 4, 4, 4, 4}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 7, 7, 7, 7, 7, 7, 7}, %  0
        { x, x, 7, x, 0, x, 7, x}, %  1
        { x, 1, 1, 1, 1, 1, 1, 1}, %  2
        { x, 1, 1, 1, 1, 1, 1, 1}, %  3
        { x, 1, 1, 1, 1, 1, 1, 1}, %  4
        { x, 1, 1, 1, 1, 1, 1, 1}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 2) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, 0, 0, x, x}, %  1
        { x, 1, 1, 1, 1, 1, 1, 1}, %  2
        { x, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, 3, 3, 3, 3, 3, 3}, %  4
        { x, 4, 4, 4, 4, 4, 4, 4}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 7, 7, 7, 7, 7, 7, 7}, %  0
        { x, x, x, x, 7, 0, x, x}, %  1
        { x, 2, 2, 2, 2, 2, 2, 2}, %  2
        { x, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 2, 2, 2, 2, 2, 2, 2}, %  4
        { x, 2, 2, 2, 2, 2, 2, 2}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 3) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, 0, x, 0, x, 0, x, x}, %  1
        { x, 1, 1, 1, 1, 1, 1, 1}, %  2
        { x, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, 3, 3, 3, 3, 3, 3}, %  4
        { x, 4, 4, 4, 4, 4, 4, 4}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 7, 7, 7, 7, 7, 7, 7}, %  0
        { x, 3, x, 0, x, 7, x, x}, %  1
        { x, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, 3, 3, 3, 3, 3, 3, 3}, %  3
        { x, 3, 3, 3, 3, 3, 3, 3}, %  4
        { x, 3, 3, 3, 3, 3, 3, 3}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 4) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, 0, 0, x, x, 0, 0}, %  1
        { x, 1, 1, 1, 1, 1, 1, 1}, %  2
        { x, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, 3, 3, 3, 3, 3, 3}, %  4
        { x, 4, 4, 4, 4, 4, 4, 4}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 8, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, 1, 8, x, x, 1, 8}, %  1
        { x, 4, 4, 4, 4, 4, 4, 4}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4}, %  3
        { x, 4, 4, 4, 4, 4, 4, 4}, %  4
        { x, 4, 4, 4, 4, 4, 4, 4}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 5) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, 0, x, 0, x, 0, x}, %  1
        { x, 1, 1, 1, 1, 1, 1, 1}, %  2
        { x, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, 3, 3, 3, 3, 3, 3}, %  4
        { x, 4, 4, 4, 4, 4, 4, 4}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 8, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, 8, x, 1, x, 8, x}, %  1
        { x, 5, 5, 5, 5, 5, 5, 5}, %  2
        { x, 5, 5, 5, 5, 5, 5, 5}, %  3
        { x, 5, 5, 5, 5, 5, 5, 5}, %  4
        { x, 5, 5, 5, 5, 5, 5, 5}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 6) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, 0, 0, x, x}, %  1
        { x, 1, 1, 1, 1, 1, 1, 1}, %  2
        { x, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, 3, 3, 3, 3, 3, 3}, %  4
        { x, 4, 4, 4, 4, 4, 4, 4}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 8, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, x, x, 8, 1, x, x}, %  1
        { x, 6, 6, 6, 6, 6, 6, 6}, %  2
        { x, 6, 6, 6, 6, 6, 6, 6}, %  3
        { x, 6, 6, 6, 6, 6, 6, 6}, %  4
        { x, 6, 6, 6, 6, 6, 6, 6}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 7) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, 0, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, 5, 5, 5, 5, 5, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 8, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, x, 1, x, 8, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, 0, 0, 0, 0, 0, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 8) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, 0, x, x, x, 0}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, 5, 5, 5, 5, 5, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 9, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, x, 9, x, x, x, 9}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, 1, 1, 1, 1, 1, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 9) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, 0, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, 5, 5, 5, 5, 5, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 9, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, 9, x, x, x, 9, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, 2, 2, 2, 2, 2, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 10) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 9, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, x, x, 9, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 11) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 9, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, 9, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 12) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, 0, 0, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,10,10,10,10,10,10,10}, %  0
        { x, x, 3,10, x, x, 3, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 13) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, 0, x, 0, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,10,10,10,10,10,10,10}, %  0
        { x, x,10, x, 3, x,10, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 14) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, 0, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,10,10,10,10,10,10,10}, %  0
        { x, x, x, x,10, 3, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 15) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, 0, x, 0, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,10,10,10,10,10,10,10}, %  0
        { x,10, x, 3, x,10, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 16) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, 0, x, x, x, 0}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,11,11,11,11,11,11,11}, %  0
        { x, x, x,11, x, x, x, 1}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 17) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, 0, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,11,11,11,11,11,11,11}, %  0
        { x, x,11, x, x, x,11, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 18) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,11,11,11,11,11,11,11}, %  0
        { x, x, x, x,11, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 19) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, 0, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,11,11,11,11,11,11,11}, %  0
        { x,11, x, x, x,11, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 20) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, 0, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,12,12,12,12,12,12,12}, %  0
        { x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 21) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, 0, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,12,12,12,12,12,12,12}, %  0
        { x, x,12, x, x, x,12, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 22) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,12,12,12,12,12,12,12}, %  0
        { x, x, x, x,12, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 23) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, 0, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,12,12,12,12,12,12,12}, %  0
        { x,12, x, x, x,12, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 24) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, 0, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,13,13,13,13,13,13,13}, %  0
        { x, x, x,13, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 25) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, 0, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,13,13,13,13,13,13,13}, %  0
        { x, x,13, x, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 26) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,13,13,13,13,13,13,13}, %  0
        { x, x, x, x,13, x, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 27) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, 0, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x,13,13,13,13,13,13,13}, %  0
        { x,13, x, x, x,13, x, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 28) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, x, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, x, 7, 7, 7, 7, 7, 7}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 29) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, x, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, x, 8, 8, 8, 8, 8, 8}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 30) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 9, 9, 9, 9, 9, 9, 0}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 31) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x,10,10,10,10,10,10, 1}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 32) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x,11,11,11,11,11,11, 2}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 33) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 5, 5, 5, 5, 5, 5, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x,12,12,12,12,12,12, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(X, Y, 34) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 5, 5, 5, 5, 5, 5, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x,13,13,13,13,13,13, x}, %  1
        { x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {YY, MM};
from_max_v_240z(_, _, _) ->
    {x, x}.

%%====================================================================
%% to_max_v_240z
%%====================================================================

to_max_v_240z(XX, YY, _)
        when XX > 7 orelse YY > 5 ->
    {x, x};
to_max_v_240z(XX, YY, 0) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, 1, 1, 1, 1, 1, x}, %  0
        { x, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4}, %  3
        { x, 5, 5, 5, 5, 5, 5, 5}, %  4
        { x, x, 4, 4, 4, 4, 4, 1}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, 0, 3, 1, 2, 0, x}, %  0
        { x, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 0, 0, 0, 0, 0, 0, 0}, %  3
        { x, 0, 0, 0, 0, 0, 0, 0}, %  4
        { x, x, 7, 7, 7, 7, 7,30}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 1) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, 1, 1, 1, 1, 1, 1}, %  0
        { x, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4}, %  3
        { x, 5, 5, 5, 5, 5, 5, 5}, %  4
        { x, x, 4, 4, 4, 4, 4, 1}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, 4, 7, 5, 6, 4,16}, %  0
        { x, 1, 1, 1, 1, 1, 1, 1}, %  1
        { x, 1, 1, 1, 1, 1, 1, 1}, %  2
        { x, 1, 1, 1, 1, 1, 1, 1}, %  3
        { x, 1, 1, 1, 1, 1, 1, 1}, %  4
        { x, x, 8, 8, 8, 8, 8,31}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 2) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4}, %  3
        { x, 5, 5, 5, 5, 5, 5, 5}, %  4
        { x, x, 4, 4, 4, 4, 4, 1}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, 2, 2, 2, 2, 2, 2, 2}, %  2
        { x, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 2, 2, 2, 2, 2, 2, 2}, %  4
        { x, x, 9, 9, 9, 9, 9,32}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 3) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 1, 1, 1, 1, 1, x}, %  0
        { x, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4}, %  3
        { x, 5, 5, 5, 5, 5, 5, 5}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 3,12,15,13,14,12, x}, %  0
        { x, 3, 3, 3, 3, 3, 3, 3}, %  1
        { x, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, 3, 3, 3, 3, 3, 3, 3}, %  3
        { x, 3, 3, 3, 3, 3, 3, 3}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 4) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4}, %  3
        { x, 5, 5, 5, 5, 5, 5, 5}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 4, 4, 4, 4, 4, 4, 4}, %  1
        { x, 4, 4, 4, 4, 4, 4, 4}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4}, %  3
        { x, 4, 4, 4, 4, 4, 4, 4}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 5) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4}, %  3
        { x, 5, 5, 5, 5, 5, 5, 5}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, 5, 5, 5, 5, 5, 5, 5}, %  2
        { x, 5, 5, 5, 5, 5, 5, 5}, %  3
        { x, 5, 5, 5, 5, 5, 5, 5}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 6) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4}, %  3
        { x, 5, 5, 5, 5, 5, 5, 5}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, x, x, x, x, x, x}, %  0
        { x, 6, 6, 6, 6, 6, 6, 6}, %  1
        { x, 6, 6, 6, 6, 6, 6, 6}, %  2
        { x, 6, 6, 6, 6, 6, 6, 6}, %  3
        { x, 6, 6, 6, 6, 6, 6, 6}, %  4
        { x, x, x, x, x, x, x, x}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 7) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, 1, 1, 1, 1, 1, 1}, %  0
        { x, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 0, 0, 0, 0, 0, 0, 0}, %  3
        { x, 0, 0, 0, 0, 0, 0, 0}, %  4
        { x, x, 1, 1, 1, 1, 1, 1}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, 1, 0, 2, 3, 1, 0}, %  0
        { x, 2, 0, 3, 1, 2, 0, 3}, %  1
        { x, 1, 3, 2, 0, 1, 3, 2}, %  2
        { x, 0, 2, 1, 3, 0, 2, 1}, %  3
        { x, 3, 1, 0, 2, 3, 1, 0}, %  4
        { x, x,28,28,28,28,28,28}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 8) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, 1, 1, 1, 1, 1, 1}, %  0
        { x, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 0, 0, 0, 0, 0, 0, 0}, %  3
        { x, 0, 0, 0, 0, 0, 0, 0}, %  4
        { x, x, 1, 1, 1, 1, 1, 1}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, 5, 4, 6, 7, 5, 4}, %  0
        { x, 6, 4, 7, 5, 6, 4, 7}, %  1
        { x, 5, 7, 6, 4, 5, 7, 6}, %  2
        { x, 4, 6, 5, 7, 4, 6, 5}, %  3
        { x, 7, 5, 4, 6, 7, 5, 4}, %  4
        { x, x,29,29,29,29,29,29}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 9) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, 1, 1, 1, 1, 1, 1}, %  0
        { x, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 0, 0, 0, 0, 0, 0, 0}, %  3
        { x, 0, 0, 0, 0, 0, 0, 0}, %  4
        { x, 1, 1, 1, 1, 1, 1, x}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, x, 9, 8,10,11, 9, 8}, %  0
        { x,10, 8,11, 9,10, 8,11}, %  1
        { x, 9,11,10, 8, 9,11,10}, %  2
        { x, 8,10, 9,11, 8,10, 9}, %  3
        { x,11, 9, 8,10,11, 9, 8}, %  4
        { x,30,30,30,30,30,30, x}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 10) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 1, 1, 1, 1, 1, x}, %  0
        { x, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 0, 0, 0, 0, 0, 0, 0}, %  3
        { x, 0, 0, 0, 0, 0, 0, 0}, %  4
        { x, 1, 1, 1, 1, 1, 1, x}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x,15,13,12,14,15,13, x}, %  0
        { x,14,12,15,13,14,12,15}, %  1
        { x,13,15,14,12,13,15,14}, %  2
        { x,12,14,13,15,12,14,13}, %  3
        { x,15,13,12,14,15,13,12}, %  4
        { x,31,31,31,31,31,31, x}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 11) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 1, 1, 1, 1, 1, x}, %  0
        { x, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 0, 0, 0, 0, 0, 0, 0}, %  3
        { x, 0, 0, 0, 0, 0, 0, 0}, %  4
        { x, 1, 1, 1, 1, 1, 1, x}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x,19,17,16,18,19,17, x}, %  0
        { x,18,16,19,17,18,16,19}, %  1
        { x,17,19,18,16,17,19,18}, %  2
        { x,16,18,17,19,16,18,17}, %  3
        { x,19,17,16,18,19,17,16}, %  4
        { x,32,32,32,32,32,32, x}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 12) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 1, 1, 1, 1, 1, x}, %  0
        { x, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 0, 0, 0, 0, 0, 0, 0}, %  3
        { x, 0, 0, 0, 0, 0, 0, 0}, %  4
        { x, 1, 1, 1, 1, 1, 1, x}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x,23,21,20,22,23,21, x}, %  0
        { x,22,20,23,21,22,20,23}, %  1
        { x,21,23,22,20,21,23,22}, %  2
        { x,20,22,21,23,20,22,21}, %  3
        { x,23,21,20,22,23,21,20}, %  4
        { x,33,33,33,33,33,33, x}  %  5
    })),
    {Y, I};
to_max_v_240z(XX, YY, 13) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x, 1, 1, 1, 1, 1, 1, x}, %  0
        { x, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 0, 0, 0, 0, 0, 0, 0}, %  3
        { x, 0, 0, 0, 0, 0, 0, 0}, %  4
        { x, 1, 1, 1, 1, 1, 1, x}  %  5
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7
        { x,27,25,24,26,27,25, x}, %  0
        { x,26,24,27,25,26,24,27}, %  1
        { x,25,27,26,24,25,27,26}, %  2
        { x,24,26,25,27,24,26,25}, %  3
        { x,27,25,24,26,27,25,24}, %  4
        { x,34,34,34,34,34,34, x}  %  5
    })),
    {Y, I};
to_max_v_240z(_, _, _) ->
    {x, x}.

%%====================================================================
%% from_max_v_570z
%%====================================================================

from_max_v_570z(X, Y, _)
        when X > 12 orelse Y > 8 ->
    {x, x};
from_max_v_570z(X, Y, 0) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, 0, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 2, 2, 2, 2}, %  3
        { 3, x, x, 3, 3, x, x, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, 0, x, 7, x}, %  1
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 0, 0, 0, 0}, %  3
        { 3, x, x, 0, 7, x, x, 0, 7, 0, 0, 0, 0}, %  4
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  5
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  6
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  7
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 1) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 2, 2, 2, 2}, %  3
        { x, 3, x, 3, x, 3, x, 3, x, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 1, 1, 1, 1}, %  3
        { x, 0, x, 7, x, 0, x, 7, x, 1, 1, 1, 1}, %  4
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  6
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  7
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 2) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 2, 2, 2, 2}, %  3
        { x, 3, 3, x, x, 3, 3, x, x, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, 7, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 2, 2, 2, 2}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 2, 2, 2, 2}, %  3
        { x, 7, 0, x, x, 7, 0, x, x, 2, 2, 2, 2}, %  4
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  5
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  7
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 3) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 2, 2, 2, 2}, %  3
        { x, x, 3, x, 3, x, 3, x, x, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 7}, %  1
        { x, x, x, x, x, x, x, x, x, 3, 3, 3, 3}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 3, 3, 3, 3}, %  3
        { x, x, 7, x, 0, x, 7, x, x, 3, 3, 3, 3}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 4) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, 0, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 2, 2, 2, 2}, %  3
        { x, x, x, 3, 3, x, x, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, 1, x, 8, x}, %  1
        { x, x, x, x, x, x, x, x, x, 4, 4, 4, 4}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4}, %  3
        { x, x, x, 1, 8, x, x, 1, 8, 4, 4, 4, 4}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  6
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 5) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 2, 2, 2, 2}, %  3
        { x, 3, x, 3, x, 3, x, 3, x, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, x, 1, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 5, 5, 5, 5}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 5, 5, 5, 5}, %  3
        { x, 1, x, 8, x, 1, x, 8, x, 5, 5, 5, 5}, %  4
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  7
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 6) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 2, 2, 2, 2}, %  3
        { x, 3, 3, x, x, 3, 3, x, x, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, x, 8, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 6, 6, 6, 6}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 6, 6, 6, 6}, %  3
        { x, 8, 1, x, x, 8, 1, x, x, 6, 6, 6, 6}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 7) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, 6, 6, 6, 6}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 7, 7, 7, 7}, %  3
        { x, x, 3, x, 3, x, 3, x, x, 8, 8, 8, 8}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 1, 8}, %  1
        { x, x, x, x, x, x, x, x, x, 7, 7, 7, 7}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7}, %  3
        { x, x, 8, x, 1, x, 8, x, x, 7, 7, 7, 7}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 8) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, 6, 6, 6, 6}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 7, 7, 7, 7}, %  3
        { x, x, x, x, 3, x, x, x, 3, 8, 8, 8, 8}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, 8, 8, 8, 8}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 8, 8, 8, 8}, %  3
        { x, x, x, x, 9, x, x, x, 9, 8, 8, 8, 8}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 9) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 6, 6, 6, 6}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 7, 7, 7, 7}, %  3
        { x, x, x, 3, x, x, x, 3, x, 8, 8, 8, 8}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  3
        { x, x, x, 9, x, x, x, 9, x, 9, 9, 9, 0}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 10) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 6, 6, 6, 6}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 7, 7, 7}, %  3
        { x, 3, x, x, x, 3, x, x, x, 8, 8, 8, 8}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, 9, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10,10,10,10}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9,10,10,10,10}, %  3
        { x, 9, x, x, x, 9, x, x, x,10,10,10, 1}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 11) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0}, %  1
        { x, x, x, x, x, x, x, x, x, 6, 6, 6, 6}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 7, 7, 7, 7}, %  3
        { x, x, 3, x, x, x, 3, x, x, 8, 8, 8, 8}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 9}, %  1
        { x, x, x, x, x, x, x, x, x,11,11,11,11}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9,11,11,11,11}, %  3
        { x, x, 9, x, x, x, 9, x, x,11,11,11, 2}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 12) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, 0, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, 6, 6, 6, 6}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 7, 7, 7, 7}, %  3
        { 3, x, x, 3, 3, x, x, 3, 3, 8, 8, 8, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x, 3, x,10, x}, %  1
        { x, x, x, x, x, x, x, x, x,12,12,12,12}, %  2
        {10,10,10,10,10,10,10,10,10,12,12,12,12}, %  3
        {10, x, x, 3,10, x, x, 3, 0,12,12,12, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 13) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, 0, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 6, 6, 6, 6}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 7, 7, 7, 7}, %  3
        { x, 3, x, 3, x, 3, x, 3, x, 8, 8, 8, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x,10, 3, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,13,13,13,13}, %  2
        {10,10,10,10,10,10,10,10,10,13,13,13,13}, %  3
        { x, 3, x,10, x, 3, x,10, x,13,13,13, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 14) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, x, x, x, x}, %  3
        { x, 3, 3, x, x, 3, 3, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x, x,10, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {10,10,10,10,10,10,10,10,10, x, x, x, x}, %  3
        { x,10, 3, x, x,10, 3, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 15) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, x, x, x, x}, %  3
        { x, x, 3, x, 3, x, 3, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 3, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {10,10,10,10,10,10,10,10,10, x, x, x, x}, %  3
        { x, x,10, x, 3, x,10, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 16) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, x, x, x, x}, %  3
        { 3, x, x, x, 3, x, x, x, 3, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11, x, x, x, x}, %  3
        {11, x, x, x,11, x, x, x, 1, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 17) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, x, x, x, x}, %  3
        { x, x, x, 3, x, x, x, 3, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11, x, x, x, x}, %  3
        { x, x, x,11, x, x, x,11, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 18) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, x, x, x, x}, %  3
        { x, 3, x, x, x, 3, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x, x,11, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11, x, x, x, x}, %  3
        { x,11, x, x, x,11, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 19) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, x, x, x, x}, %  3
        { x, x, 3, x, x, x, 3, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 1}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11, x, x, x, x}, %  3
        { x, x,11, x, x, x,11, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 20) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, x, x, x, x}, %  3
        { 3, x, x, x, 3, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12, x, x, x, x}, %  3
        {12, x, x, x,12, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 21) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, x, x, x, x}, %  3
        { x, x, x, 3, x, x, x, 3, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12, x, x, x, x}, %  3
        { x, x, x,12, x, x, x,12, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 22) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, x, x, x, x}, %  3
        { x, 3, x, x, x, 3, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x, x,12, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12, x, x, x, x}, %  3
        { x,12, x, x, x,12, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 23) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, x, x, x, x}, %  3
        { x, x, 3, x, x, x, 3, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12, x, x, x, x}, %  3
        { x, x,12, x, x, x,12, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 24) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, x, x, x, x}, %  3
        { 3, x, x, x, 3, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13, x, x, x, x}, %  3
        {13, x, x, x,13, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 25) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, x, x, x, x}, %  3
        { x, x, x, 3, x, x, x, 3, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x,13, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13, x, x, x, x}, %  3
        { x, x, x,13, x, x, x,13, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 26) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, x, x, x, x}, %  3
        { x, 3, x, x, x, 3, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x, x,13, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13, x, x, x, x}, %  3
        { x,13, x, x, x,13, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 27) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, x, x, x, x}, %  3
        { x, x, 3, x, x, x, 3, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13, x, x, x, x}, %  3
        { x, x,13, x, x, x,13, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 28) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { x, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 7, 7, 7, 7}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { x, 7, 7, 7, 7, 7, 7, 7, 7, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 29) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { x, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 8, 8, 8, 8}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { x, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 30) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 31) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10,10,10,10}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {10,10,10,10,10,10,10,10,10, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 32) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11,11,11,11}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {11,11,11,11,11,11,11,11,11, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 33) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12,12,12,12}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {12,12,12,12,12,12,12,12,12, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(X, Y, 34) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,13,13,13,13}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {13,13,13,13,13,13,13,13,13, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {YY, MM};
from_max_v_570z(_, _, _) ->
    {x, x}.

%%====================================================================
%% to_max_v_570z
%%====================================================================

to_max_v_570z(XX, YY, _)
        when XX > 12 orelse YY > 8 ->
    {x, x};
to_max_v_570z(XX, YY, 0) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 4}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 0, 1, 3,15}, %  0
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  2
        { x, 1, 2, 0, 3, 1, 2, 0,12, 0, 0, 0, 0}, %  3
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  4
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  5
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  6
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  7
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 9}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 1) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 4}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 4, 5, 7,19}, %  0
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  1
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, 1}, %  2
        { x, 5, 6, 4, 7, 5, 6, 4,16, 1, 1, 1, 1}, %  3
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  4
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  6
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  7
        { x, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 2) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 4}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, 2, 2, 2, 2}, %  2
        { x, x, x, x, x, x, x, x, x, 2, 2, 2, 2}, %  3
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  4
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  5
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  7
        { x, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,11}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 3) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, 3, 3, 3, 3}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, x, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,12,13,15, x}, %  0
        { x, x, x, x, x, x, x, x, x, 3, 3, 3, 3}, %  1
        { x, x, x, x, x, x, x, x, x, 3, 3, 3, 3}, %  2
        { 0,13,14,12,15,13,14,12, x, 3, 3, 3, 3}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 4) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 4, 4, 4, 4}, %  1
        { x, x, x, x, x, x, x, x, x, 4, 4, 4, 4}, %  2
        { x, x, x, x, x, x, x, x, x, 4, 4, 4, 4}, %  3
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  6
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 5) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, 5, 5, 5, 5}, %  2
        { x, x, x, x, x, x, x, x, x, 5, 5, 5, 5}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 6) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 6, 6, 6, 6}, %  1
        { x, x, x, x, x, x, x, x, x, 6, 6, 6, 6}, %  2
        { x, x, x, x, x, x, x, x, x, 6, 6, 6, 6}, %  3
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 7) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, 2, 0, 3}, %  0
        { x, x, x, x, x, x, x, x, x, 0, 1, 3, 2}, %  1
        { x, x, x, x, x, x, x, x, x, 3, 0, 2, 1}, %  2
        { x, 2, 3, 1, 0, 2, 3, 1, 0, 2, 3, 1, 0}, %  3
        { 3, 1, 2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3}, %  4
        { 2, 0, 1, 3, 2, 0, 1, 3, 2,28,28,28,28}, %  5
        { 1, 3, 0, 2, 1, 3, 0, 2, 1, 7, 7, 7, 7}, %  6
        { 0, 2, 3, 1, 0, 2, 3, 1, 0, 7, 7, 7, 7}, %  7
        { x,28,28,28,28,28,28,28,28, 7, 7, 7, 7}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 8) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, 6, 4, 7}, %  0
        { x, x, x, x, x, x, x, x, x, 4, 5, 7, 6}, %  1
        { x, x, x, x, x, x, x, x, x, 7, 4, 6, 5}, %  2
        { x, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4}, %  3
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7}, %  4
        { 6, 4, 5, 7, 6, 4, 5, 7, 6,29,29,29,29}, %  5
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 8, 8, 8, 8}, %  6
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 8, 8, 8, 8}, %  7
        { x,29,29,29,29,29,29,29,29, 8, 8, 8, 8}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 9) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, x,10, 8,11}, %  0
        { x, x, x, x, x, x, x, x, x, 8, 9,11,10}, %  1
        { x, x, x, x, x, x, x, x, x,11, 8,10, 9}, %  2
        { 8,10,11, 9, 8,10,11, 9, 8,10,11, 9, 8}, %  3
        {11, 9,10, 8,11, 9,10, 8,11, 9,10, 8,11}, %  4
        {10, 8, 9,11,10, 8, 9,11,10,30,30,30,30}, %  5
        { 9,11, 8,10, 9,11, 8,10, 9, 9, 9, 9, 9}, %  6
        { 8,10,11, 9, 8,10,11, 9, 8, 9, 9, 9, 9}, %  7
        {30,30,30,30,30,30,30,30,30, 9, 9, 9, x}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 10) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,13,14,12, x}, %  0
        { x, x, x, x, x, x, x, x, x,12,13,15,14}, %  1
        { x, x, x, x, x, x, x, x, x,15,12,14,13}, %  2
        {12,14,15,13,12,14,15,13, x,14,15,13,12}, %  3
        {15,13,14,12,15,13,14,12,15,13,14,12,15}, %  4
        {14,12,13,15,14,12,13,15,14,31,31,31,31}, %  5
        {13,15,12,14,13,15,12,14,13,10,10,10,10}, %  6
        {12,14,15,13,12,14,15,13,12,10,10,10,10}, %  7
        {31,31,31,31,31,31,31,31,31,10,10,10, x}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 11) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,17,18,16, x}, %  0
        { x, x, x, x, x, x, x, x, x,16,17,19,18}, %  1
        { x, x, x, x, x, x, x, x, x,19,16,18,17}, %  2
        {16,18,19,17,16,18,19,17, x,18,19,17,16}, %  3
        {19,17,18,16,19,17,18,16,19,17,18,16,19}, %  4
        {18,16,17,19,18,16,17,19,18,32,32,32,32}, %  5
        {17,19,16,18,17,19,16,18,17,11,11,11,11}, %  6
        {16,18,19,17,16,18,19,17,16,11,11,11,11}, %  7
        {32,32,32,32,32,32,32,32,32,11,11,11, x}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 12) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,21,22,20, x}, %  0
        { x, x, x, x, x, x, x, x, x,20,21,23,22}, %  1
        { x, x, x, x, x, x, x, x, x,23,20,22,21}, %  2
        {20,22,23,21,20,22,23,21, x,22,23,21,20}, %  3
        {23,21,22,20,23,21,22,20,23,21,22,20,23}, %  4
        {22,20,21,23,22,20,21,23,22,33,33,33,33}, %  5
        {21,23,20,22,21,23,20,22,21,12,12,12,12}, %  6
        {20,22,23,21,20,22,23,21,20,12,12,12,12}, %  7
        {33,33,33,33,33,33,33,33,33,12,12,12, x}  %  8
    })),
    {Y, I};
to_max_v_570z(XX, YY, 13) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x}  %  8
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12
        { x, x, x, x, x, x, x, x, x,25,26,24, x}, %  0
        { x, x, x, x, x, x, x, x, x,24,25,27,26}, %  1
        { x, x, x, x, x, x, x, x, x,27,24,26,25}, %  2
        {24,26,27,25,24,26,27,25, x,26,27,25,24}, %  3
        {27,25,26,24,27,25,26,24,27,25,26,24,27}, %  4
        {26,24,25,27,26,24,25,27,26,34,34,34,34}, %  5
        {25,27,24,26,25,27,24,26,25,13,13,13,13}, %  6
        {24,26,27,25,24,26,27,25,24,13,13,13,13}, %  7
        {34,34,34,34,34,34,34,34,34,13,13,13, x}  %  8
    })),
    {Y, I};
to_max_v_570z(_, _, _) ->
    {x, x}.

%%====================================================================
%% from_max_v_1270z
%%====================================================================

from_max_v_1270z(X, Y, _)
        when X > 16 orelse Y > 11 ->
    {x, x};
from_max_v_1270z(X, Y, 0) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 2, 2, 2, 2, 2, 2}, %  3
        { 3, x, x, 3, 3, x, x, 3, 3, x, x, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 7, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 0, 0, 0, 0, 0, 0}, %  3
        { 3, x, x, 0, 7, x, x, 0, 7, x, x, 0, 0, 0, 0, 0, 0}, %  4
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  5
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  6
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  7
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  8
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  9
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, % 10
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 1) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, x, 3, x, 3, x, 3, x, 3, x, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 1, 1, 1, 1, 1, 1}, %  3
        { x, 0, x, 7, x, 0, x, 7, x, 0, x, 1, 1, 1, 1, 1, 1}, %  4
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  6
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  7
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  8
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  9
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, % 10
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 2) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, 3, x, x, 3, 3, x, x, 3, x, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 7, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 2, 2, 2, 2, 2, 2}, %  3
        { x, 7, 0, x, x, 7, 0, x, x, 7, x, 2, 2, 2, 2, 2, 2}, %  4
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  5
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  7
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  8
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  9
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, % 10
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 3) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 2, 2, 2, 2, 2, 2}, %  3
        { x, x, 3, x, 3, x, 3, x, 3, x, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 7, x, x, 0, 7}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 3, 3, 3, 3, 3, 3}, %  3
        { x, x, 7, x, 0, x, 7, x, 0, x, 7, 3, 3, 3, 3, 3, 3}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  8
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  9
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, % 10
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 4) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 2, 2, 2, 2, 2, 2}, %  3
        { 3, x, x, 3, 3, x, x, 3, 3, x, x, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        { x,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, x, 8, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 4, 4}, %  3
        { 8, x, x, 1, 8, x, x, 1, 8, x, x, 4, 4, 4, 4, 4, 4}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  6
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  9
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, % 10
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 5) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, x, 3, x, 3, x, 3, x, 3, x, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, 1, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 5, 5, 5, 5, 5, 5}, %  3
        { x, 1, x, 8, x, 1, x, 8, x, 1, x, 5, 5, 5, 5, 5, 5}, %  4
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  7
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, % 10
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 6) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, 3, x, x, 3, 3, x, x, 3, x, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, x, 8, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 6, 6, 6, 6, 6, 6}, %  3
        { x, 8, 1, x, x, 8, 1, x, x, 8, x, 6, 6, 6, 6, 6, 6}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  8
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 7) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 7, 7, 7, 7, 7, 7}, %  3
        { x, x, 3, x, 3, x, 3, x, 3, x, 3, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        { x,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 8, x, x, 1, 8}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7}, %  3
        { x, x, 8, x, 1, x, 8, x, 1, x, 8, 7, 7, 7, 7, 7, 7}, %  4
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 8) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 7, 7, 7, 7, 7, 7}, %  3
        { 3, x, x, x, 3, x, x, x, 3, x, x, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        { x,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 8}, %  3
        { 9, x, x, x, 9, x, x, x, 9, x, x, 8, 8, 8, 8, 8, 8}, %  4
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  5
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  6
        { x, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 9) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 7, 7, 7, 7, 7, 7}, %  3
        { x, x, x, 3, x, x, x, 3, x, x, x, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  3
        { x, x, x, 9, x, x, x, 9, x, x, x, 9, 9, 9, 9, 9, 9}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  6
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 10) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 7, 7, 7, 7, 7, 7}, %  3
        { x, 3, x, x, x, 3, x, x, x, 3, x, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,10,10,10,10,10,10}, %  3
        { x, 9, x, x, x, 9, x, x, x, 9, x,10,10,10,10,10,10}, %  4
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, 1}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 11) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 7, 7, 7, 7, 7, 7}, %  3
        { x, x, 3, x, x, x, 3, x, x, x, 3, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, 9}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,11,11,11,11,11,11}, %  3
        { x, x, 9, x, x, x, 9, x, x, x, 9,11,11,11,11,11,11}, %  4
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  5
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11, 2}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 12) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 7, 7, 7, 7, 7, 7}, %  3
        { 3, x, x, 3, 3, x, x, 3, 3, x, x, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,10, x, 3, x,10, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12}, %  2
        {10,10,10,10,10,10,10,10,10,10,10,12,12,12,12,12,12}, %  3
        {10, x, x, 3,10, x, x, 3,10, x, x,12,12,12,12,12,12}, %  4
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  5
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  6
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 13) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 7, 7, 7, 7, 7, 7}, %  3
        { x, 3, x, 3, x, 3, x, 3, x, 3, x, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10, 3, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13}, %  2
        {10,10,10,10,10,10,10,10,10,10,10,13,13,13,13,13,13}, %  3
        { x, 3, x,10, x, 3, x,10, x, 3, x,13,13,13,13,13,13}, %  4
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  5
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  6
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 14) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, x, x, x, x, x, x}, %  3
        { x, 3, 3, x, x, 3, 3, x, x, 3, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 3, x,10, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {10,10,10,10,10,10,10,10,10,10,10, x, x, x, x, x, x}, %  3
        { x,10, 3, x, x,10, 3, x, x,10, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 15) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, x, x, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, x, x, x, x, x, x}, %  3
        { x, x, 3, x, 3, x, 3, x, 3, x, 3, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 3,10, x, x, 3, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {10,10,10,10,10,10,10,10,10,10,10, x, x, x, x, x, x}, %  3
        { x, x,10, x, 3, x,10, x, 3, x, 0, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 16) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, x, x, x, x, x, x}, %  3
        { 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x,11, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11,11,11, x, x, x, x, x, x}, %  3
        {11, x, x, x,11, x, x, x,11, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 17) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, x, x, x, x, x, x}, %  3
        { x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11,11,11, x, x, x, x, x, x}, %  3
        { x, x, x,11, x, x, x,11, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 18) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11,11,11, x, x, x, x, x, x}, %  3
        { x,11, x, x, x,11, x, x, x,11, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 19) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, x, x, x, x, x, x}, %  3
        { x, x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, 1}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11,11,11, x, x, x, x, x, x}, %  3
        { x, x,11, x, x, x,11, x, x, x, 1, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 20) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, x, x, x, x, x, x}, %  3
        { 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12,12,12, x, x, x, x, x, x}, %  3
        {12, x, x, x,12, x, x, x,12, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 21) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, x, x, x, x, x, x}, %  3
        { x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12,12,12, x, x, x, x, x, x}, %  3
        { x, x, x,12, x, x, x,12, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 22) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12,12,12, x, x, x, x, x, x}, %  3
        { x,12, x, x, x,12, x, x, x,12, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 23) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, x, x, x, x, x, x}, %  3
        { x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12,12,12, x, x, x, x, x, x}, %  3
        { x, x,12, x, x, x,12, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 24) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, x, x, x, x, x, x}, %  3
        { 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13,13,13, x, x, x, x, x, x}, %  3
        {13, x, x, x,13, x, x, x,13, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 25) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, x, x, x, x, x, x}, %  3
        { x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13,13,13, x, x, x, x, x, x}, %  3
        { x, x, x,13, x, x, x,13, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 26) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13,13,13, x, x, x, x, x, x}, %  3
        { x,13, x, x, x,13, x, x, x,13, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 27) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, x, x, x, x, x, x}, %  3
        { x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13,13,13, x, x, x, x, x, x}, %  3
        { x, x,13, x, x, x,13, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 28) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 29) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 30) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 31) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {10,10,10,10,10,10,10,10,10,10,10, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 32) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {11,11,11,11,11,11,11,11,11,11,11, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 33) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {12,12,12,12,12,12,12,12,12,12,12, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(X, Y, 34) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {13,13,13,13,13,13,13,13,13,13,13, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {YY, MM};
from_max_v_1270z(_, _, _) ->
    {x, x}.

%%====================================================================
%% to_max_v_1270z
%%====================================================================

to_max_v_1270z(XX, YY, _)
        when XX > 16 orelse YY > 11 ->
    {x, x};
to_max_v_1270z(XX, YY, 0) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        { x,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, x}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, 2, 0, 1, 3,15}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  2
        { x, 1, 2, 0, 3, 1, 2, 0, 3, 1,15, 0, 0, 0, 0, 0, 0}, %  3
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  4
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  5
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  6
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  7
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  8
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  9
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, % 10
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, x}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 1) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        { x,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, 7}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, 6, 4, 5, 7,19}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1}, %  2
        { x, 5, 6, 4, 7, 5, 6, 4, 7, 5,19, 1, 1, 1, 1, 1, 1}, %  3
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  4
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  6
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  7
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  8
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  9
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, % 10
        { x, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 2) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        { x,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, 7}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2}, %  3
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  4
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  5
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  7
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  8
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  9
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, % 10
        { x, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,11}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 3) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,15,14,12,13,15, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3}, %  2
        { 0,13,14,12,15,13,14,12,15,13, x, 3, 3, 3, 3, 3, 3}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  8
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  9
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 4) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4}, %  3
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  6
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  9
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 5) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  7
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 6) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6}, %  3
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  8
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 7) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, 3, 1, 2, 0, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 3, 2, 0, 1, 3, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 2, 1, 3, 0, 2, 1}, %  2
        { x, 2, 3, 1, 0, 2, 3, 1, 0, 2, 3, 1, 0, 2, 3, 1, 0}, %  3
        { 3, 1, 2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3}, %  4
        { 2, 0, 1, 3, 2, 0, 1, 3, 2, 0, 1,28,28,28,28,28,28}, %  5
        { 1, 3, 0, 2, 1, 3, 0, 2, 1, 3, 0, 7, 7, 7, 7, 7, 7}, %  6
        { 0, 2, 3, 1, 0, 2, 3, 1, 0, 2, 3, 7, 7, 7, 7, 7, 7}, %  7
        {28,28,28,28,28,28,28,28,28,28,28, 7, 7, 7, 7, 7, 7}, %  8
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  9
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 10
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 8) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, 7, 5, 6, 4, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 7, 6, 4, 5, 7, 6}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 6, 5, 7, 4, 6, 5}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4}, %  3
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7}, %  4
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5,29,29,29,29,29,29}, %  5
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 8, 8, 8, 8, 8, 8}, %  6
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 8, 8, 8, 8, 8, 8}, %  7
        {29,29,29,29,29,29,29,29,29,29,29, 8, 8, 8, 8, 8, 8}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 10
        { x, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 9) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, x}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, x,11, 9,10, 8,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11,10, 8, 9,11,10}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,10, 9,11, 8,10, 9}, %  2
        { 8,10,11, 9, 8,10,11, 9, 8,10,11, 9, 8,10,11, 9, 8}, %  3
        {11, 9,10, 8,11, 9,10, 8,11, 9,10, 8,11, 9,10, 8,11}, %  4
        {10, 8, 9,11,10, 8, 9,11,10, 8, 9,30,30,30,30,30,30}, %  5
        { 9,11, 8,10, 9,11, 8,10, 9,11, 8, 9, 9, 9, 9, 9, 9}, %  6
        { 8,10,11, 9, 8,10,11, 9, 8,10,11, 9, 9, 9, 9, 9, 9}, %  7
        {30,30,30,30,30,30,30,30,30,30,30, 9, 9, 9, 9, 9, 9}, %  8
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, x}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 10) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, x}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,12,15,13,14,12, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,15,14,12,13,15,14}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14,13,15,12,14,13}, %  2
        {12,14,15,13,12,14,15,13,12,14, x,13,12,14,15,13,12}, %  3
        {15,13,14,12,15,13,14,12,15,13,14,12,15,13,14,12,15}, %  4
        {14,12,13,15,14,12,13,15,14,12,13,31,31,31,31,31,31}, %  5
        {13,15,12,14,13,15,12,14,13,15,12,10,10,10,10,10,10}, %  6
        {12,14,15,13,12,14,15,13,12,14,15,10,10,10,10,10,10}, %  7
        {31,31,31,31,31,31,31,31,31,31,31,10,10,10,10,10,10}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, x}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 11) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, x}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,16,19,17,18,16, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,19,18,16,17,19,18}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,18,17,19,16,18,17}, %  2
        {16,18,19,17,16,18,19,17,16,18, x,17,16,18,19,17,16}, %  3
        {19,17,18,16,19,17,18,16,19,17,18,16,19,17,18,16,19}, %  4
        {18,16,17,19,18,16,17,19,18,16,17,32,32,32,32,32,32}, %  5
        {17,19,16,18,17,19,16,18,17,19,16,11,11,11,11,11,11}, %  6
        {16,18,19,17,16,18,19,17,16,18,19,11,11,11,11,11,11}, %  7
        {32,32,32,32,32,32,32,32,32,32,32,11,11,11,11,11,11}, %  8
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11, x}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 12) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, x}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,20,23,21,22,20, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,23,22,20,21,23,22}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,22,21,23,20,22,21}, %  2
        {20,22,23,21,20,22,23,21,20,22, x,21,20,22,23,21,20}, %  3
        {23,21,22,20,23,21,22,20,23,21,22,20,23,21,22,20,23}, %  4
        {22,20,21,23,22,20,21,23,22,20,21,33,33,33,33,33,33}, %  5
        {21,23,20,22,21,23,20,22,21,23,20,12,12,12,12,12,12}, %  6
        {20,22,23,21,20,22,23,21,20,22,23,12,12,12,12,12,12}, %  7
        {33,33,33,33,33,33,33,33,33,33,33,12,12,12,12,12,12}, %  8
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  9
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 10
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12, x}  % 11
    })),
    {Y, I};
to_max_v_1270z(XX, YY, 13) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, x}  % 11
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
        { x, x, x, x, x, x, x, x, x, x, x,24,27,25,26,24, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,27,26,24,25,27,26}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,26,25,27,24,26,25}, %  2
        {24,26,27,25,24,26,27,25,24,26, x,25,24,26,27,25,24}, %  3
        {27,25,26,24,27,25,26,24,27,25,26,24,27,25,26,24,27}, %  4
        {26,24,25,27,26,24,25,27,26,24,25,34,34,34,34,34,34}, %  5
        {25,27,24,26,25,27,24,26,25,27,24,13,13,13,13,13,13}, %  6
        {24,26,27,25,24,26,27,25,24,26,27,13,13,13,13,13,13}, %  7
        {34,34,34,34,34,34,34,34,34,34,34,13,13,13,13,13,13}, %  8
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  9
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 10
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13, x}  % 11
    })),
    {Y, I};
to_max_v_1270z(_, _, _) ->
    {x, x}.

%%====================================================================
%% from_max_v_2210z
%%====================================================================

from_max_v_2210z(X, Y, _)
        when X > 20 orelse Y > 14 ->
    {x, x};
from_max_v_2210z(X, Y, 0) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, 1}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7, 2, 2, 2, 2, 2, 2, 2, 2}, %  3
        { 3, x, x, 3, 3, x, x, 3, 3, x, x, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 11
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 12
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 13
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 7, x, 0, x, 7, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 0, 0, 0, 0, 0, 0, 0, 0}, %  3
        { 3, x, x, 0, 7, x, x, 0, 7, x, x, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0}, %  4
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  5
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  6
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  7
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  8
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  9
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, % 10
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, % 11
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, % 12
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, % 13
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 1) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, 0, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, 1}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 7, 6, 2, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, x, 3, x, 3, x, 3, x, 3, x, 3, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 11
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 12
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 13
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, 7, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, 1}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 1, 1, 1, 1, 1, 1, 1, 1}, %  3
        { x, 0, x, 7, x, 0, x, 7, x, 0, x, 7, x, 1, 1, 1, 1, 1, 1, 1, 1}, %  4
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  6
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  7
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  8
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  9
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, % 10
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, % 11
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, % 12
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, % 13
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 2) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, 1}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 6, 5, 2, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, 3, x, x, 3, 3, x, x, 3, 3, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 11
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 12
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 13
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 7, x, 0, x, 7, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 2, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 7, 0, x, x, 7, 0, x, x, 7, 0, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  4
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  5
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  7
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  8
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  9
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, % 10
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, % 11
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, % 12
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, % 13
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 3) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, x, x, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, 1}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4, 2, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, x, 3, x, 3, x, 3, x, 3, x, 3, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 11
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 12
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 13
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7, 7, 7}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 7, x, x, 0, 7}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  2
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 3, 3, 3, 3, 3, 3, 3, 3}, %  3
        { x, x, 7, x, 0, x, 7, x, 0, x, 7, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  8
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  9
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, % 10
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, % 11
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, % 12
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, % 13
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 4) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, 1}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7, 2, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, x, x, 3, 3, x, x, 3, 3, x, x, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 11
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 12
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 13
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 8, x, 1, x, 8, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4, 4, 4}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { x, x, x, 1, 8, x, x, 1, 8, x, x, 1, 8, 4, 4, 4, 4, 4, 4, 4, 4}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  6
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  9
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, % 10
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, % 11
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, % 12
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, % 13
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 5) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, 0, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, 1}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 7, 6, 2, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, x, 3, x, 3, x, 3, x, 3, x, 3, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 11
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 12
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 13
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, x, x, 8, 1, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 5, 5, 5, 5, 5, 5, 5, 5}, %  3
        { x, 1, x, 8, x, 1, x, 8, x, 1, x, 8, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  7
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, % 10
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, % 11
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, % 12
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, % 13
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 6) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, 1}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 6, 5, 2, 2, 2, 2, 2, 2, 2, 2}, %  3
        { x, 3, 3, x, x, 3, 3, x, x, 3, 3, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 11
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 12
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 13
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 8, x, 1, x, 8, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 6, 6, 6, 6, 6, 6, 6, 6}, %  3
        { x, 8, 1, x, x, 8, 1, x, x, 8, 1, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  8
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 11
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 12
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 13
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 7) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4, 7, 7, 7, 7, 7, 7, 7, 7}, %  3
        { x, x, 3, x, 3, x, 3, x, 3, x, 3, x, x, 8, 8, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  8
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  9
        { x,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8, 8, 8}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 8, x, x, 1, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7, 7, 7}, %  2
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 7}, %  3
        { x, x, 8, x, 1, x, 8, x, 1, x, 8, x, x, 7, 7, 7, 7, 7, 7, 7, 7}, %  4
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  7
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  9
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 8) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  3
        { x, x, x, x, 3, x, x, x, 3, x, x, x, 3, 8, 8, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  8
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  9
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8, 8, 8}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 8, 8, 8}, %  3
        { x, x, x, x, 9, x, x, x, 9, x, x, x, 9, 8, 8, 8, 8, 8, 8, 8, 8}, %  4
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  5
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 9) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 7, 6, 7, 7, 7, 7, 7, 7, 7, 7}, %  3
        { x, x, x, 3, x, x, x, 3, x, x, x, 3, x, 8, 8, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  8
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  9
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9, 9, 9}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  3
        { x, x, x, 9, x, x, x, 9, x, x, x, 9, x, 9, 9, 9, 9, 9, 9, 9, 9}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  6
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 10) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 7, 7, 7, 7, 7, 7, 7}, %  3
        { x, 3, x, x, x, 3, x, x, x, 3, x, x, x, 8, 8, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  8
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  9
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, 9, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10,10,10}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,10,10,10,10,10,10,10,10}, %  3
        { x, 9, x, x, x, 9, x, x, x, 9, x, x, x,10,10,10,10,10,10,10,10}, %  4
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  7
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, 1}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 11) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4, 7, 7, 7, 7, 7, 7, 7, 7}, %  3
        { x, x, 3, x, x, x, 3, x, x, x, 3, x, x, 8, 8, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  8
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  9
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9, 9, 9}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, 9}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11,11,11}, %  2
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,11,11,11,11,11,11,11,11}, %  3
        { x, x, 9, x, x, x, 9, x, x, x, 9, x, x,11,11,11,11,11,11,11,11}, %  4
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  5
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  8
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 12) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x, 0, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  3
        { 3, x, x, 3, 3, x, x, 3, 3, x, x, 3, 3, 8, 8, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  8
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  9
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, x,10, x, 3, x,10, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12,12,12}, %  2
        {10,10,10,10,10,10,10,10,10,10,10,10,10,12,12,12,12,12,12,12,12}, %  3
        {10, x, x, 3,10, x, x, 3,10, x, x, 3, 0,12,12,12,12,12,12,12,12}, %  4
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  5
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  6
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  7
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  8
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  9
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 13) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, x, x, 0, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 7, 6, 7, 7, 7, 7, 7, 7, 7, 7}, %  3
        { x, 3, x, 3, x, 3, x, 3, x, 3, x, 3, x, 8, 8, 8, 8, 8, 8, 8, 8}, %  4
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  5
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  6
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  7
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  8
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  9
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10, 3, x, x,10, 3, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13,13,13}, %  2
        {10,10,10,10,10,10,10,10,10,10,10,10,10,13,13,13,13,13,13,13,13}, %  3
        { x, 3, x,10, x, 3, x,10, x, 3, x,10, x,13,13,13,13,13,13,13,13}, %  4
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  5
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  6
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  7
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  8
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  9
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 14) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, 0, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 6, 5, x, x, x, x, x, x, x, x}, %  3
        { x, 3, 3, x, x, 3, 3, x, x, 3, 3, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,10, x, 3, x,10, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {10,10,10,10,10,10,10,10,10,10,10,10,10, x, x, x, x, x, x, x, x}, %  3
        { x,10, 3, x, x,10, 3, x, x,10, 3, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 15) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, x, x, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4, x, x, x, x, x, x, x, x}, %  3
        { x, x, 3, x, 3, x, 3, x, 3, x, 3, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10,10,10}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 3,10, x, x, 3, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {10,10,10,10,10,10,10,10,10,10,10,10,10, x, x, x, x, x, x, x, x}, %  3
        { x, x,10, x, 3, x,10, x, 3, x,10, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 16) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x,11, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11,11,11,11,11, x, x, x, x, x, x, x, x}, %  3
        {11, x, x, x,11, x, x, x,11, x, x, x, 1, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 17) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 7, 6, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x,11, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11,11,11,11,11, x, x, x, x, x, x, x, x}, %  3
        { x, x, x,11, x, x, x,11, x, x, x,11, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 18) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 6, 5, x, x, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x,11, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11,11,11,11,11, x, x, x, x, x, x, x, x}, %  3
        { x,11, x, x, x,11, x, x, x,11, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 19) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4, x, x, x, x, x, x, x, x}, %  3
        { x, x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11,11,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, 1}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {11,11,11,11,11,11,11,11,11,11,11,11,11, x, x, x, x, x, x, x, x}, %  3
        { x, x,11, x, x, x,11, x, x, x,11, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 20) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12,12,12,12,12, x, x, x, x, x, x, x, x}, %  3
        {12, x, x, x,12, x, x, x,12, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 21) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 7, 6, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x,12, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12,12,12,12,12, x, x, x, x, x, x, x, x}, %  3
        { x, x, x,12, x, x, x,12, x, x, x,12, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 22) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 6, 5, x, x, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x,12, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12,12,12,12,12, x, x, x, x, x, x, x, x}, %  3
        { x,12, x, x, x,12, x, x, x,12, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 23) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4, x, x, x, x, x, x, x, x}, %  3
        { x, x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12,12,12}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {12,12,12,12,12,12,12,12,12,12,12,12,12, x, x, x, x, x, x, x, x}, %  3
        { x, x,12, x, x, x,12, x, x, x,12, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 24) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 4, 3, 1, 2, 4, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13,13,13,13,13, x, x, x, x, x, x, x, x}, %  3
        {13, x, x, x,13, x, x, x,13, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 25) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 1, 3, 2, 4, 1, 3, 2}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 7, 6, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x,13, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13,13,13,13,13, x, x, x, x, x, x, x, x}, %  3
        { x, x, x,13, x, x, x,13, x, x, x,13, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 26) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 4, 2, 1, 3, 4, 2, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, 0, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 6, 5, x, x, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x,13, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13,13,13,13,13, x, x, x, x, x, x, x, x}, %  3
        { x,13, x, x, x,13, x, x, x,13, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 27) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 3, 1, 4, 2, 3, 1, 4}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4, x, x, x, x, x, x, x, x}, %  3
        { x, x, 3, x, x, x, 3, x, x, x, 3, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13,13,13}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        {13,13,13,13,13,13,13,13,13,13,13,13,13, x, x, x, x, x, x, x, x}, %  3
        { x, x,13, x, x, x,13, x, x, x,13, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 28) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 7, 7, 7, 7, 7, 7, 7}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 29) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, 8, 8, 8, 8, 8, 8, 8}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 30) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, 9, 9, 9, 9}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 31) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10,10,10,10,10,10,10,10}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {10,10,10,10,10,10,10,10,10,10,10,10,10, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 32) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,11,11,11,11}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {11,11,11,11,11,11,11,11,11,11,11,11,11, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 33) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12,12,12,12,12,12,12,12}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {12,12,12,12,12,12,12,12,12,12,12,12,12, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(X, Y, 34) ->
    YY = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,13,13,13,13}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {13,13,13,13,13,13,13,13,13,13,13,13,13, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {YY, MM};
from_max_v_2210z(_, _, _) ->
    {x, x}.

%%====================================================================
%% to_max_v_2210z
%%====================================================================

to_max_v_2210z(XX, YY, _)
        when XX > 20 orelse YY > 14 ->
    {x, x};
to_max_v_2210z(XX, YY, 0) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 11
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 12
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14}, % 13
        { x,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,10}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 3, 2, 0, 1, 3,15}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 1, 2, 0, 3, 1, 2, 0, 3, 1, 2, 0,12, 0, 0, 0, 0, 0, 0, 0, 0}, %  3
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  4
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  5
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  6
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  7
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  8
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, %  9
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, % 10
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, % 11
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, % 12
        { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, % 13
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 9}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 1) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 11
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 12
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14}, % 13
        { x,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,10}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 7, 6, 4, 5, 7,19}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, 1}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, 1}, %  2
        { x, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4,16, 1, 1, 1, 1, 1, 1, 1, 1}, %  3
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  4
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  6
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  7
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  8
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, %  9
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, % 10
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, % 11
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, % 12
        { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, % 13
        { x, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,10}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 2) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 11
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 12
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14}, % 13
        { x,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13, x}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  3
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  4
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  5
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  7
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  8
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, %  9
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, % 10
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, % 11
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, % 12
        { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}, % 13
        { x, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, x}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 3) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 11
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 12
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12,13,15,14,12,13,15, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  2
        { 0,13,14,12,15,13,14,12,15,13,14,12, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  8
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  9
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, % 10
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, % 11
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, % 12
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 4) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 11
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 12
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4, 4, 4}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4, 4, 4}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  4
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  5
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  6
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  9
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, % 10
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, % 11
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, % 12
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 5) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 11
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 12
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 5, 5, 5, 5, 5, 5}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  5
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  6
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  7
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, % 10
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, % 11
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, % 12
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 6) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 2, 2, 2, 2, 2, 2, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 3, 3, 3, 3, 3, 3}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 4, 4, 4, 4, 4, 4}, %  3
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 11
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 12
        {14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 6, 6, 6, 6, 6, 6, 6}, %  3
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  4
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  5
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  6
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  7
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  8
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 11
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 12
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 7) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 11
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 12
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 13
        { x,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 0, 3, 1, 2, 0, 3}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 1, 3, 2, 0, 1, 3, 2}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 0, 2, 1, 3, 0, 2, 1}, %  2
        { x, 2, 3, 1, 0, 2, 3, 1, 0, 2, 3, 1, 0, 2, 3, 1, 0, 2, 3, 1, 0}, %  3
        { 3, 1, 2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3}, %  4
        { 2, 0, 1, 3, 2, 0, 1, 3, 2, 0, 1, 3, 2,28,28,28,28,28,28,28,28}, %  5
        { 1, 3, 0, 2, 1, 3, 0, 2, 1, 3, 0, 2, 1, 7, 7, 7, 7, 7, 7, 7, 7}, %  6
        { 0, 2, 3, 1, 0, 2, 3, 1, 0, 2, 3, 1, 0, 7, 7, 7, 7, 7, 7, 7, 7}, %  7
        {28,28,28,28,28,28,28,28,28,28,28,28,28, 7, 7, 7, 7, 7, 7, 7, 7}, %  8
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, %  9
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 11
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 12
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 13
        { x, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 8) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 11
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 12
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 13
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 4, 7, 5, 6, 4, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 5, 7, 6, 4, 5, 7, 6}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 4, 6, 5, 7, 4, 6, 5}, %  2
        { x, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4}, %  3
        { 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7, 5, 6, 4, 7}, %  4
        { 6, 4, 5, 7, 6, 4, 5, 7, 6, 4, 5, 7, 6,29,29,29,29,29,29,29,29}, %  5
        { 5, 7, 4, 6, 5, 7, 4, 6, 5, 7, 4, 6, 5, 8, 8, 8, 8, 8, 8, 8, 8}, %  6
        { 4, 6, 7, 5, 4, 6, 7, 5, 4, 6, 7, 5, 4, 8, 8, 8, 8, 8, 8, 8, 8}, %  7
        {29,29,29,29,29,29,29,29,29,29,29,29,29, 8, 8, 8, 8, 8, 8, 8, 8}, %  8
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, %  9
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 10
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 11
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 12
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 13
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 9) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  2
        { x, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 11
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 12
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 13
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, x}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,10, 8,11, 9,10, 8,11}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, 9,11,10, 8, 9,11,10}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, 8,10, 9,11, 8,10, 9}, %  2
        { x,10,11, 9, 8,10,11, 9, 8,10,11, 9, 8,10,11, 9, 8,10,11, 9, 8}, %  3
        {11, 9,10, 8,11, 9,10, 8,11, 9,10, 8,11, 9,10, 8,11, 9,10, 8,11}, %  4
        {10, 8, 9,11,10, 8, 9,11,10, 8, 9,11,10,30,30,30,30,30,30,30,30}, %  5
        { 9,11, 8,10, 9,11, 8,10, 9,11, 8,10, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  6
        { 8,10,11, 9, 8,10,11, 9, 8,10,11, 9, 8, 9, 9, 9, 9, 9, 9, 9, 9}, %  7
        {30,30,30,30,30,30,30,30,30,30,30,30,30, 9, 9, 9, 9, 9, 9, 9, 9}, %  8
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, %  9
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 10
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 11
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 12
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 13
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, x}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 10) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 11
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 12
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 13
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, x}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,14,12,15,13,14,12, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12,13,15,14,12,13,15,14}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15,12,14,13,15,12,14,13}, %  2
        {12,14,15,13,12,14,15,13,12,14,15,13, x,14,15,13,12,14,15,13,12}, %  3
        {15,13,14,12,15,13,14,12,15,13,14,12,15,13,14,12,15,13,14,12,15}, %  4
        {14,12,13,15,14,12,13,15,14,12,13,15,14,31,31,31,31,31,31,31,31}, %  5
        {13,15,12,14,13,15,12,14,13,15,12,14,13,10,10,10,10,10,10,10,10}, %  6
        {12,14,15,13,12,14,15,13,12,14,15,13,12,10,10,10,10,10,10,10,10}, %  7
        {31,31,31,31,31,31,31,31,31,31,31,31,31,10,10,10,10,10,10,10,10}, %  8
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, %  9
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 10
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 11
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 12
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, % 13
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, x}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 11) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 11
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 12
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 13
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, x}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,16,19,17,18,16, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16,17,19,18,16,17,19,18}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,19,16,18,17,19,16,18,17}, %  2
        {16,18,19,17,16,18,19,17,16,18,19,17, x,18,19,17,16,18,19,17,16}, %  3
        {19,17,18,16,19,17,18,16,19,17,18,16,19,17,18,16,19,17,18,16,19}, %  4
        {18,16,17,19,18,16,17,19,18,16,17,19,18,32,32,32,32,32,32,32,32}, %  5
        {17,19,16,18,17,19,16,18,17,19,16,18,17,11,11,11,11,11,11,11,11}, %  6
        {16,18,19,17,16,18,19,17,16,18,19,17,16,11,11,11,11,11,11,11,11}, %  7
        {32,32,32,32,32,32,32,32,32,32,32,32,32,11,11,11,11,11,11,11,11}, %  8
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, %  9
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 10
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 11
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 12
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11}, % 13
        {11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11, x}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 12) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 11
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 12
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 13
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,21,22,20,23,21,22,20, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,20,21,23,22,20,21,23,22}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,23,20,22,21,23,20,22,21}, %  2
        {20,22,23,21,20,22,23,21,20,22,23,21, x,22,23,21,20,22,23,21,20}, %  3
        {23,21,22,20,23,21,22,20,23,21,22,20,23,21,22,20,23,21,22,20,23}, %  4
        {22,20,21,23,22,20,21,23,22,20,21,23,22,33,33,33,33,33,33,33,33}, %  5
        {21,23,20,22,21,23,20,22,21,23,20,22,21,12,12,12,12,12,12,12,12}, %  6
        {20,22,23,21,20,22,23,21,20,22,23,21,20,12,12,12,12,12,12,12,12}, %  7
        {33,33,33,33,33,33,33,33,33,33,33,33,33,12,12,12,12,12,12,12,12}, %  8
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, %  9
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 10
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 11
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 12
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12}, % 13
        {12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12, x}  % 14
    })),
    {Y, I};
to_max_v_2210z(XX, YY, 13) ->
    Y = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 1, 1, 1, 1, 1, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  2
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, x, 0, 0, 0, 0, 0, 0, 0, 0}, %  3
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0}, %  4
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1}, %  5
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2}, %  6
        { 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}, %  7
        { 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4}, %  8
        { 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}, %  9
        { 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6}, % 10
        { 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7}, % 11
        { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8}, % 12
        { 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9}, % 13
        {10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, x}  % 14
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
        { x, x, x, x, x, x, x, x, x, x, x, x, x,25,26,24,27,25,26,24, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,24,25,27,26,24,25,27,26}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,27,24,26,25,27,24,26,25}, %  2
        {24,26,27,25,24,26,27,25,24,26,27,25, x,26,27,25,24,26,27,25,24}, %  3
        {27,25,26,24,27,25,26,24,27,25,26,24,27,25,26,24,27,25,26,24,27}, %  4
        {26,24,25,27,26,24,25,27,26,24,25,27,26,34,34,34,34,34,34,34,34}, %  5
        {25,27,24,26,25,27,24,26,25,27,24,26,25,13,13,13,13,13,13,13,13}, %  6
        {24,26,27,25,24,26,27,25,24,26,27,25,24,13,13,13,13,13,13,13,13}, %  7
        {34,34,34,34,34,34,34,34,34,34,34,34,34,13,13,13,13,13,13,13,13}, %  8
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, %  9
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 10
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 11
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 12
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13}, % 13
        {13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13, x}  % 14
    })),
    {Y, I};
to_max_v_2210z(_, _, _) ->
    {x, x}.

%%====================================================================
%% test
%%====================================================================

