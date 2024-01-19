-module(r4_fuse_map).

-export([run/0]).

-export([from_name/2]).
-export([to_name/2]).

%%====================================================================
%% run
%%====================================================================

run() ->
    run(max_v_240z, 8, 4),
    run(max_v_570z, 13, 7),
    run(max_v_1270z, 17, 10),
    run(max_v_2210z, 21, 13),
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

from_name({{r4, X, Y}, {interconnect, I}, Value}, Density) ->
    case from_name(X, Y, I, Density) of
        {x, _} ->
            false;

        {_, x} ->
            false;

        {XX, MM} ->
            {ok, {{r4, XX, Y}, {mux, MM}, Value}}
    end;
from_name({{r4, X, Y}, {interconnect, I}, Key, Value}, Density) ->
    case from_name(X, Y, I, Density) of
        {x, _} ->
            false;

        {_, x} ->
            false;

        {XX, MM} ->
            {ok, {{r4, XX, Y}, {mux, MM}, Key, Value}}
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

%%====================================================================
%% from_max_v_240z
%%====================================================================

from_max_v_240z(X, Y, _)
        when X > 8 orelse Y > 4 ->
    {x, x};
from_max_v_240z(X, Y, 0) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  1
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  2
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  3
        { x, x, 2, 3, 4, 5, 6, 7, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 0, 2, 0, 0, 2, 0, x}, %  1
        { x, x, 2, 0, 0, 2, 0, 0, x}, %  2
        { x, x, 0, 0, 2, 0, 0, 2, x}, %  3
        { x, x, 0, 2, 0, 0, 2, 0, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 1) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  1
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  2
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  3
        { x, x, 2, 3, 4, 5, 6, 7, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 3, 0, 3, 3, 0, 3, x}, %  1
        { x, x, 0, 3, 3, 0, 3, 3, x}, %  2
        { x, x, 3, 3, 0, 3, 3, 0, x}, %  3
        { x, x, 3, 0, 3, 3, 0, 3, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 2) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  1
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  2
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  3
        { x, x, 2, 3, 4, 5, 6, 7, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 2, 3, 1, 2, 3, 1, x}, %  1
        { x, x, 3, 1, 2, 3, 1, 2, x}, %  2
        { x, x, 1, 2, 3, 1, 2, 3, x}, %  3
        { x, x, 2, 3, 1, 2, 3, 1, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 3) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 2, 3, 4, 5, 6, 7, x}, %  1
        { x, 1, 2, 3, 4, 5, 6, 7, x}, %  2
        { x, 1, 2, 3, 4, 5, 6, 7, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 0, 1, 1, 2, 1, 1, 2, x}, %  1
        { x, 0, 1, 2, 1, 1, 2, 1, x}, %  2
        { x, 0, 2, 1, 1, 2, 1, 1, x}, %  3
        { x, 0, 1, 1, 2, 1, 1, 2, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 4) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  1
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  2
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  3
        { x, x, 2, 3, 4, 5, 6, 7, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 4, 6, 4, 4, 6, 4, x}, %  1
        { x, x, 6, 4, 4, 6, 4, 4, x}, %  2
        { x, x, 4, 4, 6, 4, 4, 6, x}, %  3
        { x, x, 4, 6, 4, 4, 6, 4, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 5) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  1
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  2
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  3
        { x, x, 2, 3, 4, 5, 6, 7, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 7, 4, 7, 7, 4, 7, x}, %  1
        { x, x, 4, 7, 7, 4, 7, 7, x}, %  2
        { x, x, 7, 7, 4, 7, 7, 4, x}, %  3
        { x, x, 7, 4, 7, 7, 4, 7, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 6) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  1
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  2
        { x, x, 2, 3, 4, 5, 6, 7, x}, %  3
        { x, x, 2, 3, 4, 5, 6, 7, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 6, 7, 5, 6, 7, 5, x}, %  1
        { x, x, 7, 5, 6, 7, 5, 6, x}, %  2
        { x, x, 5, 6, 7, 5, 6, 7, x}, %  3
        { x, x, 6, 7, 5, 6, 7, 5, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 7) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 2, 3, 4, 5, 6, 7, x}, %  1
        { x, 1, 2, 3, 4, 5, 6, 7, x}, %  2
        { x, 1, 2, 3, 4, 5, 6, 7, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 5, 5, 6, 5, 5, 6, x}, %  1
        { x, 1, 5, 6, 5, 5, 6, 5, x}, %  2
        { x, 1, 6, 5, 5, 6, 5, 5, x}, %  3
        { x, 1, 5, 5, 6, 5, 5, 6, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 8) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 5, 6, 7, x, x, 8, x}, %  1
        { x, x, 5, 6, 7, x, x, 8, x}, %  2
        { x, x, 5, 6, 7, x, x, 8, x}, %  3
        { x, x, 5, 6, 7, x, x, 8, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 8,10, 8, x, x, 0, x}, %  1
        { x, x,10, 8, 8, x, x, 0, x}, %  2
        { x, x, 8, 8,10, x, x, 0, x}, %  3
        { x, x, 8,10, 8, x, x, 0, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 9) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 5, 6, 7, x, x, 8, x}, %  1
        { x, x, 5, 6, 7, x, x, 8, x}, %  2
        { x, x, 5, 6, 7, x, x, 8, x}, %  3
        { x, x, 5, 6, 7, x, x, 8, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,11, 8,11, x, x, 1, x}, %  1
        { x, x, 8,11,11, x, x, 1, x}, %  2
        { x, x,11,11, 8, x, x, 1, x}, %  3
        { x, x,11, 8,11, x, x, 1, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 10) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 5, 6, 7, x, x, 8, x}, %  1
        { x, x, 5, 6, 7, x, x, 8, x}, %  2
        { x, x, 5, 6, 7, x, x, 8, x}, %  3
        { x, x, 5, 6, 7, x, x, 8, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,10,11, 9, x, x, 2, x}, %  1
        { x, x,11, 9,10, x, x, 2, x}, %  2
        { x, x, 9,10,11, x, x, 2, x}, %  3
        { x, x,10,11, 9, x, x, 2, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 11) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 5, 6, 7, x, x, 8, x}, %  1
        { x, 1, 5, 6, 7, x, x, 8, x}, %  2
        { x, 1, 5, 6, 7, x, x, 8, x}, %  3
        { x, 1, 5, 6, 7, x, x, 8, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 2, 9, 9,10, x, x, 3, x}, %  1
        { x, 2, 9,10, 9, x, x, 3, x}, %  2
        { x, 2,10, 9, 9, x, x, 3, x}, %  3
        { x, 2, 9, 9,10, x, x, 3, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 12) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 5, 6, 7, x, x, 8, x}, %  1
        { x, x, 5, 6, 7, x, x, 8, x}, %  2
        { x, x, 5, 6, 7, x, x, 8, x}, %  3
        { x, x, 5, 6, 7, x, x, 8, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,12,14,12, x, x, 4, x}, %  1
        { x, x,14,12,12, x, x, 4, x}, %  2
        { x, x,12,12,14, x, x, 4, x}, %  3
        { x, x,12,14,12, x, x, 4, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 13) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 5, 6, 7, x, x, 8, x}, %  1
        { x, x, 5, 6, 7, x, x, 8, x}, %  2
        { x, x, 5, 6, 7, x, x, 8, x}, %  3
        { x, x, 5, 6, 7, x, x, 8, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,15,12,15, x, x, 5, x}, %  1
        { x, x,12,15,15, x, x, 5, x}, %  2
        { x, x,15,15,12, x, x, 5, x}, %  3
        { x, x,15,12,15, x, x, 5, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 14) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 5, 6, 7, x, x, 8, x}, %  1
        { x, x, 5, 6, 7, x, x, 8, x}, %  2
        { x, x, 5, 6, 7, x, x, 8, x}, %  3
        { x, x, 5, 6, 7, x, x, 8, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,14,15,13, x, x, 6, x}, %  1
        { x, x,15,13,14, x, x, 6, x}, %  2
        { x, x,13,14,15, x, x, 6, x}, %  3
        { x, x,14,15,13, x, x, 6, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 15) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 5, 6, 7, x, x, 8, x}, %  1
        { x, 1, 5, 6, 7, x, x, 8, x}, %  2
        { x, 1, 5, 6, 7, x, x, 8, x}, %  3
        { x, 1, 5, 6, 7, x, x, 8, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 3,13,13,14, x, x, 7, x}, %  1
        { x, 3,13,14,13, x, x, 7, x}, %  2
        { x, 3,14,13,13, x, x, 7, x}, %  3
        { x, 3,13,13,14, x, x, 7, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 19) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, x, x, x, x, x, x, x}, %  1
        { x, 1, x, x, x, x, x, x, x}, %  2
        { x, 1, x, x, x, x, x, x, x}, %  3
        { x, 1, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 4, x, x, x, x, x, x, x}, %  1
        { x, 4, x, x, x, x, x, x, x}, %  2
        { x, 4, x, x, x, x, x, x, x}, %  3
        { x, 4, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 23) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, x, x, x, x, x, x, x}, %  1
        { x, 1, x, x, x, x, x, x, x}, %  2
        { x, 1, x, x, x, x, x, x, x}, %  3
        { x, 1, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 5, x, x, x, x, x, x, x}, %  1
        { x, 5, x, x, x, x, x, x, x}, %  2
        { x, 5, x, x, x, x, x, x, x}, %  3
        { x, 5, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 27) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, x, x, x, x, x, x, x}, %  1
        { x, 1, x, x, x, x, x, x, x}, %  2
        { x, 1, x, x, x, x, x, x, x}, %  3
        { x, 1, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 6, x, x, x, x, x, x, x}, %  1
        { x, 6, x, x, x, x, x, x, x}, %  2
        { x, 6, x, x, x, x, x, x, x}, %  3
        { x, 6, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 31) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, x, x, x, x, x, x, x}, %  1
        { x, 1, x, x, x, x, x, x, x}, %  2
        { x, 1, x, x, x, x, x, x, x}, %  3
        { x, 1, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 7, x, x, x, x, x, x, x}, %  1
        { x, 7, x, x, x, x, x, x, x}, %  2
        { x, 7, x, x, x, x, x, x, x}, %  3
        { x, 7, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 32) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 3, x, x, x, x, x, x, x}, %  1
        { x, 3, x, x, x, x, x, x, x}, %  2
        { x, 3, x, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,10, x, x, x, x, x, x, x}, %  1
        { x, 8, x, x, x, x, x, x, x}, %  2
        { x, 8, x, x, x, x, x, x, x}, %  3
        { x,10, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 33) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 2, x, x, x, x, x, x, x}, %  1
        { x, 2, x, x, x, x, x, x, x}, %  2
        { x, 2, x, x, x, x, x, x, x}, %  3
        { x, 2, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 8, x, x, x, x, x, x, x}, %  1
        { x,10, x, x, x, x, x, x, x}, %  2
        { x, 8, x, x, x, x, x, x, x}, %  3
        { x, 8, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 35) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 4, x, x, x, x, x, x, x}, %  1
        { x, 4, x, x, x, x, x, x, x}, %  2
        { x, 4, x, x, x, x, x, x, x}, %  3
        { x, 4, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 8, x, x, x, x, x, x, x}, %  1
        { x, 8, x, x, x, x, x, x, x}, %  2
        { x,10, x, x, x, x, x, x, x}, %  3
        { x, 8, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 36) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 3, x, x, x, x, x, x, x}, %  1
        { x, 3, x, x, x, x, x, x, x}, %  2
        { x, 3, x, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 8, x, x, x, x, x, x, x}, %  1
        { x,11, x, x, x, x, x, x, x}, %  2
        { x,11, x, x, x, x, x, x, x}, %  3
        { x, 8, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 37) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 2, x, x, x, x, x, x, x}, %  1
        { x, 2, x, x, x, x, x, x, x}, %  2
        { x, 2, x, x, x, x, x, x, x}, %  3
        { x, 2, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,11, x, x, x, x, x, x, x}, %  1
        { x, 8, x, x, x, x, x, x, x}, %  2
        { x,11, x, x, x, x, x, x, x}, %  3
        { x,11, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 39) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 4, x, x, x, x, x, x, x}, %  1
        { x, 4, x, x, x, x, x, x, x}, %  2
        { x, 4, x, x, x, x, x, x, x}, %  3
        { x, 4, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,11, x, x, x, x, x, x, x}, %  1
        { x,11, x, x, x, x, x, x, x}, %  2
        { x, 8, x, x, x, x, x, x, x}, %  3
        { x,11, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 40) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 3, x, x, x, x, x, x, x}, %  1
        { x, 3, x, x, x, x, x, x, x}, %  2
        { x, 3, x, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,11, x, x, x, x, x, x, x}, %  1
        { x, 9, x, x, x, x, x, x, x}, %  2
        { x,10, x, x, x, x, x, x, x}, %  3
        { x,11, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 41) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 2, x, x, x, x, x, x, x}, %  1
        { x, 2, x, x, x, x, x, x, x}, %  2
        { x, 2, x, x, x, x, x, x, x}, %  3
        { x, 2, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,10, x, x, x, x, x, x, x}, %  1
        { x,11, x, x, x, x, x, x, x}, %  2
        { x, 9, x, x, x, x, x, x, x}, %  3
        { x,10, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 43) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 4, x, x, x, x, x, x, x}, %  1
        { x, 4, x, x, x, x, x, x, x}, %  2
        { x, 4, x, x, x, x, x, x, x}, %  3
        { x, 4, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 9, x, x, x, x, x, x, x}, %  1
        { x,10, x, x, x, x, x, x, x}, %  2
        { x,11, x, x, x, x, x, x, x}, %  3
        { x, 9, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 44) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 3, x, x, x, x, x, x, x}, %  1
        { x, 3, x, x, x, x, x, x, x}, %  2
        { x, 3, x, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 9, x, x, x, x, x, x, x}, %  1
        { x,10, x, x, x, x, x, x, x}, %  2
        { x, 9, x, x, x, x, x, x, x}, %  3
        { x, 9, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 45) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 2, x, x, x, x, x, x, x}, %  1
        { x, 2, x, x, x, x, x, x, x}, %  2
        { x, 2, x, x, x, x, x, x, x}, %  3
        { x, 2, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 9, x, x, x, x, x, x, x}, %  1
        { x, 9, x, x, x, x, x, x, x}, %  2
        { x,10, x, x, x, x, x, x, x}, %  3
        { x, 9, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 47) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 4, x, x, x, x, x, x, x}, %  1
        { x, 4, x, x, x, x, x, x, x}, %  2
        { x, 4, x, x, x, x, x, x, x}, %  3
        { x, 4, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,10, x, x, x, x, x, x, x}, %  1
        { x, 9, x, x, x, x, x, x, x}, %  2
        { x, 9, x, x, x, x, x, x, x}, %  3
        { x,10, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 48) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 3, x, x, x, x, x, x, x}, %  1
        { x, 3, x, x, x, x, x, x, x}, %  2
        { x, 3, x, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,14, x, x, x, x, x, x, x}, %  1
        { x,12, x, x, x, x, x, x, x}, %  2
        { x,12, x, x, x, x, x, x, x}, %  3
        { x,14, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 49) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 2, x, x, x, x, x, x, x}, %  1
        { x, 2, x, x, x, x, x, x, x}, %  2
        { x, 2, x, x, x, x, x, x, x}, %  3
        { x, 2, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,12, x, x, x, x, x, x, x}, %  1
        { x,14, x, x, x, x, x, x, x}, %  2
        { x,12, x, x, x, x, x, x, x}, %  3
        { x,12, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 51) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 4, x, x, x, x, x, x, x}, %  1
        { x, 4, x, x, x, x, x, x, x}, %  2
        { x, 4, x, x, x, x, x, x, x}, %  3
        { x, 4, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,12, x, x, x, x, x, x, x}, %  1
        { x,12, x, x, x, x, x, x, x}, %  2
        { x,14, x, x, x, x, x, x, x}, %  3
        { x,12, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 52) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 3, x, x, x, x, x, x, x}, %  1
        { x, 3, x, x, x, x, x, x, x}, %  2
        { x, 3, x, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,12, x, x, x, x, x, x, x}, %  1
        { x,15, x, x, x, x, x, x, x}, %  2
        { x,15, x, x, x, x, x, x, x}, %  3
        { x,12, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 53) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 2, x, x, x, x, x, x, x}, %  1
        { x, 2, x, x, x, x, x, x, x}, %  2
        { x, 2, x, x, x, x, x, x, x}, %  3
        { x, 2, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,15, x, x, x, x, x, x, x}, %  1
        { x,12, x, x, x, x, x, x, x}, %  2
        { x,15, x, x, x, x, x, x, x}, %  3
        { x,15, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 55) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 4, x, x, x, x, x, x, x}, %  1
        { x, 4, x, x, x, x, x, x, x}, %  2
        { x, 4, x, x, x, x, x, x, x}, %  3
        { x, 4, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,15, x, x, x, x, x, x, x}, %  1
        { x,15, x, x, x, x, x, x, x}, %  2
        { x,12, x, x, x, x, x, x, x}, %  3
        { x,15, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 56) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 3, x, x, x, x, x, x, x}, %  1
        { x, 3, x, x, x, x, x, x, x}, %  2
        { x, 3, x, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,15, x, x, x, x, x, x, x}, %  1
        { x,13, x, x, x, x, x, x, x}, %  2
        { x,14, x, x, x, x, x, x, x}, %  3
        { x,15, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 57) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 2, x, x, x, x, x, x, x}, %  1
        { x, 2, x, x, x, x, x, x, x}, %  2
        { x, 2, x, x, x, x, x, x, x}, %  3
        { x, 2, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,14, x, x, x, x, x, x, x}, %  1
        { x,15, x, x, x, x, x, x, x}, %  2
        { x,13, x, x, x, x, x, x, x}, %  3
        { x,14, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 59) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 4, x, x, x, x, x, x, x}, %  1
        { x, 4, x, x, x, x, x, x, x}, %  2
        { x, 4, x, x, x, x, x, x, x}, %  3
        { x, 4, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,13, x, x, x, x, x, x, x}, %  1
        { x,14, x, x, x, x, x, x, x}, %  2
        { x,15, x, x, x, x, x, x, x}, %  3
        { x,13, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 60) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 3, x, x, x, x, x, x, x}, %  1
        { x, 3, x, x, x, x, x, x, x}, %  2
        { x, 3, x, x, x, x, x, x, x}, %  3
        { x, 3, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,13, x, x, x, x, x, x, x}, %  1
        { x,14, x, x, x, x, x, x, x}, %  2
        { x,13, x, x, x, x, x, x, x}, %  3
        { x,13, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 61) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 2, x, x, x, x, x, x, x}, %  1
        { x, 2, x, x, x, x, x, x, x}, %  2
        { x, 2, x, x, x, x, x, x, x}, %  3
        { x, 2, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,13, x, x, x, x, x, x, x}, %  1
        { x,13, x, x, x, x, x, x, x}, %  2
        { x,14, x, x, x, x, x, x, x}, %  3
        { x,13, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(X, Y, 63) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 4, x, x, x, x, x, x, x}, %  1
        { x, 4, x, x, x, x, x, x, x}, %  2
        { x, 4, x, x, x, x, x, x, x}, %  3
        { x, 4, x, x, x, x, x, x, x}  %  4
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,14, x, x, x, x, x, x, x}, %  1
        { x,13, x, x, x, x, x, x, x}, %  2
        { x,13, x, x, x, x, x, x, x}, %  3
        { x,14, x, x, x, x, x, x, x}  %  4
    })),
    {XX, MM};
from_max_v_240z(_, _, _) ->
    {x, x}.

%%====================================================================
%% to_max_v_240z
%%====================================================================

to_max_v_240z(XX, YY, _)
        when XX > 8 orelse YY > 4 ->
    {x, x};
to_max_v_240z(XX, YY, 0) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  1
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  2
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 7}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 3, 0, 1, 0, 0, 1, 0, 8}, %  1
        { x, 3, 1, 0, 0, 1, 0, 0, 8}, %  2
        { x, 3, 0, 0, 1, 0, 0, 1, 8}, %  3
        { x, 3, 0, 1, 0, 0, 1, 0, 8}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 1) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  1
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  2
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 7}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 7, 3, 3, 2, 3, 3, 2, 9}, %  1
        { x, 7, 3, 2, 3, 3, 2, 3, 9}, %  2
        { x, 7, 2, 3, 3, 2, 3, 3, 9}, %  3
        { x, 7, 3, 3, 2, 3, 3, 2, 9}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 2) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  1
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  2
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 7}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,11, 2, 0, 3, 2, 0, 3,10}, %  1
        { x,11, 0, 3, 2, 0, 3, 2,10}, %  2
        { x,11, 3, 2, 0, 3, 2, 0,10}, %  3
        { x,11, 2, 0, 3, 2, 0, 3,10}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 3) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  1
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  2
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 7}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,15, 1, 2, 1, 1, 2, 1,11}, %  1
        { x,15, 2, 1, 1, 2, 1, 1,11}, %  2
        { x,15, 1, 1, 2, 1, 1, 2,11}, %  3
        { x,15, 1, 2, 1, 1, 2, 1,11}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 4) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  1
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  2
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 7}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,19, 4, 5, 4, 4, 5, 4,12}, %  1
        { x,19, 5, 4, 4, 5, 4, 4,12}, %  2
        { x,19, 4, 4, 5, 4, 4, 5,12}, %  3
        { x,19, 4, 5, 4, 4, 5, 4,12}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 5) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  1
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  2
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 7}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,23, 7, 7, 6, 7, 7, 6,13}, %  1
        { x,23, 7, 6, 7, 7, 6, 7,13}, %  2
        { x,23, 6, 7, 7, 6, 7, 7,13}, %  3
        { x,23, 7, 7, 6, 7, 7, 6,13}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 6) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  1
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  2
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 7}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,27, 6, 4, 7, 6, 4, 7,14}, %  1
        { x,27, 4, 7, 6, 4, 7, 6,14}, %  2
        { x,27, 7, 6, 4, 7, 6, 4,14}, %  3
        { x,27, 6, 4, 7, 6, 4, 7,14}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 7) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  1
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  2
        { x, 1, 2, 3, 4, 5, 6, 7, 7}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 7}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x,31, 5, 6, 5, 5, 6, 5,15}, %  1
        { x,31, 6, 5, 5, 6, 5, 5,15}, %  2
        { x,31, 5, 5, 6, 5, 5, 6,15}, %  3
        { x,31, 5, 6, 5, 5, 6, 5,15}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 8) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  1
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  2
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  3
        { x, x, 1, 1, 1, 2, 3, 4, x}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,33,36,35, 8, 9, 8, x}, %  1
        { x, x,37,32,35, 9, 8, 8, x}, %  2
        { x, x,33,32,39, 8, 8, 9, x}, %  3
        { x, x,33,36,35, 8, 9, 8, x}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 9) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  1
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  2
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  3
        { x, x, 1, 1, 1, 2, 3, 4, x}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,45,44,43,11,11,10, x}, %  1
        { x, x,45,40,47,11,10,11, x}, %  2
        { x, x,41,44,47,10,11,11, x}, %  3
        { x, x,45,44,43,11,11,10, x}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 10) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  1
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  2
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  3
        { x, x, 1, 1, 1, 2, 3, 4, x}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,41,32,47,10, 8,11, x}, %  1
        { x, x,33,44,43, 8,11,10, x}, %  2
        { x, x,45,40,35,11,10, 8, x}, %  3
        { x, x,41,32,47,10, 8,11, x}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 11) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  1
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  2
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  3
        { x, x, 1, 1, 1, 2, 3, 4, x}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,37,40,39, 9,10, 9, x}, %  1
        { x, x,41,36,39,10, 9, 9, x}, %  2
        { x, x,37,36,43, 9, 9,10, x}, %  3
        { x, x,37,40,39, 9,10, 9, x}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 12) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  1
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  2
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  3
        { x, x, 1, 1, 1, 2, 3, 4, x}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,49,52,51,12,13,12, x}, %  1
        { x, x,53,48,51,13,12,12, x}, %  2
        { x, x,49,48,55,12,12,13, x}, %  3
        { x, x,49,52,51,12,13,12, x}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 13) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  1
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  2
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  3
        { x, x, 1, 1, 1, 2, 3, 4, x}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,61,60,59,15,15,14, x}, %  1
        { x, x,61,56,63,15,14,15, x}, %  2
        { x, x,57,60,63,14,15,15, x}, %  3
        { x, x,61,60,59,15,15,14, x}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 14) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  1
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  2
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  3
        { x, x, 1, 1, 1, 2, 3, 4, x}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,57,48,63,14,12,15, x}, %  1
        { x, x,49,60,59,12,15,14, x}, %  2
        { x, x,61,56,51,15,14,12, x}, %  3
        { x, x,57,48,63,14,12,15, x}  %  4
    })),
    {X, I};
to_max_v_240z(XX, YY, 15) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  1
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  2
        { x, x, 1, 1, 1, 2, 3, 4, x}, %  3
        { x, x, 1, 1, 1, 2, 3, 4, x}  %  4
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8
        { x, x, x, x, x, x, x, x, x}, %  0
        { x, x,53,56,55,13,14,13, x}, %  1
        { x, x,57,52,55,14,13,13, x}, %  2
        { x, x,53,52,59,13,13,14, x}, %  3
        { x, x,53,56,55,13,14,13, x}  %  4
    })),
    {X, I};
to_max_v_240z(_, _, _) ->
    {x, x}.

%%====================================================================
%% from_max_v_570z
%%====================================================================

from_max_v_570z(X, Y, _)
        when X > 13 orelse Y > 7 ->
    {x, x};
from_max_v_570z(X, Y, 0) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, 2, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, 2, 0, 0, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, 0, 0, 2, x}, %  3
        { 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, x}, %  4
        { 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, x}, %  5
        { 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, x}, %  6
        { 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 1) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 3, 0, 3, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, 0, 3, 3, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, 3, 3, 0, x}, %  3
        { x, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, x}, %  4
        { x, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, x}, %  5
        { x, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, x}, %  6
        { x, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 2) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 2, 3, 1, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, 3, 1, 2, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, 1, 2, 3, x}, %  3
        { x, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, x}, %  4
        { x, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, x}, %  5
        { x, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, x}, %  6
        { x, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 3) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 1, 1, 2, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, 1, 2, 1, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, 2, 1, 1, x}, %  3
        { x, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, x}, %  4
        { x, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, x}, %  5
        { x, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, x}, %  6
        { x, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 4) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 4, 6, 4, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, 6, 4, 4, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, 4, 4, 6, x}, %  3
        { 1, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, x}, %  4
        { 1, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, x}, %  5
        { 1, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, x}, %  6
        { 1, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 5) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 7, 4, 7, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, 4, 7, 7, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, 7, 7, 4, x}, %  3
        { x, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, x}, %  4
        { x, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, x}, %  5
        { x, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, x}, %  6
        { x, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 6) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 6, 7, 5, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, 7, 5, 6, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, 5, 6, 7, x}, %  3
        { x, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, x}, %  4
        { x, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, x}, %  5
        { x, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, x}, %  6
        { x, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 7) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9,10,11,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9,10,11,12, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9,10,11,12, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, 5, 5, 6, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, 5, 6, 5, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, 6, 5, 5, x}, %  3
        { x, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, x}, %  4
        { x, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, x}, %  5
        { x, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, x}, %  6
        { x, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 8) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  3
        { 0, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  4
        { 0, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  5
        { 0, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  6
        { 0, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, x}, %  3
        { 2, 8,10, 8, 8,10, 8, 8,10, 8, x, x, 0, x}, %  4
        { 2,10, 8, 8,10, 8, 8,10, 8, 8, x, x, 0, x}, %  5
        { 2, 8, 8,10, 8, 8,10, 8, 8,10, x, x, 0, x}, %  6
        { 2, 8,10, 8, 8,10, 8, 8,10, 8, x, x, 0, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 9) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, x}, %  3
        { x,11, 8,11,11, 8,11,11, 8,11, x, x, 1, x}, %  4
        { x, 8,11,11, 8,11,11, 8,11,11, x, x, 1, x}, %  5
        { x,11,11, 8,11,11, 8,11,11, 8, x, x, 1, x}, %  6
        { x,11, 8,11,11, 8,11,11, 8,11, x, x, 1, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 10) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 2, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 2, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 2, x}, %  3
        { x,10,11, 9,10,11, 9,10,11, 9, x, x, 2, x}, %  4
        { x,11, 9,10,11, 9,10,11, 9,10, x, x, 2, x}, %  5
        { x, 9,10,11, 9,10,11, 9,10,11, x, x, 2, x}, %  6
        { x,10,11, 9,10,11, 9,10,11, 9, x, x, 2, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 11) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 3, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 3, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 3, x}, %  3
        { x, 9, 9,10, 9, 9,10, 9, 9,10, x, x, 3, x}, %  4
        { x, 9,10, 9, 9,10, 9, 9,10, 9, x, x, 3, x}, %  5
        { x,10, 9, 9,10, 9, 9,10, 9, 9, x, x, 3, x}, %  6
        { x, 9, 9,10, 9, 9,10, 9, 9,10, x, x, 3, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 12) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  3
        { 0, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  4
        { 0, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  5
        { 0, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  6
        { 0, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 4, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 4, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 4, x}, %  3
        { 3,12,14,12,12,14,12,12,14,12, x, x, 4, x}, %  4
        { 3,14,12,12,14,12,12,14,12,12, x, x, 4, x}, %  5
        { 3,12,12,14,12,12,14,12,12,14, x, x, 4, x}, %  6
        { 3,12,14,12,12,14,12,12,14,12, x, x, 4, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 13) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 5, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 5, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 5, x}, %  3
        { x,15,12,15,15,12,15,15,12,15, x, x, 5, x}, %  4
        { x,12,15,15,12,15,15,12,15,15, x, x, 5, x}, %  5
        { x,15,15,12,15,15,12,15,15,12, x, x, 5, x}, %  6
        { x,15,12,15,15,12,15,15,12,15, x, x, 5, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 14) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,13, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 6, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 6, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 6, x}, %  3
        { x,14,15,13,14,15,13,14,15,13, x, x, 6, x}, %  4
        { x,15,13,14,15,13,14,15,13,14, x, x, 6, x}, %  5
        { x,13,14,15,13,14,15,13,14,15, x, x, 6, x}, %  6
        { x,14,15,13,14,15,13,14,15,13, x, x, 6, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 15) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, x, x,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, x, x,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, x, x,13, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12, x, x,13, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, 7, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, 7, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, 7, x}, %  3
        { x,13,13,14,13,13,14,13,13,14, x, x, 7, x}, %  4
        { x,13,14,13,13,14,13,13,14,13, x, x, 7, x}, %  5
        { x,14,13,13,14,13,13,14,13,13, x, x, 7, x}, %  6
        { x,13,13,14,13,13,14,13,13,14, x, x, 7, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 16) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 19) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 20) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 23) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 24) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 28) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 31) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 32) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 8, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, 8, x, x, x, x}, %  3
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 33) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 8, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, 8, x, x, x, x}, %  3
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 34) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 35) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 8, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 8, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 36) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 8, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  3
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 37) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 8, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  3
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 38) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 39) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, 8, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 40) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  3
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 41) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  3
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 42) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 43) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 44) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  3
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 45) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  3
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 46) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 47) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 48) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,14, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 49) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,14, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 50) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 51) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,14, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 52) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  3
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 53) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  3
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 54) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 55) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 56) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,14, x, x, x, x}, %  3
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 57) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,14, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  3
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 58) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 59) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,14, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,15, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 60) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,11, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,14, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  3
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 61) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,10, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,14, x, x, x, x}, %  3
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 62) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(X, Y, 63) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,12, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,14, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x,13, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}  %  7
    })),
    {XX, MM};
from_max_v_570z(_, _, _) ->
    {x, x}.

%%====================================================================
%% to_max_v_570z
%%====================================================================

to_max_v_570z(XX, YY, _)
        when XX > 13 orelse YY > 7 ->
    {x, x};
to_max_v_570z(XX, YY, 0) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 0, 1, 0, 8}, %  1
        { x, x, x, x, x, x, x, x, x, x, 1, 0, 0, 8}, %  2
        { x, x, x, x, x, x, x, x, x, x, 0, 0, 1, 8}, %  3
        { 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 8}, %  4
        { 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 8}, %  5
        { 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 8}, %  6
        { 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 8}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 1) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 3, 3, 2, 9}, %  1
        { x, x, x, x, x, x, x, x, x, x, 3, 2, 3, 9}, %  2
        { x, x, x, x, x, x, x, x, x, x, 2, 3, 3, 9}, %  3
        { 4, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 9}, %  4
        { 4, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 9}, %  5
        { 4, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 9}, %  6
        { 4, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 9}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 2) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 2, 0, 3,10}, %  1
        { x, x, x, x, x, x, x, x, x, x, 0, 3, 2,10}, %  2
        { x, x, x, x, x, x, x, x, x, x, 3, 2, 0,10}, %  3
        { 8, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3,10}, %  4
        { 8, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2,10}, %  5
        { 8, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0,10}, %  6
        { 8, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3,10}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 3) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 1, 2, 1,11}, %  1
        { x, x, x, x, x, x, x, x, x, x, 2, 1, 1,11}, %  2
        { x, x, x, x, x, x, x, x, x, x, 1, 1, 2,11}, %  3
        {12, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1,11}, %  4
        {12, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1,11}, %  5
        {12, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2,11}, %  6
        {12, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1,11}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 4) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 4, 5, 4,12}, %  1
        { x, x, x, x, x, x, x, x, x, x, 5, 4, 4,12}, %  2
        { x, x, x, x, x, x, x, x, x, x, 4, 4, 5,12}, %  3
        {16, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4,12}, %  4
        {16, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4,12}, %  5
        {16, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5,12}, %  6
        {16, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4,12}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 5) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 7, 7, 6,13}, %  1
        { x, x, x, x, x, x, x, x, x, x, 7, 6, 7,13}, %  2
        { x, x, x, x, x, x, x, x, x, x, 6, 7, 7,13}, %  3
        {20, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6,13}, %  4
        {20, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7,13}, %  5
        {20, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7,13}, %  6
        {20, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6,13}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 6) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 6, 4, 7,14}, %  1
        { x, x, x, x, x, x, x, x, x, x, 4, 7, 6,14}, %  2
        { x, x, x, x, x, x, x, x, x, x, 7, 6, 4,14}, %  3
        {24, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7,14}, %  4
        {24, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6,14}, %  5
        {24, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4,14}, %  6
        {24, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7,14}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 7) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  1
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  2
        { x, x, x, x, x, x, x, x, x, x,10,11,12,12}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,12}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 5, 6, 5,15}, %  1
        { x, x, x, x, x, x, x, x, x, x, 6, 5, 5,15}, %  2
        { x, x, x, x, x, x, x, x, x, x, 5, 5, 6,15}, %  3
        {28, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5,15}, %  4
        {28, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5,15}, %  5
        {28, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6,15}, %  6
        {28, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5,15}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 8) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 9, 9, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, 9, 9, 9, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, 9, 9, 9, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,33,36,35, x}, %  1
        { x, x, x, x, x, x, x, x, x, x,37,32,35, x}, %  2
        { x, x, x, x, x, x, x, x, x, x,33,32,39, x}, %  3
        { x,34,37,32, 8, 9, 8, 8, 9, 8, 8, 9, 8, x}, %  4
        { x,38,33,32, 9, 8, 8, 9, 8, 8, 9, 8, 8, x}, %  5
        { x,34,33,36, 8, 8, 9, 8, 8, 9, 8, 8, 9, x}, %  6
        { x,34,37,32, 8, 9, 8, 8, 9, 8, 8, 9, 8, x}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 9) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 7,45,44,43, x}, %  1
        { x, x, x, x, x, x, x, x, x, 7,45,40,47, x}, %  2
        { x, x, x, x, x, x, x, x, x, 7,41,44,47, x}, %  3
        { x,46,45,40,11,11,10,11,11,10,11,11,10, x}, %  4
        { x,46,41,44,11,10,11,11,10,11,11,10,11, x}, %  5
        { x,42,45,44,10,11,11,10,11,11,10,11,11, x}, %  6
        { x,46,45,40,11,11,10,11,11,10,11,11,10, x}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 10) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 9, 9, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, 9, 9, 9, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, 9, 9, 9, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,41,32,47, x}, %  1
        { x, x, x, x, x, x, x, x, x, x,33,44,43, x}, %  2
        { x, x, x, x, x, x, x, x, x, x,45,40,35, x}, %  3
        { x,42,33,44,10, 8,11,10, 8,11,10, 8,11, x}, %  4
        { x,34,45,40, 8,11,10, 8,11,10, 8,11,10, x}, %  5
        { x,46,41,32,11,10, 8,11,10, 8,11,10, 8, x}, %  6
        { x,42,33,44,10, 8,11,10, 8,11,10, 8,11, x}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 11) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,15,37,40,39, x}, %  1
        { x, x, x, x, x, x, x, x, x,15,41,36,39, x}, %  2
        { x, x, x, x, x, x, x, x, x,15,37,36,43, x}, %  3
        { x,38,41,36, 9,10, 9, 9,10, 9, 9,10, 9, x}, %  4
        { x,42,37,36,10, 9, 9,10, 9, 9,10, 9, 9, x}, %  5
        { x,38,37,40, 9, 9,10, 9, 9,10, 9, 9,10, x}, %  6
        { x,38,41,36, 9,10, 9, 9,10, 9, 9,10, 9, x}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 12) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,19,49,52,51, x}, %  1
        { x, x, x, x, x, x, x, x, x,19,53,48,51, x}, %  2
        { x, x, x, x, x, x, x, x, x,19,49,48,55, x}, %  3
        { x,50,53,48,12,13,12,12,13,12,12,13,12, x}, %  4
        { x,54,49,48,13,12,12,13,12,12,13,12,12, x}, %  5
        { x,50,49,52,12,12,13,12,12,13,12,12,13, x}, %  6
        { x,50,53,48,12,13,12,12,13,12,12,13,12, x}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 13) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,23,61,60,59, x}, %  1
        { x, x, x, x, x, x, x, x, x,23,61,56,63, x}, %  2
        { x, x, x, x, x, x, x, x, x,23,57,60,63, x}, %  3
        { x,62,61,56,15,15,14,15,15,14,15,15,14, x}, %  4
        { x,62,57,60,15,14,15,15,14,15,15,14,15, x}, %  5
        { x,58,61,60,14,15,15,14,15,15,14,15,15, x}, %  6
        { x,62,61,56,15,15,14,15,15,14,15,15,14, x}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 14) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, 9, 9, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, 9, 9, 9, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, 9, 9, 9, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x,57,48,63, x}, %  1
        { x, x, x, x, x, x, x, x, x, x,49,60,59, x}, %  2
        { x, x, x, x, x, x, x, x, x, x,61,56,51, x}, %  3
        { x,58,49,60,14,12,15,14,12,15,14,12,15, x}, %  4
        { x,50,61,56,12,15,14,12,15,14,12,15,14, x}, %  5
        { x,62,57,48,15,14,12,15,14,12,15,14,12, x}, %  6
        { x,58,49,60,14,12,15,14,12,15,14,12,15, x}  %  7
    })),
    {X, I};
to_max_v_570z(XX, YY, 15) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  2
        { x, x, x, x, x, x, x, x, x, 9, 9, 9, 9, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, x}  %  7
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x,31,53,56,55, x}, %  1
        { x, x, x, x, x, x, x, x, x,31,57,52,55, x}, %  2
        { x, x, x, x, x, x, x, x, x,31,53,52,59, x}, %  3
        { x,54,57,52,13,14,13,13,14,13,13,14,13, x}, %  4
        { x,58,53,52,14,13,13,14,13,13,14,13,13, x}, %  5
        { x,54,53,56,13,13,14,13,13,14,13,13,14, x}, %  6
        { x,54,57,52,13,14,13,13,14,13,13,14,13, x}  %  7
    })),
    {X, I};
to_max_v_570z(_, _, _) ->
    {x, x}.

%%====================================================================
%% from_max_v_1270z
%%====================================================================

from_max_v_1270z(X, Y, _)
        when X > 17 orelse Y > 10 ->
    {x, x};
from_max_v_1270z(X, Y, 0) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 2, 0, 0, 2, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 2, 0, 0, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, 2, 0, 0, 2, x}, %  3
        { 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, x}, %  4
        { 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, x}, %  5
        { 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, x}, %  6
        { 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, x}, %  7
        { 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, x}, %  8
        { 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, x}, %  9
        { 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 1) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11,12,13,14,15,16, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11,12,13,14,15,16, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11,12,13,14,15,16, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 8, 0, 3, 3, 0, 3, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 8, 3, 3, 0, 3, 3, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 8, 3, 0, 3, 3, 0, x}, %  3
        { x, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, x}, %  4
        { x, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, x}, %  5
        { x, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, x}, %  6
        { x, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, x}, %  7
        { x, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, x}, %  8
        { x, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, x}, %  9
        { x, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 2) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 3, 1, 2, 3, 1, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 3, 1, 2, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 2, 3, 1, 2, 3, x}, %  3
        { x, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, x}, %  4
        { x, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, x}, %  5
        { x, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, x}, %  6
        { x, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, x}, %  7
        { x, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, x}, %  8
        { x, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, x}, %  9
        { x, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 3) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 1, 1, 2, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 2, 1, 1, 2, 1, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 2, 1, 1, x}, %  3
        { x, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, x}, %  4
        { x, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, x}, %  5
        { x, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, x}, %  6
        { x, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, x}, %  7
        { x, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, x}, %  8
        { x, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, x}, %  9
        { x, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 4) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 6, 4, 4, 6, 4, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 6, 4, 4, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 4, 6, 4, 4, 6, x}, %  3
        { 1, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, x}, %  4
        { 1, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, x}, %  5
        { 1, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, x}, %  6
        { 1, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, x}, %  7
        { 1, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, x}, %  8
        { 1, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, x}, %  9
        { 1, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 5) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11,12,13,14,15,16, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11,12,13,14,15,16, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11,12,13,14,15,16, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 9, 4, 7, 7, 4, 7, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 9, 7, 7, 4, 7, 7, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 9, 7, 4, 7, 7, 4, x}, %  3
        { x, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, x}, %  4
        { x, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, x}, %  5
        { x, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, x}, %  6
        { x, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, x}, %  7
        { x, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, x}, %  8
        { x, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, x}, %  9
        { x, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 6) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 7, 5, 6, 7, 5, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 5, 6, 7, 5, 6, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 6, 7, 5, 6, 7, x}, %  3
        { x, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, x}, %  4
        { x, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, x}, %  5
        { x, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, x}, %  6
        { x, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, x}, %  7
        { x, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, x}, %  8
        { x, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, x}, %  9
        { x, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 7) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 5, 6, 5, 5, 6, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 6, 5, 5, 6, 5, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 6, 5, 5, x}, %  3
        { x, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, x}, %  4
        { x, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, x}, %  5
        { x, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, x}, %  6
        { x, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, x}, %  7
        { x, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, x}, %  8
        { x, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, x}, %  9
        { x, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 8) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  3
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  4
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  5
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  6
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  7
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  8
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  9
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,10, 8, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 8, 8, x, x, 0, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 8,10, x, x, 0, x}, %  3
        { 2, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, x, x, 0, x}, %  4
        { 2, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8, x, x, 0, x}, %  5
        { 2,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, x, x, 0, x}, %  6
        { 2, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, x, x, 0, x}, %  7
        { 2, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8, x, x, 0, x}, %  8
        { 2,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, x, x, 0, x}, %  9
        { 2, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, x, x, 0, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 9) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 8,11, x, x, 1, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,11,11, x, x, 1, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,11, 8, x, x, 1, x}, %  3
        { x,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11, x, x, 1, x}, %  4
        { x,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, x, x, 1, x}, %  5
        { x, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8, x, x, 1, x}, %  6
        { x,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11, x, x, 1, x}, %  7
        { x,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, x, x, 1, x}, %  8
        { x, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8, x, x, 1, x}, %  9
        { x,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11, x, x, 1, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 10) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,11, 9, x, x, 2, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 9,10, x, x, 2, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,10,11, x, x, 2, x}, %  3
        { x, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9, x, x, 2, x}, %  4
        { x,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10, x, x, 2, x}, %  5
        { x,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, x, x, 2, x}, %  6
        { x, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9, x, x, 2, x}, %  7
        { x,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10, x, x, 2, x}, %  8
        { x,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, x, x, 2, x}, %  9
        { x, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9, x, x, 2, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 11) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 9,10, x, x, 3, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,10, 9, x, x, 3, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 9, 9, x, x, 3, x}, %  3
        { x,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x, x, 3, x}, %  4
        { x, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x, x, 3, x}, %  5
        { x, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9, x, x, 3, x}, %  6
        { x,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x, x, 3, x}, %  7
        { x, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x, x, 3, x}, %  8
        { x, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9, x, x, 3, x}, %  9
        { x,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x, x, 3, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 12) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  3
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  4
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  5
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  6
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  7
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  8
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  9
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,14,12, x, x, 4, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,12, x, x, 4, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,14, x, x, 4, x}, %  3
        { 3,12,12,14,12,12,14,12,12,14,12,12,14,12, x, x, 4, x}, %  4
        { 3,12,14,12,12,14,12,12,14,12,12,14,12,12, x, x, 4, x}, %  5
        { 3,14,12,12,14,12,12,14,12,12,14,12,12,14, x, x, 4, x}, %  6
        { 3,12,12,14,12,12,14,12,12,14,12,12,14,12, x, x, 4, x}, %  7
        { 3,12,14,12,12,14,12,12,14,12,12,14,12,12, x, x, 4, x}, %  8
        { 3,14,12,12,14,12,12,14,12,12,14,12,12,14, x, x, 4, x}, %  9
        { 3,12,12,14,12,12,14,12,12,14,12,12,14,12, x, x, 4, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 13) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11,15,16, x, x,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11,15,16, x, x,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11,15,16, x, x,17, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11,12,15, x, x, 5, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11,15,15, x, x, 5, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11,15,12, x, x, 5, x}, %  3
        { x,15,15,12,15,15,12,15,15,12,15,15,12,15, x, x, 5, x}, %  4
        { x,15,12,15,15,12,15,15,12,15,15,12,15,15, x, x, 5, x}, %  5
        { x,12,15,15,12,15,15,12,15,15,12,15,15,12, x, x, 5, x}, %  6
        { x,15,15,12,15,15,12,15,15,12,15,15,12,15, x, x, 5, x}, %  7
        { x,15,12,15,15,12,15,15,12,15,15,12,15,15, x, x, 5, x}, %  8
        { x,12,15,15,12,15,15,12,15,15,12,15,15,12, x, x, 5, x}, %  9
        { x,15,15,12,15,15,12,15,15,12,15,15,12,15, x, x, 5, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 14) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,15,13, x, x, 6, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,13,14, x, x, 6, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,14,15, x, x, 6, x}, %  3
        { x,13,14,15,13,14,15,13,14,15,13,14,15,13, x, x, 6, x}, %  4
        { x,14,15,13,14,15,13,14,15,13,14,15,13,14, x, x, 6, x}, %  5
        { x,15,13,14,15,13,14,15,13,14,15,13,14,15, x, x, 6, x}, %  6
        { x,13,14,15,13,14,15,13,14,15,13,14,15,13, x, x, 6, x}, %  7
        { x,14,15,13,14,15,13,14,15,13,14,15,13,14, x, x, 6, x}, %  8
        { x,15,13,14,15,13,14,15,13,14,15,13,14,15, x, x, 6, x}, %  9
        { x,13,14,15,13,14,15,13,14,15,13,14,15,13, x, x, 6, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 15) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,15,16, x, x,17, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16, x, x,17, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,13,14, x, x, 7, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,14,13, x, x, 7, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,13,13, x, x, 7, x}, %  3
        { x,14,13,13,14,13,13,14,13,13,14,13,13,14, x, x, 7, x}, %  4
        { x,13,13,14,13,13,14,13,13,14,13,13,14,13, x, x, 7, x}, %  5
        { x,13,14,13,13,14,13,13,14,13,13,14,13,13, x, x, 7, x}, %  6
        { x,14,13,13,14,13,13,14,13,13,14,13,13,14, x, x, 7, x}, %  7
        { x,13,13,14,13,13,14,13,13,14,13,13,14,13, x, x, 7, x}, %  8
        { x,13,14,13,13,14,13,13,14,13,13,14,13,13, x, x, 7, x}, %  9
        { x,14,13,13,14,13,13,14,13,13,14,13,13,14, x, x, 7, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 16) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 20) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 24) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 25) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 28) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 29) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 32) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 33) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x}, %  3
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 34) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x}, %  3
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 35) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 36) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 37) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  3
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 38) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x}, %  3
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 39) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 40) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 41) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x}, %  3
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 42) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  3
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 43) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 44) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 45) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x}, %  3
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 46) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x}, %  3
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 47) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 48) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 49) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  3
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 50) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 51) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 52) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 53) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  3
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 54) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  3
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 55) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 56) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 57) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  3
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 58) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  3
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 59) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 60) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 61) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  3
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 62) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  3
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(X, Y, 63) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 10
    })),
    {XX, MM};
from_max_v_1270z(_, _, _) ->
    {x, x}.

%%====================================================================
%% to_max_v_1270z
%%====================================================================

to_max_v_1270z(XX, YY, _)
        when XX > 17 orelse YY > 10 ->
    {x, x};
to_max_v_1270z(XX, YY, 0) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, 0, 0, 1, 0, 8}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 1, 0, 0, 8}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, 1, 0, 0, 1, 8}, %  3
        { 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 8}, %  4
        { 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 8}, %  5
        { 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 8}, %  6
        { 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 8}, %  7
        { 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 8}, %  8
        { 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 8}, %  9
        { 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 8}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 1) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 3, 2, 3, 3, 2, 9}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 2, 3, 3, 2, 3, 9}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 2, 3, 3, 9}, %  3
        { 4, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 9}, %  4
        { 4, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 9}, %  5
        { 4, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 9}, %  6
        { 4, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 9}, %  7
        { 4, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 9}, %  8
        { 4, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 9}, %  9
        { 4, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 9}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 2) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 0, 3, 2, 0, 3,10}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 3, 2, 0, 3, 2,10}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 2, 0, 3, 2, 0,10}, %  3
        { 8, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3,10}, %  4
        { 8, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2,10}, %  5
        { 8, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0,10}, %  6
        { 8, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3,10}, %  7
        { 8, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2,10}, %  8
        { 8, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0,10}, %  9
        { 8, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3,10}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 3) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 2, 1, 1, 2, 1,11}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 2, 1, 1,11}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 1, 1, 2,11}, %  3
        {12, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1,11}, %  4
        {12, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1,11}, %  5
        {12, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2,11}, %  6
        {12, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1,11}, %  7
        {12, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1,11}, %  8
        {12, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2,11}, %  9
        {12, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1,11}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 4) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 5, 4, 4, 5, 4,12}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 5, 4, 4,12}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 4, 5, 4, 4, 5,12}, %  3
        {16, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4,12}, %  4
        {16, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4,12}, %  5
        {16, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5,12}, %  6
        {16, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4,12}, %  7
        {16, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4,12}, %  8
        {16, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5,12}, %  9
        {16, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4,12}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 5) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 7, 6, 7, 7, 6,13}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 6, 7, 7, 6, 7,13}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 7, 7, 6, 7, 7,13}, %  3
        {20, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6,13}, %  4
        {20, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7,13}, %  5
        {20, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7,13}, %  6
        {20, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6,13}, %  7
        {20, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7,13}, %  8
        {20, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7,13}, %  9
        {20, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6,13}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 6) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 4, 7, 6, 4, 7,14}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 7, 6, 4, 7, 6,14}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 6, 4, 7, 6, 4,14}, %  3
        {24, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7,14}, %  4
        {24, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6,14}, %  5
        {24, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4,14}, %  6
        {24, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7,14}, %  7
        {24, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6,14}, %  8
        {24, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4,14}, %  9
        {24, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7,14}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 7) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,12,13,14,15,16,16}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,16}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, 6, 5, 5, 6, 5,15}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 6, 5, 5,15}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, 5, 6, 5, 5, 6,15}, %  3
        {28, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5,15}, %  4
        {28, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5,15}, %  5
        {28, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6,15}, %  6
        {28, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5,15}, %  7
        {28, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5,15}, %  8
        {28, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6,15}, %  9
        {28, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5,15}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 8) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 1,39,34,33, 9, 8, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 1,35,34,37, 8, 8, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 1,35,38,33, 8, 9, x}, %  3
        { x,34,33,36, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, x}, %  4
        { x,34,37,32, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, x}, %  5
        { x,38,33,32, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, x}, %  6
        { x,34,33,36, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, x}, %  7
        { x,34,37,32, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, x}, %  8
        { x,38,33,32, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, x}, %  9
        { x,34,33,36, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, x}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 9) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, 5,47,42,45,11,10, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, 5,43,46,45,10,11, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, 5,47,46,41,11,11, x}, %  3
        { x,42,45,44,10,11,11,10,11,11,10,11,11,10,11,11,10, x}, %  4
        { x,46,45,40,11,11,10,11,11,10,11,11,10,11,11,10,11, x}, %  5
        { x,46,41,44,11,10,11,11,10,11,11,10,11,11,10,11,11, x}, %  6
        { x,42,45,44,10,11,11,10,11,11,10,11,11,10,11,11,10, x}, %  7
        { x,46,45,40,11,11,10,11,11,10,11,11,10,11,11,10,11, x}, %  8
        { x,46,41,44,11,10,11,11,10,11,11,10,11,11,10,11,11, x}, %  9
        { x,42,45,44,10,11,11,10,11,11,10,11,11,10,11,11,10, x}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 10) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,12,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,12,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,12,13, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,35,46,41, 8,11, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,47,42,33,11,10, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,43,34,45,10, 8, x}, %  3
        { x,46,41,32,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11, x}, %  4
        { x,42,33,44,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, x}, %  5
        { x,34,45,40, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8, x}, %  6
        { x,46,41,32,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11, x}, %  7
        { x,42,33,44,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, x}, %  8
        { x,34,45,40, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8, x}, %  9
        { x,46,41,32,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11, x}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 11) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,13,43,38,37,10, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,13,39,38,41, 9, 9, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,13,39,42,37, 9,10, x}, %  3
        { x,38,37,40, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x}, %  4
        { x,38,41,36, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9, x}, %  5
        { x,42,37,36,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x}, %  6
        { x,38,37,40, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x}, %  7
        { x,38,41,36, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9, x}, %  8
        { x,42,37,36,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x}, %  9
        { x,38,37,40, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 12) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,12,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,12,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,12,13, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,55,50,49,13,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,51,50,53,12,12, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,51,54,49,12,13, x}, %  3
        { x,50,49,52,12,12,13,12,12,13,12,12,13,12,12,13,12, x}, %  4
        { x,50,53,48,12,13,12,12,13,12,12,13,12,12,13,12,12, x}, %  5
        { x,54,49,48,13,12,12,13,12,12,13,12,12,13,12,12,13, x}, %  6
        { x,50,49,52,12,12,13,12,12,13,12,12,13,12,12,13,12, x}, %  7
        { x,50,53,48,12,13,12,12,13,12,12,13,12,12,13,12,12, x}, %  8
        { x,54,49,48,13,12,12,13,12,12,13,12,12,13,12,12,13, x}, %  9
        { x,50,49,52,12,12,13,12,12,13,12,12,13,12,12,13,12, x}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 13) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,12,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,12,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,11,11,11,12,13, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x,63,58,61,15,14, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x,59,62,61,14,15, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x,63,62,57,15,15, x}, %  3
        { x,58,61,60,14,15,15,14,15,15,14,15,15,14,15,15,14, x}, %  4
        { x,62,61,56,15,15,14,15,15,14,15,15,14,15,15,14,15, x}, %  5
        { x,62,57,60,15,14,15,15,14,15,15,14,15,15,14,15,15, x}, %  6
        { x,58,61,60,14,15,15,14,15,15,14,15,15,14,15,15,14, x}, %  7
        { x,62,61,56,15,15,14,15,15,14,15,15,14,15,15,14,15, x}, %  8
        { x,62,57,60,15,14,15,15,14,15,15,14,15,15,14,15,15, x}, %  9
        { x,58,61,60,14,15,15,14,15,15,14,15,15,14,15,15,14, x}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 14) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,25,51,62,57,12,15, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,25,63,58,49,15,14, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,25,59,50,61,14,12, x}, %  3
        { x,62,57,48,15,14,12,15,14,12,15,14,12,15,14,12,15, x}, %  4
        { x,58,49,60,14,12,15,14,12,15,14,12,15,14,12,15,14, x}, %  5
        { x,50,61,56,12,15,14,12,15,14,12,15,14,12,15,14,12, x}, %  6
        { x,62,57,48,15,14,12,15,14,12,15,14,12,15,14,12,15, x}, %  7
        { x,58,49,60,14,12,15,14,12,15,14,12,15,14,12,15,14, x}, %  8
        { x,50,61,56,12,15,14,12,15,14,12,15,14,12,15,14,12, x}, %  9
        { x,62,57,48,15,14,12,15,14,12,15,14,12,15,14,12,15, x}  % 10
    })),
    {X, I};
to_max_v_1270z(XX, YY, 15) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,11,11,11,11,12,13, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13, x}  % 10
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x,29,59,54,53,14,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x,29,55,54,57,13,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x,29,55,58,53,13,14, x}, %  3
        { x,54,53,56,13,13,14,13,13,14,13,13,14,13,13,14,13, x}, %  4
        { x,54,57,52,13,14,13,13,14,13,13,14,13,13,14,13,13, x}, %  5
        { x,58,53,52,14,13,13,14,13,13,14,13,13,14,13,13,14, x}, %  6
        { x,54,53,56,13,13,14,13,13,14,13,13,14,13,13,14,13, x}, %  7
        { x,54,57,52,13,14,13,13,14,13,13,14,13,13,14,13,13, x}, %  8
        { x,58,53,52,14,13,13,14,13,13,14,13,13,14,13,13,14, x}, %  9
        { x,54,53,56,13,13,14,13,13,14,13,13,14,13,13,14,13, x}  % 10
    })),
    {X, I};
to_max_v_1270z(_, _, _) ->
    {x, x}.

%%====================================================================
%% from_max_v_2210z
%%====================================================================

from_max_v_2210z(X, Y, _)
        when X > 21 orelse Y > 13 ->
    {x, x};
from_max_v_2210z(X, Y, 0) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 10
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 11
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 12
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 2, 0, 0, 2, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 2, 0, 0, 2, 0, 0, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 0, 0, 2, 0, 0, 2, x}, %  3
        { 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, x}, %  4
        { 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, x}, %  5
        { 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, x}, %  6
        { 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, x}, %  7
        { 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, x}, %  8
        { 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, x}, %  9
        { 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, x}, % 10
        { 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, x}, % 11
        { 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, x}, % 12
        { 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 1) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 10
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 11
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 12
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 0, 3, 3, 0, 3, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 0, 3, 3, 0, 3, 3, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 3, 3, 0, 3, 3, 0, x}, %  3
        { x, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, x}, %  4
        { x, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, x}, %  5
        { x, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, x}, %  6
        { x, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, x}, %  7
        { x, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, x}, %  8
        { x, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, x}, %  9
        { x, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, x}, % 10
        { x, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, x}, % 11
        { x, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, x}, % 12
        { x, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, 3, 0, 3, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 2) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 10
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 11
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 12
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 3, 1, 2, 3, 1, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 3, 1, 2, 3, 1, 2, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 1, 2, 3, 1, 2, 3, x}, %  3
        { x, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, x}, %  4
        { x, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, x}, %  5
        { x, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, x}, %  6
        { x, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, x}, %  7
        { x, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, x}, %  8
        { x, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, x}, %  9
        { x, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, x}, % 10
        { x, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, x}, % 11
        { x, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, x}, % 12
        { x, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 3) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 10
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 11
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 12
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 1, 1, 2, 1, 1, 2, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 2, 1, 1, 2, 1, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 1, 1, 2, 1, 1, x}, %  3
        { x, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, x}, %  4
        { x, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, x}, %  5
        { x, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, x}, %  6
        { x, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, x}, %  7
        { x, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, x}, %  8
        { x, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, x}, %  9
        { x, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, x}, % 10
        { x, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, x}, % 11
        { x, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, x}, % 12
        { x, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 4) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 10
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 11
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 12
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 6, 4, 4, 6, 4, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 6, 4, 4, 6, 4, 4, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 4, 4, 6, 4, 4, 6, x}, %  3
        { 1, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, x}, %  4
        { 1, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, x}, %  5
        { 1, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, x}, %  6
        { 1, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, x}, %  7
        { 1, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, x}, %  8
        { 1, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, x}, %  9
        { 1, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, x}, % 10
        { 1, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, x}, % 11
        { 1, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, x}, % 12
        { 1, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, 4, 6, 4, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 5) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 10
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 11
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 12
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 7, 4, 7, 7, 4, 7, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 4, 7, 7, 4, 7, 7, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 7, 7, 4, 7, 7, 4, x}, %  3
        { x, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, x}, %  4
        { x, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, x}, %  5
        { x, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, x}, %  6
        { x, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, x}, %  7
        { x, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, x}, %  8
        { x, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, x}, %  9
        { x, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, x}, % 10
        { x, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, x}, % 11
        { x, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, x}, % 12
        { x, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, 7, 4, 7, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 6) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 10
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 11
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 12
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 6, 7, 5, 6, 7, 5, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 7, 5, 6, 7, 5, 6, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 5, 6, 7, 5, 6, 7, x}, %  3
        { x, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, x}, %  4
        { x, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, x}, %  5
        { x, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, x}, %  6
        { x, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, x}, %  7
        { x, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, x}, %  8
        { x, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, x}, %  9
        { x, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, x}, % 10
        { x, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, x}, % 11
        { x, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, x}, % 12
        { x, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, 6, 7, 5, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 7) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,14,15,16,17,18,19,20, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,14,15,16,17,18,19,20, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,14,15,16,17,18,19,20, x}, %  3
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  4
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  5
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  6
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  7
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  8
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, %  9
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 10
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 11
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}, % 12
        { x, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, 6, 5, 5, 6, 5, 5, 6, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, 5, 5, 6, 5, 5, 6, 5, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, 5, 6, 5, 5, 6, 5, 5, x}, %  3
        { x, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, x}, %  4
        { x, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, x}, %  5
        { x, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, x}, %  6
        { x, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, x}, %  7
        { x, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, x}, %  8
        { x, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, x}, %  9
        { x, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, x}, % 10
        { x, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, x}, % 11
        { x, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, x}, % 12
        { x, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 8) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  3
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  4
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  5
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  6
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  7
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  8
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  9
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 10
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 11
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 12
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 8, 8,10, 8, x, x, 0, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 8,10, 8, 8, x, x, 0, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,10, 8, 8,10, x, x, 0, x}, %  3
        { 2,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, x, x, 0, x}, %  4
        { 2, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8, x, x, 0, x}, %  5
        { 2, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, x, x, 0, x}, %  6
        { 2,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, x, x, 0, x}, %  7
        { 2, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8, x, x, 0, x}, %  8
        { 2, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, x, x, 0, x}, %  9
        { 2,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, x, x, 0, x}, % 10
        { 2, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8, x, x, 0, x}, % 11
        { 2, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, x, x, 0, x}, % 12
        { 2,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, 8,10, 8, x, x, 0, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 9) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 10
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 11
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 12
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,11,11, 8,11, x, x, 1, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,11, 8,11,11, x, x, 1, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 8,11,11, 8, x, x, 1, x}, %  3
        { x, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11, x, x, 1, x}, %  4
        { x,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, x, x, 1, x}, %  5
        { x,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8, x, x, 1, x}, %  6
        { x, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11, x, x, 1, x}, %  7
        { x,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, x, x, 1, x}, %  8
        { x,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8, x, x, 1, x}, %  9
        { x, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11, x, x, 1, x}, % 10
        { x,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, x, x, 1, x}, % 11
        { x,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8, x, x, 1, x}, % 12
        { x, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11,11, 8,11, x, x, 1, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 10) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 10
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 11
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 12
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 9,10,11, 9, x, x, 2, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,10,11, 9,10, x, x, 2, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,11, 9,10,11, x, x, 2, x}, %  3
        { x,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9, x, x, 2, x}, %  4
        { x, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10, x, x, 2, x}, %  5
        { x,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, x, x, 2, x}, %  6
        { x,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9, x, x, 2, x}, %  7
        { x, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10, x, x, 2, x}, %  8
        { x,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, x, x, 2, x}, %  9
        { x,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9, x, x, 2, x}, % 10
        { x, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10, x, x, 2, x}, % 11
        { x,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, x, x, 2, x}, % 12
        { x,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9,10,11, 9, x, x, 2, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 11) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 10
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 11
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 12
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,10, 9, 9,10, x, x, 3, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 9, 9,10, 9, x, x, 3, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 9,10, 9, 9, x, x, 3, x}, %  3
        { x, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x, x, 3, x}, %  4
        { x,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x, x, 3, x}, %  5
        { x, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9, x, x, 3, x}, %  6
        { x, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x, x, 3, x}, %  7
        { x,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x, x, 3, x}, %  8
        { x, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9, x, x, 3, x}, %  9
        { x, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x, x, 3, x}, % 10
        { x,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x, x, 3, x}, % 11
        { x, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9, x, x, 3, x}, % 12
        { x, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x, x, 3, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 12) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  3
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  4
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  5
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  6
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  7
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  8
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  9
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 10
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 11
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 12
        { 0, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,12,12,14,12, x, x, 4, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,12,14,12,12, x, x, 4, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,12,12,14, x, x, 4, x}, %  3
        { 3,14,12,12,14,12,12,14,12,12,14,12,12,14,12,12,14,12, x, x, 4, x}, %  4
        { 3,12,12,14,12,12,14,12,12,14,12,12,14,12,12,14,12,12, x, x, 4, x}, %  5
        { 3,12,14,12,12,14,12,12,14,12,12,14,12,12,14,12,12,14, x, x, 4, x}, %  6
        { 3,14,12,12,14,12,12,14,12,12,14,12,12,14,12,12,14,12, x, x, 4, x}, %  7
        { 3,12,12,14,12,12,14,12,12,14,12,12,14,12,12,14,12,12, x, x, 4, x}, %  8
        { 3,12,14,12,12,14,12,12,14,12,12,14,12,12,14,12,12,14, x, x, 4, x}, %  9
        { 3,14,12,12,14,12,12,14,12,12,14,12,12,14,12,12,14,12, x, x, 4, x}, % 10
        { 3,12,12,14,12,12,14,12,12,14,12,12,14,12,12,14,12,12, x, x, 4, x}, % 11
        { 3,12,14,12,12,14,12,12,14,12,12,14,12,12,14,12,12,14, x, x, 4, x}, % 12
        { 3,14,12,12,14,12,12,14,12,12,14,12,12,14,12,12,14,12, x, x, 4, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 13) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 10
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 11
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 12
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,15,15,12,15, x, x, 5, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,15,12,15,15, x, x, 5, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,12,15,15,12, x, x, 5, x}, %  3
        { x,12,15,15,12,15,15,12,15,15,12,15,15,12,15,15,12,15, x, x, 5, x}, %  4
        { x,15,15,12,15,15,12,15,15,12,15,15,12,15,15,12,15,15, x, x, 5, x}, %  5
        { x,15,12,15,15,12,15,15,12,15,15,12,15,15,12,15,15,12, x, x, 5, x}, %  6
        { x,12,15,15,12,15,15,12,15,15,12,15,15,12,15,15,12,15, x, x, 5, x}, %  7
        { x,15,15,12,15,15,12,15,15,12,15,15,12,15,15,12,15,15, x, x, 5, x}, %  8
        { x,15,12,15,15,12,15,15,12,15,15,12,15,15,12,15,15,12, x, x, 5, x}, %  9
        { x,12,15,15,12,15,15,12,15,15,12,15,15,12,15,15,12,15, x, x, 5, x}, % 10
        { x,15,15,12,15,15,12,15,15,12,15,15,12,15,15,12,15,15, x, x, 5, x}, % 11
        { x,15,12,15,15,12,15,15,12,15,15,12,15,15,12,15,15,12, x, x, 5, x}, % 12
        { x,12,15,15,12,15,15,12,15,15,12,15,15,12,15,15,12,15, x, x, 5, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 14) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,17,18,19,20, x, x,21, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 10
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 11
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 12
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13,14,15,13, x, x, 6, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,13,14, x, x, 6, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,15,13,14,15, x, x, 6, x}, %  3
        { x,15,13,14,15,13,14,15,13,14,15,13,14,15,13,14,15,13, x, x, 6, x}, %  4
        { x,13,14,15,13,14,15,13,14,15,13,14,15,13,14,15,13,14, x, x, 6, x}, %  5
        { x,14,15,13,14,15,13,14,15,13,14,15,13,14,15,13,14,15, x, x, 6, x}, %  6
        { x,15,13,14,15,13,14,15,13,14,15,13,14,15,13,14,15,13, x, x, 6, x}, %  7
        { x,13,14,15,13,14,15,13,14,15,13,14,15,13,14,15,13,14, x, x, 6, x}, %  8
        { x,14,15,13,14,15,13,14,15,13,14,15,13,14,15,13,14,15, x, x, 6, x}, %  9
        { x,15,13,14,15,13,14,15,13,14,15,13,14,15,13,14,15,13, x, x, 6, x}, % 10
        { x,13,14,15,13,14,15,13,14,15,13,14,15,13,14,15,13,14, x, x, 6, x}, % 11
        { x,14,15,13,14,15,13,14,15,13,14,15,13,14,15,13,14,15, x, x, 6, x}, % 12
        { x,15,13,14,15,13,14,15,13,14,15,13,14,15,13,14,15,13, x, x, 6, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 15) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,17,18,19,20, x, x,21, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,17,18,19,20, x, x,21, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,17,18,19,20, x, x,21, x}, %  3
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  4
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  5
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  6
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  7
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  8
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, %  9
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 10
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 11
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}, % 12
        { x, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20, x, x,21, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11,14,13,13,14, x, x, 7, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11,13,13,14,13, x, x, 7, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11,13,14,13,13, x, x, 7, x}, %  3
        { x,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14, x, x, 7, x}, %  4
        { x,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13, x, x, 7, x}, %  5
        { x,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13, x, x, 7, x}, %  6
        { x,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14, x, x, 7, x}, %  7
        { x,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13, x, x, 7, x}, %  8
        { x,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13, x, x, 7, x}, %  9
        { x,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14, x, x, 7, x}, % 10
        { x,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13, x, x, 7, x}, % 11
        { x,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13, x, x, 7, x}, % 12
        { x,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14, x, x, 7, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 16) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 4, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 20) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 5, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 23) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 24) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 6, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 27) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 28) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 0, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 7, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 31) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 32) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x, x, x}, %  3
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 33) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x, x, x}, %  3
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 34) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 35) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 36) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x, x, x}, %  3
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 37) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x, x, x}, %  3
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 38) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 8, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 39) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 8, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 40) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x, x, x}, %  3
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 41) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x, x, x}, %  3
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 42) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {11, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 43) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,11, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 44) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x, x, x}, %  3
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 45) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x, x, x}, %  3
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 46) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {10, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 9, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 47) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,10, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 9, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 48) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x, x, x}, %  3
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 49) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 50) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 51) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 52) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 53) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x, x, x}, %  3
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 54) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {12, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 55) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,12, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 56) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  3
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 57) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 58) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {15, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 59) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 60) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15, x, x, x, x, x, x, x, x}, %  3
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 3, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 61) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  3
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 2, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  3
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 62) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { 1, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  3
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        {14, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        {13, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(X, Y, 63) ->
    XX = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,16, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    MM = element(X + 1, element(Y + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,14, x, x, x, x, x, x, x, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13, x, x, x, x, x, x, x, x}, %  3
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  4
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  5
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  6
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  7
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  8
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  9
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 10
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 11
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, % 12
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}  % 13
    })),
    {XX, MM};
from_max_v_2210z(_, _, _) ->
    {x, x}.

%%====================================================================
%% to_max_v_2210z
%%====================================================================

to_max_v_2210z(XX, YY, _)
        when XX > 21 orelse YY > 13 ->
    {x, x};
to_max_v_2210z(XX, YY, 0) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 10
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 11
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 12
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 0, 1, 0, 0, 1, 0, 8}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 1, 0, 0, 1, 0, 0, 8}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 0, 0, 1, 0, 0, 1, 8}, %  3
        { 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 8}, %  4
        { 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 8}, %  5
        { 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 8}, %  6
        { 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 8}, %  7
        { 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 8}, %  8
        { 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 8}, %  9
        { 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 8}, % 10
        { 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 8}, % 11
        { 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 8}, % 12
        { 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 8}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 1) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 10
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 11
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 12
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 3, 3, 2, 3, 3, 2, 9}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 3, 2, 3, 3, 2, 3, 9}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 2, 3, 3, 2, 3, 3, 9}, %  3
        { 4, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 9}, %  4
        { 4, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 9}, %  5
        { 4, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 9}, %  6
        { 4, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 9}, %  7
        { 4, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 9}, %  8
        { 4, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 9}, %  9
        { 4, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 9}, % 10
        { 4, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 9}, % 11
        { 4, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 9}, % 12
        { 4, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 3, 3, 2, 9}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 2) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 10
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 11
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 12
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 3, 2, 0, 3, 2, 0, 3,10}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 0, 3, 2, 0, 3, 2,10}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 0, 3, 2, 0, 3, 2, 0,10}, %  3
        { 8, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3,10}, %  4
        { 8, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2,10}, %  5
        { 8, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0,10}, %  6
        { 8, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3,10}, %  7
        { 8, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2,10}, %  8
        { 8, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0,10}, %  9
        { 8, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3,10}, % 10
        { 8, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2,10}, % 11
        { 8, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0,10}, % 12
        { 8, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3, 2, 0, 3,10}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 3) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 10
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 11
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 12
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 1, 2, 1, 1, 2, 1,11}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 1, 2, 1, 1, 2, 1, 1,11}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 2, 1, 1, 2, 1, 1, 2,11}, %  3
        {12, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1,11}, %  4
        {12, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1,11}, %  5
        {12, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2,11}, %  6
        {12, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1,11}, %  7
        {12, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1,11}, %  8
        {12, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2,11}, %  9
        {12, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1,11}, % 10
        {12, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1,11}, % 11
        {12, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2,11}, % 12
        {12, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1,11}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 4) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 10
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 11
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 12
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 4, 5, 4, 4, 5, 4,12}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 5, 4, 4, 5, 4, 4,12}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 4, 4, 5, 4, 4, 5,12}, %  3
        {16, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4,12}, %  4
        {16, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4,12}, %  5
        {16, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5,12}, %  6
        {16, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4,12}, %  7
        {16, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4,12}, %  8
        {16, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5,12}, %  9
        {16, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4,12}, % 10
        {16, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4,12}, % 11
        {16, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5,12}, % 12
        {16, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4, 4, 5, 4,12}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 5) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 10
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 11
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 12
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 7, 7, 6, 7, 7, 6,13}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 7, 6, 7, 7, 6, 7,13}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 6, 7, 7, 6, 7, 7,13}, %  3
        {20, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6,13}, %  4
        {20, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7,13}, %  5
        {20, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7,13}, %  6
        {20, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6,13}, %  7
        {20, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7,13}, %  8
        {20, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7,13}, %  9
        {20, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6,13}, % 10
        {20, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7,13}, % 11
        {20, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7,13}, % 12
        {20, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6,13}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 6) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 10
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 11
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 12
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 7, 6, 4, 7, 6, 4, 7,14}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 4, 7, 6, 4, 7, 6,14}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 4, 7, 6, 4, 7, 6, 4,14}, %  3
        {24, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7,14}, %  4
        {24, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6,14}, %  5
        {24, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4,14}, %  6
        {24, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7,14}, %  7
        {24, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6,14}, %  8
        {24, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4,14}, %  9
        {24, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7,14}, % 10
        {24, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6,14}, % 11
        {24, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4,14}, % 12
        {24, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7, 6, 4, 7,14}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 7) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,14,15,16,17,18,19,20,20}, %  3
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  4
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  5
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  6
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  7
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  8
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, %  9
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 10
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 11
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}, % 12
        { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,20}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 5, 6, 5, 5, 6, 5,15}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 5, 6, 5, 5, 6, 5, 5,15}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, 6, 5, 5, 6, 5, 5, 6,15}, %  3
        {28, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5,15}, %  4
        {28, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5,15}, %  5
        {28, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6,15}, %  6
        {28, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5,15}, %  7
        {28, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5,15}, %  8
        {28, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6,15}, %  9
        {28, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5,15}, % 10
        {28, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5,15}, % 11
        {28, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6,15}, % 12
        {28, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5, 5, 6, 5,15}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 8) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,14,15,16,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,14,15,16,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,14,15,16,17, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 10
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 11
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 12
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,33,32,39, 8, 8, 9, 8, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,33,36,35, 8, 9, 8, 8, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,37,32,35, 9, 8, 8, 9, x}, %  3
        { x,38,33,32, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, x}, %  4
        { x,34,33,36, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, x}, %  5
        { x,34,37,32, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, x}, %  6
        { x,38,33,32, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, x}, %  7
        { x,34,33,36, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, x}, %  8
        { x,34,37,32, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, x}, %  9
        { x,38,33,32, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, x}, % 10
        { x,34,33,36, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, x}, % 11
        { x,34,37,32, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, x}, % 12
        { x,38,33,32, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, 8, 9, 8, x}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 9) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 10
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 11
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 12
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 7,41,44,47,10,11,11,10, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 7,45,44,43,11,11,10,11, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, 7,45,40,47,11,10,11,11, x}, %  3
        { x,46,41,44,11,10,11,11,10,11,11,10,11,11,10,11,11,10,11,11,10, x}, %  4
        { x,42,45,44,10,11,11,10,11,11,10,11,11,10,11,11,10,11,11,10,11, x}, %  5
        { x,46,45,40,11,11,10,11,11,10,11,11,10,11,11,10,11,11,10,11,11, x}, %  6
        { x,46,41,44,11,10,11,11,10,11,11,10,11,11,10,11,11,10,11,11,10, x}, %  7
        { x,42,45,44,10,11,11,10,11,11,10,11,11,10,11,11,10,11,11,10,11, x}, %  8
        { x,46,45,40,11,11,10,11,11,10,11,11,10,11,11,10,11,11,10,11,11, x}, %  9
        { x,46,41,44,11,10,11,11,10,11,11,10,11,11,10,11,11,10,11,11,10, x}, % 10
        { x,42,45,44,10,11,11,10,11,11,10,11,11,10,11,11,10,11,11,10,11, x}, % 11
        { x,46,45,40,11,11,10,11,11,10,11,11,10,11,11,10,11,11,10,11,11, x}, % 12
        { x,46,41,44,11,10,11,11,10,11,11,10,11,11,10,11,11,10,11,11,10, x}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 10) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,14,15,16,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,14,15,16,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,14,15,16,17, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 10
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 11
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 12
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,45,40,35,11,10, 8,11, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,41,32,47,10, 8,11,10, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,33,44,43, 8,11,10, 8, x}, %  3
        { x,34,45,40, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11, x}, %  4
        { x,46,41,32,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, x}, %  5
        { x,42,33,44,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8, x}, %  6
        { x,34,45,40, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11, x}, %  7
        { x,46,41,32,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, x}, %  8
        { x,42,33,44,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8, x}, %  9
        { x,34,45,40, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11, x}, % 10
        { x,46,41,32,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, x}, % 11
        { x,42,33,44,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8, x}, % 12
        { x,34,45,40, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11,10, 8,11, x}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 11) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 10
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 11
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 12
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15,37,36,43, 9, 9,10, 9, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15,37,40,39, 9,10, 9, 9, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,15,41,36,39,10, 9, 9,10, x}, %  3
        { x,42,37,36,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x}, %  4
        { x,38,37,40, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9, x}, %  5
        { x,38,41,36, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x}, %  6
        { x,42,37,36,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x}, %  7
        { x,38,37,40, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9, x}, %  8
        { x,38,41,36, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x}, %  9
        { x,42,37,36,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x}, % 10
        { x,38,37,40, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9, x}, % 11
        { x,38,41,36, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, x}, % 12
        { x,42,37,36,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, 9,10, 9, x}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 12) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,14,15,16,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,14,15,16,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,14,15,16,17, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 10
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 11
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 12
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,49,48,55,12,12,13,12, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,49,52,51,12,13,12,12, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x,53,48,51,13,12,12,13, x}, %  3
        { x,54,49,48,13,12,12,13,12,12,13,12,12,13,12,12,13,12,12,13,12, x}, %  4
        { x,50,49,52,12,12,13,12,12,13,12,12,13,12,12,13,12,12,13,12,12, x}, %  5
        { x,50,53,48,12,13,12,12,13,12,12,13,12,12,13,12,12,13,12,12,13, x}, %  6
        { x,54,49,48,13,12,12,13,12,12,13,12,12,13,12,12,13,12,12,13,12, x}, %  7
        { x,50,49,52,12,12,13,12,12,13,12,12,13,12,12,13,12,12,13,12,12, x}, %  8
        { x,50,53,48,12,13,12,12,13,12,12,13,12,12,13,12,12,13,12,12,13, x}, %  9
        { x,54,49,48,13,12,12,13,12,12,13,12,12,13,12,12,13,12,12,13,12, x}, % 10
        { x,50,49,52,12,12,13,12,12,13,12,12,13,12,12,13,12,12,13,12,12, x}, % 11
        { x,50,53,48,12,13,12,12,13,12,12,13,12,12,13,12,12,13,12,12,13, x}, % 12
        { x,54,49,48,13,12,12,13,12,12,13,12,12,13,12,12,13,12,12,13,12, x}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 13) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 10
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 11
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 12
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,23,57,60,63,14,15,15,14, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,23,61,60,59,15,15,14,15, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,23,61,56,63,15,14,15,15, x}, %  3
        { x,62,57,60,15,14,15,15,14,15,15,14,15,15,14,15,15,14,15,15,14, x}, %  4
        { x,58,61,60,14,15,15,14,15,15,14,15,15,14,15,15,14,15,15,14,15, x}, %  5
        { x,62,61,56,15,15,14,15,15,14,15,15,14,15,15,14,15,15,14,15,15, x}, %  6
        { x,62,57,60,15,14,15,15,14,15,15,14,15,15,14,15,15,14,15,15,14, x}, %  7
        { x,58,61,60,14,15,15,14,15,15,14,15,15,14,15,15,14,15,15,14,15, x}, %  8
        { x,62,61,56,15,15,14,15,15,14,15,15,14,15,15,14,15,15,14,15,15, x}, %  9
        { x,62,57,60,15,14,15,15,14,15,15,14,15,15,14,15,15,14,15,15,14, x}, % 10
        { x,58,61,60,14,15,15,14,15,15,14,15,15,14,15,15,14,15,15,14,15, x}, % 11
        { x,62,61,56,15,15,14,15,15,14,15,15,14,15,15,14,15,15,14,15,15, x}, % 12
        { x,62,57,60,15,14,15,15,14,15,15,14,15,15,14,15,15,14,15,15,14, x}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 14) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 10
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 11
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 12
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,27,61,56,51,15,14,12,15, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,27,57,48,63,14,12,15,14, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,27,49,60,59,12,15,14,12, x}, %  3
        { x,50,61,56,12,15,14,12,15,14,12,15,14,12,15,14,12,15,14,12,15, x}, %  4
        { x,62,57,48,15,14,12,15,14,12,15,14,12,15,14,12,15,14,12,15,14, x}, %  5
        { x,58,49,60,14,12,15,14,12,15,14,12,15,14,12,15,14,12,15,14,12, x}, %  6
        { x,50,61,56,12,15,14,12,15,14,12,15,14,12,15,14,12,15,14,12,15, x}, %  7
        { x,62,57,48,15,14,12,15,14,12,15,14,12,15,14,12,15,14,12,15,14, x}, %  8
        { x,58,49,60,14,12,15,14,12,15,14,12,15,14,12,15,14,12,15,14,12, x}, %  9
        { x,50,61,56,12,15,14,12,15,14,12,15,14,12,15,14,12,15,14,12,15, x}, % 10
        { x,62,57,48,15,14,12,15,14,12,15,14,12,15,14,12,15,14,12,15,14, x}, % 11
        { x,58,49,60,14,12,15,14,12,15,14,12,15,14,12,15,14,12,15,14,12, x}, % 12
        { x,50,61,56,12,15,14,12,15,14,12,15,14,12,15,14,12,15,14,12,15, x}  % 13
    })),
    {X, I};
to_max_v_2210z(XX, YY, 15) ->
    X = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,13,13,13,13,14,15,16,17, x}, %  3
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  4
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  5
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  6
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  7
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  8
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, %  9
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 10
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 11
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}, % 12
        { x, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17, x}  % 13
    })),
    I = element(XX + 1, element(YY + 1, {
        % 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
        { x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x}, %  0
        { x, x, x, x, x, x, x, x, x, x, x, x, x,31,53,52,59,13,13,14,13, x}, %  1
        { x, x, x, x, x, x, x, x, x, x, x, x, x,31,53,56,55,13,14,13,13, x}, %  2
        { x, x, x, x, x, x, x, x, x, x, x, x, x,31,57,52,55,14,13,13,14, x}, %  3
        { x,58,53,52,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13, x}, %  4
        { x,54,53,56,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13, x}, %  5
        { x,54,57,52,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14, x}, %  6
        { x,58,53,52,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13, x}, %  7
        { x,54,53,56,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13, x}, %  8
        { x,54,57,52,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14, x}, %  9
        { x,58,53,52,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13, x}, % 10
        { x,54,53,56,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13, x}, % 11
        { x,54,57,52,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14, x}, % 12
        { x,58,53,52,14,13,13,14,13,13,14,13,13,14,13,13,14,13,13,14,13, x}  % 13
    })),
    {X, I};
to_max_v_2210z(_, _, _) ->
    {x, x}.

