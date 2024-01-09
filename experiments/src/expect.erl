-module(expect).

-export([control/4]).
-export([fuse/3]).
-export([fuse/4]).

%%====================================================================
%% control
%%====================================================================

control({_, _, #{signals := Signals}}, Signal, Control, Port) ->
    #{Signal := #{dests := [Dest]}} = Signals,
    #{port := Port, route := Route} = Dest,
    case Route of
        [{lab_control_mux, _, _, 0, Control} | _] ->
            ok;

        [{lab_control_mux, _, _, 0, Other} | _] ->
            io:format("Expect:~n", []),
            io:format("  ~w -> control #~b -> ~s~n", [Signal, Control, Port]),
            io:format("Got:~n", []),
            io:format("  ~w -> control #~b -> ~s~n", [Signal, Other, Port]),
            throw(stop);

        Other ->
            io:format("Expect:~n", []),
            io:format("  ~w -> control #~b -> ~s~n", [Signal, Control, Port]),
            io:format("Got:~n", []),
            io:format("  ~p~n", [Other]),
            throw(stop)
    end.

%%====================================================================
%% fuse
%%====================================================================

fuse(Matrix, Pattern, Fuse) ->
    case lists:usort(Pattern) of
        [_] ->
            ok;

        _ ->
            Fuses = matrix:pattern_is(Matrix, Pattern),
            case Fuses of
                [{_, Fuse}] ->
                    ok;

                _ ->
                    case lists:keyfind(Fuse, 2, Fuses) of
                        {_, _} ->
                            ok;

                        false ->
                            io:format("Expecting:~n  ~w~n", [Fuse]),
                            io:format("Candidates:~n  ~p~n", [Fuses]),
                            throw(stop)
                    end
            end
    end.

%%--------------------------------------------------------------------

fuse(Matrix, Pattern, Fuse1, Fuse2) ->
    case lists:usort(Pattern) of
        [_] ->
            ok;

        _ ->
            Fuses = matrix:pattern_is(Matrix, Pattern),
            case Fuses of
                [{_, Fuse1}, {_, Fuse2}] ->
                    ok;

                [{_, Fuse2}, {_, Fuse1}] ->
                    ok;

                _ ->
                    io:format("Expecting:~n  ~w~n  ~w~n", [Fuse1, Fuse2]),
                    io:format("Candidates:~n  ~p~n", [Fuses]),
                    throw(stop)
            end
    end.
