-module(expect).

-export([control/4]).
-export([fuse/3]).
-export([fuse/4]).
-export([lut/4]).

-include("decompile.hrl").

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
            throw(control_not_as_expected);

        Other ->
            io:format("Expect:~n", []),
            io:format("  ~w -> control #~b -> ~s~n", [Signal, Control, Port]),
            io:format("Got:~n", []),
            io:format("  ~p~n", [Other]),
            throw(control_not_found)
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
            case lists:keyfind(Fuse, 2, Fuses) of
                {_, _} ->
                    ok;

                _ ->
                    io:format("Pattern:~n  ~w~n", [Pattern]),
                    io:format("Expecting:~n  ~w~n", [Fuse]),
                    io:format("Candidates:~n  ~p~n", [Fuses]),
                    throw(fuse_not_found)
            end
    end.

%%--------------------------------------------------------------------

fuse(Matrix, Pattern, Fuse1, Fuse2) ->
    case lists:usort(Pattern) of
        [_] ->
            ok;

        _ ->
            Fuses = matrix:pattern_is(Matrix, Pattern),
            Found1 = lists:keyfind(Fuse1, 2, Fuses),
            Found2 = lists:keyfind(Fuse2, 2, Fuses),
            case {Found1, Found2} of
                {{_, _}, {_, _}} ->
                    ok;

                _ ->
                    io:format("Pattern:~n  ~w~n", [Pattern]),
                    io:format("Expecting:~n  ~w~n  ~w~n", [Fuse1, Fuse2]),
                    io:format("Candidates:~n  ~p~n", [Fuses]),
                    throw(fuses_not_found)
            end
    end.

%%====================================================================
%% lut
%%====================================================================

lut(Name, At, LC, Expect) ->
    lut(Name, At, LC, Expect, lut_value(Expect)).

%%--------------------------------------------------------------------

lut(_, _, #lc{lut = LUT}, _, LUT) ->
    ok;
lut(Name, At, #lc{lut = GotLUT}, Expect, ExpectLUT) ->
    io:format(" --> ~p ~p~n", [Name, At]),
    io:format("EXPECT (~4.16.0B) ~p~n", [ExpectLUT, Expect]),
    io:format("   GOT (~4.16.0B)~n", [GotLUT]),
    throw(lut_not_as_expected).

%%--------------------------------------------------------------------

lut_value(a_xor_c) -> ?LUT_A_XOR_C;
lut_value(b_xor_c) -> ?LUT_B_XOR_C;
lut_value(c_xor_d) -> ?LUT_C_XOR_D.

