-module(ufm_mux_theory).

-export([run/0]).

% Applying the global_mux_theory approach to UFM tdo selection mux.

%%====================================================================
%% run
%%====================================================================

run() ->
    experiments(experiment_cache:iterate()).

%%--------------------------------------------------------------------

experiments(false) ->
    ok;
experiments({Key, Experiment = {Device, _, _}, Iterator}) ->
    Density = device:density(Device),
    case experiment(Density, Experiment) of
        ok ->
            experiments(experiment_cache:iterate(Iterator));

        {error, Error, Fuses, Signals} ->
            contradiction(Density, Key, Fuses, Signals, Error)
    end.

%%--------------------------------------------------------------------

experiment(Density, Experiment) ->
    {ok, Fuses} = experiment:fuses(Experiment),
    {ok, #{signals := Signals}} = experiment:rcf(Experiment),
    try
        Model = fuses(Density, Fuses),
        signals(Signals, Model),
        ok
    catch
        throw:Throw ->
            {error, Throw, Fuses, Signals}
    end.

%%--------------------------------------------------------------------

contradiction(Density, Key, Fuses0, Signals, Error) ->
    io:format("~n => ~s~n", [Key]),
    io:format("~nFUSES:~n", []),
    Fuses = fuses:subtract(Fuses0, density:minimal_fuses(Density)),
    lists:foreach(fun (Fuse) -> contradiction(Density, Fuse) end, Fuses),
    io:format("~nSIGNALS:~n  ~p~n", [Signals]),
    io:format("~nDENSITY:~n  ~p~n", [Density]),
    io:format("~nERROR:~n  ~p~n", [Error]),
    ok.

%%--------------------------------------------------------------------

contradiction(Density, Fuse) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, Name} ->
            io:format("  ~w~n", [Name]);

        {error, Error} ->
            io:format("  ~w~n", [Error])
    end.

%%====================================================================
%% fuses
%%====================================================================

fuses(Density, Fuses) ->
    lists:foldl(fun (Fuse, UFMs) ->
       fuse(Density, Fuse, UFMs)
    end, #{}, Fuses).

%%--------------------------------------------------------------------

fuse(Density, Fuse, UFMs) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {{ufm, _, _}, UFM, Key = from3, Value}} when is_atom(UFM) ->
            fuse_mux(UFM, Key, Value, UFMs);

        {ok, {{ufm, _, _}, UFM, Key = from4, Value}} when is_atom(UFM) ->
            fuse_mux(UFM, Key, Value, UFMs);

        {ok, {{ufm, _, _}, UFM, Key = from6, Value}} when is_atom(UFM) ->
            fuse_mux(UFM, Key, Value, UFMs);

        _ ->
            UFMs
    end.

%%--------------------------------------------------------------------

fuse_mux(UFM, Key, Value, UFMs) ->
    case UFMs of
        #{UFM := #{Key := Existing}} when Existing =:= Value ->
            UFMs;

        #{UFM := #{Key := Existing}} ->
            throw({UFM, Key, Value, existing, Existing});

        #{UFM := Muxes} ->
            UFMs#{UFM => Muxes#{Key => Value}};

        _ ->
            UFMs#{UFM => #{Key => Value}}
    end.

%%====================================================================
%% signals
%%====================================================================

signals(Signals, Model) ->
    maps:foreach(fun (_, Signal) -> signal(Signal, Model) end, Signals).

%%--------------------------------------------------------------------

signal(#{dests := Dests}, Model) ->
    lists:foreach(fun (Dest) -> signal_dest(Dest, Model) end, Dests).

%%--------------------------------------------------------------------

signal_dest(Dest, Model) ->
    case signal_route(Dest) of
        {ok, X, Y, UFM, Route} ->
            io:format("~p <- ~p~n", [UFM, Route]),
            case theory(X, Y, UFM, Model) of
                Theory when Theory =:= Route ->
                    ok;

                Theory ->
                    throw({UFM, Route, theory, Theory})
            end;

        false ->
            ok
    end.

%%--------------------------------------------------------------------

signal_route(#{ufm := {ufm, X, Y}, port := UFM, route := [Route | _]}) ->
    {ok, X, Y, UFM, Route};
signal_route(_) ->
    false.

%%====================================================================
%% theory
%%====================================================================

theory(X, Y, UFM, Model) ->
    case Model of
        #{UFM := #{from4 := Mux4, from3 := Mux3}} ->
            theory_large(X, Y, Mux4, Mux3);

        #{UFM := #{from6 := Mux6, from3 := Mux3}} ->
            theory_small(X, Y, Mux6, Mux3);

        #{UFM := Muxes} ->
            Muxes;

        _ ->
            UFM
    end.

%%--------------------------------------------------------------------

theory_small(X, Y, Mux6, Mux3) ->
    {interconnect, N} = global_mux_map:to_small_interconnect(Mux6, Mux3),
    {local_interconnect, X, Y, 0, N}.

%%--------------------------------------------------------------------

theory_large(X, Y, Mux6, Mux3) ->
    {interconnect, N} = global_mux_map:to_large_interconnect(Mux6, Mux3),
    {local_interconnect, X, Y, 0, N}.

