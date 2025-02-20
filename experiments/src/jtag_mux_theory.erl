-module(jtag_mux_theory).

-export([run/0]).

% Applying the global_mux_theory approach to JTAG tdo selection mux.

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
    lists:foldl(fun (Fuse, JTAGs) ->
       fuse(Density, Fuse, JTAGs)
    end, #{}, Fuses).

%%--------------------------------------------------------------------

fuse(Density, Fuse, JTAGs) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {{jtag, _, _}, JTAG, Key = from3, Value}} ->
            fuse_mux(JTAG, Key, Value, JTAGs);

        {ok, {{jtag, _, _}, JTAG, Key = from4, Value}} ->
            fuse_mux(JTAG, Key, Value, JTAGs);

        {ok, {{jtag, _, _}, JTAG, Key = from6, Value}} ->
            fuse_mux(JTAG, Key, Value, JTAGs);

        _ ->
            JTAGs
    end.

%%--------------------------------------------------------------------

fuse_mux(JTAG, Key, Value, JTAGs) ->
    case JTAGs of
        #{JTAG := #{Key := Existing}} when Existing =:= Value ->
            JTAGs;

        #{JTAG := #{Key := Existing}} ->
            throw({JTAG, Key, Value, existing, Existing});

        #{JTAG := Muxes} ->
            JTAGs#{JTAG => Muxes#{Key => Value}};

        _ ->
            JTAGs#{JTAG => #{Key => Value}}
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
        {ok, X, Y, JTAG, Route} ->
            io:format("~p <- ~p~n", [JTAG, Route]),
            case theory(X, Y, JTAG, Model) of
                Theory when Theory =:= Route ->
                    ok;

                Theory ->
                    throw({JTAG, Route, theory, Theory})
            end;

        false ->
            ok
    end.

%%--------------------------------------------------------------------

signal_route(#{jtag := {jtag, X, Y}, port := JTAG, route := [Route | _]}) ->
    {ok, X, Y, JTAG, Route};
signal_route(_) ->
    false.

%%====================================================================
%% theory
%%====================================================================

theory(X, Y, JTAG, Model) ->
    case Model of
        #{JTAG := #{from4 := Mux4, from3 := Mux3}} ->
            theory_large(X, Y, Mux4, Mux3);

        #{JTAG := #{from6 := Mux6, from3 := Mux3}} ->
            theory_small(X, Y, Mux6, Mux3);

        #{JTAG := Muxes} ->
            Muxes;

        _ ->
            JTAG
    end.

%%--------------------------------------------------------------------

theory_small(X, Y, Mux6, Mux3) ->
    {interconnect, N} = global_mux_map:to_small_interconnect(Mux6, Mux3),
    {local_interconnect, X, Y, 0, N}.

%%--------------------------------------------------------------------

theory_large(X, Y, Mux6, Mux3) ->
    {interconnect, N} = global_mux_map:to_large_interconnect(Mux6, Mux3),
    {local_interconnect, X, Y, 0, N}.

