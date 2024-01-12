-module(global_mux_theory).

-export([run/0]).

% This is a theory of how GLobal interconnects are muxed into the
% Global clock lines.
%
% The theory is based on the data produced by `global_mux_playground`.
%
% ASIDE: Muxes seen so far in the MAX V architecture seem to be all
%   one-hot (or one-cold) selecting multiplexers (rather than
%   binary encoded).
%
% Each Global line has a two dimentional mux.
%
% 5M240Z has the two coordinates as:
%  * a 6-to-1 mux `output6`, and
%  * a 3-to-1 mux `output3`.
% Combined the form either a 18-to-1 mux.
%
% Other densities have the two coordinates as:
%  * a 4-to-1 mux `output4`, and
%  * a 3-to-1 mux `output3`.
% Combined the form either a 12-to-1 mux.
%
%  Checking
% ==========
%
% This theory is checked against all cached experiments
% as a non-exhaustive proof.
%
% It scans the fuses in each experiment looking
% for output6, output4 & output3 one-cold muxes whilst:
%
% * confirming that they are actually _one-cold_, and
% * building up a local_interconnect model.
%
% Then match the model up with the RCF file.

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
    lists:foldl(fun (Fuse, GLobals) -> fuse(Density, Fuse, GLobals) end, #{}, Fuses).

%%--------------------------------------------------------------------

fuse(Density, Fuse, GLobals) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {GLobal, Key = from3, Value}} ->
            fuse_mux(GLobal, Key, Value, GLobals);

        {ok, {GLobal, Key = from4, Value}} ->
            fuse_mux(GLobal, Key, Value, GLobals);

        {ok, {GLobal, Key = from6, Value}} ->
            fuse_mux(GLobal, Key, Value, GLobals);

        _ ->
            GLobals
    end.

%%--------------------------------------------------------------------

fuse_mux(GLobal, Key, Value, GLobals) ->
    case GLobals of
        #{GLobal := #{Key := Existing}} when Existing =:= Value ->
            GLobals;

        #{GLobal := #{Key := Existing}} ->
            throw({GLobal, Key, Value, existing, Existing});

        #{GLobal := Muxes} ->
            GLobals#{GLobal => Muxes#{Key => Value}};

        _ ->
            GLobals#{GLobal => #{Key => Value}}
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

signal_dest(#{route := Route0}, Model) ->
    case signal_route(Route0) of
        {ok, X, Y, Global, Route} ->
            case theory(X, Y, Global, Model) of
                Theory when Theory =:= Route ->
                    ok;

                Theory ->
                    throw({Global, Route, theory, Theory})
            end;

        false ->
            ok
    end.

%%--------------------------------------------------------------------

signal_route([]) ->
    false;
signal_route([{global_clk_mux, X, Y, 0, N}, Interconnect | _]) ->
    {ok, X, Y, {global, N}, Interconnect};
signal_route([_ | Route]) ->
    signal_route(Route).

%%====================================================================
%% theory
%%====================================================================

theory(X, Y, Global, Model) ->
    case Model of
        #{Global := #{from4 := Mux4, from3 := Mux3}} ->
            theory_large(X, Y, Mux4, Mux3);

        #{Global := #{from6 := Mux6, from3 := Mux3}} ->
            theory_small(X, Y, Mux6, Mux3);

        #{Global := Muxes} ->
            Muxes;

        _ ->
            Global
    end.

%%--------------------------------------------------------------------

theory_small(X, Y, Mux6, Mux3) ->
    {interconnect, N} = global_mux_map:to_small_interconnect(Mux6, Mux3),
    {local_interconnect, X, Y, 0, N}.

%%--------------------------------------------------------------------

theory_large(X, Y, Mux6, Mux3) ->
    {interconnect, N} = global_mux_map:to_large_interconnect(Mux6, Mux3),
    {local_interconnect, X, Y, 0, N}.

