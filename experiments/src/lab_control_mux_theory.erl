-module(lab_control_mux_theory).

-export([run/0]).

% This is a theory of how interconnects are muxed into the six control
% lines.
%
% The theory is based on the data produced by `lab_control_mux_playground`.
%
% The mapping turns out to be the same as data_c & data_d from lc_data_mux_map.
%
%  Checking
% ==========
%
% This theory is checked against all cached experiments
% as a non-exhaustive proof.
%
% It scans the fuses in each experiment looking
% for data_#6 & data_#3 one-hot muxes whilst:
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
    io:format("~nDENSITY: ~s~n", [Density]),
    io:format("~nFUSES:~n", []),
    Fuses = fuses:subtract(Fuses0, density:minimal_fuses(Density)),
    lists:foreach(fun (Fuse) -> contradiction(Density, Fuse) end, Fuses),
    io:format("~nSIGNALS:~n  ~p~n", [Signals]),
    io:format("~nERROR:~n  ~p~n", [Error]),
    ok.

%%--------------------------------------------------------------------

contradiction(Density, Fuse) ->
    io:format("  ~w~n", [fuse_map:to_location(Fuse, Density)]).

%%====================================================================
%% fuses
%%====================================================================

fuses(Density, Fuses) ->
    lists:foldl(fun (Fuse, Controls) ->
        fuse(Density, Fuse, Controls)
    end, #{}, Fuses).

%%--------------------------------------------------------------------

fuse(Density, Fuse, Controls) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {{lab, X, Y}, {control, N}, Key = from3, Value}} ->
            Control = {lab, X, Y, control, N},
            fuse_mux(Control, Key, Value, Controls);

        {ok, {{lab, X, Y}, {control, N}, Key = from6, Value}} ->
            Control = {lab, X, Y, control, N},
            fuse_mux(Control, Key, Value, Controls);

        _ ->
            Controls
    end.

%%--------------------------------------------------------------------

fuse_mux(Control, Key, Value, Controls) ->
    case Controls of
        #{Control := #{Key := Existing}} when Existing =:= Value ->
            Controls;

        #{Control := #{Key := Existing}} ->
            throw({Control, Key, Value, existing, Existing});

        #{Control := Muxes} ->
            Controls#{Control => Muxes#{Key => Value}};

        _ ->
            Controls#{Control => #{Key => Value}}
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

signal_dest(#{lc := _, route := Route}, Model) ->
    signal_route(Route, Model);
signal_dest(_, _) ->
    ok.

%%--------------------------------------------------------------------

signal_route([], _) ->
    ok;
signal_route([{lab_control_mux, X, Y, 0, N}, Route | _], Model) ->
    Control = {lab, X, Y, control, N},
    case theory(Control, Model) of
        Theory when Theory =:= Route ->
            ok;

        Theory ->
            throw({Control, Route, theory, Theory, Model})
    end;
signal_route([_ | Route], Model) ->
    signal_route(Route, Model).

%%====================================================================
%% theory
%%====================================================================

theory(Control = {lab, X, Y, control, N}, Model) ->
    case Model of
        #{Control := #{from6 := Mux6, from3 := Mux3}} ->
            theory(X, Y, port(N), Mux6, Mux3);

        #{Control := Muxes} ->
            Muxes;

        _ ->
            Control
    end.

%%--------------------------------------------------------------------

theory(X, Y, Port, Mux6, Mux3) ->
    case lc_data_mux_map:to_interconnect(Port, Mux6, Mux3) of
        {local_line, N} ->
            {local_line, X, Y, 0, N};

        {interconnect, N} ->
            {local_interconnect, X, Y, 0, N}
    end.

%%--------------------------------------------------------------------

port(0) -> data_c;
port(1) -> data_d;
port(2) -> data_c;
port(3) -> data_d;
port(4) -> data_c;
port(5) -> data_d.

