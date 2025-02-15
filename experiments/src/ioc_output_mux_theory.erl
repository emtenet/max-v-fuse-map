-module(ioc_output_mux_theory).

-export([run/0]).

% This is a theory of how IOB local interconnects are muxed into the
% IOC outputs.
%
% The theory is based on the data produced by `ioc_output_mux_playground`.
%
% ASIDE: Muxes seen so far in the MAX V architecture seem to be all
%   one-hot (or one-cold) selecting multiplexers (rather than
%   binary encoded).
%
% Each IOC output has a two dimentional mux.
%
% Side IOCs have the two coordinates as:
%  * a 6-to-1 mux `output6`, and
%  * a 3-to-1 mux `output3`.
% Combined the form either a 18-to-1 mux.
%
% Top/bottom IOCs have the two coordinates as:
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
    io:format("~nERROR:~n  ~p~n", [Error]),
    ok.

%%--------------------------------------------------------------------

contradiction(Density, Fuse) ->
    io:format("  ~.20w = ~w~n", [
        fuse_map:to_location(Fuse, Density),
        fuse_map:to_name(Fuse, Density)
    ]).

%%====================================================================
%% fuses
%%====================================================================

fuses(Density, Fuses) ->
    lists:foldl(fun (Fuse, IOCs) -> fuse(Density, Fuse, IOCs) end, #{}, Fuses).

%%--------------------------------------------------------------------

fuse(Density, Fuse, IOCs) ->
    case fuse_map:to_name(Fuse, Density) of
        {ok, {IOC, Key = fast_out}} ->
            fuse_mux(IOC, Key, on, IOCs);

        {ok, {IOC, Key = output3, Value}} ->
            fuse_mux(IOC, Key, Value, IOCs);

        {ok, {IOC, Key = output4, Value}} ->
            fuse_mux(IOC, Key, Value, IOCs);

        {ok, {IOC, Key = output6, Value}} ->
            fuse_mux(IOC, Key, Value, IOCs);

        _ ->
            IOCs
    end.

%%--------------------------------------------------------------------

fuse_mux(IOC, Key, Value, IOCs) ->
    case IOCs of
        #{IOC := #{Key := Existing}} when Existing =:= Value ->
            IOCs;

        #{IOC := #{Key := Existing}} ->
            throw({IOC, Key, Value, existing, Existing});

        #{IOC := Muxes} ->
            IOCs#{IOC => Muxes#{Key => Value}};

        _ ->
            IOCs#{IOC => #{Key => Value}}
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

signal_dest(Dest = #{ioc := IOC, port := data_in}, Model) ->
    Route = signal_route(Dest),
    case theory(IOC, Model) of
        Theory when Theory =:= Route ->
            ok;

        Theory ->
            throw({IOC, Route, theory, Theory})
    end;
signal_dest(_, _) ->
    ok.

%%--------------------------------------------------------------------

signal_route(#{ioc := {ioc, 1, 0, _}, route := [], name := Name}) ->
    <<"altera_reserved_tdo">> = Name,
    none;
signal_route(#{ioc := {ioc, 0, 3, _}, route := [], name := Name}) ->
    <<"altera_reserved_tdo">> = Name,
    none;
signal_route(Dest = #{route := []}) ->
    throw({route_is_empty, Dest});
signal_route(#{route := [{io_bypass_out, _, _, _, _}, FastOut]}) ->
    FastOut;
signal_route(#{route := [{io_data_out, _, _, _, _}, Interconnect | _]}) ->
    Interconnect.

%%====================================================================
%% theory
%%====================================================================

theory(IOC, Model) ->
    case Model of
        #{IOC := #{output4 := Mux4, output3 := Mux3, fast_out := on}} ->
            theory_col_fast_out(IOC, Mux4, Mux3);

        #{IOC := #{output4 := Mux4, output3 := Mux3}} ->
            theory_col(IOC, Mux4, Mux3);

        #{IOC := #{output6 := Mux6, output3 := Mux3}} ->
            theory_row(IOC, Mux6, Mux3);

        #{IOC := Muxes = #{output3 := _}} ->
            Muxes;

        #{IOC := Muxes = #{output4 := _}} ->
            Muxes;

        #{IOC := Muxes = #{output6 := _}} ->
            Muxes;

        #{IOC := #{fast_out := on}} ->
            theory_row_fast_out(IOC);

        #{IOC := Muxes} ->
            Muxes;

        _ ->
            none
    end.

%%--------------------------------------------------------------------

theory_col({ioc, X, Y, _}, Mux4, Mux3) ->
    {interconnect, N} = ioc_output_mux_map:to_col_interconnect(Mux4, Mux3),
    {local_interconnect, X, Y, 0, N}.

%%--------------------------------------------------------------------

theory_col_fast_out(IOC, Mux4, Mux3) ->
    fast_out = ioc_output_mux_map:to_col_interconnect(Mux4, Mux3),
    ioc_output_mux_map:fast_out_column(IOC).

%%--------------------------------------------------------------------

theory_row({ioc, X, Y, _}, Mux6, Mux3) ->
    {interconnect, N} = ioc_output_mux_map:to_row_interconnect(Mux6, Mux3),
    {local_interconnect, X, Y, 0, N}.

%%--------------------------------------------------------------------

theory_row_fast_out(IOC) ->
    ioc_output_mux_map:fast_out_row(IOC).

