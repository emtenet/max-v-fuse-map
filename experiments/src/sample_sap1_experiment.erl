-module(sample_sap1_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    {ok, _} = experiment:compile([
        source(Device, Seed)
        ||
        Seed <- lists:seq(1, 20)
    ]),
    ok.

%%--------------------------------------------------------------------

source(Device, Seed) ->
    {ok, Verilog} = file:read_file("priv/sap1.v"),
    #{
        title => {sap1, Seed},
        device => Device,
        settings => [
            {global_clock, clk, true},
            {seed, Seed}
        ],
        verilog => Verilog
    }.

