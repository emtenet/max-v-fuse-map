-module(sample_6502_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    density(max_v_570z),
    density(max_v_1270z),
    density(max_v_2210z),
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
    {ok, Verilog} = file:read_file("priv/6502.v"),
    #{
        title => {6502, Seed},
        device => Device,
        settings => [
            {global_clock, clk, true},
            {global_clock, reset, true},
            {seed, Seed}
        ],
        verilog => Verilog
    }.

