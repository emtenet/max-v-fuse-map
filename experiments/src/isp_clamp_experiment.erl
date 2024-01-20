-module(isp_clamp_experiment).

-export([run/0]).

% This experiment looks at the different ISP clamp settings
%
%  * high
%  * low
%  * sample and sustain
%  * tri-state
%
% No fuses!

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    io:format(" => ~s~n", [Density]),
    Device = density:largest_device(Density),
    [Pin | _] = device:pins(Device),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source:in_out(Device, ISP, [{isp_clamp, ISP}], 0, Pin)
        ||
        ISP <- setting:isp_clamps()
    ]),
    Matrix = matrix:build(Density, Experiments),
    %
    matrix:print(Matrix),
    %
    {matrix, _, []} = Matrix,
    ok.

