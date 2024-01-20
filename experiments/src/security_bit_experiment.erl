-module(security_bit_experiment).

-export([run/0]).

% This experiment looks at the security bit fuse.
%
% There is no fuse!
%
% Only a section of the POF file is changed!

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
        source:in_out(Device, off, [{security_bit, false}], 0, Pin),
        source:in_out(Device, on, [{security_bit, true}], 0, Pin)
    ]),
    Matrix = matrix:build(Density, Experiments),
    %
    matrix:print(Matrix),
    %
    {matrix, _, []} = Matrix,
    ok.

