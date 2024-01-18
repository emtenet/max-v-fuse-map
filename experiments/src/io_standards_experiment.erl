-module(io_standards_experiment).

-export([run/0]).

% This experiment looks at the different io standards
%
%  * 1.2 V
%  * 1.5 V
%  * 1.8 V
%  * 2.5 V
%  * 3.3 V CMOS
%  * 3.3 V TTL
%
% It appears that there are no fuses for these.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    io:format(" => ~s~n", [Density]),
    Device = density:largest_device(Density),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source(Device, IO)
        ||
        IO <- setting:io_standards()
    ]),
    Matrix = matrix:build(Density, Experiments),
    %
    matrix:print(Matrix),
    %
    {matrix, _, []} = Matrix,
    ok.

%%--------------------------------------------------------------------

source(Device, IO) ->
    #{
        title => IO,
        device => Device,
        settings => [
            {global_clock, clk, true},
            {io_standard, IO}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  output wire q\n"
            ");\n"
            "  dff ff (\n"
            "    .d(a && b),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

