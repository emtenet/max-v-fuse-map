-module(fuse_count_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun experiment/1, device:list()).

%%--------------------------------------------------------------------

experiment(Device) ->
    Title = <<"fuse count">>,
    {ok, Experiment} = experiment:compile(#{
        title => Title,
        device => Device,
        settings => [],
        verilog => <<
            "module experiment (\n"
            "  input wire d,\n"
            "  output wire q\n"
            ");\n"
            "  assign q = d;\n"
            "endmodule\n"
        >>
    }),
    {ok, POF} = experiment:pof(Experiment),
    FuseCount = pof_file:fuse_count(POF),
    Density = device:density(Device),
    FuseCount = density:fuse_count(Density),
    io:format("fuse_count(~s) -> ~p.~n", [Device, FuseCount]).

