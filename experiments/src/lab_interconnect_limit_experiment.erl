-module(lab_interconnect_limit_experiment).

-export([run/0]).

% Experiment to confirm that each LAB only has 26 interconnects.
%
% The experiment tries to use 28 inputs to a LAB and failes with the
% following message extract in the error:
%
%   LAB legality constraint that was not satisfied:
%   LAB requires more input signals requiring LAB lines than are available.
%   Resources used: 28. Resources available: 26.

-define(DEVICE, max_v_240z_t100).

%%====================================================================
%% run
%%====================================================================

run() ->
    {ok, Experiments} = experiment:compile_to_fuses([
        #{
            title => limit,
            device => ?DEVICE,
            settings => [
                {location, a, {lc, 2, 2, 0}},
                {location, b, {lc, 2, 2, 1}},
                {location, c, {lc, 2, 2, 2}},
                {location, d, {lc, 2, 2, 3}},
                {location, e, {lc, 2, 2, 4}},
                {location, f, {lc, 2, 2, 5}},
                {location, g, {lc, 2, 2, 6}}
            ],
            verilog => <<
                "module experiment (\n"
                "  input wire a0,\n"
                "  input wire a1,\n"
                "  input wire a2,\n"
                "  input wire a3,\n"
                "  output wire aq,\n"
                "  input wire b0,\n"
                "  input wire b1,\n"
                "  input wire b2,\n"
                "  input wire b3,\n"
                "  output wire bq,\n"
                "  input wire c0,\n"
                "  input wire c1,\n"
                "  input wire c2,\n"
                "  input wire c3,\n"
                "  output wire cq,\n"
                "  input wire d0,\n"
                "  input wire d1,\n"
                "  input wire d2,\n"
                "  input wire d3,\n"
                "  output wire dq,\n"
                "  input wire e0,\n"
                "  input wire e1,\n"
                "  input wire e2,\n"
                "  input wire e3,\n"
                "  output wire eq,\n"
                "  input wire f0,\n"
                "  input wire f1,\n"
                "  input wire f2,\n"
                "  input wire f3,\n"
                "  output wire fq,\n"
                "  input wire g0,\n"
                "  input wire g1,\n"
                "  input wire g2,\n"
                "  input wire g3,\n"
                "  output wire gq\n"
                ");\n"
                "  lcell a (\n"
                "    .in(a0 && a1 && a2 && a3),\n"
                "    .out(aq)\n"
                "  );\n"
                "  lcell b (\n"
                "    .in(b0 && b1 && b2 && b3),\n"
                "    .out(bq)\n"
                "  );\n"
                "  lcell c (\n"
                "    .in(c0 && c1 && c2 && c3),\n"
                "    .out(cq)\n"
                "  );\n"
                "  lcell d (\n"
                "    .in(d0 && d1 && d2 && d3),\n"
                "    .out(dq)\n"
                "  );\n"
                "  lcell e (\n"
                "    .in(e0 && e1 && e2 && e3),\n"
                "    .out(eq)\n"
                "  );\n"
                "  lcell f (\n"
                "    .in(f0 && f1 && f2 && f3),\n"
                "    .out(fq)\n"
                "  );\n"
                "  lcell g (\n"
                "    .in(g0 && g1 && g2 && g3),\n"
                "    .out(gq)\n"
                "  );\n"
                "endmodule\n"
             >>
        }
    ]),
    io:format("~p~n", [Experiments]),
    ok.

