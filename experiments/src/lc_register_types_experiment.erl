-module(lc_register_types_experiment).

-export([run/0]).

% This experiment looks at the different types of registers
%
%  * D
%  * T
%  * JK
%  * SR
%
% It appears that there is no special hardware for these types.
% They all compile down to a single type of register.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    io:format(" => ~s~n", [Density]),
    Device = density:largest_device(Density),
    [Clk | _] = device:gclk_pins(Device),
    [A, B, Q | _] = device:pins(Device),
    [LAB | _] = device:labs(Device),
    LC = lab:lc(LAB, 0),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        source_d(Device, LC, Clk, A, B, Q),
        source_t(Device, LC, Clk, A, B, Q),
        source_jk(Device, LC, Clk, A, B, Q),
        source_sr(Device, LC, Clk, A, B, Q)
    ]),
    Matrix0 = matrix:build(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{c4, _, _}, _, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{lab, _, _}, _, _}) -> true;
        ({{lab, _, _}, _, _, _}) -> true;
        ({{lc, _, _, _}, _, _}) -> true;
        ({{r4, _, _}, _, _, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %display:control_routing(Experiments),
    %
    {matrix, [d,t,jk,sr], []} = Matrix,
    ok.

%%--------------------------------------------------------------------

source_d(Device, LC, Clk, A, B, Q) ->
    #{
        title => d,
        device => Device,
        settings => [
            {global_clock, clk, true},
            {location, clk, Clk},
            {location, ff, LC},
            {location, a, A},
            {location, b, B},
            {location, q, Q}
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

%%--------------------------------------------------------------------

source_t(Device, LC, Clk, A, B, Q) ->
    #{
        title => t,
        device => Device,
        settings => [
            {global_clock, clk, true},
            {location, clk, Clk},
            {location, ff, LC},
            {location, a, A},
            {location, b, B},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  output wire q\n"
            ");\n"
            "  tff ff (\n"
            "    .t(a && b),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_jk(Device, LC, Clk, A, B, Q) ->
    #{
        title => jk,
        device => Device,
        settings => [
            {global_clock, clk, true},
            {location, clk, Clk},
            {location, ff, LC},
            {location, a, A},
            {location, b, B},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  output wire q\n"
            ");\n"
            "  jkff ff (\n"
            "    .j(a),\n"
            "    .k(b),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_sr(Device, LC, Clk, A, B, Q) ->
    #{
        title => sr,
        device => Device,
        settings => [
            {global_clock, clk, true},
            {location, clk, Clk},
            {location, ff, LC},
            {location, a, A},
            {location, b, B},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  output wire q\n"
            ");\n"
            "  srff ff (\n"
            "    .s(a),\n"
            "    .r(b),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

