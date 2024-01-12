-module(local_line_experiment).

-export([run/0]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    Pins0 = device:pins(Device),
    iterate:labs(Device, Pins0, 1,
        fun (LAB, Pins) ->
            sources(Device, LAB, Pins)
        end,
        fun (LAB, _, Experiments) ->
            experiments(Device, LAB, Experiments)
        end,
        {batch, 4}
     ).

%%--------------------------------------------------------------------

sources(Device, LAB, {Pin}) ->
    [
        source(Device, lab:lc(LAB, 0), lab:lc(LAB, 1), lab:lc(LAB, 5), Pin),
        source(Device, lab:lc(LAB, 1), lab:lc(LAB, 2), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 2), lab:lc(LAB, 3), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 3), lab:lc(LAB, 4), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 4), lab:lc(LAB, 5), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 5), lab:lc(LAB, 6), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 6), lab:lc(LAB, 7), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 7), lab:lc(LAB, 8), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 8), lab:lc(LAB, 9), lab:lc(LAB, 0), Pin),
        source(Device, lab:lc(LAB, 9), lab:lc(LAB, 0), lab:lc(LAB, 5), Pin)
    ].

%%--------------------------------------------------------------------

experiments(Device, LAB, Experiments) ->
    io:format(" ==> ~s ~p~n", [Device, LAB]),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, lut, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %
    lists:foreach(fun always_local_line/1, Experiments),
    %
    expect:fuse(Matrix, [0,1,1,1,1,1,1,1,1,0], {lab:lc(LAB, 0), local_line}),
    expect:fuse(Matrix, [0,0,1,1,1,1,1,1,1,1], {lab:lc(LAB, 1), local_line}),
    expect:fuse(Matrix, [1,0,0,1,1,1,1,1,1,1], {lab:lc(LAB, 2), local_line}),
    expect:fuse(Matrix, [1,1,0,0,1,1,1,1,1,1], {lab:lc(LAB, 3), local_line}),
    expect:fuse(Matrix, [1,1,1,0,0,1,1,1,1,1], {lab:lc(LAB, 4), local_line}),
    expect:fuse(Matrix, [1,1,1,1,0,0,1,1,1,1], {lab:lc(LAB, 5), local_line}),
    expect:fuse(Matrix, [1,1,1,1,1,0,0,1,1,1], {lab:lc(LAB, 6), local_line}),
    expect:fuse(Matrix, [1,1,1,1,1,1,0,0,1,1], {lab:lc(LAB, 7), local_line}),
    expect:fuse(Matrix, [1,1,1,1,1,1,1,0,0,1], {lab:lc(LAB, 8), local_line}),
    expect:fuse(Matrix, [1,1,1,1,1,1,1,1,0,0], {lab:lc(LAB, 9), local_line}),
    ok.

%%--------------------------------------------------------------------

source(Device, A, B, Thru, Pin) ->
    #{
        title => {A, B},
        device => Device,
        settings => [
            {location, a, A},
            {location, b, B},
            {location, thru, Thru},
            {location, q, Pin}
        ],
        verilog => <<
            "module experiment (\n"
            "  output wire q\n"
            ");\n"
            "  wire a_q;\n"
            "  wire b_q;\n"
            "  lcell a (\n"
            "    .in(1),\n"
            "    .out(a_q)\n"
            "  );\n"
            "  lcell b (\n"
            "    .in(1),\n"
            "    .out(b_q)\n"
            "  );\n"
            "  lcell thru (\n"
            "    .in(a_q && b_q),\n"
            "    .out(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

always_local_line({_, _, #{signals := #{a := A, b := B}}}) ->
    #{dests := [#{route := [{local_line, _, _, 0, _}]}]} = A,
    #{dests := [#{route := [{local_line, _, _, 0, _}]}]} = B,
    ok.

