-module(lc_output_experiment).

-export([run/0]).

-export([source/4]).

% Find LUT out (left / right) fuses that drive the direct-link
% interconnects to neighbouring LABs.
%
% By selecting LCs that have a fast-out to an IOC directly above or
% below we can check that there is a LUT out to the right.
%
% By selecting a "thru" LAB that has LABs to the left and right
% we can create experiments that route via direct-link to the
% left and right.

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    lists:foreach(
        fun (IOB) -> iob(Density, Device, IOB) end,
        density:iobs(Density)
    ).

%%--------------------------------------------------------------------

iob(Density, Device, {IOB = {iob, X, IOY}, Thru = {lab, X, Y}})
        when (IOY =:= Y + 1) orelse (IOY =:= Y - 1) ->
    LABs = density:labs(Density),
    Left = {lab, X - 1, Y},
    Right = {lab, X + 1, Y},
    case lists:member(Thru, LABs) andalso
         lists:member(Left, LABs) andalso
         lists:member(Right, LABs) of
        true ->
            Pins = pins(Density, Device, IOB, Thru),
            iob(Density, Device, Pins, Thru, Left, Right);

        false ->
            ok
    end;
iob(_, _, _) ->
    ok.

%%--------------------------------------------------------------------

pins(Density, Device, IOB, Thru) ->
    Pins = device:iocs(Device, IOB),
    FastOuts = density:fast_outs(Density),
    lists:filtermap(fun (Pin) -> pin(Pin, FastOuts, Thru) end, Pins).

%%--------------------------------------------------------------------

pin({IOC, Pin}, FastOuts, Thru) ->
    case lists:keyfind(IOC, 1, FastOuts) of
        {_, LC, _} ->
            Thru = lc:lab(LC),
            {true, {IOC, Pin, LC}};

        _ ->
            false
    end.

%%--------------------------------------------------------------------

iob(_, _, [], _, _, _) ->
    ok;
iob(Density, Device, Pins, ThruLAB, Left, Right) ->
    io:format(" ==> ~s thru ~p~n", [Density, ThruLAB]),
    {ok, Experiments} = experiment:compile_to_fuses([
        source(Device, LC, Thru, Pin)
        ||
        LC <- lab:lcs(Left) ++ lab:lcs(Right),
        {_, Pin, Thru} <- Pins
    ]),
    Matrix0 = matrix:build(Density, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({_, fast_slew_rate}) -> true;
        ({_, data_a6, _}) -> true;
        ({_, data_a3, _}) -> true;
        ({_, data_b6, _}) -> true;
        ({_, data_b3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_d6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, lut, _}) -> true;
        (_) -> false
    end),
    %
    %matrix:print(Matrix),
    %
    fast_outs(Matrix, Pins),
    lut_outs(Matrix, Pins, Left, Right),
    ok.

%%--------------------------------------------------------------------

source(Device, LC = {lc, _, _, _}, Thru = {lc, _, _, _}, Pin)
        when is_atom(Pin) ->
    #{
        title => {LC, thru, Thru, Pin},
        device => Device,
        settings => [
            {location, lut, LC},
            {location, thru, Thru},
            {location, q, Pin}
        ],
        verilog => <<
            "module experiment (\n"
            "  output wire q\n"
            ");\n"
            "  wire lut_q;\n"
            "  lcell lut (\n"
            "    .in(1),\n"
            "    .out(lut_q)\n"
            "  );\n"
            "  lcell thru (\n"
            "    .in(lut_q),\n"
            "    .out(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

fast_outs(Matrix, [A, B, C, D]) ->
    [
        fast_out(Matrix, A, [0, 1, 1, 1]),
        fast_out(Matrix, B, [1, 0, 1, 1]),
        fast_out(Matrix, C, [1, 1, 0, 1]),
        fast_out(Matrix, D, [1, 1, 1, 0])
    ];
fast_outs(Matrix, [A, B, C]) ->
    [
        fast_out(Matrix, A, [0, 1, 1]),
        fast_out(Matrix, B, [1, 0, 1]),
        fast_out(Matrix, C, [1, 1, 0])
    ];
fast_outs(Matrix, [A, B]) ->
    [
        fast_out(Matrix, A, [0, 1]),
        fast_out(Matrix, B, [1, 0])
    ].

%%--------------------------------------------------------------------

fast_out(Matrix, {_, _, LC}, SubPattern) ->
    Pattern = lists:flatten(lists:duplicate(20, SubPattern)),
    Fuse = {LC, lut_out, right},
    expect:fuse(Matrix, Pattern, Fuse).

%%--------------------------------------------------------------------

lut_outs(Matrix, [_, _, _, _], Left, Right) ->
    lut_outs(Matrix, [0, 0, 0, 0], [1, 1, 1, 1], Left, Right);
lut_outs(Matrix, [_, _, _], Left, Right) ->
    lut_outs(Matrix, [0, 0, 0], [1, 1, 1], Left, Right);
lut_outs(Matrix, [_, _], Left, Right) ->
    lut_outs(Matrix, [0, 0], [1, 1], Left, Right).

%%--------------------------------------------------------------------

lut_outs(Matrix, Zeros, Ones, Left, Right) ->
    [
        lut_out(Matrix, lut_pattern(Zeros, Ones, LC, right))
        ||
        LC <- lab:lcs(Left)
    ] ++ [
        lut_out(Matrix, lut_pattern(Zeros, Ones, LC, left))
        ||
        LC <- lab:lcs(Right)
    ].

%%--------------------------------------------------------------------

lut_pattern(Zeros, Ones, LC = {lc, _, _, N}, right) ->
    Pattern = lists:flatten([
        lists:duplicate(N, Ones),
        Zeros,
        lists:duplicate(19 - N, Ones)
    ]),
    {LC, right, Pattern};
lut_pattern(Zeros, Ones, LC = {lc, _, _, N}, left) ->
    Pattern = lists:flatten([
        lists:duplicate(10 + N, Ones),
        Zeros,
        lists:duplicate(9 - N, Ones)
    ]),
    {LC, left, Pattern}.

%%--------------------------------------------------------------------

lut_out(Matrix, {LC, Dir, Pattern}) ->
    expect:fuse(Matrix, Pattern, {LC, lut_out, Dir}).

