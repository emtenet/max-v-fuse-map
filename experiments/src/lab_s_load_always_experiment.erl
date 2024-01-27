-module(lab_s_load_always_experiment).

-export([run/0]).

% The fuse {X, Y, line, 20, 20} that has been called:
%
%   {{lab, X, Y}, s_load, unknown}
%
% can now be better called:
%
%   {{lab, X, Y}, s_load, not_always}
%
% since it is `1` when .sload(1) (i.e. always s-load)
%
% and the normal naming for `0` is therfore not-always.

%%====================================================================
%% run
%%====================================================================

run() ->
    %lists:foreach(fun density/1, density:list()),
    density(max_v_240z),
    ok.

%%--------------------------------------------------------------------

density(Density) ->
    Device = density:largest_device(Density),
    device(Density, Device).

%%--------------------------------------------------------------------

device(Density, Device) ->
    [LAB | _] = device:labs(Device),
    Gclks = device:gclk_pins(Device),
    {Pins, _} = lists:split(8, lists:subtract(device:pins(Device), Gclks)),
    io:format(" ==> ~p ~p~n", [Device, LAB]),
    [Clk | _] = Gclks,
    Common = list_to_tuple([LAB, Clk | Pins]),
    Sources = [
        source(Device, Common, Clrn, Asdata, Aload, Sclr, Sload)
        ||
        Clrn <- [clrn, 1],
        Asdata <- [asdata, 0, 1],
        Aload <- [aload, 0],
        Sclr <- [sclr, 0],
        Sload <- [sload, 0, 1]
    ],
    %experiment:flush(Sources),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf(Sources),
    Matrix0 = matrix:build(Device, Experiments),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{ioc, _, _, _}, _}) -> true;
        ({_, lut, _}) -> true;
        ({_, local_line, lut}) -> true;
        ({_, data_a3, _}) -> true;
        ({_, data_a6, _}) -> true;
        ({_, data_b3, _}) -> true;
        ({_, data_b6, _}) -> true;
        ({_, data_c3, _}) -> true;
        ({_, data_c6, _}) -> true;
        ({_, data_d3, _}) -> true;
        ({_, data_d6, _}) -> true;
        (_) -> false
    end),
    %
    matrix:print(Matrix),
    %display:control_routing(Experiments),
    %
    {ok, Fuse} = fuse_map:from_name({LAB, s_load, not_always}, Density),
    io:format("~p~n", [[
        Name
        ||
        {Name, Fuses, _} <- Experiments,
        not lists:member(Fuse, Fuses)
    ]]),
    %
    ok.

%%--------------------------------------------------------------------

source(Device, {LAB, Clk, ClrnPin, AsdataPin, AloadPin, SclrPin, SloadPin, A, B, Q}, ClrnExpr, AsdataExpr, AloadExpr, SclrExpr, SloadExpr) ->
    Clrn = expr(ClrnExpr),
    Asdata = expr(AsdataExpr),
    Aload = expr(AloadExpr),
    Sclr = expr(SclrExpr),
    Sload = expr(SloadExpr),
    #{
        title => {ClrnExpr, AsdataExpr, AloadExpr, SclrExpr, SloadExpr},
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, clrn, ClrnPin},
            {location, asdata, AsdataPin},
            {location, aload, AloadPin},
            {location, sclr, SclrPin},
            {location, sload, SloadPin},
            {location, ff, lab:lc(LAB, 0)},
            {location, lut1, lab:lc(LAB, 1)},
            {location, lut2, lab:lc(LAB, 2)},
            {location, a, A},
            {location, b, B},
            {location, q, Q}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire clrn,\n"
            "  input wire asdata,\n"
            "  input wire aload,\n"
            "  input wire sclr,\n"
            "  input wire sload,\n"
            "  input wire a,\n"
            "  input wire b,\n"
            "  output wire q\n"
            ");\n"
            "  wire chain1;\n"
            "  wire chain2;\n"
            "  dffeas ff (\n"
            "    .d(a && b),\n"
            "    .clk(clk),\n"
            "    .clrn(", Clrn/binary, "),\n"
            "    .prn(1),\n"
            "    .ena(1),\n"
            "    .asdata(", Asdata/binary, "),\n"
            "    .aload(", Aload/binary, "),\n"
            "    .sclr(", Sclr/binary, "),\n"
            "    .sload(", Sload/binary, "),\n"
            "    .q(chain1)\n"
            "  );\n"
            "  lcell lut1 (\n"
            "    .in(chain1 && clrn && asdata),\n"
            "    .out(chain2)\n"
            "  );\n"
            "  lcell lut2 (\n"
            "    .in(chain2 && aload && sclr && sload),\n"
            "    .out(q)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

expr(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
expr(0) ->
    <<"0">>;
expr(1) ->
    <<"1">>.

