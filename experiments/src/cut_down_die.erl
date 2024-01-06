-module(cut_down_die).

-export([run/0]).

% If a 5M40Z is a cut-down 5M240Z, 240 labs in 4 rows of 6 columns,
% then can all of those labs be used even if it is meant to be a
% 40 LC device?
%
% Seems like the answer is yes!

-include("max_v.hrl").

%%====================================================================
%% run
%%====================================================================

run() ->
    Device = max_v_40z_e64,
    io:format(" => ~s labs~n", [Device]),
    Sources = [
        source(Device, {x, 2, y, 4}, <<"LC_X2_Y4_N0">>),
        source(Device, {x, 2, y, 3}, <<"LC_X2_Y3_N0">>),
        source(Device, {x, 2, y, 2}, <<"LC_X2_Y2_N0">>),
        source(Device, {x, 2, y, 1}, <<"LC_X2_Y1_N0">>),
        source(Device, {x, 3, y, 4}, <<"LC_X3_Y4_N0">>),
        source(Device, {x, 3, y, 3}, <<"LC_X3_Y3_N0">>),
        source(Device, {x, 3, y, 2}, <<"LC_X3_Y2_N0">>),
        source(Device, {x, 3, y, 1}, <<"LC_X3_Y1_N0">>),
        source(Device, {x, 4, y, 4}, <<"LC_X4_Y4_N0">>),
        source(Device, {x, 4, y, 3}, <<"LC_X4_Y3_N0">>),
        source(Device, {x, 4, y, 2}, <<"LC_X4_Y2_N0">>),
        source(Device, {x, 4, y, 1}, <<"LC_X4_Y1_N0">>),
        source(Device, {x, 5, y, 4}, <<"LC_X5_Y4_N0">>),
        source(Device, {x, 5, y, 3}, <<"LC_X5_Y3_N0">>),
        source(Device, {x, 5, y, 2}, <<"LC_X5_Y2_N0">>),
        source(Device, {x, 5, y, 1}, <<"LC_X5_Y1_N0">>),
        source(Device, {x, 6, y, 4}, <<"LC_X6_Y4_N0">>),
        source(Device, {x, 6, y, 3}, <<"LC_X6_Y3_N0">>),
        source(Device, {x, 6, y, 2}, <<"LC_X6_Y2_N0">>),
        source(Device, {x, 6, y, 1}, <<"LC_X6_Y1_N0">>),
        source(Device, {x, 7, y, 4}, <<"LC_X7_Y4_N0">>),
        source(Device, {x, 7, y, 3}, <<"LC_X7_Y3_N0">>),
        source(Device, {x, 7, y, 2}, <<"LC_X7_Y2_N0">>),
        source(Device, {x, 7, y, 1}, <<"LC_X7_Y1_N0">>)
    ],
    {ok, _Experiments} = experiment:compile_to_rcf(Sources),
    ok.

%%--------------------------------------------------------------------

source(Device, Title, LUT) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {raw, <<"set_location_assignment -to lit ", LUT/binary, "\n">>}
        ],
        verilog => <<
            "module experiment (\n"
            "  output wire q\n"
            ");\n"
            "  lcell lut (.in(0), .out(q));\n"
            "endmodule\n"
        >>
    }.

