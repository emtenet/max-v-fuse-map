-module(megafunctions_experiment).

-export([run/0]).

-export([source_add/1]).
-export([source_sub/1]).
-export([source_add_sub/1]).
-export([source_cin_add/1]).
-export([source_cin_0_add/1]).
-export([source_cin_1_add/1]).
-export([source_cin_sub/1]).
-export([source_cin_add_sub/1]).
-export([source_cin_0_add_sub/1]).
-export([source_cin_1_add_sub/1]).
-export([source_accumulator/1]).
-export([source_accumulator2/1]).
-export([source_mux_2/1]).
-export([source_mux_3/1]).
-export([source_mux_4/1]).
-export([source_left_shift/1]).
-export([source_right_shift/1]).
-export([source_shifter/1]).
-export([source_counter/1]).
-export([source_counter_sclr_sload/1]).
-export([source_counter_mod/1]).
-export([source_counter_mod_sclr_sload/1]).
-export([source_register_chain/1]).
-export([source_ram_1_port/1]).
-export([source_sld_virtual_jtag/2]).

%%====================================================================
%% run
%%====================================================================

run() ->
    lists:foreach(fun density/1, density:list()).

%%--------------------------------------------------------------------

density(Density) ->
    io:format(" ==> ~s~n", [Density]),
    Device = density:largest_device(Density),
    %ok = experiment:flush(source_add_sub(Device)),
    {ok, Experiments} = experiment:compile_to_fuses_and_rcf([
        % 7289 |*| | {2,3,line,23,cell,15}
        % 8568 |*| | {2,3,line,22,cell,20}
        % 9082 |*| | {2,3,line,24,cell,22}
        %source_add_sub(Device, <<"3">>, <<"4">>, 1)
        %source_add_sub(Device, <<"7">>, <<"8">>, 1)
        %  7289 |*| | {2,3,line,23,cell,15}
        %  8568 |*| | {2,3,line,22,cell,20}
        %  9082 |*| | {2,3,line,24,cell,22}
        % 15736 |*| | {3,3,line,22,cell,20}
        %source_add_sub(Device, <<"11">>, <<"12">>, 1)
        %  8568 |*| | | | | | | {2,3,line,22,cell,20}
        %  9082 |*| | | | | | | {2,3,line,24,cell,22}
        % 15736 |*| | | | | | | {3,3,line,22,cell,20}
        %  7289 |*| | | | |*| | {2,3,line,23,cell,15}
        % 14457 |*|*|*| | | |*| {3,3,line,23,cell,15}
        %          4 4 4 4 3 4  {{lab,2,3},add_sub,{control,?}}
        %          3 3 4 4 4 3  {{lab,3,3},add_sub,{control,?}}
        %source_add_sub(Device, <<"15">>, <<"16">>, 1),
        %source_add_sub(Device, <<"15">>, <<"16">>, 2),
        %source_add_sub(Device, <<"15">>, <<"16">>, 3),
        %source_add_sub(Device, <<"15">>, <<"16">>, 4),
        %source_add_sub(Device, <<"15">>, <<"16">>, 5),
        %source_add_sub(Device, <<"15">>, <<"16">>, 6)
        %  8568 |*| | {2,3,line,22,cell,20}
        %  9082 |*| | {2,3,line,24,cell,22}
        % 15736 |*| | {3,3,line,22,cell,20}
        %source_add_sub(Device, <<"19">>, <<"20">>, 1)
        %  8568 |*| | | | {2,3,line,22,cell,20}
        %  9082 |*| | | | {2,3,line,24,cell,22}
        % 15736 |*| | | | {3,3,line,22,cell,20}
        % 22904 |*| | | | {4,3,line,22,cell,20}
        %source_add_sub(Device, <<"23">>, <<"24">>, 1),
        %source_add_sub(Device, <<"23">>, <<"24">>, 2),
        %source_add_sub(Device, <<"23">>, <<"24">>, 3)
        %
        %source_add(Device),
        %source_sub(Device),
        %source_add_sub(Device),
        %source_cin_add(Device),
        %source_cin_0_add(Device),
        %source_cin_1_add(Device),
        %source_cin_sub(Device),
        %source_cin_add_sub(Device),
        %source_cin_0_add_sub(Device),
        %source_cin_1_add_sub(Device)
        %
        source_add(Device, <<"23">>, <<"24">>, 1),
        source_add_sub(Device, <<"23">>, <<"24">>, 1),
        source_cin_add_sub(Device)
        %
        %source_accumulator(Device),
        %source_accumulator2(Device)
        %
        %source_mux_2(Device),
        %source_mux_3(Device),
        %source_mux_4(Device)
        %source_left_shift(Device),
        %source_right_shift(Device)
        %
        %source_shifter(Device)
        %
        %source_counter(Device),
        %source_counter_sclr_sload(Device),
        %source_counter_mod(Device),
        %source_counter_mod_sclr_sload(Device)
        %
        %source_register_chain(Device)
        %
        %source_ram_1_port(Device)
        %
        %source_sld_virtual_jtag(Device, <<"0">>),
        %source_sld_virtual_jtag(Device, <<"1">>)
    ]),
    Minimal = density:minimal_fuses(Density),
    Matrix0 = matrix:build(Density, [{minimal, Minimal} | Experiments]),
    Matrix = matrix:remove_fuses(Matrix0, fun
        ({{iob, _, _}, _, _}) -> true;
        ({{iob, _, _}, _, _, _}) -> true;
        ({{ioc, _, _, _}, _}) -> true;
        ({{ioc, _, _, _}, _, _}) -> true;
        ({{lab, _, _}, {control, _}, _, _}) -> true;
        ({{lab, _, _}, {interconnect, _}, _}) -> true;
        ({{lab, _, _}, {interconnect, _}, _, _}) -> true;
        ({_, {mux, _}, _}) -> true;
        ({_, {mux, _}, _, _}) -> true;
        ({_, {interconnect, _}, _, _}) -> true;
        ({_, lut, _}) -> true;
        ({_, output_local, lut}) -> true;
        ({_, output_left, lut}) -> true;
        ({_, output_right, lut}) -> true;
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
    matrix:print(Matrix),
    %display:control_routing(Experiments),
    display:routing(Experiments, Density),
    throw(stop),
    ok.

%%--------------------------------------------------------------------

source_add(Device) ->
    source_add(Device, <<"3">>, <<"4">>, 1).

%%--------------------------------------------------------------------

source_add(Device, Top, Width, Seed) ->
    #{
        title => add,
        device => Device,
        settings => [
            {seed, Seed},
            {location, <<"lpm_add_sub:mega|addcore:adder|a_csnbuffer:result_node|cs_buffer[0]~0">>, {lc, 5, 2, 0}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [", Top/binary, ":0] a,\n"
            "  input wire [", Top/binary, ":0] b,\n"
            "  output wire [", Top/binary, ":0] s\n"
            ");\n"
            "  lpm_add_sub mega (\n"
            "    .dataa(a),\n"
            "    .datab(b),\n"
            "    .result(s)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"ADD\",\n"
            "    mega.lpm_hint = \"ONE_INPUT_IS_CONSTANT=NO,CIN_USED=NO\",\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.lpm_type = \"LPM_ADD_SUB\",\n"
            "    mega.lpm_width = ", Width/binary, ";\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_sub(Device) ->
    #{
        title => sub,
        device => Device,
        settings => [
            {location, <<"lpm_add_sub:mega|addcore:adder|a_csnbuffer:result_node|cs_buffer[0]~0">>, {lc, 5, 2, 0}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [3:0] a,\n"
            "  input wire [3:0] b,\n"
            "  output wire [3:0] s\n"
            ");\n"
            "  lpm_add_sub mega (\n"
            "    .dataa(a),\n"
            "    .datab(b),\n"
            "    .result(s)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"SUB\",\n"
            "    mega.lpm_hint = \"ONE_INPUT_IS_CONSTANT=NO,CIN_USED=NO\",\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.lpm_type = \"LPM_ADD_SUB\",\n"
            "    mega.lpm_width = 4;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_add_sub(Device) ->
    source_add_sub(Device, <<"3">>, <<"4">>, 1).

%%--------------------------------------------------------------------

source_add_sub(Device, Top, Width, Seed) ->
    #{
        title => add_sub,
        device => Device,
        settings => [
            {seed, Seed},
            {location, <<"lpm_add_sub:mega|alt_stratix_add_sub:stratix_adder|add_sub_cell[0]">>, {lc, 5, 2, 0}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [", Top/binary, ":0] a,\n"
            "  input wire [", Top/binary, ":0] b,\n"
            "  input wire add_sub,\n"
            "  output wire [", Top/binary, ":0] s\n"
            ");\n"
            "  lpm_add_sub mega (\n"
            "    .add_sub(add_sub),\n"
            "    .dataa(a),\n"
            "    .datab(b),\n"
            "    .result(s)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"UNUSED\",\n"
            "    mega.lpm_hint = \"ONE_INPUT_IS_CONSTANT=NO,CIN_USED=NO\",\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.lpm_type = \"LPM_ADD_SUB\",\n"
            "    mega.lpm_width = ", Width/binary, ";\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_cin_add(Device) ->
    #{
        title => cin_add,
        device => Device,
        settings => [
            {location, <<"lpm_add_sub:mega|addcore:adder|a_csnbuffer:result_node|cs_buffer[0]~0">>, {lc, 5, 2, 1}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire cin,\n"
            "  input wire [13:0] a,\n"
            "  input wire [13:0] b,\n"
            "  output wire [13:0] s\n"
            ");\n"
            "  lpm_add_sub mega (\n"
            "    .cin(cin),\n"
            "    .dataa(a),\n"
            "    .datab(b),\n"
            "    .result(s)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"ADD\",\n"
            "    mega.lpm_hint = \"ONE_INPUT_IS_CONSTANT=NO,CIN_USED=YES\",\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.lpm_type = \"LPM_ADD_SUB\",\n"
            "    mega.lpm_width = 14;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_cin_0_add(Device) ->
    #{
        title => cin_0_add,
        device => Device,
        settings => [
            {location, <<"lpm_add_sub:mega|addcore:adder|a_csnbuffer:result_node|cs_buffer[0]~0">>, {lc, 5, 2, 1}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [13:0] a,\n"
            "  input wire [13:0] b,\n"
            "  output wire [13:0] s\n"
            ");\n"
            "  lpm_add_sub mega (\n"
            "    .cin(0),\n"
            "    .dataa(a),\n"
            "    .datab(b),\n"
            "    .result(s)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"ADD\",\n"
            "    mega.lpm_hint = \"ONE_INPUT_IS_CONSTANT=NO,CIN_USED=YES\",\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.lpm_type = \"LPM_ADD_SUB\",\n"
            "    mega.lpm_width = 14;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_cin_1_add(Device) ->
    #{
        title => cin_1_add,
        device => Device,
        settings => [
            {location, <<"lpm_add_sub:mega|addcore:adder|a_csnbuffer:result_node|cs_buffer[0]~0">>, {lc, 5, 2, 1}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [13:0] a,\n"
            "  input wire [13:0] b,\n"
            "  output wire [13:0] s\n"
            ");\n"
            "  lpm_add_sub mega (\n"
            "    .cin(1),\n"
            "    .dataa(a),\n"
            "    .datab(b),\n"
            "    .result(s)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"ADD\",\n"
            "    mega.lpm_hint = \"ONE_INPUT_IS_CONSTANT=NO,CIN_USED=YES\",\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.lpm_type = \"LPM_ADD_SUB\",\n"
            "    mega.lpm_width = 14;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_cin_sub(Device) ->
    #{
        title => cin_sub,
        device => Device,
        settings => [
            {location, <<"lpm_add_sub:mega|addcore:adder|a_csnbuffer:result_node|cs_buffer[0]~0">>, {lc, 5, 2, 1}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire cin,\n"
            "  input wire [13:0] a,\n"
            "  input wire [13:0] b,\n"
            "  output wire [13:0] s\n"
            ");\n"
            "  lpm_add_sub mega (\n"
            "    .cin(cin),\n"
            "    .dataa(a),\n"
            "    .datab(b),\n"
            "    .result(s)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"SUB\",\n"
            "    mega.lpm_hint = \"ONE_INPUT_IS_CONSTANT=NO,CIN_USED=YES\",\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.lpm_type = \"LPM_ADD_SUB\",\n"
            "    mega.lpm_width = 14;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_cin_add_sub(Device) ->
    #{
        title => cin_add_sub,
        device => Device,
        settings => [
            %{location, <<"lpm_add_sub:mega|alt_stratix_add_sub:stratix_adder|add_sub_cell[0]~0">>, {lc, 5, 2, 0}}
            {location, <<"lpm_add_sub:mega|alt_stratix_add_sub:stratix_adder|result[0]">>, {lc, 5, 2, 0}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire cin,\n"
            "  input wire [23:0] a,\n"
            "  input wire [23:0] b,\n"
            "  input wire add_sub,\n"
            "  output wire [23:0] s\n"
            ");\n"
            "  lpm_add_sub mega (\n"
            "    .cin(cin),\n"
            "    .add_sub(add_sub),\n"
            "    .dataa(a),\n"
            "    .datab(b),\n"
            %"    .cout(cout),\n"
            %"    .overflow(overflow),\n"
            "    .result(s)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"UNUSED\",\n"
            "    mega.lpm_hint = \"ONE_INPUT_IS_CONSTANT=NO,CIN_USED=YES\",\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.lpm_type = \"LPM_ADD_SUB\",\n"
            "    mega.lpm_width = 24;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_cin_0_add_sub(Device) ->
    #{
        title => cin_0_add_sub,
        device => Device,
        settings => [
            {location, <<"lpm_add_sub:mega|alt_stratix_add_sub:stratix_adder|result[0]">>, {lc, 5, 2, 0}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [13:0] a,\n"
            "  input wire [13:0] b,\n"
            "  input wire add_sub,\n"
            "  output wire [13:0] s\n"
            ");\n"
            "  lpm_add_sub mega (\n"
            "    .cin(0),\n"
            "    .add_sub(add_sub),\n"
            "    .dataa(a),\n"
            "    .datab(b),\n"
            %"    .cout(cout),\n"
            %"    .overflow(overflow),\n"
            "    .result(s)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"UNUSED\",\n"
            "    mega.lpm_hint = \"ONE_INPUT_IS_CONSTANT=NO,CIN_USED=YES\",\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.lpm_type = \"LPM_ADD_SUB\",\n"
            "    mega.lpm_width = 14;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_cin_1_add_sub(Device) ->
    #{
        title => cin_1_add_sub,
        device => Device,
        settings => [
            {location, <<"lpm_add_sub:mega|alt_stratix_add_sub:stratix_adder|result[0]">>, {lc, 5, 2, 0}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [13:0] a,\n"
            "  input wire [13:0] b,\n"
            "  input wire add_sub,\n"
            "  output wire [13:0] s\n"
            ");\n"
            "  lpm_add_sub mega (\n"
            "    .cin(1),\n"
            "    .add_sub(add_sub),\n"
            "    .dataa(a),\n"
            "    .datab(b),\n"
            %"    .cout(cout),\n"
            %"    .overflow(overflow),\n"
            "    .result(s)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"UNUSED\",\n"
            "    mega.lpm_hint = \"ONE_INPUT_IS_CONSTANT=NO,CIN_USED=YES\",\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.lpm_type = \"LPM_ADD_SUB\",\n"
            "    mega.lpm_width = 14;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_accumulator(Device) ->
    #{
        title => accumulator,
        device => Device,
        settings => [
            {global_clock, clk, true},
            {global_clock, aclr, true},
            {location, <<"lpm_add_sub:mega|alt_stratix_add_sub:stratix_adder|add_sub_cell[0]">>, {lc, 2, 3, 0}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire aclr,\n"
            "  input wire [15:0] b,\n"
            "  input wire add_sub,\n"
            "  output wire [15:0] s\n"
            ");\n"
            "  lpm_add_sub mega (\n"
            "    .clock(clk),\n"
            "    .aclr(aclr),\n"
            "    .add_sub(add_sub),\n"
            "    .dataa(s),\n"
            "    .datab(b),\n"
            "    .result(s)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"UNUSED\",\n"
            "    mega.lpm_hint = \"ONE_INPUT_IS_CONSTANT=NO,CIN_USED=NO\",\n"
            "    mega.lpm_pipeline = 1,\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.lpm_type = \"LPM_ADD_SUB\",\n"
            "    mega.lpm_width = 16;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_accumulator2(Device) ->
    #{
        title => accumulator2,
        device => Device,
        settings => [
            {global_clock, clk, true},
            {global_clock, aclr, true},
            {location, <<"altaccumulate:mega|accum_95f:accum_cell|result[0]">>, {lc, 2, 3, 1}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire aclr,\n"
            "  input wire [15:0] d,\n"
            "  input wire add_sub,\n"
            "  output wire [15:0] q\n"
            ");\n"
            "  altaccumulate mega (\n"
            "    .clock(clk),\n"
            "    .aclr(aclr),\n"
            "    .add_sub(add_sub),\n"
            "    .data(d),\n"
            "    .result(q)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_representation = \"UNSIGNED\",\n"
            "    mega.width_in = 16,\n"
            "    mega.width_out = 16;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_mux_2(Device) ->
    #{
        title => mux_2,
        device => Device,
        settings => [
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire sel,\n"
            "  input wire [3:0] a,\n"
            "  input wire [3:0] b,\n"
            "  output wire [3:0] q\n"
            ");\n"
            "  lpm_mux mega (\n"
            "    .data({a, b}),\n"
            "    .sel(sel),\n"
            "    .result(q)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_size = 2,\n"
            "    mega.lpm_type = \"LPM_MUX\",\n"
            "    mega.lpm_width = 4,\n"
            "    mega.lpm_widths = 1;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_mux_3(Device) ->
    #{
        title => mux_3,
        device => Device,
        settings => [
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [1:0] sel,\n"
            "  input wire [3:0] a,\n"
            "  input wire [3:0] b,\n"
            "  input wire [3:0] c,\n"
            "  output wire [3:0] q\n"
            ");\n"
            "  lpm_mux mega (\n"
            "    .data({a, b, c}),\n"
            "    .sel(sel),\n"
            "    .result(q)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_size = 3,\n"
            "    mega.lpm_type = \"LPM_MUX\",\n"
            "    mega.lpm_width = 4,\n"
            "    mega.lpm_widths = 2;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_mux_4(Device) ->
    #{
        title => mux_4,
        device => Device,
        settings => [
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire [1:0] sel,\n"
            "  input wire [3:0] a,\n"
            "  input wire [3:0] b,\n"
            "  input wire [3:0] c,\n"
            "  input wire [3:0] d,\n"
            "  output wire [3:0] q\n"
            ");\n"
            "  lpm_mux mega (\n"
            "    .data({a, b, c, d}),\n"
            "    .sel(sel),\n"
            "    .result(q)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_size = 4,\n"
            "    mega.lpm_type = \"LPM_MUX\",\n"
            "    mega.lpm_width = 4,\n"
            "    mega.lpm_widths = 2;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_left_shift(Device) ->
    #{
        title => left_shift,
        device => Device,
        settings => [
            {global_clock, clk, true},
            {global_clock, sclr, true},
            {global_clock, sload, true}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire sclr,\n"
            "  input wire sload,\n"
            "  input wire [3:0] sdata,\n"
            "  input wire shiftin,\n"
            "  output wire shiftout\n"
            ");\n"
            "  lpm_shiftreg mega (\n"
            "    .clock(clk),\n"
            "    .sclr(sclr),\n"
            "    .load(sload),\n"
            "    .data(sdata),\n"
            "    .shiftin(shiftin),\n"
            "    .shiftout(shiftout)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_type = \"LPM_MUX\",\n"
            "    mega.lpm_direction = \"LEFT\",\n"
            "    mega.lpm_width = 4;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_right_shift(Device) ->
    #{
        title => right_shift,
        device => Device,
        settings => [
            {global_clock, clk, true},
            {global_clock, sclr, true},
            {global_clock, sload, true}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire sclr,\n"
            %"  input wire sload,\n"
            %"  input wire [3:0] sdata,\n"
            "  input wire shiftin,\n"
            "  output wire shiftout\n"
            ");\n"
            "  lpm_shiftreg mega (\n"
            "    .clock(clk),\n"
            "    .sclr(sclr),\n"
            %"    .load(sload),\n"
            %"    .data(sdata),\n"
            "    .shiftin(shiftin),\n"
            "    .shiftout(shiftout)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_type = \"LPM_MUX\",\n"
            "    mega.lpm_direction = \"RIGHT\",\n"
            "    mega.lpm_width = 4;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_shifter(Device) ->
    #{
        title => shifter,
        device => Device,
        settings => [
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire direction,\n"
            "  input wire [1:0] distance,\n"
            "  input wire [3:0] d,\n"
            "  output wire [3:0] q\n"
            ");\n"
            "  lpm_clshift mega (\n"
            "    .data(d),\n"
            "    .direction(direction),\n"
            "    .distance(distance),\n"
            "    .result(q)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_type = \"LPM_CLSHIFT\",\n"
            "    mega.lpm_shifttype = \"LOGICAL\",\n"
            "    mega.lpm_width = 4,\n"
            "    mega.lpm_widthdist = 2;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_counter(Device) ->
    #{
        title => counter,
        device => Device,
        settings => [
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire updown,\n"
            "  output wire [3:0] q\n"
            ");\n"
            "  lpm_counter mega (\n"
            "    .clock(clk),\n"
            "    .updown(updown),\n"
            "    .q(q),\n"
            "    .aclr(0),\n"
            "    .aload(0),\n"
            "    .aset(0),\n"
            "    .cin(1),\n"
            "    .clk_en(1),\n"
            "    .cnt_en(1),\n"
            "    .cout(),\n"
            "    .data({4{1'b0}}),\n"
            "    .eq(),\n"
            "    .sclr(0),\n"
            "    .sload(0),\n"
            "    .sset(0)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"UNUSED\",\n"
            "    mega.lpm_port_updown = \"PORT_USED\",\n"
            "    mega.lpm_type = \"LPM_COUNTER\",\n"
            "    mega.lpm_width = 4;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_counter_sclr_sload(Device) ->
    #{
        title => counter_sclr_sload,
        device => Device,
        settings => [
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire sclr,\n"
            "  input wire sload,\n"
            "  input wire updown,\n"
            "  input wire [3:0] d,\n"
            "  output wire [3:0] q\n"
            ");\n"
            "  lpm_counter mega (\n"
            "    .clock(clk),\n"
            "    .data(d),\n"
            "    .sclr(sclr),\n"
            "    .sload(sload),\n"
            "    .updown(updown),\n"
            "    .q(q),\n"
            "    .aclr(0),\n"
            "    .aload(0),\n"
            "    .aset(0),\n"
            "    .cin(1),\n"
            "    .clk_en(1),\n"
            "    .cnt_en(1),\n"
            "    .cout(),\n"
            "    .eq(),\n"
            "    .sset(0)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"UNUSED\",\n"
            "    mega.lpm_port_updown = \"PORT_USED\",\n"
            "    mega.lpm_type = \"LPM_COUNTER\",\n"
            "    mega.lpm_width = 4;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_counter_mod(Device) ->
    #{
        title => counter_mod,
        device => Device,
        settings => [
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire updown,\n"
            "  output wire [3:0] q\n"
            ");\n"
            "  lpm_counter mega (\n"
            "    .clock(clk),\n"
            "    .updown(updown),\n"
            "    .q(q),\n"
            "    .aclr(0),\n"
            "    .aload(0),\n"
            "    .aset(0),\n"
            "    .cin(1),\n"
            "    .clk_en(1),\n"
            "    .cnt_en(1),\n"
            "    .cout(),\n"
            "    .data({4{1'b0}}),\n"
            "    .eq(),\n"
            "    .sclr(0),\n"
            "    .sload(0),\n"
            "    .sset(0)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"UNUSED\",\n"
            "    mega.lpm_modulus = 10,\n"
            "    mega.lpm_port_updown = \"PORT_USED\",\n"
            "    mega.lpm_type = \"LPM_COUNTER\",\n"
            "    mega.lpm_width = 4;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_counter_mod_sclr_sload(Device) ->
    #{
        title => counter_mod_sclr_sload,
        device => Device,
        settings => [
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire sclr,\n"
            "  input wire sload,\n"
            "  input wire updown,\n"
            "  input wire [3:0] d,\n"
            "  output wire [3:0] q\n"
            ");\n"
            "  lpm_counter mega (\n"
            "    .clock(clk),\n"
            "    .data(d),\n"
            "    .sclr(sclr),\n"
            "    .sload(sload),\n"
            "    .updown(updown),\n"
            "    .q(q),\n"
            "    .aclr(0),\n"
            "    .aload(0),\n"
            "    .aset(0),\n"
            "    .cin(1),\n"
            "    .clk_en(1),\n"
            "    .cnt_en(1),\n"
            "    .cout(),\n"
            "    .eq(),\n"
            "    .sset(0)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_direction = \"UNUSED\",\n"
            "    mega.lpm_modulus = 10,\n"
            "    mega.lpm_port_updown = \"PORT_USED\",\n"
            "    mega.lpm_type = \"LPM_COUNTER\",\n"
            "    mega.lpm_width = 4;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_register_chain(Device) ->
    #{
        title => register_chain,
        device => Device,
        settings => [
            {global_clock, clk, true},
            {location, ff0, {lc, 2, 3, 0}},
            {location, ff1, {lc, 2, 3, 1}},
            {location, lut, {lc, 2, 3, 1}}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire d,\n"
            "  output wire q,\n"
            "  input wire x0,\n"
            "  input wire x1,\n"
            "  input wire x2,\n"
            "  input wire x3,\n"
            "  output wire xq\n"
            ");\n"
            "  wire d1;\n"
            "  dff ff0 (\n"
            "    .d(d),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(d1)\n"
            "  );\n"
            "  dff ff1 (\n"
            "    .d(d1),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(q)\n"
            "  );\n"
            "  lcell lut (\n"
            "    .in(x0 & x1 & x2 & x3),\n"
            "    .out(xq)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_ram_1_port(Device) ->
    #{
        title => ram_1_port,
        device => Device,
        settings => [
            {global_clock, inclock, true},
            {global_clock, outclock, true}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire inclock,\n"
            "  input wire outclock,\n"
            "  input wire [3:0] address,\n"
            "  input wire [7:0] data,\n"
            "  input wire we,\n"
            "  output wire [7:0] q\n"
            ");\n"
            "  lpm_ram_dq mega (\n"
            "    .inclock(inclock),\n"
            "    .address(address),\n"
            "    .data(data),\n"
            "    .we(we),\n"
            "    .outclock(outclock),\n"
            "    .q(q)\n"
            "  );\n"
            "  defparam\n"
            "    mega.intended_device_family = \"MAX V\",\n"
            "    mega.lpm_address_control = \"REGISTERED\",\n"
            "    mega.lpm_indata = \"REGISTERED\",\n"
            "    mega.lpm_outdata = \"REGISTERED\",\n"
            "    mega.lpm_type = \"LPM_RAM_DQ\",\n"
            "    mega.lpm_width = 8,\n"
            "    mega.lpm_widthad = 4;\n"
            "endmodule\n"
        >>
    }.

%%--------------------------------------------------------------------

source_sld_virtual_jtag(Device, Index) ->
    #{
        title => {sld_virtual_jtag, Index},
        device => Device,
        settings => [
        ],
        verilog => <<
            "module experiment (\n"
            "  output wire tck,\n"
            "  output wire tdi,\n"
            "  input wire tdo,\n"
            "  output wire tms,\n"
            "  input wire [3:0] ir_out,\n"
            "  output wire [3:0] ir_in,\n"
            "  output wire virtual_state_cdr,\n"
            "  output wire virtual_state_sdr,\n"
            "  output wire virtual_state_e1dr,\n"
            "  output wire virtual_state_pdr,\n"
            "  output wire virtual_state_e2dr,\n"
            "  output wire virtual_state_udr,\n"
            "  output wire virtual_state_cir,\n"
            "  output wire virtual_state_uir,\n"
            "  output wire jtag_state_tlr,\n"
            "  output wire jtag_state_rti,\n"
            "  output wire jtag_state_sdrs,\n"
            "  output wire jtag_state_cdr,\n"
            "  output wire jtag_state_sdr,\n"
            "  output wire jtag_state_e1dr,\n"
            "  output wire jtag_state_pdr,\n"
            "  output wire jtag_state_e2dr,\n"
            "  output wire jtag_state_udr,\n"
            "  output wire jtag_state_sirs,\n"
            "  output wire jtag_state_cir,\n"
            "  output wire jtag_state_sir,\n"
            "  output wire jtag_state_e1ir,\n"
            "  output wire jtag_state_pir,\n"
            "  output wire jtag_state_e2ir,\n"
            "  output wire jtag_state_uir\n"
            ");\n"
            "  sld_virtual_jtag mega (\n"
            "    .tck(tck),\n"
            "    .tdi(tdi),\n"
            "    .tdo(tdo),\n"
            "    .tms(tms),\n"
            "    .ir_out(ir_out),\n"
            "    .ir_in(ir_in),\n"
            "    .virtual_state_cdr(virtual_state_cdr),\n"
            "    .virtual_state_sdr(virtual_state_sdr),\n"
            "    .virtual_state_e1dr(virtual_state_e1dr),\n"
            "    .virtual_state_pdr(virtual_state_pdr),\n"
            "    .virtual_state_e2dr(virtual_state_e2dr),\n"
            "    .virtual_state_udr(virtual_state_udr),\n"
            "    .virtual_state_cir(virtual_state_cir),\n"
            "    .virtual_state_uir(virtual_state_uir),\n"
            "    .jtag_state_tlr(jtag_state_tlr),\n"
            "    .jtag_state_rti(jtag_state_rti),\n"
            "    .jtag_state_sdrs(jtag_state_sdrs),\n"
            "    .jtag_state_cdr(jtag_state_cdr),\n"
            "    .jtag_state_sdr(jtag_state_sdr),\n"
            "    .jtag_state_e1dr(jtag_state_e1dr),\n"
            "    .jtag_state_pdr(jtag_state_pdr),\n"
            "    .jtag_state_e2dr(jtag_state_e2dr),\n"
            "    .jtag_state_udr(jtag_state_udr),\n"
            "    .jtag_state_sirs(jtag_state_sirs),\n"
            "    .jtag_state_cir(jtag_state_cir),\n"
            "    .jtag_state_sir(jtag_state_sir),\n"
            "    .jtag_state_e1ir(jtag_state_e1ir),\n"
            "    .jtag_state_pir(jtag_state_pir),\n"
            "    .jtag_state_e2ir(jtag_state_e2ir),\n"
            "    .jtag_state_uir(jtag_state_uir)\n"
            "  );\n"
            "  defparam\n"
            "    mega.lpm_type = \"SLD_VIRTUAL_JTAG\",\n"
            "    mega.sld_auto_instance_index = \"NO\",\n"
            "    mega.sld_instance_index = ", Index/binary, ",\n"
            "    mega.sld_ir_width = 4;\n"
            "endmodule\n"
        >>
    }.

