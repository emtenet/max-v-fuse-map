-module(source).

-export([in_oe_out/5]).
-export([in_oe_out/6]).
-export([in_out/4]).
-export([in_out/5]).
-export([in_lut_out/5]).
-export([in_reg_out/6]).
-export([ins_lut_outs/6]).
-export([lut_out/3]).
-export([open_drain/4]).

-export([ioc/1]).
-export([pin/1]).

%%====================================================================
%% in_oe_out
%%====================================================================

in_oe_out(Device, Title, I, OE, O) ->
    in_oe_out(Device, Title, [], I, OE, O).

%%--------------------------------------------------------------------

in_oe_out(Device, Title, Settings, I, OE, O)
        when is_integer(I) andalso
             is_integer(OE) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, o, pin(O)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  output wire o\n"
            ");\n"
            "  alt_outbuf_tri pad (.i(", ($0 + I), "), .oe(", ($0 + OE), "), .o(o));\n"
            "endmodule\n"
        >>
    };
in_oe_out(Device, Title, Settings, I, {inv, OE}, O)
        when is_integer(I) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, o, pin(O)},
            {location, oe, pin(OE)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire oe,\n"
            "  output wire o\n"
            ");\n"
            "  alt_outbuf_tri pad (.i(", ($0 + I), "), .oe(! oe), .o(o));\n"
            "endmodule\n"
        >>
    };
in_oe_out(Device, Title, Settings, I, OE, O)
        when is_integer(I) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, o, pin(O)},
            {location, oe, pin(OE)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire oe,\n"
            "  output wire o\n"
            ");\n"
            "  alt_outbuf_tri pad (.i(", ($0 + I), "), .oe(oe), .o(o));\n"
            "endmodule\n"
        >>
    };
in_oe_out(Device, Title, Settings, {inv, I}, OE, O)
        when is_integer(OE) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, i, pin(I)},
            {location, o, pin(O)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire i,\n"
            "  output wire o\n"
            ");\n"
            "  alt_outbuf_tri pad (.i(! i), .oe(", ($0 + OE), "), .o(o));\n"
            "endmodule\n"
        >>
    };
in_oe_out(Device, Title, Settings, I, OE, O)
        when is_integer(OE) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, i, pin(I)},
            {location, o, pin(O)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire i,\n"
            "  output wire o\n"
            ");\n"
            "  alt_outbuf_tri pad (.i(i), .oe(", ($0 + OE), "), .o(o));\n"
            "endmodule\n"
        >>
    };
in_oe_out(Device, Title, Settings, {inv, I}, {inv, OE}, O) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, i, pin(I)},
            {location, oe, pin(OE)},
            {location, o, pin(O)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire i,\n"
            "  input wire oe,\n"
            "  output wire o\n"
            ");\n"
            "  alt_outbuf_tri pad (.i(! i), .oe(! oe), .o(o));\n"
            "endmodule\n"
        >>
    };
in_oe_out(Device, Title, Settings, {inv, I}, OE, O) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, i, pin(I)},
            {location, oe, pin(OE)},
            {location, o, pin(O)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire i,\n"
            "  input wire oe,\n"
            "  output wire o\n"
            ");\n"
            "  alt_outbuf_tri pad (.i(! i), .oe(oe), .o(o));\n"
            "endmodule\n"
        >>
    };
in_oe_out(Device, Title, Settings, I, {inv, OE}, O) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, i, pin(I)},
            {location, oe, pin(OE)},
            {location, o, pin(O)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire i,\n"
            "  input wire oe,\n"
            "  output wire o\n"
            ");\n"
            "  alt_outbuf_tri pad (.i(i), .oe(! oe), .o(o));\n"
            "endmodule\n"
        >>
    };
in_oe_out(Device, Title, Settings, I, OE, O) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, i, pin(I)},
            {location, oe, pin(OE)},
            {location, o, pin(O)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire i,\n"
            "  input wire oe,\n"
            "  output wire o\n"
            ");\n"
            "  alt_outbuf_tri pad (.i(i), .oe(oe), .o(o));\n"
            "endmodule\n"
        >>
    }.

%%====================================================================
%% in_out
%%====================================================================

in_out(Device, Title, I, O) ->
    in_out(Device, Title, [], I, O).

%%--------------------------------------------------------------------

in_out(Device, Title, Settings, I, O) when is_integer(I) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, o, pin(O)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  output wire o\n"
            ");\n"
            "  assign o = ", ($0 + I), ";\n"
            "endmodule\n"
        >>
    };
in_out(Device, Title, Settings, {inv, I}, O) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, i, pin(I)},
            {location, o, pin(O)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire i,\n"
            "  output wire o\n"
            ");\n"
            "  assign o = ! i;\n"
            "endmodule\n"
        >>
    };
in_out(Device, Title, Settings, I, O) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, i, pin(I)},
            {location, o, pin(O)}
            |
            Settings
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire i,\n"
            "  output wire o\n"
            ");\n"
            "  assign o = i;\n"
            "endmodule\n"
        >>
    }.

%%====================================================================
%% in_lut_out
%%====================================================================

in_lut_out(Device, Title, I, LC, O) when is_integer(I) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, via, LC},
            {location, o, pin(O)}
        ],
        verilog => <<
            "module experiment (\n"
            "  output wire o\n"
            ");\n"
            "  lcell via (.in(", ($0 + I), "), .out(o));\n"
            "endmodule\n"
        >>
    };
in_lut_out(Device, Title, I, LC, O) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, i, pin(I)},
            {location, via, LC},
            {location, o, pin(O)}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire i,\n"
            "  output wire o\n"
            ");\n"
            "  lcell via (.in(i), .out(o));\n"
            "endmodule\n"
        >>
    }.

%%====================================================================
%% in_reg_out
%%====================================================================

in_reg_out(Device, Title, Clk, I, LC, O) when is_integer(I) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, via, LC},
            {location, o, pin(O)}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  output wire o\n"
            ");\n"
            "  dff via (\n"
            "    .d(", ($0 + I), "),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(o)\n"
            "  );\n"
            "endmodule\n"
        >>
    };
in_reg_out(Device, Title, Clk, I, LC, O) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, clk, Clk},
            {global_clock, clk, true},
            {location, i, pin(I)},
            {location, via, LC},
            {location, o, pin(O)}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire clk,\n"
            "  input wire i,\n"
            "  output wire o\n"
            ");\n"
            "  dff via (\n"
            "    .d(i),\n"
            "    .clk(clk),\n"
            "    .clrn(1),\n"
            "    .prn(1),\n"
            "    .q(o)\n"
            "  );\n"
            "endmodule\n"
        >>
    }.

%%====================================================================
%% ins_lut_outs
%%====================================================================

ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G, H, I, J, K, L], [Q, R, S | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, h, pin(H)},
            {location, i, pin(I)},
            {location, j, pin(J)},
            {location, k, pin(K)},
            {location, l, pin(L)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, w, {lc, X, Y, 2}},
            {location, q, pin(Q)},
            {location, r, pin(R)},
            {location, s, pin(S)}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "library altera_mf;\n"
            "use altera_mf.altera_mf_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    h : in STD_LOGIC;\n"
            "    i : in STD_LOGIC;\n"
            "    j : in STD_LOGIC;\n"
            "    k : in STD_LOGIC;\n"
            "    l : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC;\n"
            "    s : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g AND h,\n"
            "    a_out => r\n"
            "  );\n",
            "  w: LCELL port map (\n"
            "    a_in => i AND j AND k AND l,\n"
            "    a_out => s\n"
            "  );\n",
            "end behavioral;\n"
        >>
    };
ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G, H, I, J, K], [Q, R, S | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, h, pin(H)},
            {location, i, pin(I)},
            {location, j, pin(J)},
            {location, k, pin(K)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, w, {lc, X, Y, 2}},
            {location, q, pin(Q)},
            {location, r, pin(R)},
            {location, s, pin(S)}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "library altera_mf;\n"
            "use altera_mf.altera_mf_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    h : in STD_LOGIC;\n"
            "    i : in STD_LOGIC;\n"
            "    j : in STD_LOGIC;\n"
            "    k : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC;\n"
            "    s : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g AND h,\n"
            "    a_out => r\n"
            "  );\n",
            "  w: LCELL port map (\n"
            "    a_in => i AND j AND k,\n"
            "    a_out => s\n"
            "  );\n",
            "end behavioral;\n"
        >>
    };
ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G, H, I, J], [Q, R, S | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, h, pin(H)},
            {location, i, pin(I)},
            {location, j, pin(J)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, w, {lc, X, Y, 2}},
            {location, q, pin(Q)},
            {location, r, pin(R)},
            {location, s, pin(S)}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "library altera_mf;\n"
            "use altera_mf.altera_mf_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    h : in STD_LOGIC;\n"
            "    i : in STD_LOGIC;\n"
            "    j : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC;\n"
            "    s : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g AND h,\n"
            "    a_out => r\n"
            "  );\n",
            "  w: LCELL port map (\n"
            "    a_in => i AND j,\n"
            "    a_out => s\n"
            "  );\n",
            "end behavioral;\n"
        >>
    };
ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G, H, I], [Q, R, S | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, h, pin(H)},
            {location, i, pin(I)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, w, {lc, X, Y, 2}},
            {location, q, pin(Q)},
            {location, r, pin(R)},
            {location, s, pin(S)}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "library altera_mf;\n"
            "use altera_mf.altera_mf_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    h : in STD_LOGIC;\n"
            "    i : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC;\n"
            "    s : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g,\n"
            "    a_out => r\n"
            "  );\n",
            "  w: LCELL port map (\n"
            "    a_in => h AND i,\n"
            "    a_out => s\n"
            "  );\n",
            "end behavioral;\n"
        >>
    };
ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G, H], [Q, R | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, h, pin(H)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, q, pin(Q)},
            {location, r, pin(R)}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "library altera_mf;\n"
            "use altera_mf.altera_mf_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    h : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g AND h,\n"
            "    a_out => r\n"
            "  );\n",
            "end behavioral;\n"
        >>
    };
ins_lut_outs(Device, Title, X, Y, [A, B, C, D, E, F, G], [Q, R | _]) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, a, pin(A)},
            {location, b, pin(B)},
            {location, c, pin(C)},
            {location, d, pin(D)},
            {location, e, pin(E)},
            {location, f, pin(F)},
            {location, g, pin(G)},
            {location, u, {lc, X, Y, 0}},
            {location, v, {lc, X, Y, 1}},
            {location, q, pin(Q)},
            {location, r, pin(R)}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "library altera_mf;\n"
            "use altera_mf.altera_mf_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    a : in STD_LOGIC;\n"
            "    b : in STD_LOGIC;\n"
            "    c : in STD_LOGIC;\n"
            "    d : in STD_LOGIC;\n"
            "    e : in STD_LOGIC;\n"
            "    f : in STD_LOGIC;\n"
            "    g : in STD_LOGIC;\n"
            "    q : out STD_LOGIC;\n"
            "    r : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n",
            "begin\n"
            "  u: LCELL port map (\n"
            "    a_in => a AND b AND c AND d,\n"
            "    a_out => q\n"
            "  );\n",
            "  v: LCELL port map (\n"
            "    a_in => e AND f AND g,\n"
            "    a_out => r\n"
            "  );\n",
            "end behavioral;\n"
        >>
    }.

%%====================================================================
%% lut_out
%%====================================================================

lut_out(Device, LC, Out) ->
    #{
        title => {LC, to, ioc(Out)},
        device => Device,
        settings => [
            {location, lut, LC},
            {location, q, pin(Out)}
        ],
        vhdl => <<
            "library IEEE;\n"
            "use IEEE.STD_LOGIC_1164.ALL;\n"
            "library altera;\n"
            "use altera.altera_primitives_components.all;\n"
            "library altera_mf;\n"
            "use altera_mf.altera_mf_components.all;\n"
            "\n"
            "entity experiment is\n"
            "  port (\n"
            "    q : out STD_LOGIC\n"
            "  );\n"
            "end experiment;\n"
            "\n"
            "architecture behavioral of experiment is\n"
            "begin\n"
            "  lut: LCELL port map (\n"
            "    a_in => '1',\n"
            "    a_out => q\n"
            "  );\n"
            "end behavioral;\n"
        >>
    }.

%%====================================================================
%% open_drain
%%====================================================================

open_drain(Device, Title, In, Out) ->
    #{
        title => Title,
        device => Device,
        settings => [
            {location, i, pin(In)},
            {location, o, pin(Out)}
        ],
        verilog => <<
            "module experiment (\n"
            "  input wire i,\n"
            "  output wire o\n"
            ");\n"
            "  opndrn pad (.in(i), .out(o));\n"
            "endmodule\n"
        >>
    }.

%%====================================================================
%% helpers
%%====================================================================

ioc(IOC = {ioc, _, _, _}) ->
    IOC;
ioc({IOC = {ioc, _, _, _}, _}) ->
    IOC.

%%--------------------------------------------------------------------

pin(Pin) when is_atom(Pin) ->
    Pin;
pin({_, Pin}) when is_atom(Pin) ->
    Pin.

