module experiment(
	input CLK
);


reg[7:0] bus;

always @(*) begin
	if (ir_en) begin
		bus = ir_out;
	end else if (adder_en) begin
		bus = adder_out;
	end else if (a_en) begin
		bus = a_out;
	end else if (mem_en) begin
		bus = mem_out;
	end else if (pc_en) begin
		bus = pc_out;
	end else begin
		bus = 8'b0;
	end
end

wire rst;
wire hlt;
wire clk;
clock clock(
	.hlt(hlt),
	.clk_in(CLK),
	.clk_out(clk)
);

wire pc_inc;
wire pc_en;
wire[7:0] pc_out;
pc pc(
	.clk(clk),
	.rst(rst),
	.inc(pc_inc),
	.out(pc_out)
);


wire mar_load;
wire mem_en;
wire[7:0] mem_out;
memory mem(
	.clk(clk),
	.rst(rst),
	.load(mar_load),
	.bus(bus),
	.out(mem_out)
);


wire a_load;
wire a_en;
wire[7:0] a_out;
reg_a reg_a(
	.clk(clk),
	.rst(rst),
	.load(a_load),
	.bus(bus),
	.out(a_out)
);


wire b_load;
wire[7:0] b_out;
reg_b reg_b(
	.clk(clk),
	.rst(rst),
	.load(b_load),
	.bus(bus),
	.out(b_out)
);


wire adder_sub;
wire adder_en;
wire[7:0] adder_out;
adder adder(
	.a(a_out),
	.b(b_out),
	.sub(adder_sub),
	.out(adder_out)
);


wire ir_load;
wire ir_en;
wire[7:0] ir_out;
ir ir(
	.clk(clk),
	.rst(rst),
	.load(ir_load),
	.bus(bus),
	.out(ir_out)
);

controller controller(
	.clk(clk),
	.rst(rst),
	.opcode(ir_out[7:4]),
	.out(
	{
		hlt,
		pc_inc,
		pc_en,
		mar_load,
		mem_en,
		ir_load,
		ir_en,
		a_load,
		a_en,
		b_load,
		adder_sub,
		adder_en
	})
);

endmodule

module clock(
	input hlt,
	input clk_in,
	output clk_out
);

assign clk_out = (hlt) ? 1'b0 : clk_in;

endmodule

module pc(
	input clk,
	input rst,
	input inc,
	output[7:0] out
);

reg[3:0] pc;

always @(posedge clk, posedge rst) begin
	if (rst) begin
		pc <= 4'b0;
	end else if (inc) begin
		pc <= pc + 1;
	end
end

assign out = pc;

endmodule

module memory(
	input clk,
	input rst,
	input load,
	input[7:0] bus,
	output[7:0] out
);

initial begin
	ram[ 0] = 8'h0D;
	ram[ 1] = 8'h1E;
	ram[ 2] = 8'h2F;
	ram[ 3] = 8'hF0;
	ram[ 4] = 8'h00;
	ram[ 5] = 8'h00;
	ram[ 6] = 8'h00;
	ram[ 7] = 8'h00;
	ram[ 8] = 8'h00;
	ram[ 9] = 8'h00;
	ram[10] = 8'h00;
	ram[11] = 8'h00;
	ram[12] = 8'h00;
	ram[13] = 8'h03;
	ram[14] = 8'h04;
	ram[15] = 8'h02;
end

reg[3:0] mar;
reg[7:0] ram[0:15];

always @(posedge clk, posedge rst) begin
	if (rst) begin
		mar <= 4'b0;
	end else if (load) begin
		mar <= bus[3:0];
	end
end

assign out = ram[mar];

endmodule

module reg_a(
	input clk,
	input rst,
	input load,
	input[7:0] bus,
	output[7:0] out
);

reg[7:0] reg_a;

always @(posedge clk, posedge rst) begin
	if (rst) begin
		reg_a <= 8'b0;
	end else if (load) begin
		reg_a <= bus;
	end
end

assign out = reg_a;

endmodule

module reg_b(
	input clk,
	input rst,
	input load,
	input[7:0] bus,
	output[7:0] out
);

reg[7:0] reg_b;

always @(posedge clk, posedge rst) begin
	if (rst) begin
		reg_b <= 8'b0;
	end else if (load) begin
		reg_b <= bus;
	end
end

assign out = reg_b;

endmodule

module adder(
	input[7:0] a,
	input[7:0] b,
	input sub,
	output[7:0] out
);

assign out = (sub) ? a-b : a+b;

endmodule

module ir(
	input clk,
	input rst,
	input load,
	input[7:0] bus,
	output[7:0] out
);

reg[7:0] ir;

always @(posedge clk, posedge rst) begin
	if (rst) begin
		ir <= 8'b0;
	end else if (load) begin
		ir <= bus;
	end
end

assign out = ir;

endmodule

module controller(
	input clk,
	input rst,
	input[3:0] opcode,
	output[11:0] out
);

localparam SIG_HLT       = 11;
localparam SIG_PC_INC    = 10;
localparam SIG_PC_EN     = 9;
localparam SIG_MEM_LOAD  = 8;
localparam SIG_MEM_EN    = 7;
localparam SIG_IR_LOAD   = 6;
localparam SIG_IR_EN     = 5;
localparam SIG_A_LOAD    = 4;
localparam SIG_A_EN      = 3;
localparam SIG_B_LOAD    = 2;
localparam SIG_ADDER_SUB = 1;
localparam SIG_ADDER_EN  = 0;

localparam OP_LDA = 4'b0000;
localparam OP_ADD = 4'b0001;
localparam OP_SUB = 4'b0010;
localparam OP_HLT = 4'b1111;

reg[2:0]  stage;
reg[11:0] ctrl_word;

always @(negedge clk, posedge rst) begin
	if (rst) begin
		stage <= 0;
	end else begin
		if (stage == 5) begin
			stage <= 0;
		end else begin
			stage <= stage + 1;
		end
	end
end

always @(*) begin
	ctrl_word = 12'b0;

	case (stage)
		0: begin
			ctrl_word[SIG_PC_EN] = 1;
			ctrl_word[SIG_MEM_LOAD] = 1;
		end
		1: begin
			ctrl_word[SIG_PC_INC] = 1;
		end
		2: begin
			ctrl_word[SIG_MEM_EN] = 1;
			ctrl_word[SIG_IR_LOAD] = 1;
		end
		3: begin
			case (opcode)
				OP_LDA: begin
					ctrl_word[SIG_IR_EN] = 1;
					ctrl_word[SIG_MEM_LOAD] = 1;
				end
				OP_ADD: begin
					ctrl_word[SIG_IR_EN] = 1;
					ctrl_word[SIG_MEM_LOAD] = 1;
				end
				OP_SUB: begin
					ctrl_word[SIG_IR_EN] = 1;
					ctrl_word[SIG_MEM_LOAD] = 1;
				end
				OP_HLT: begin
					ctrl_word[SIG_HLT] = 1;
				end
			endcase
		end
		4: begin
			case (opcode)
				OP_LDA: begin
					ctrl_word[SIG_MEM_EN] = 1;
					ctrl_word[SIG_A_LOAD] = 1;
				end
				OP_ADD: begin
					ctrl_word[SIG_MEM_EN] = 1;
					ctrl_word[SIG_B_LOAD] = 1;
				end
				OP_SUB: begin
					ctrl_word[SIG_MEM_EN] = 1;
					ctrl_word[SIG_B_LOAD] = 1;
				end
			endcase
		end
		5: begin
			case (opcode)
				OP_ADD: begin
					ctrl_word[SIG_ADDER_EN] = 1;
					ctrl_word[SIG_A_LOAD] = 1;
				end
				OP_SUB: begin
					ctrl_word[SIG_ADDER_SUB] = 1;
					ctrl_word[SIG_ADDER_EN] = 1;
					ctrl_word[SIG_A_LOAD] = 1;
				end
			endcase
		end
	endcase
end

assign out = ctrl_word;

endmodule

