/*
  This module simulates a 32 bit register file attached to a processor core.
*/
module register32 (
  parameter DataSz=32,
  input 	CLK,
  input 	RESET,
  input 	in_write_enable,
  input [4:0] 	in_write_register_select,
  input [4:0] 	in_read_register_select_0,
  input [4:0] 	in_read_register_select_1,
  input [4:0] 	in_read_register_select_2,
  input [DataSz:0]  in_write_data,
  output [DataSz:0] out_read_data_0,
  output [DataSz:0] out_read_data_1,
  output [DataSz:0] out_read_data_2
);

parameter integer num_registers = 32;
reg [DataSz:0] register_bank [(num_registers - 1):0];

// load operations.
assign out_read_data_0 = register_bank [in_read_register_select_0];
assign out_read_data_1 = register_bank [in_read_register_select_1];
assign out_read_data_2 = register_bank [in_read_register_select_2];

// store operations.
always @(posedge CLK)
begin
  if (in_write_enable && !RESET)
  begin
    register_bank [in_write_register_select] <= in_write_data;
  end
  $display("[Register] Reg Value in Verilog 0: %d %d", in_read_register_select_0, out_read_data_0);
  $display("[Register] Reg Value in Verilog 1: %d %d", in_read_register_select_1, out_read_data_1);
  $display("[Register] Reg Value in Verilog 2: %d %d", in_read_register_select_2, out_read_data_2);
  $display("[Register] Reg Write Value in Verilog: %d %d %d", in_write_enable, in_write_register_select, in_write_data);
end
endmodule
