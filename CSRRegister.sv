/*
  This model simulates a CSR register file (used by the RISC-V Zicsr extension).
*/
module csr_register (
  input         CLK,
  input         RESET,
  input         in_read_csr_enable,
  input         in_write_csr_enable,
  input         in_write_fcsr_enable,
  input [11:0]  in_read_csr_select,
  input [11:0]  in_write_csr_select,
  input [31:0]  in_write_csr_data,
  input [31:0]  in_write_fcsr_data,
  output [31:0] out_read_csr_data
);

parameter integer num_registers = 4096; // 2^12
reg [31:0] registers [(num_registers - 1):0];

parameter integer fcsr_index = 3;

// read operations.
assign out_read_csr_data = registers [in_read_csr_select];
// assign out_read_csr_data = registers [0];
always @(posedge CLK)
begin
  $write("[CSRRegister] read csr sel %d\n", in_read_csr_select);
  $write("[CSRRegister] read csr val %d\n", out_read_csr_data);
end

// write csr operation.
always @(posedge CLK)
begin
  if (in_write_csr_enable && !RESET)
  begin
    registers [in_write_csr_select] <= in_write_csr_data;
    // registers [0] <= in_write_csr_data;
    $write("[CSRRegister] write csr sel %d\n", in_write_csr_select);
    $write("[CSRRegister] write csr val %d\n", in_write_csr_data);
  end

  if (in_write_fcsr_enable && !RESET)
  begin
    registers [fcsr_index] <= in_write_fcsr_data;
  end
end
endmodule
