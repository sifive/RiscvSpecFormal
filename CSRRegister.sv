/*
  This model simulates a CSR register file (used by the RISC-V Zicsr extension).
*/
module csr_register (
  input         CLK,
  input         RESET,
  input         in_read_csr_enable,
  input         in_read_fcsr_enable,
  input         in_write_csr_enable,
  input         in_write_fcsr_enable,
  input [11:0]  in_read_csr_select,
  input [11:0]  in_write_csr_select,
  input [31:0]  in_write_csr_data,
  input [31:0]  in_write_fcsr_data,
  output [31:0] out_read_csr_data,
  output [31:0] out_read_fcsr_data
);

parameter integer num_registers = 4096; // 2^12
reg [31:0] registers [(num_registers - 1):0];

parameter integer fflags_index = 1;
parameter integer frm_index = 2;
parameter integer fcsr_index = 3;

// read operations.
always
begin
  $write("[CSRRegister] read csr sel %d\n", in_read_csr_select);
  assign out_read_fcsr_data = registers [fcsr_index];
  $write("[CSRRegister] read fcsr val %d\n", registers [fcsr_index]);
  if (in_read_csr_select == fflags_index)
  begin
    // read the fflags field within the fcsr register.
    out_read_csr_data = (registers [fcsr_index] & 32'h1F);
    $write("[CSRRegister] read fflags NEW 3 %d\n", out_read_csr_data);
  end else if (in_read_csr_select == frm_index)
  begin
    // read the frm field within the fcsr register.
    out_read_csr_data = ((registers [fcsr_index] >> 5) & 7);
    $write("[CSRRegister] read frm %d\n", out_read_csr_data);
  end else
  begin
    out_read_csr_data = registers [in_read_csr_select];
    $write("[CSRRegister] read other csr %d\n", out_read_csr_data);
  end
end

// write csr operation.
always @(posedge CLK)
begin
  if (in_write_csr_enable & !RESET)
  begin
    if (in_write_csr_select == fflags_index)
    begin
      // write to the fflags field within the fcsr register.
      $write("[CSRRegister] write fflags: %x\n", ((registers [fcsr_index] & 32'hFFFFFFE0) | (in_write_csr_data & 31)));
      registers [fcsr_index] = (registers [fcsr_index] & 32'hFFFFFFE0) | (in_write_csr_data & 31);
    end else if (in_write_csr_select == frm_index)
    begin
      // write to the frm field within the fcsr register.
      $write("[CSRRegister] write frm deleted: %x\n", ((registers [fcsr_index] & 32'hFFFFFF1F)));
      $write("[CSRRegister] write frm anded: %x\n", (in_write_csr_data & 7));
      $write("[CSRRegister] write frm shifted: %x\n", ((in_write_csr_data & 7) << 5));
      $write("[CSRRegister] write frm result: %x\n", ((registers [fcsr_index] & 32'hFFFFFF1F) | ((in_write_csr_data & 7) << 5)));
      registers [fcsr_index] = (registers [fcsr_index] & 32'hFFFFFF1F) | ((in_write_csr_data & 7) << 5);
    end else
    begin
      $write("[CSRRegister] write default");
      registers [in_write_csr_select] = in_write_csr_data;
    end
    $write("[CSRRegister] write csr sel %d\n", in_write_csr_select);
    $write("[CSRRegister] write csr val %d\n", in_write_csr_data);
  end

  if (in_write_fcsr_enable & !RESET)
  begin
    registers [fcsr_index] = in_write_fcsr_data;
    $write("[CSRRegister] write fcsr: %x\n", in_write_fcsr_data);
  end
  $write("[CSRRegister] fcsr new val %d\n", registers [fcsr_index]);
end
endmodule
