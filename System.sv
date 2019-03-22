/*
 This module combines the register, memory, and procesor cores
 into a composite system model.
 */
`include "Processor.sv"
`include "Memory32.sv"
//`include "Register.sv"
//`include "CSRRegister.sv"

module system(
	      input 	    CLK,
	      input 	    RESET,
	      output [31:0] pc,
	      output 	    pc_enable
	      );

   // Fetch wires.

   logic 		    fetch_enable;
   logic [31:0] 	    fetch_address_req;
   logic [31:0] 	    fetch_res;
   
   // Register read wires.

   wire 		 logic read_reg_1_enable_req;
   wire 		 logic read_reg_2_enable_req;
   wire 		 logic read_freg_1_enable_req;
   wire 		 logic read_freg_2_enable_req;
   wire 		 logic read_freg_3_enable_req;
   
   wire 		 logic[4:0] read_reg_1_id_req;
   wire 		 logic[4:0] read_reg_2_id_req;
   wire 		 logic[4:0] read_freg_1_id_req;
   wire 		 logic[4:0] read_freg_2_id_req;
   wire 		 logic[4:0] read_freg_3_id_req;
   
   wire 		 logic[31:0] read_reg_1_res;
   wire 		 logic[31:0] read_reg_2_res;
   wire 		 logic[31:0] read_freg_1_res;
   wire 		 logic[31:0] read_freg_2_res;
   wire 		 logic[31:0] read_freg_3_res;

   // Register write wires.

   struct 		 packed {
      logic [4:0] 	 index;
      logic [31:0] 	 data;
   } proc_core_regWrite_req;
   
   struct 		 packed {
      logic [4:0] 	 index;
      logic [31:0] 	 data;
   } proc_core_fregWrite_req;
   
   struct 		 packed {
      logic [11:0] 	 index;
      logic [31:0] 	 data;
   } proc_core_csrWrite_req;
   
   wire 		 logic proc_core_regWrite_enable_req;
   wire 		 logic proc_core_fregWrite_enable_req;
   wire 		 logic proc_core_csrWrite_enable_req;
   
   // CSR read wires.
   
   wire 		 logic read_csr_en_req;
   wire 		 logic [11:0] read_csr_sel_req;
   wire 		 logic [31:0] read_csr_data_res;

   wire 		 logic read_fcsr_en_req;
   wire 		 logic [31:0] read_fcsr_data_res;

   // CSR write wires.

   wire 		 logic write_csr_en_req;
   wire 		 logic [11:0] write_csr_sel_req;
   wire 		 logic [31:0] write_csr_data_req;

   assign write_csr_sel_req = proc_core_csrWrite_req.index;
   assign write_csr_data_req = proc_core_csrWrite_req.data;

   wire 		 logic write_fcsr_en_req;
   wire 		 logic [31:0] write_fcsr_data_req;

   // Memory wires

   wire 		 logic[31:0] memRead_address_req;
   wire 		 logic memRead_enable_req;

   // TODO: does this match PktWithException MemRead in FU.v?
   wire                  logic [31:0] memRead_res;
   
   struct 		 packed {
      logic [31:0] 	 addr;
      logic [31:0] 	 data;
   } memWrite_req;

   wire 		 logic memWrite_enable_req;

   struct 		 packed {
      logic 		 valid;
      logic [3:0] 	 data;
   } memWrite_res;

   // System components and connections

   top system (
	       .proc_core_readMem1$_enable(fetch_enable),
	       .proc_core_readMem1$_argument(fetch_address_req),
	       .proc_core_readMem1$_return(fetch_res),

	       .proc_core_readMem2$_enable(memRead_enable_req),
	       .proc_core_readMem2$_argument(memRead_address_req),
	       .proc_core_readMem2$_return(memRead_res),

	       .proc_core_memWrite$_enable(memWrite_enable_req),
	       .proc_core_memWrite$_argument(memWrite_req),

	       .proc_core_pc$_enable(pc_enable),
	       .proc_core_pc$_argument(pc),

	       .CLK(CLK),
	       .RESET(RESET)
	       );

   (* TODO: wire up exceptions. *)

   wire 		    ram_void1;
   wire 		    ram_void2;
   wire 		    ram_void3;

   memory32 ram (
		 .CLK (CLK),
		 .RESET (RESET),
		 .in_fetch_enable (fetch_enable),
		 .in_write_enable (memWrite_enable_req),
		 .in_fetch_address (fetch_address_req),
		 .in_read_address (memRead_address_req),
		 .in_write_address (memWrite_req.addr),
		 .in_write_data (memWrite_req.data),
		 .out_fetch_data (fetch_res),
		 .out_read_data (memRead_res),
		 .out_fetch_exception (ram_void1),
		 .out_read_exception (ram_void2),
		 .out_write_exception (ram_void3)
		 );

   wire 		    logic [4:0] register_void0;
   wire 		    logic [31:0] register_void1;

   always @(posedge CLK)
     begin
	$write ("[System] read csr en %b\n", read_csr_en_req);
	$write ("[System] read csr sel %d\n", read_csr_sel_req);
	$write ("[System] read csr val %d\n", read_csr_data_res);
     end

endmodule
