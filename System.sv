`include "Processor.sv"

module system(
	      input 	    CLK,
	      input 	    RESET,
	      output [31:0] pc,
	      output 	    pc_enable
	      );

   top system (
	       .proc_core_pc$_enable(pc_enable),
	       .proc_core_pc$_argument(pc),

	       .CLK(CLK),
	       .RESET(RESET)
	       );

endmodule
