/*
 This module simulates the 32 bit memory attached to the processor core.
 */
module memory32 (
		 input 	       CLK,
		 input 	       RESET,
		 input 	       in_fetch_enable,
		 input 	       in_write_enable,
		 input [31:0]  in_fetch_address,
		 input [31:0]  in_read_address,
		 input [31:0]  in_write_address,
		 input [31:0]  in_write_data,
		 output [31:0] out_fetch_data,
		 output [31:0] out_read_data,
		 output [1:0]  out_reservation, // indicates whether or not a memory reservation was successfull 
		 output        out_fetch_exception,
		 output        out_read_exception,
		 output        out_write_exception
		 );
   parameter int 	       size = 20;
   parameter int 	       numBlockBytes = (2**size-1);
   parameter int 	       startAddr = 2**31;
   
   parameter int 	       numWordBytes = 4;
   logic [size-1:0] 	       in_fetch_addr, in_read_addr, in_write_addr;
   assign in_fetch_addr = in_fetch_address[size-1:0];
   assign in_read_addr = in_read_address[size-1:0];
   assign in_write_addr = in_write_address[size-1:0];
   reg [7:0] 		       block [numBlockBytes:0];
   string 		       testfile;
   string 		       signature = "";
   int unsigned 	       sign_size = 0;
   integer 		       fd;
   bit 			       has_signature = |($value$plusargs("signature=%s", signature));
   bit 			       sig_size_present = |($value$plusargs("sign_size=%d", sign_size));
   typedef logic [31:0]        cmpl_fmt_t;
   initial begin
      $value$plusargs("testfile=%s", testfile);
      $readmemh (testfile, block);
      fd = $fopen("signature", "w");
   end
   function automatic void write_signature_file(int unsigned signature_bytes);
      int cmpl_idx = 0;
      //cmpl_fmt_t cmpl_sig[sign/4];
      for (int idx = numBlockBytes-sign_size+1; idx < numBlockBytes; idx = idx+4) begin
	 $fdisplay(fd, "%x", {block[idx+3], block[idx+2], block[idx+1], block[idx]});
	 cmpl_idx += 1;
      end
      //$writememh(signature, cmpl_sig, 0, (sign_size/4)-1);
   endfunction : write_signature_file
   final begin
      if (has_signature && sig_size_present) begin
	 write_signature_file(sign_size);
      end
      $display("|Finishing up everything|");
      $fclose(fd);
   end
   
   /*
    This memory unit only supports 64 bit word requests. It returns
    an exception for any request that is not aligned to a 64 bit
    word boundary.
    Note: this restriction prevents 
    */
   assign out_fetch_exception = 0; // in_read_addr >= (numBlockBytes - numWordBytes);
   assign out_read_exception = 0; // in_read_addr >= (numBlockBytes - numWordBytes);
   assign out_write_exception = 0; // in_write_addr >= (numBlockBytes - numWordBytes);
   /*
    This memory model executes requests in the order in which they
    are recieved and all harts within the executing environment,
    accordingly see memory operations from other harts executing in
    program order. Accordingly, reservation requests always return
    true.
    */
   assign out_reservation = 2'd2;
   // fetch operations.
   assign out_fetch_data = {
			    block [in_fetch_addr + 3],
			    block [in_fetch_addr + 2],
			    block [in_fetch_addr + 1],
			    block [in_fetch_addr + 0]
			    };
   // load operations.
   assign out_read_data = {
			   block [in_read_addr + 3],
			   block [in_read_addr + 2],
			   block [in_read_addr + 1],
			   block [in_read_addr + 0]
			   };
   /*
    store operations.
    The processor model assumes that each read and write operation
    reads and writes either 32 or 64 bit words depending on the
    processor's XLEN parameter. When the processor needs to write
    smaller units (such as bytes or half-words) it reads the entire
    32/64 bit word, modifies the relevant bits, and writes the entire
    word back as the result.
    */
   always @(posedge CLK)
     begin
	$write ("[Memory] read address %d\n", in_read_address);
	$write ("[Memory] read data %d\n", out_read_data);
	if (in_write_enable && !RESET)
	  begin
	     block [in_write_addr + 3] <= in_write_data [31:24];
	     block [in_write_addr + 2] <= in_write_data [23:16];
	     block [in_write_addr + 1] <= in_write_data [15:8];
	     block [in_write_addr + 0] <= in_write_data [7:0];
	     $write ("[Memory] write address %h\n", in_write_address);
	     $write ("[Memory] write data %d\n", in_write_data);
	  end
     end
endmodule


