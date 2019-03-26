#include "verilated.h"
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <getopt.h>
#include "verilated_vcd_c.h"

#include "Vtop_proc_core_mem_reg_file.h"
#include "Vtop_top.h"
#include "Vtop.h"

int main(int argc, char ** argv, char **env) {
  Verilated::commandArgs(argc, argv);
  Verilated::traceEverOn(true);

  Vtop* top = new Vtop;
  VerilatedVcdC* tfp = new VerilatedVcdC;
  top->trace(tfp,99);
  tfp->open("trace.vcd");

  vluint64_t main_time = 0;

  uint32_t timeout = 5000;

  uint32_t pass_address, fail_address = 0, sign_size;
  bool hasfail, finished;
  std::string testfile, signature;

  uint64_t numBlockBytes = (1<<20)-1;

  finished = false;

  VL_VALUEPLUSARGS_INN(-1, "signature=%s", signature);

  FILE* signature_fd = fopen(signature.c_str(), "w");

  if(signature_fd == NULL) {
    fprintf(stderr, "Can't open signature file %s\n", signature.c_str());
    exit(-1);
  }
  
  VL_VALUEPLUSARGS_INI(32, "sign_size=%d", sign_size);
  
  VL_VALUEPLUSARGS_INI(32, "pass_address=%h", pass_address);
  if(VL_VALUEPLUSARGS_INI(32, "fail_address=%h", fail_address)) {
    hasfail = true;
  } else {
    hasfail = false;
  }
  
  while(!Verilated::gotFinish() && main_time < timeout && !finished){
    printf("MainTime = %lu\n", main_time);
    top->CLK = main_time%2;
    if(main_time < 10)
      top->RESET = 1;
    else top->RESET = 0;

    top->eval();
    tfp->dump(main_time);

    if(top->proc_core_pc__024_enable) {
      if(top->proc_core_pc__024_argument == pass_address) {
	fprintf(stderr, "Passed at address: %x\n", pass_address);
        finished = true;
      }	else if(hasfail && top->proc_core_pc__024_argument == fail_address) {
	fprintf(stderr, "FAILED FAILED FAILED FAILED FAILED FAILED!\n");
        finished = true;
      }
      fflush(stdout);
    }
    main_time++;
  }

  if (main_time >= timeout) {
    fprintf(stderr, "Simulation Timed Out\n");
  }

  
  for(int i = numBlockBytes-sign_size+1; i < numBlockBytes; i+=4) {
    for(int j = 3; j >= 0; j--) {
      fprintf(signature_fd, "%02x", top->top->proc_core_mem_reg_file__024_inst->proc_core_mem_reg_file__024_data[i+j]);
    }
    fprintf(signature_fd, "\n");
  }
  
  top->final();
  tfp->close();
  delete top;
  delete tfp;
  return 0;
}
