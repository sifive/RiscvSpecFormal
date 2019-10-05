#include "verilated.h"
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <getopt.h>
#include "verilated_vcd_c.h"

#include "Vsystem_proc_core_mem_reg_file.h"
#include "Vsystem_system.h"
#include "Vsystem.h"

int main(int argc, char ** argv, char **env) {
  Verilated::commandArgs(argc, argv);
  Verilated::traceEverOn(true);

  Vsystem* system = new Vsystem;
  VerilatedVcdC* tfp = new VerilatedVcdC;
  system->trace(tfp,99);
  tfp->open("trace.vcd");

  vluint64_t main_time = 0;

  int result = 0;
  uint32_t timeout = 110000;

  uint32_t tohost_address, tohost_val;
  uint32_t  sign_size;
  bool finished;
  std::string testfile, signature;

  uint64_t numBlockBytes = (1<<20)-1;

  finished = false;

  VL_VALUEPLUSARGS_INN(-1, "signature=%s", signature);

  VL_VALUEPLUSARGS_INI(32, "sign_size=%d", sign_size);
  
  VL_VALUEPLUSARGS_INI(32, "tohost_address=%h", tohost_address);
  
  while(!Verilated::gotFinish() && main_time < timeout && !finished){
    printf("MainTime = %lu\n", main_time);

    system->CLK = 1;
    system->RESET = main_time < 10;
    system->eval();

    tohost_val = 0;
    for(int j = 3; j >= 0; j--) {
      tohost_val = (tohost_val << 8) | system->system->proc_core_mem_reg_file__024_inst->proc_core_mem_reg_file__024_data[tohost_address+j];
    }
    if (!system->RESET) {
      if(tohost_val == 1) {
        fprintf(stdout, "Passed\n");
        fprintf(stderr, "Passed\n");
        finished = true;
      } else if(tohost_val != 0) {
        fprintf(stdout, "FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED\n");
        fprintf(stderr, "FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED\n");
        finished = true;
        result=1;
      }
    }
    
    tfp->dump(main_time++);
    system->CLK = 0;
    system->eval();
    tfp->dump(main_time++);

    fflush(stdout);
  }

  if (main_time >= timeout) {
    fprintf(stdout, "TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT\n");
    fprintf(stderr, "TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT\n");
    result=1;
  }

  
  if(signature.c_str() != 0) {
    FILE* signature_fd = fopen(signature.c_str(), "w");
    if(signature_fd != NULL) {
      for(int i = numBlockBytes-sign_size+1; i < numBlockBytes; i+=4) {
        for(int j = 3; j >= 0; j--) {
          fprintf(signature_fd, "%02x", system->system->proc_core_mem_reg_file__024_inst->proc_core_mem_reg_file__024_data[i+j]);
        }
        fprintf(signature_fd, "\n");
      }
    }
  }
  
  system->final();
  tfp->close();
  delete system;
  delete tfp;
  return result;
}
