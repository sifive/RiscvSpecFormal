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
  uint32_t timeout = 50000;

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

    system->CLK = 1;
    system->RESET = main_time < 10;
    system->eval();

    if (!system->RESET) {
      if(system->proc_core_pc__024_enable) {
        if(system->proc_core_pc__024_argument == pass_address) {
          fprintf(stdout, "Passed\n");
	        fprintf(stderr, "Passed\n");
          finished = true;
        }	else if(hasfail && system->proc_core_pc__024_argument == fail_address) {
          fprintf(stdout, "FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED\n");
          fprintf(stderr, "FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED\n");
          finished = true;
          result=1;
        }
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

  
  for(int i = numBlockBytes-sign_size+1; i < numBlockBytes; i+=4) {
    for(int j = 3; j >= 0; j--) {
      fprintf(signature_fd, "%02x", system->system->proc_core_mem_reg_file__024_inst->proc_core_mem_reg_file__024_data[i+j]);
    }
    fprintf(signature_fd, "\n");
  }
  
  system->final();
  tfp->close();
  delete system;
  delete tfp;
  return result;
}
