#include "verilated.h"
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <getopt.h>
#include "verilated_vcd_c.h"

#include "Vsystem.h"

int main(int argc, char ** argv, char **env) {
  Verilated::commandArgs(argc, argv);
  Verilated::traceEverOn(true);

  Vsystem* top = new Vsystem;
  VerilatedVcdC* tfp = new VerilatedVcdC;
  top->trace(tfp,99);
  tfp->open("trace.vcd");

  vluint64_t main_time = 0;

  uint32_t timeout = 1<<10;

  uint32_t pass_address, fail_address = 0, sign_size;
  bool hasfail, finished;
  std::string testfile, signature;

  finished = false;

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

    if(top->pc_enable) {
      if(top->pc == pass_address) {
	fprintf(stderr, "Passed at address: %x\n", pass_address);
        finished = true;
      }	else if(hasfail && top->pc == fail_address) {
	fprintf(stderr, "Failed at address: %x\n", fail_address);
        finished = true;
      }
      fflush(stdout);
    }
    main_time++;
  }

  if (main_time >= timeout) {
    printf("Simulation Timed Out\n");
  }

  top->final();
  tfp->close();
  delete top;
  delete tfp;
  return 0;
}
