#include "verilated.h"
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <getopt.h>
#include "verilated_vcd_c.h"

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

  uint32_t timeout = 100;

  while(!Verilated::gotFinish() && main_time < timeout){
    printf("MainTime = %lu\n", main_time);

    system->CLK = 1;
    system->RESET = main_time < 10;
    system->eval();

    tfp->dump(main_time++);
    system->CLK = 0;
    system->eval();
    tfp->dump(main_time++);

    fflush(stdout);
  }

  system->final();
  tfp->close();
  delete system;
  delete tfp;
  return 0;
}
