#include "verilated.h"
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include "verilated_vcd_c.h"

#include "Vsystem.h"

int main(int argc, char ** argv, char **env) {
  Verilated::commandArgs(argc, argv);
  Verilated::traceEverOn(true);

  Vsystem* top = new Vsystem;
  VerilatedVcdC* tfp = new VerilatedVcdC;
  top->trace(tfp,99);
  tfp->open("trace.vcd");

  uint32_t in;
  uint8_t roundMode;

  uint8_t outtop_exp, outchisel_exp, outtop_flags, outchisel_flags;
  uint32_t outtop_sig, outchisel_sig;
  bool outtop_sign, outchisel_sign, tiny;

  vluint64_t main_time = 0;
  int ready_chisel = 0;
  int ready_top = 0;
  uint32_t outtop, outchisel;

  uint32_t timeout = 1<<10;

  int fail_pc = strtol(argv[1], NULL, 16);
  int pass_pc = strtol(argv[2], NULL, 16);

  fprintf(stderr, "Fail PC: %x Pass PC: %x\n", fail_pc, pass_pc);

  while(!Verilated::gotFinish() && main_time < timeout){
    top->CLK = main_time%2;
    if(main_time < 10)
      top->RESET = 1;
    else top->RESET = 0;

    top->eval();
    tfp->dump(main_time);
    if(top->pc_enable && top->pc == fail_pc) {
      fprintf(stderr, "Failed\n");
      
      top->final();
      tfp->close();
      delete top;
      delete tfp;
      return 0;

    } else if(top->pc_enable && top->pc == pass_pc) {
      fprintf(stderr, "Passed\n");

      top->final();
      tfp->close();
      delete top;
      delete tfp;
      return 0;
    }
    main_time++;
  }

  if (main_time >= timeout) {
      printf("\033[31;1mSimulation Timed Out\033[0m\n");
  }

  tfp->dump(main_time);

  top->final();
  tfp->close();
  delete top;
  delete tfp;
  return 0;
}
