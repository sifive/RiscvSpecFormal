#include "verilated.h"
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <getopt.h>
#include "verilated_vcd_c.h"

#include "Vsystem.h"

int main(int argc, char ** argv, char **env) {
  int c;
  while (1) {
    int this_option_optind = optind ? optind : 1;
    int option_index = 0;
    static struct option long_options[] = {
        {"help",    no_argument,       0,  0 },
        {0,         0,                 0,  0 }
    };

    c = getopt_long(argc, argv, "hv", long_options, &option_index);
    if (c == -1) break;

    switch (c) {
      case 'h':
        printf("./VSystem [OPTIONS] [PASS ADDRESS] [FAIL ADDRESS]\n\nExample: Vsystem -v 8000010C\n");
        exit (0);
        break;
      case '?':
        break;
      default:
        printf("?? getopt returned character code 0%o ??\n", c);
    }
  }

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

  while(!Verilated::gotFinish() && main_time < timeout){
    top->CLK = main_time%2;
    if(main_time < 10)
      top->RESET = 1;
    else top->RESET = 0;

    top->eval();
    tfp->dump(main_time);
    int arg_index;
    for (arg_index = 1; arg_index < argc; arg_index ++) {
      int halt_address = strtol (argv [arg_index], NULL, 16);
      if(top->pc_enable && top->pc == halt_address) {
        printf("\033[31;1mFinished at address: %x\033[0m\n", halt_address);
        if (arg_index == 1) {
          printf("Passed\n");
        } else {
          printf("Failed\n");
        }
        fflush(stdout);
        top->final();
        tfp->close();
        delete top;
        delete tfp;
        return 0;
      }
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
