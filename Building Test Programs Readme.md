Risc-V Build Tool Modification Readme
=====================================

In order to compile this version of the RISC-V processor model using
the RISC-V build tools, you must modify the RISC-V test linker script
so that the starting address is 0x0 instead of 0x80000000. The new
linker script is listed below. Replace riscv-tests/env/p/link.ld
with the file contents given below:

```
OUTPUT_ARCH( "riscv" )
ENTRY(_start)

SECTIONS
{
  . = 0x00000000;
  .text.init : { *(.text.init) }
  . = ALIGN(0x1000);
  .tohost : { *(.tohost) }
  . = ALIGN(0x1000);
  .text : { *(.text) }
  . = ALIGN(0x1000);
  .data : { *(.data) }
  .bss : { *(.bss) }
  _end = .;
}
```
