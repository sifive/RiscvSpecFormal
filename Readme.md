# RISC-V ISA Formal Specification

## Name

RISC-V Kami Spec

## Authors

1. Murali Vijayaraghavan (SiFive)
2. Kade Phillips (MIT)
3. Larry Lee (SiFive) 

## Spec sources snapshot

https://github.com/sifive/RiscvSpecKami

Note that the master branch of the RISC-V Kami Spec only presents the current stable release version.

## Spec sources live development

https://github.com/sifive/RiscvSpecKami

## Licence

## Metalanguage

The RISC-V Kami Spec is a RISC-V processor model written in Gallina and Ltac. Gallina is a functional programming language that uses higher order types to express logical assertions and proofs. Ltac is a procedural macro language used principlally to write proofs that compiles into Gallina. Using Gallina and Ltac, programmers may describe algorithms, express claims about those algorithms, and write proofs that support those claims. Developments written in Gallina and Ltac can be compiled and verified using Coq, a general purpose proof assistant.

The RISC-V Kami Spec uses the Kami library to model a RISC-V processor. The Kami library provides a domain specific language designed to describe and model digital circuits. Originally developed at MIT, SiFive has extended the Kami library to include a Verilog code generator that can generate Verilog code that represents circuits described using Kami's DSL. The RISC-V processor presented here uses Kami's DSL.

The complete processor model includes register and memory modules written in Verilog. These modules can be linked to the Verilog code generated from the Kami model to produce a fully functioning RISC-V processor model in Verilog.

## Dependencies on what tools

The RISC-V Kami Spec relies on the following tools/libraries:

1. Coq
2. Kami
3. Verilog compilation and simulation tools

## Tool ecosystem: what other work has been/is being done with those tools

The product of over 30 years of development, Coq is a leading proof assistant and is under active development. It has been used to develop formally verified C compilers (see the CompCert project), and to prove numerous mathematical theorems. 

The result of over 5 years of development, Kami was created to model digital circuits and has been sponsored by SiFive. It has been used to model and verify processors developed at SiFive and is being developed in conjunction with MIT.

## Motivation

SiFive's goal is to do more than just create a functional model of a RISC-V processor. Rather, our goal is to use Coq's general purpose theorem proving features to formalize the RISC-V semantics, to validate our model against these semantics, and to verify the correctness of our model implementation using certified programming techniques.

Using Coq and Kami, we can do more than create programs that simulate RISC-V processors; we can create a framework for abstractly representing and reasoning over the behavior of systems using RISC-V.

## Current functional coverage

The current model covers RV-IF. However, the current implementation can easily be extended to support a larger subset of the RISC-V instruction set. Rather than hardcode the interpretation and behavior of every individual RISC-V instruction, the current model uses an extensible instruction database that may be expanded with little to no modification of the model's core. The resulting modularity means that, once the core has become stable, our model can be extended and modified easily to track changes to RISC-V spec.

## Current specification of assembly syntax and encoding

## Current treatment of concurrency

The implementation model presented here consists of a single RISC-V processor. 

## Current treatment of floating-point

We currently support the entire floating-point instruction set (RVF). 

## Current capabilities

Our model can be compiled to Verilog. Accordingly, it can be simulated and synthesized using standard tools. 

### Emulation

### Use as test oracle in tandem verification

### Theorem-prover definitions that support proof

Our processor model is implemented in Kami. Accordingly, it's behavior is immediately verifiable using Coq. In addition, various tools and utilities exist for export Coq definitions and theorems into other proof assistants.

### Use in documentation

Our model's instruction database will provide a description of every instruction in the RISC-V spec. Anyone looking to determine the behavior of a given RISC-V instruction will eventually be able to refer to the instruction database and find a description of its semantics.

### Use in test generation

### Use for concurrency-model litmus test evaluation

## Current test coverage

### RISC-V compliance tests

### OS boot testing

### Concurrency litmus test testing

### Other

## Plans for future functional coverage

Our aspiration is to eventually cover the entire RISC-V instruction set, including both user and machine modes.

## Plans for long-term access, maintenance, etc.

SiFive plans to sponsor the RISC-V Kami spec for the indefinite future. 

## Example instructions

## Documentation for model and tools

### snapshot of "Reading Guide", for those who just want to read it like an ISA manual

RISC-V Spec Kami refers to "instruction databases" to model the behavior of the instructions that it supports. These database entries are referred to as "functional units" internally, and are stored in the following files: Alu.v, Fpu.v, and Mem.v.

Each of these contains a datastructure that lists various RISC-V instructions. Each instruction entry has the following format:

```
{|
  instName    = ...
  extensions  = ...
  uniqId      = ...
  inputXform  = ...
  outputXform = ...
  optMemXform = ...
  instHints   = ...
|}
```

Every functional unit has a single "semantic function" which gives the basic operation performed by all of the affiliated instructions. 

The `instName` field gives the instruction's name. The `extensions` field lists the RISC-V extensions that provide the instruction. The `uniqId` field, specifies the bit ranges in the instruction's encoding that uniquely identify the instruction. Finally `instHints` specifies the registers used by the instruction.

The remaining three fields, `inputXform`, `outputXform`, and `optMemXform` are more complicated.

`inputXform` specify how the values stored within various registers are transformed before being processed by the semantic function associated with the instruction's functional unit. `outputXform` specifies how the values returned by the semantic function are transformed before being written to various registers. And, `optMemXform` specifies how values returned by the semantic function are transformed before being written to memory. Note that of non store instructions `optMemXform` is omitted.

For example, Fpu.v contains the following entry for the `fnadd.s` instruction:

```
{|
  instName   := "fadd.s";
  extensions := ["RV32F"; "RV64F"];
  uniqId
    := [
         fieldVal instSizeField ('b"11");
         fieldVal opcodeField   ('b"10011");
         fieldVal fmtField      ('b"00");
         fieldVal funct7Field   ('b"0000000")
       ];
  inputXform  := add_in_pkt $0;
  outputXform := muladd_out_pkt;
  optMemXform := None;
  instHints := falseHints[[hasFrs1 := true]][[hasFrs2 := true]][[hasFrd := true]] 
|};
```

Anyone may refer to these instruction database files to learn about the instructions defined within the RISC-V ISA.

Eventually, we will refine and collect these instruction databases into a single file to improve readability.

### snapshot of "How to Compile/Run Guide" for those who want to execute a model on programs (ISA tests, Compliance tests, other programs)

Our package includes two scripts for building and running our simulator program.

To build the simulator from source simply run: `./doGenerate.sh`. See `./doGenerate.sh --help` for more information about building the program.

To run RISC-V binaries within the simulator, simply run: `.runELF.sh PATH`.

The following provides further details about the process used to build our processor model from source.

Our processor model is implemented using Kami. To generate a Verilog module, you must complete the following steps:

1. Compile the Kami modules using Coqc

This will generate a Haskell file named Target.hs

2. Compile PrettyPrintVerilog.hs using GHC

This will produce a Verilog generator that outputs the processor model described in Target.hs.

3. Generate Verilog by running PrettyPrintVerilog

This will generate Processor.sv which represents the processor core.

4. Load System.sv into a Verilog simulator, etc.

At this point you can load System.sv into a Verilog simulator or any other program. System.sv includes both the processor model defined in Processor.sv and the memory and register file models contained in MemoryX.sv and RegisterX.sv and combines them into a functioning system.

MemoryX.sv reads a hex file named "MemoryInit.hex" and loads the data stored therein into the memory of the system being simulated.

To simplify the process outlined above, we provide a script `build.sh` that performs steps 1-3 and returns a Verilog module that represents a RISC-V processor core (Processor.sv).

We use Verilator to simulate the execution of our processor model. Verilator compiles Verilog files into C code that, when compiled and run, simulates the behavior of the given Verilog modules. The program should be linked to System.cpp, which generates a trace of the processor while it runs. This program stops once the simulator reaches a given timeout measured in clock cycles.

To simplify running RISC-V programs, we include an additional script named `runELF.sh`.

Lastly, we provide a script named `runRVtests.sh` to compile and simulate tests from the RISC-V test suite. 

### snapshot of "How to Extend Guide" for those who want to extend the model to capture new ISA extensions/experiments.

To add additional RISC-V instructions to our model, developers must add instruction entries to our instruction databases.

The first step is to determine which functional unit to should be used to execute the new instruction.

Every functional unit has an associated "semantic function". A semantic function is a generic function. For example, the "fcmp" functional unit has a semantic function that compares two floating point numbers.

If none of the functional units can execute the new instruction, the developer needs to create a new functional unit entry.

A function unit has the following format:

```
{|
  fuName  := NAME;
  fuFunc  := SEMANTICFUNCTION;
  fuInsts := []
|}.
```

`fuName` is a string that represents the functional unit's name. `fuFunc` is a Gallina function that represents the functional unit's semantic function. `fuInsts` is a list of instruction entries.

Once the developer has selected, or created, the functional unit whose semantic function performs the given operation, they need to create an instruction entry. For example:

```
{|
  instName   := "fadd.s";
  extensions := ["RV32F"; "RV64F"];
  uniqId
    := [
         fieldVal instSizeField ('b"11");
         fieldVal opcodeField   ('b"10011");
         fieldVal fmtField      ('b"00");
         fieldVal funct7Field   ('b"0000000")
       ];
  inputXform  := add_in_pkt $0;
  outputXform := muladd_out_pkt;
  optMemXform := None;
  instHints := falseHints[[hasFrs1 := true]][[hasFrs2 := true]][[hasFrd := true]] 
|}
```

The developer will need to define the `inputXform`, `outputXform`, and `optMemXform` transformation functions.
