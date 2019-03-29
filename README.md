# RISC-V ISA Formal Specification

## Name

RISC-V ISA Spec in Kami

## Authors

1. Murali Vijayaraghavan (SiFive)
2. Larry Lee (SiFive) 

## Spec sources snapshot

https://github.com/sifive/RiscvSpecFormal

Note that the master branch of the RISC-V Kami Spec only presents the current stable release version.

## Spec sources live development

https://github.com/sifive/RiscvSpecFormal/ProcKami

The idea is to keep the top-level git repository always stable, and link it to the stable version of the submodule constantly.

## Licence

Apache 2.0

## Metalanguage

The RISC-V Kami Spec is a RISC-V processor model written in Gallina and Ltac. Gallina is a functional programming language that uses higher order types to express logical assertions and proofs. Ltac is a procedural macro language used principlally to write proofs that compiles into Gallina. Using Gallina and Ltac, programmers may describe algorithms, express claims about those algorithms, and write proofs that support those claims. Developments written in Gallina and Ltac can be compiled and verified using Coq, a general purpose proof assistant.

The RISC-V Kami Spec uses the Kami library to model a RISC-V processor. The Kami library provides a domain specific language designed to describe and model digital circuits. Originally developed at MIT, SiFive has extended the Kami library to include a Verilog code generator that can generate Verilog code that represents circuits described using Kami's DSL. The RISC-V processor presented here uses Kami's DSL.

The complete processor model includes register and memory modules written in Verilog. These modules can be linked to the Verilog code generated from the Kami model to produce a fully functioning RISC-V processor model in Verilog.

## Dependencies on what tools

The RISC-V Kami Spec relies on the following tools/libraries:

1. Coq 8.9.x
2. Kami
3. Verilator >= 4.014 (for compiling and simulating verilog)
4. BBV (Bedrock Bit Vector) -- it implements the bit-vector library in Coq
5. coq-record-update -- it implements notation for updating structures in place
6. clang (C-compiler that can run verilator generated C code fast)
7. GHC 8.4.x (Haskell compiler to generate verilog)

## Tool ecosystem: what other work has been/is being done with those tools

The product of over 30 years of development, Coq is a leading proof assistant and is under active development. It has been used to develop formally verified C compilers (see the CompCert project), and to prove numerous mathematical theorems. 

The result of over 5 years of development, Kami was created at MIT to model digital circuits and has since been developed by SiFive. It has been used to model and verify processors developed at SiFive.

The BBV and coq-record-update libraries are fairly small libraries maintained by MIT.

The clang compiler, verilator and GHC compiler are all fairly mature open source tools.

## Motivation

SiFive's goal is to do more than just create a functional model of a RISC-V processor. Rather, our goal is to use Coq's general purpose theorem proving features to formalize the RISC-V semantics, to validate our implementation against these semantics, and to verify the correctness of our model implementation using certified programming techniques.

Using Coq and Kami, we can do more than create programs that simulate RISC-V processors; we can create a framework for abstractly representing and reasoning over the behavior of systems using RISC-V.

## Current functional coverage

### Base ISA and extensions

The current model covers RV-IMACF. However, the current implementation can easily be extended to support a larger subset of the RISC-V instruction set. Rather than hardcode the interpretation and behavior of every individual RISC-V instruction, the current model uses an extensible instruction database that may be expanded with little to no modification of the model's core. The resulting modularity means that, once the core has become stable, our model can be extended and modified easily to track changes to RISC-V spec.

### Privilege levels
It currently supports only user privilege level, with Machine and Supervisor privilege levels in unstable branches.

### Parameterisation of the model
The extensible instruction database is written in a parameterized manner, which can be instantiated to 32-bits, 64-bits, 128-bits or any other bit-widths for data path. It also supports extensions using a parameter table that lists all the extensions to generate in the formal model.

## Current specification of assembly syntax and encoding

The specification is written as a database, i.e., a list of entries in Gallina, with the specification of each instruction written as a Kami LetExpression. The syntax is discussed in greater detail below.

## Current treatment of concurrency

The implementation model presented here consists of a single RISC-V processor with plans to extend it to multicore systems, implementing the most relaxed version of the memory model. In fact, since Kami is a general purpose hardware specification language, it is trivial to design arbitrarily complex concurrent hardware systems in Kami (see the Multicore system + hierarchical caches written in Kami in the ICFP 2018 Kami paper).

## Current treatment of floating-point

We currently support the entire floating-point instruction set (RVF). The floating point unit is completely implemented in Kami. The unstable version also contains RVD (double-precision floating point).

## What the specification and associated tooling generates and enables

Our model can be compiled to Verilog. Accordingly, it can be simulated and synthesized using standard tools. 

### Emulation

Since it generates verilog, verilator can be used to run the generated verilog

### Use as test oracle in tandem verification
It prints out the instruction packets through various stages (i.e. Fetch, Decode, Execute, Memory, Writeback, even though these stages all together happen in one clock cycle). This trace can be used in tandem verification.

### Theorem-prover definitions that support proof

Our processor model is implemented in Kami. Accordingly, it's behavior is immediately verifiable using Coq. We have developed an elaborate ecosystem for Kami in Coq that includes several tactics to perform proof search and several general theorems about the semantics of Kami that helps in proving designs to be equivalent to the formal specification. In addition, various tools and utilities exist for export Coq definitions and theorems into other proof assistants.

### Use in documentation

Our model's instruction database will provide a description of every instruction in the RISC-V spec. Anyone looking to determine the behavior of a given RISC-V instruction will eventually be able to refer to the instruction database and find a description of its semantics.

[//]: # "### Use in test generation"

[//]: # "### Use for concurrency-model litmus test evaluation"

### Current test coverage

It passes all the "rv32u.-p-.*" tests in riscv-tests

### RISC-V compliance tests

We have a target in the compliance test suite.
https://github.com/riscv/riscv-compliance/tree/master/riscv-target/sifive-formal

[//]: # "### OS boot testing"

[//]: # "### Concurrency litmus test testing"

[//]: # "### Other"

## Plans for future functional coverage

Our aspiration is to eventually cover the entire RISC-V instruction set, including both supervisor and machine modes.

## Plans for long-term access, maintenance, etc.

SiFive plans to sponsor the RISC-V Kami spec for the indefinite future, with all the standard extensions being implemented in this spec.

[//]: # "## Example instructions"

## Documentation for model and tools

### snapshot of "Reading Guide", for those who just want to read it like an ISA manual

RISC-V Spec Kami refers to "instruction databases" to model the behavior of the instructions that it supports. These database entries are referred to as "functional units" internally, and are stored in the following directories: FuncUnits/Alu/*.v, FuncUnits/Fpu/*.v, and FuncUnits/Mem/*.v.

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

Every functional unit has a single "semantic function" which gives the basic operation performed by all of the affiliated instructions. For instance, the "add" functional unit supports several RISC-V instructions such as ADD, SUB, SLT (set less than), etc. This grouping of instructions into functional units enable generating optimized microarchitectures without having to reimplement the data-path functions.

The `instName` field gives the instruction's name. The `extensions` field lists the RISC-V extensions that provide the instruction. The `uniqId` field, specifies the bit encoding that uniquely identifies the instruction. Finally `instHints` specifies the registers read or written by the instruction.

The remaining three fields, `inputXform`, `outputXform`, and `optMemXform` are more complicated.

`inputXform` specify how the values stored within various registers are transformed before being processed by the semantic function associated with the instruction's functional unit. `outputXform` specifies how the values returned by the semantic function are transformed before being written to various registers. And, `optMemXform` takes as input the output of the semantic function and the value returned by memory (for load/store/AMO instructions), and provide the value written to the register and to the memory. Note that of non memory instructions `optMemXform` is omitted.

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
  instHints := falseHints{*hasFrs1 := true*}{*hasFrs2 := true*}{*hasFrd := true*}
|};
```

Anyone may refer to these instruction database files to learn about the instructions defined within the RISC-V ISA.

### snapshot of "How to Compile/Run Guide" for those who want to execute a model on programs (ISA tests, Compliance tests, other programs)

Our package includes two scripts for building and running our simulator program.

To get all the submodules first, type `git submodule update --init`.

To build the simulator from source simply run: `./doGenerate.sh`. See `./doGenerate.sh --help` for more information about building the program.

To run RISC-V binaries within the simulator, simply run: `./runElf.sh $PATH/file.elf`, where `$PATH` represents the directory containing the RISC-V binary and `file.elf` represents the RISC-V binary.

To run the suite of RISC-V binaries supported by the simulator, simply run: `./runTests.sh $PATH`, where `$PATH` represents the directory where the RISC-V binaries reside. The file `runTests.sh` lists all the binaries currently running on the simulator.

In summary:
```
$ git submodule update --init
$ ./doGenerate.sh
$ ./runElf.sh $PATH/file.elf
$ ./runTests.sh $PATH
```

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
  fuInsts := [ ... ]
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
  instHints := falseHints{*hasFrs1 := true*}{*hasFrs2 := true*}{*hasFrd := true*}
|}
```

The developer will need to define the `inputXform`, `outputXform`, and `optMemXform` transformation functions.

## Compilation

This package consists of a set of Kami files. The main processor model is written in Kami - a domain specific language embedded within Gallina. The Kami language constructs are defined in Kami. The model is defined in ProcKami.

This model can be used to generate a processor simulator. The standard way to do this is to:

1. Compile the Kami/Coq files
2. "Extract" these files into Haskell
3. Compile the Haskell files to produce the Verilog code generator
4. Run the Verilog code generator to produce a Verilog processor model
5. Pass the Verilog model to Verilator to produce C source code for the processor simulator
6. Compile the C simulator.

The `doGenerate.sh` script performs these steps when building the simulator.
