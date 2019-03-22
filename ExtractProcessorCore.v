(*
  This script contains the extraction command used to translate the
  RISC-V processor core model into Haskell, which is the first step
  in generating the model's Verilog.
 *)
Require Import Kami.All.
Require Import ProcKami.ProcessorCoreInstance.

Definition rtlMod := rtlModParam "MemoryInit.hex".

Separate Extraction size RtlModule WriteRegFile wordToNat getFins rtlMod.

