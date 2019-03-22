(*
  This script contains the extraction command used to translate the
  RISC-V processor core model into Haskell, which is the first step
  in generating the model's Verilog.
 *)
Require Import Kami.All.
Require Import ProcKami.ProcessorCoreInstance.

Separate Extraction Nat.testbit rtlMod size RtlModule WriteRegFile wordToNat getFins.

