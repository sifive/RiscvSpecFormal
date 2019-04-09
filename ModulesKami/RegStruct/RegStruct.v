Require Import Kami.All.

Local Open Scope kami_expr.
Local Open Scope kami_action.

Definition Struct_RegReads' ty n (k: Fin.t n -> Kind) (s: Fin.t n -> string): ActionT ty (Struct k s) :=
  fold_left (fun acc i =>
               LETA tmp <- acc;
                 Read val: (k i) <- (s i);
                 Ret (UpdateStruct #tmp i #val))%kami_action
            (getFins n) (Ret (Const ty (getDefaultConst (Struct k s)))).

Definition Struct_RegWrites' ty n (k: Fin.t n -> Kind) (s: Fin.t n -> string) (e: Struct k s @# ty): ActionT ty Void :=
  fold_left (fun acc i =>
               LETA _ <- acc;
                 Write (s i) : (k i) <- ReadStruct e i ;
                 Retv)
            (getFins n) Retv.

Definition Struct_RegReads ty k: ActionT ty k :=
  match k return ActionT ty k with
  | Struct _ ki si => Struct_RegReads' ty ki si
  | Bool => Ret (Const ty (getDefaultConst Bool))
  | Bit n => Ret (Const ty (getDefaultConst (Bit n)))
  | Array n k => Ret (Const ty (getDefaultConst (Array n k)))
  end.

Definition Struct_RegWrites ty k (e: k @# ty): ActionT ty Void:=
  match k return k @# ty -> ActionT ty Void with
  | Struct _ ki si => fun e => Struct_RegWrites' e
  | Bool => fun _ => Retv
  | Bit n => fun _ => Retv
  | Array n k => fun _ => Retv
  end e.

Definition MayStruct_RegReads' ty n (s: Fin.t n -> string) (vals: Fin.t n -> {k: Kind & option (ConstT k)}):
  ActionT ty (Struct (fun i => projT1 (vals i)) s) :=
  fold_left (fun acc i =>
               LETA tmp <- acc;
                 match projT2 (vals i) with
                 | Some uval =>
                   LET val: projT1 (vals i) <- Const ty uval;
                     Ret (UpdateStruct #tmp i #val)
                 | None =>
                   Read val : projT1 (vals i) <- s i;
                     Ret (UpdateStruct #tmp i #val)
                 end)%kami_action
            (getFins n) (Ret (Const ty (getDefaultConst (Struct _ _)))).
  
Definition MayStruct_RegWrites' ty n (s: Fin.t n -> string) (vals: Fin.t n -> {k: Kind & option (ConstT k)})
  (e: Struct (fun i => projT1 (vals i)) s @# ty):
  ActionT ty Void :=
  fold_left (fun acc i =>
               LETA _ <- acc;
                 match projT2 (vals i) with
                 | Some uval =>
                   Retv
                 | None =>
                   Write (s i) : projT1 (vals i) <- ReadStruct e i ;
                     Retv
                 end)%kami_action
            (getFins n) Retv.
Local Close Scope kami_action.
Local Close Scope kami_expr.

Notation "name :: ty" := (name%string, existT (fun k => option (ConstT k)) ty None) (only parsing) : kami_maystruct_scope.
Notation "name ::# ty #:: val " := (name%string, existT (fun k => option (ConstT k)) ty (Some val))
                                      (only parsing, at level 99): kami_maystruct_scope.

Delimit Scope kami_maystruct_scope with kami_maystruct.

Record MayStruct n := { vals  : Fin.t n -> {k: Kind & option (ConstT k)} ;
                        names : Fin.t n -> string }.

Definition getMayStruct n ls: MayStruct n :=
  {| vals  := fun i => snd (Vector.nth ls i) ;
     names := fun i => fst (Vector.nth ls i)
  |}.

Notation "'MAYSTRUCT' { s1 ; .. ; sN }" :=
  (getMayStruct (Vector.cons _ s1%kami_maystruct _ .. (Vector.cons _ sN%kami_maystruct _ (Vector.nil _)) ..)).

Definition MayStruct_RegReads ty n (v: MayStruct n) := MayStruct_RegReads' ty (names v) (vals v).
Definition MayStruct_RegWrites (ty: Kind -> Type) n (v: MayStruct n) e := MayStruct_RegWrites' (ty := ty) (s := names v) (vals v) e.





(* Definition MayStruct_RegReads' ty n (k: Fin.t n -> Kind) (s: Fin.t n -> string) (vals: forall i, option (ConstT (k i))): *)
(*   ActionT ty (Struct k s) := *)
(*   fold_left (fun acc i => *)
(*                LETA tmp <- acc; *)
(*                  match vals i with *)
(*                  | Some uval => *)
(*                    LET val : (k i) <- $$ uval; *)
(*                      Ret (UpdateStruct #tmp i #val) *)
(*                  | None => *)
(*                    Read val : (k i) <- s i; *)
(*                      Ret (UpdateStruct #tmp i #val) *)
(*                  end)%kami_action *)
(*             (getFins n) (Ret (Const ty (getDefaultConst (Struct _ _)))). *)
  
(* Definition MayStruct_RegWrites' ty n (k: Fin.t n -> Kind) (s: Fin.t n -> string) (vals: forall i, option (ConstT (k i))) *)
(*   (e: Struct k s @# ty): *)
(*   ActionT ty Void := *)
(*   fold_left (fun acc i => *)
(*                LETA _ <- acc; *)
(*                  match vals i with *)
(*                  | Some uval => *)
(*                    Retv *)
(*                  | None => *)
(*                    Write (s i) : (k i) <- ReadStruct e i ; *)
(*                      Retv *)
(*                  end)%kami_action *)
(*             (getFins n) Retv. *)

