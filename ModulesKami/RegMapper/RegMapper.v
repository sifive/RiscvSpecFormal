Require Import Kami.All RegStruct.

Notation divCeil x y := (Nat.div (x + (y - 1)) y).
Notation div_pack8 k := (divCeil (size k) 8).
Notation lg_pack8 k := (Nat.log2_up (divCeil (size k) 8)).
Notation pow2_pack8 k := (pow2 (lg_pack8 k)).

(* Section shit. *)
(*   Context {realAddrSz lgMaskSz: nat}. *)
(*   Variable addr: word (lgMaskSz + realAddrSz). *)
(*   Definition getStartByte := (wordToNat (split1 _ _ addr)). *)
(*   Definition getFinishByte k := (getStartByte + div_pack8 k). *)
(*   Definition getFinishPacket k maskSz := (divCeil (getFinishByte k) maskSz). *)
(*   Definition getFinishPacketByte k maskSz := (getFinishPacket k maskSz * maskSz). *)
(* End shit. *)

(* Eval compute in (@getStartByte 4 2 WO~1~1~0~1~1~1). *)
(* Eval compute in (@getFinishByte 4 2 WO~1~1~0~1~1~1 (Bit 1)). *)
(* Eval compute in (@getFinishPacket 4 2 WO~1~1~0~1~1~1 (Bit 1) 4). *)
(* Eval compute in (@getFinishPacketByte 4 2 WO~1~1~0~1~1~1 (Bit 1) 4). *)

Notation getStartByte addr := (wordToNat (split1 _ _ addr)).
Notation getFinishByte addr k := (getStartByte addr + div_pack8 k).
Notation getFinishPacket addr k maskSz :=
  (divCeil (getFinishByte addr k) maskSz).
Notation getFinishPacketByte addr k maskSz :=
  (getFinishPacket addr k maskSz * maskSz).


Definition putRightPosition ty k (val: k @# ty) start finish :=
  {< $$ (natToWord (finish - (start + size k)) 0), pack val, $$ (natToWord start 0)>}%kami_expr.

Lemma divCeil_ge x y: y <> 0 -> divCeil x y * y >= x.
Proof.
  intros.
  pose proof (Nat.div_mod (x + (y-1)) y ltac:(Omega.omega)) as sth.
  rewrite Nat.mul_comm in sth.
  pose proof (Nat.mod_le (x + (y-1)) _ H) as sth2. 
  assert (sth3: divCeil x y * y = x + (y-1) - ((x + (y-1))mod y)) by Omega.omega.
  Opaque Nat.div.
  simpl.
  rewrite sth3.
  Transparent Nat.div.
  pose proof (Nat.mod_bound_pos (x + (y-1)) y ltac:(Omega.omega) ltac:(Omega.omega)).
  Omega.omega.
Qed.

(* Eval compute in (getFinishPacket (WO~1~1~0~1~1~0: word (2 + 4)) (Bit 20) 4). *)

(* Eval compute in (map (@evalExpr _) (makeSplitBits 4 2 WO~1~1~0~1~1~0 (Const type (WO~1~0~1)))). *)
(* Eval compute in (evalExpr (makeSplitMask 4 2 type WO~1~1~0~1~1~0 (Bit 9)) (Fin.FS Fin.F1)). *)

Definition byteAlign ty k (e: k @# ty): (Bit (div_pack8 k * 8) @# ty).
  refine (castBits _ (ZeroExtend (div_pack8 k * 8 - size k) (pack e))).
  abstract (pose proof (@divCeil_ge (size k) 8 ltac:(Omega.omega)); Omega.omega).
Defined.

Section RegMapper.
  Variable ty: Kind -> Type.
  Variable realAddrSz lgMaskSz: nat.
  
  Local Notation maskSz := (pow2 lgMaskSz).
  Local Notation addrSz := (lgMaskSz + realAddrSz).
  Local Notation dataSz := (maskSz * 8).

  (* For tile-link, addr, mask and size should all be compatible, which is why maskSz, dataSz are powers of 2 *)
  
  Definition RegMapT :=
    STRUCT
      { "addr" :: Bit realAddrSz ;
        "mask" :: Bit maskSz ;
        "data" :: Bit dataSz }.

  Definition FullRegMapT :=
    STRUCT { "isRd" :: Bool ;
             "info" :: RegMapT }.

  Record GenRegField :=
    { grf_addr  : word realAddrSz ;
      grf_mask  : Bit maskSz @# ty;
      grf_read  : RegMapT @# ty -> ActionT ty (Bit dataSz) ;
      grf_write : RegMapT @# ty -> ActionT ty Void }.

  Local Open Scope kami_action.
  Local Open Scope kami_expr.
  Definition createRegMap (rq: Maybe FullRegMapT @# ty) (ls: list GenRegField): ActionT ty (Bit dataSz) :=
    If rq @% "valid"
    then (If rq @% "data" @% "isRd"
          then GatherActions (map (fun x =>
                                     If (rq @% "data" @% "info" @% "addr") ==
                                     ($$ (grf_addr x))
                                       && ((rq @% "data" @% "info" @% "mask" & (grf_mask x)) != $ 0)
                                   then (LETA retVal <- grf_read x (rq @% "data" @% "info");
                                           Ret #retVal)
                                   else Ret ($$ (natToWord dataSz 0))
                                    as retVal;
                                     Ret #retVal) ls) as listVals;
                 Ret (CABit Bor listVals)
          else GatherActions (map (fun x =>
                                     If (rq @% "data" @% "info" @% "addr") ==
                                     ($$ (grf_addr x))
                                       && ((rq @% "data" @% "info" @% "mask" & (grf_mask x)) != $ 0)
                                   then grf_write x (rq @% "data" @% "info")
                                   else Retv; Retv) ls) as _;
            Ret ($$ (natToWord dataSz 0))
            as retVal;
            Ret #retVal)
  else Ret $0
  as retVal;
  Ret #retVal.
  Local Close Scope kami_expr.
  Local Close Scope kami_action.

  Definition makeSplitBits (addr: word addrSz) (k: Kind) (e: k @# ty): Array (getFinishPacket addr k maskSz) (Bit dataSz) @# ty.
    refine
      (unpack (Array (getFinishPacket addr k maskSz) (Bit dataSz))
              (castBits _ (putRightPosition (byteAlign e) (getStartByte addr * 8) (getFinishPacketByte addr k maskSz * 8)))).
    Opaque Nat.div.
    abstract (
        simpl;
        assert (divCeil (getStartByte addr + (size k + 7) / 8) maskSz * maskSz * 8 >= getStartByte addr * 8 + (size k + 7) / 8 * 8) by
            (pose proof (divCeil_ge (getStartByte addr + div_pack8 k) (pow2_ne_zero lgMaskSz)); simpl in *;
             lia);
        rewrite Nat.mul_assoc;
        lia).
    Transparent Nat.div.
  Defined.

  Definition makeSplitMask (addr: word addrSz) (k: Kind): Array (getFinishPacket addr k maskSz) (Bit maskSz) @# ty.
    refine
      (unpack (Array (getFinishPacket addr k maskSz) (Bit maskSz))
              (castBits _ (putRightPosition ($$ (wones (getFinishByte addr k - getStartByte addr)))%kami_expr
                                            (getStartByte addr) (getFinishPacketByte addr k maskSz)))).
    Opaque Nat.div.
    abstract (
        simpl;
        assert (divCeil (getStartByte addr + (size k + 7) / 8) maskSz * maskSz >= getStartByte addr + (size k + 7) / 8) by
            (pose proof (divCeil_ge (getStartByte addr + div_pack8 k) (pow2_ne_zero lgMaskSz)); simpl in *;
             lia);
        lia).
    Transparent Nat.div.
  Defined.

  Definition makeJoinBits (addr: word addrSz) (k: Kind) (e: Array (getFinishPacket addr k maskSz) (Bit dataSz) @# ty): k @# ty.
    refine
      (unpack k (UniBit
                   (TruncLsb _ (div_pack8 k * 8 - size k))
                   (castBits
                      _
                      (ConstExtract (getStartByte addr * 8)
                                    (div_pack8 k * 8)
                                    (getFinishPacket addr k maskSz * dataSz - getFinishByte addr k * 8)
                                    (castBits _ (pack e)))))).
    abstract (pose proof (@divCeil_ge (size k) 8 ltac:(Omega.omega)); Omega.omega).
    Opaque Nat.div.
    abstract (
        simpl;
        pose proof (@divCeil_ge (getStartByte addr + (size k + 7) / 8) maskSz (pow2_ne_zero lgMaskSz));
        nia).
    Transparent Nat.div.
  Defined.

  Record SimpleRegGroup :=
    { srg_addr  : word addrSz ;
      srg_kind  : Kind ;
      srg_read  : ActionT ty srg_kind ;
      srg_write : (srg_kind @# ty) -> ActionT ty Void;
      srg_name  : option string 
    }.

  Local Notation "x @## y" := (x ++ "_" ++ natToHexStr (proj1_sig (Fin.to_nat y)))%string (at level 0).

  Local Notation "'CallDebug' x @@@ y (@ z @) ; cont" := (match srg_name x with
                                                          | None => cont
                                                          | Some x' => Call (x' ++ "_" ++ y)((z): _); cont
                                                          end)%kami_action (at level 100).

  Definition expandRqMask (m: Bit maskSz @# ty): Bit dataSz @# ty.
    refine (castBits _ (pack (BuildArray (fun i => replicate (ReadArrayConst (unpack (Array maskSz (Bit 1)) (castBits _ m)) i) 8))));
      abstract (auto; simpl; lia).
  Defined.

  (* Eval compute in (evalExpr (unpack (Array 2 (Bit 1)) (Const type WO~1~0)%kami_expr) (Fin.FS Fin.F1)). *)

  (* Eval compute in (evalExpr (pack (unpack (Array 2 (Bit 1)) (Const type WO~1~0)%kami_expr))). *)

  Local Open Scope kami_action.
  Local Open Scope kami_expr.
  Definition readWriteBytes_Gen (x: SimpleRegGroup): list GenRegField :=
    map (fun y => {| grf_addr  := (split2 _ realAddrSz (srg_addr x) ^+ $(proj1_sig (Fin.to_nat y)))%word ;
                     grf_mask  := ReadArrayConst (makeSplitMask (srg_addr x) (srg_kind x)) y ;
                       (* $$(wones maskSz) << $$ (split1 _ realAddrSz (srg_addr x)) ; *)
                       (* natToWord maskSz (proj1_sig (Fin.to_nat y)) ; *)
                     grf_read  :=
                       fun rm =>
                         (LETA readK: srg_kind x <- srg_read x ;
                            LET readVal: Bit dataSz <-
                                             ReadArrayConst (makeSplitBits (srg_addr x) #readK)
                                             y;
                            LET maskVal: Bit dataSz <-
                                             ReadArrayConst
                                             (makeSplitBits
                                                (srg_addr x)
                                                (Const ty (wones (size (srg_kind x)))))
                                             y;
                            LET finalVal: Bit dataSz <- expandRqMask (rm @% "mask") & #readVal & #maskVal;
                            (* CallDebug x @@@ "addrR" @## y (@ $$ (split2 _ realAddrSz (srg_addr x) ^+ $(proj1_sig (Fin.to_nat y)))%word @) ; *)
                            (* CallDebug x @@@ "maskR" @## y (@ (ReadArrayConst (makeSplitMask (srg_addr x) (srg_kind x)) y) @); *)
                            (* CallDebug x @@@ "readValR" @## y (@ #readVal @); *)
                            (* CallDebug x @@@ "maskValR" @## y (@ #maskVal @); *)
                            (* CallDebug x @@@ "finalValR" @## y (@ #finalVal @); *)
                            Ret #finalVal) ;
                     grf_write :=
                       fun rm =>
                         (LETA readK: srg_kind x <- srg_read x ;
                            LET readVal <- makeSplitBits (srg_addr x) #readK;
                            LET maskVal <- makeSplitBits (srg_addr x)
                                (Const ty (wones (size (srg_kind x))));
                            LET maskVali <- ReadArrayConst #maskVal y;
                            LET t1Val <- (expandRqMask (rm @% "mask") & #maskVali) & rm @% "data";
                            LET t2Val <- (~(expandRqMask (rm @% "mask") & #maskVali)) & (ReadArrayConst # readVal y);
                            LET t3Val <- #t1Val | #t2Val;
                            LET finalVal <- UpdateArrayConst #readVal y #t3Val;
                            (* CallDebug x @@@ "addrW" @## y *)
                            (*           (@ {< $$ (split2 _ realAddrSz (srg_addr x) ^+ $(proj1_sig (Fin.to_nat y)))%word, $$WO~0~0 >} @); *)
                            (* CallDebug x @@@ "addrBaseW" @## y *)
                            (*           (@ {< $$ (split2 _ realAddrSz (srg_addr x))%word, $$WO~0~0 >} @); *)
                            (* CallDebug x @@@ "addrOffsetW" @## y *)
                            (*           (@ {< $$ (natToWord 4 (proj1_sig (Fin.to_nat y)))%word >} @); *)
                            (* CallDebug x @@@ "expandW" @## y (@ expandRqMask (rm @% "mask") @); *)
                            (* CallDebug x @@@ "rqMaskW" @## y (@ rm @% "mask" @); *)
                            (* CallDebug x @@@ "rqAddrW" @## y (@ rm @% "addr" @); *)
                            (* CallDebug x @@@ "rqDataW" @## y (@ rm @% "data" @); *)
                            (* CallDebug x @@@ "maskW" @## y (@ (ReadArrayConst (makeSplitMask (srg_addr x) (srg_kind x)) y) @); *)
                            (* CallDebug x @@@ "readValW" @## y (@ #readVal @); *)
                            (* CallDebug x @@@ "maskValW" @## y (@ #maskVal @); *)
                            (* CallDebug x @@@ "maskValiW" @## y (@ #maskVali @); *)
                            (* CallDebug x @@@ "t1ValW" @## y (@ #t1Val @); *)
                            (* CallDebug x @@@ "t2ValW" @## y (@ #t2Val @); *)
                            (* CallDebug x @@@ "testValW" @## y (@ #t3Val @); *)
                            (* CallDebug x @@@ "finalValW" @## y (@ #finalVal @); *)
                            srg_write x (makeJoinBits (srg_addr x) (srg_kind x) #finalVal)
                         )
                  |})
         (getFins (getFinishPacket (srg_addr x) (srg_kind x) maskSz)).

  Definition readWriteBytes rq ls :=
    createRegMap rq (concat (map readWriteBytes_Gen ls)).

  Record SingleReg :=
    { sr_addr : word addrSz ;
      sr_kind : Kind ;
      sr_name : sum (string * bool) (ConstT sr_kind)}.

  Definition SingleReg_Gen (x: SingleReg) := {| srg_addr  := sr_addr x ;
                                                srg_kind  := sr_kind x ;
                                                srg_read  :=
                                                  match sr_name x with
                                                  | inl (name, _) =>
                                                    (Read val : (sr_kind x) <- name ;
                                                       Ret #val)
                                                  | inr uval =>
                                                    Ret ($$ uval)
                                                  end ;
                                                srg_write :=
                                                  fun val =>
                                                    match sr_name x with
                                                    | inl (name, true) =>
                                                      (Write name : (sr_kind x) <- val ;
                                                         Retv)
                                                    | _ =>
                                                      Retv
                                                    end ;
                                                srg_name := match sr_name x with
                                                            | inl (name, _) => Some name
                                                            | _ => None
                                                            end
                                             |}.

  Definition readWriteBytes_SingleReg rq ls := createRegMap rq (concat (map (fun x => readWriteBytes_Gen (SingleReg_Gen x)) ls)).

  (* Record SingleNonReg := *)
  (*   { snr_addr : word addrSz ; *)
  (*     snr_kind : Kind ; *)
  (*     snr_val  : ConstT snr_kind }. *)

  (* Definition readWriteBytes_SingleNonReg_Gen (x: SingleNonReg) := *)
  (*   readWriteBytes_Gen {| srg_addr  := snr_addr x ; *)
  (*                         srg_kind  := snr_kind x ; *)
  (*                         srg_read  := Ret (Const ty (snr_val x)) ; *)
  (*                         srg_write := fun val => Retv |}. *)

  (* Definition readWriteBytes_SingleNonReg rq ls := createRegMap rq (concat (map readWriteBytes_SingleNonReg_Gen ls)). *)
  
  Record GroupReg :=
    { gr_addr : word addrSz ;
      gr_kind : Kind ;
      gr_name : string
    }.

  Definition GroupReg_Gen (x: GroupReg) := {| srg_addr  := gr_addr x ;
                                              srg_kind  := gr_kind x ;
                                              srg_read  := Struct_RegReads ty (gr_kind x) ;
                                              srg_write := fun val => Struct_RegWrites val ;
                                              srg_name  := Some (gr_name x)
                                           |}.
  
  Definition readWriteBytes_GroupReg rq ls := createRegMap rq (concat (map (fun x => readWriteBytes_Gen (GroupReg_Gen x)) ls)).

  Record MayGroupReg :=
    { mgr_addr : word addrSz ;
      mgr_size : nat ;
      mgr_kind : MayStruct mgr_size ;
      mgr_name : string
    }.

  Definition MayStruct_Struct n (x: MayStruct n) := Struct (fun i => projT1 (vals x i)) (names x).


  Definition MayGroupReg_Gen (x: MayGroupReg) := {| srg_addr  := mgr_addr x ;
                                                    srg_kind  := MayStruct_Struct (mgr_kind x) ;
                                                    srg_read  := MayStruct_RegReads ty (mgr_kind x) ;
                                                    srg_write := fun val => MayStruct_RegWrites (mgr_kind x) val ;
                                                    srg_name  := Some (mgr_name x)
                                                 |}.

  Definition readWriteBytes_MayGroupReg rq ls := createRegMap rq (concat (map (fun x => readWriteBytes_Gen (MayGroupReg_Gen x)) ls)).

  
  Local Close Scope kami_expr.
  Local Close Scope kami_action.

End RegMapper.

(* Lemma helper_pow2_pack8 k: (pow2_pack8 k * 8 >= size k)%nat. *)
(* Proof. *)
(*   remember (size k) as x; clear Heqx. *)
(*   pose proof (@divCeil_ge x 8 ltac:(Omega.omega)) as sth. *)
(*   pose proof (log2_up_pow2 (divCeil x 8)). *)
(*   Omega.omega. *)
(* Qed. *)

(* Fixpoint wordSplitter' n t: word (t * n) -> list (word n) := *)
(*   match t return word (t * n) -> list (word n) with *)
(*   | 0 => fun _ => nil *)
(*   | S m => fun w => split1 n (m * n) w :: @wordSplitter' _ m (split2 n (m * n) w) *)
(*   end. *)

(* Definition wordSplitter n (pf: n <> 0) sz (w: word sz): list (word n). *)
(*   refine *)
(*     (wordSplitter' n (divCeil sz n) (nat_cast word _ ({< natToWord (divCeil sz n * n - sz) 0, w>})%word)). *)
(*   abstract (pose proof (divCeil_ge sz pf); *)
(*             Omega.omega). *)
(* Defined. *)

(* Fixpoint exprSplitter' ty n t: (Bit (t * n) @# ty) -> list (Bit n @# ty) := *)
(*     match t return Bit(t * n) @# ty -> list (Bit n @# ty) with *)
(*     | 0 => fun _ => nil *)
(*     | S m => fun w => UniBit (TruncLsb n (m * n)) w :: @exprSplitter' _ _ m (UniBit (TruncMsb n (m * n)) w) *)
(*     end. *)

(* Definition exprSplitter ty n (pf: n <> 0) sz (w: Bit sz @# ty): list (Bit n @# ty). *)
(*   refine *)
(*     (exprSplitter' n (divCeil sz n) (castBits _ ({< Const ty (natToWord (divCeil sz n * n - sz) 0), w>})%kami_expr)). *)
(*   abstract (pose proof (divCeil_ge sz pf); *)
(*             Omega.omega). *)
(* Defined. *)

(* Fixpoint convertBoolsToWord ls: word (length ls) := *)
(*   match ls return word (length ls) with *)
(*   | nil => WO *)
(*   | x :: xs => WS x (convertBoolsToWord xs) *)
(*   end. *)

(* Definition byteAlignMask k: word (div_pack8 k * 8). *)
(*   refine (nat_cast word _ (combine (wones (size k)) (natToWord (div_pack8 k * 8 - size k) 0))). *)
(*   abstract (pose proof (@divCeil_ge (size k) 8 ltac:(Omega.omega)); Omega.omega). *)
(* Defined. *)

(* Definition putRightPositionWord n (w: word n) start finish := *)
(*   {< (natToWord (finish - (start + n)) 0), w, (natToWord start 0)>}%word. *)



(* Eval compute in (map (@evalExpr _) (@exprSplitter type 3 ltac:(Omega.omega) 4 (Const type WO~0~1~0~1))). *)
(* Eval compute in (map (@evalExpr _) (@exprSplitter type 3 ltac:(Omega.omega) _ (Const type WO~1~1~0~1~1~0~1~0))). *)
(* Eval compute in ((@wordSplitter 3 ltac:(Omega.omega) _ (WO~1~1~0~1~1~0~1~0))). *)

(* Goal True. *)
(*   pose (evalExpr (putRightPosition (Const type (WO~1~1~1)%word) 4 16)). *)
(*   simpl in f. *)
(*   assert (f = ($0)~1~1~1~0~0~0~0). *)
(*   unfold f. *)
(*   auto. *)
(*   auto. *)
(* Defined. *)


