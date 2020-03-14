(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

Require Coq.extraction.Extraction.
Extraction Language OCaml.

From GB Require Import U4.
From GB Require Import U8.
From GB Require Import U16.
From GB Require Import Cpu_v.

Extract Inductive bool => "bool" [ "true" "false" ].

Extract Inductive option => "option" [ "Some" "None" ].

Extract Inductive u4 => "int" [
  "0x00" "0x01" "0x02" "0x03"
  "0x04" "0x05" "0x06" "0x07"
  "0x08" "0x09" "0x0A" "0x0B"
  "0x0C" "0x0D" "0x0E" "0x0F"
].

Extract Inductive interrupt => "System.interrupt" [
  "System.Int_VBlank"
  "System.Int_Stat"
  "System.Int_Timer"
  "System.Int_Serial"
  "System.Int_Pins"
].

(* TODO REMOVE *)
Extract Constant U4.u4_cmp => "(fun a b ->
  if a = b then Eq else if a < b then Lt else Gt
)".

(******************************************************************************)

Extract Constant Cpu_v.System => "System.t".
Extract Constant Cpu_v.sys_tick => "System.tick".

Extract Constant Cpu_v.sys_is_interrupt_pending => "System.is_interrupt_pending".
Extract Constant Cpu_v.sys_is_interrupt_enabled => "System.is_interrupt_enabled".
Extract Constant Cpu_v.sys_clear_interrupt => "System.clear_interrupt".

Extract Constant Cpu_v.sys_read =>
"(fun sys addr ->
  match addr with
  | Pair(Pair(lo_lo, lo_hi), Pair(hi_lo, hi_hi)) ->
    let hi = ((hi_hi lsl 4) lor hi_lo) lsl 8 in
    let lo = ((lo_hi lsl 4) lor lo_lo) lsl 0 in
    match System.read sys (hi lor lo) with
    | None -> None
    | Some v ->
      Some (Pair(Pair(v land 0xF, (v lsr 4) land 0xF), sys))
)".

Extract Constant Cpu_v.sys_write =>
"(fun sys addr v ->
  match addr with
  | Pair(Pair(lo_lo, lo_hi), Pair(hi_lo, hi_hi)) ->
    match v with
    | Pair (v_lo, v_hi) ->
      let hi = ((hi_hi lsl 4) lor hi_lo) lsl 8 in
      let lo = ((lo_hi lsl 4) lor lo_lo) lsl 0 in
      System.write sys (hi lor lo) ((v_hi lsl 4) lor v_lo)
)".

Extraction "cpu_v.ml" Cpu_v.System Cpu_v.create Cpu_v.tick.

