(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

Require Coq.extraction.Extraction.
Extraction Language OCaml.

From GB Require Import U4.
From GB Require Import U8.
From GB Require Import U16.
From GB Require Import Cpu_v.

Extract Inductive unit => "unit" ["()"].

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

(******************************************************************************)

Extraction "cpu_v.ml" Cpu_v.create Cpu_v.tick.

