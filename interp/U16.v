(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

Require Import Coq.Bool.Bool.
Require Import Coq.Init.Nat.
Require Import Coq.Arith.PeanoNat.
Require Import Omega.

From GB Require Import U4.
From GB Require Import U8.



Definition u16 : Type := u8 * u8.

Definition u16_lo (v: u16): u8 :=
  match v with
  | (lo, _) => lo
  end.

Definition u16_hi (v: u16): u8 :=
  match v with
  | (_, hi) => hi
  end.

Definition inc_wrap_u16 (v: u16): u16 :=
  match v with
  | (lo, hi) =>
    match u8_inc lo with
    | (lo', c) =>
      match c with
      | false => (lo', hi)
      | true =>
        match u8_inc hi with
        | (hi', _) => (lo', hi')
        end
      end
    end
  end.

Definition dec_wrap_u16 (v: u16): u16 :=
  match v with
  | (lo, hi) =>
    match u8_dec lo with
    | (lo', c) =>
      match c with
      | false => (lo', hi)
      | true =>
        match u8_dec hi with
        | (hi', _) => (lo', hi')
        end
      end
    end
  end.

Definition u16_add (a: u16) (b: u16): (u16 * bool) :=
  match a with
  | (a_lo, a_hi) =>
    match b with
    | (b_lo, b_hi) =>
      match u8_add a_lo b_lo with
      | (lo, false) =>
        match u8_add a_hi b_hi with
        | (hi, false) => ((lo, hi), false)
        | (hi, true) => ((lo, hi), true)
        end
      | (lo, true) =>
        match u8_add a_hi b_hi with
        | (hi0, c) =>
          match u8_inc hi0 with
          | (hi, c') => ((lo, hi), c || c')
          end
        end
      end
    end
  end.

Definition u16_add_wrap (a: u16) (b: u16): u16 :=
  match u16_add a b with
  | (s, _) => s
  end
  .

Definition u16_is_zero (a: u16): bool :=
  match a with
  | ((x0, x0), (x0, x0)) => true
  | _ => false
  end.

Definition u8_sext_u16 (a: u8) :=
  match u8_msb a with
  | true  => (a, (xf, xf))
  | false => (a, (x0, x0))
  end.

(*
Definition sub_u16 (a: u16) (b: u16): (u16 * bool) :=
  match a with
  | (a_lo, a_hi) =>
    match b with
    | (b_lo, b_hi) =>
      match sub_u8 a_lo b_lo with
      | (lo, false) =>
        match sub_u8 a_hi b_hi with
        | (hi, false) => ((lo, hi), false)
        |
        end
      | (lo, true) =>

    end
  end.
*)
