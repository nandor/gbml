(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

Require Import Coq.Bool.Bool.
Require Import Coq.Init.Nat.
Require Import Coq.Arith.PeanoNat.

Require Import Omega.

Inductive u4 :=
  | x0
  | x1
  | x2
  | x3
  | x4
  | x5
  | x6
  | x7
  | x8
  | x9
  | xa
  | xb
  | xc
  | xd
  | xe
  | xf
  .

Definition u4_to_bits v :=
  match v with
  | x0 => (false, false, false, false)
  | x1 => ( true, false, false, false)
  | x2 => (false,  true, false, false)
  | x3 => ( true,  true, false, false)
  | x4 => (false, false,  true, false)
  | x5 => ( true, false,  true, false)
  | x6 => (false,  true,  true, false)
  | x7 => ( true,  true,  true, false)
  | x8 => (false, false, false,  true)
  | x9 => ( true, false, false,  true)
  | xa => (false,  true, false,  true)
  | xb => ( true,  true, false,  true)
  | xc => (false, false,  true,  true)
  | xd => ( true, false,  true,  true)
  | xe => (false,  true,  true,  true)
  | xf => ( true,  true,  true,  true)
  end.


Definition u4_of_bits v :=
  match v with
  | (false, false, false, false) => x0
  | ( true, false, false, false) => x1
  | (false,  true, false, false) => x2
  | ( true,  true, false, false) => x3
  | (false, false,  true, false) => x4
  | ( true, false,  true, false) => x5
  | (false,  true,  true, false) => x6
  | ( true,  true,  true, false) => x7
  | (false, false, false,  true) => x8
  | ( true, false, false,  true) => x9
  | (false,  true, false,  true) => xa
  | ( true,  true, false,  true) => xb
  | (false, false,  true,  true) => xc
  | ( true, false,  true,  true) => xd
  | (false,  true,  true,  true) => xe
  | ( true,  true,  true,  true) => xf
  end.


Definition u4_inc (v: u4): (u4 * bool) :=
  match v with
  | x0 => (x1, false)
  | x1 => (x2, false)
  | x2 => (x3, false)
  | x3 => (x4, false)
  | x4 => (x5, false)
  | x5 => (x6, false)
  | x6 => (x7, false)
  | x7 => (x8, false)
  | x8 => (x9, false)
  | x9 => (xa, false)
  | xa => (xb, false)
  | xb => (xc, false)
  | xc => (xd, false)
  | xd => (xe, false)
  | xe => (xf, false)
  | xf => (x0, true)
  end.

Definition u4_dec (v: u4): (u4 * bool) :=
  match v with
  | x0 => (xf, true)
  | x1 => (x0, false)
  | x2 => (x1, false)
  | x3 => (x2, false)
  | x4 => (x3, false)
  | x5 => (x4, false)
  | x6 => (x5, false)
  | x7 => (x6, false)
  | x8 => (x7, false)
  | x9 => (x8, false)
  | xa => (x9, false)
  | xb => (xa, false)
  | xc => (xb, false)
  | xd => (xc, false)
  | xe => (xd, false)
  | xf => (xe, false)
  end.

Definition u4_add (a: u4) (b: u4): (u4 * bool) :=
  match a with
  | x0 =>
    match b with
    | x0 => (x0, false)
    | x1 => (x1, false)
    | x2 => (x2, false)
    | x3 => (x3, false)
    | x4 => (x4, false)
    | x5 => (x5, false)
    | x6 => (x6, false)
    | x7 => (x7, false)
    | x8 => (x8, false)
    | x9 => (x9, false)
    | xa => (xa, false)
    | xb => (xb, false)
    | xc => (xc, false)
    | xd => (xd, false)
    | xe => (xe, false)
    | xf => (xf, false)
    end
  | x1 =>
    match b with
    | x0 => (x1, false)
    | x1 => (x2, false)
    | x2 => (x3, false)
    | x3 => (x4, false)
    | x4 => (x5, false)
    | x5 => (x6, false)
    | x6 => (x7, false)
    | x7 => (x8, false)
    | x8 => (x9, false)
    | x9 => (xa, false)
    | xa => (xb, false)
    | xb => (xc, false)
    | xc => (xd, false)
    | xd => (xe, false)
    | xe => (xf, false)
    | xf => (x0, true)
    end
  | x2 =>
    match b with
    | x0 => (x2, false)
    | x1 => (x3, false)
    | x2 => (x4, false)
    | x3 => (x5, false)
    | x4 => (x6, false)
    | x5 => (x7, false)
    | x6 => (x8, false)
    | x7 => (x9, false)
    | x8 => (xa, false)
    | x9 => (xb, false)
    | xa => (xc, false)
    | xb => (xd, false)
    | xc => (xe, false)
    | xd => (xf, false)
    | xe => (x0, true)
    | xf => (x1, true)
    end
  | x3 =>
    match b with
    | x0 => (x3, false)
    | x1 => (x4, false)
    | x2 => (x5, false)
    | x3 => (x6, false)
    | x4 => (x7, false)
    | x5 => (x8, false)
    | x6 => (x9, false)
    | x7 => (xa, false)
    | x8 => (xb, false)
    | x9 => (xc, false)
    | xa => (xd, false)
    | xb => (xe, false)
    | xc => (xf, false)
    | xd => (x0, true)
    | xe => (x1, true)
    | xf => (x2, true)
    end
  | x4 =>
    match b with
    | x0 => (x4, false)
    | x1 => (x5, false)
    | x2 => (x6, false)
    | x3 => (x7, false)
    | x4 => (x8, false)
    | x5 => (x9, false)
    | x6 => (xa, false)
    | x7 => (xb, false)
    | x8 => (xc, false)
    | x9 => (xd, false)
    | xa => (xe, false)
    | xb => (xf, false)
    | xc => (x0, true)
    | xd => (x1, true)
    | xe => (x2, true)
    | xf => (x3, true)
    end
  | x5 =>
    match b with
    | x0 => (x5, false)
    | x1 => (x6, false)
    | x2 => (x7, false)
    | x3 => (x8, false)
    | x4 => (x9, false)
    | x5 => (xa, false)
    | x6 => (xb, false)
    | x7 => (xc, false)
    | x8 => (xd, false)
    | x9 => (xe, false)
    | xa => (xf, false)
    | xb => (x0, true)
    | xc => (x1, true)
    | xd => (x2, true)
    | xe => (x3, true)
    | xf => (x4, true)
    end
  | x6 =>
    match b with
    | x0 => (x6, false)
    | x1 => (x7, false)
    | x2 => (x8, false)
    | x3 => (x9, false)
    | x4 => (xa, false)
    | x5 => (xb, false)
    | x6 => (xc, false)
    | x7 => (xd, false)
    | x8 => (xe, false)
    | x9 => (xf, false)
    | xa => (x0, true)
    | xb => (x1, true)
    | xc => (x2, true)
    | xd => (x3, true)
    | xe => (x4, true)
    | xf => (x5, true)
    end
  | x7 =>
    match b with
    | x0 => (x7, false)
    | x1 => (x8, false)
    | x2 => (x9, false)
    | x3 => (xa, false)
    | x4 => (xb, false)
    | x5 => (xc, false)
    | x6 => (xd, false)
    | x7 => (xe, false)
    | x8 => (xf, false)
    | x9 => (x0, true)
    | xa => (x1, true)
    | xb => (x2, true)
    | xc => (x3, true)
    | xd => (x4, true)
    | xe => (x5, true)
    | xf => (x6, true)
    end
  | x8 =>
    match b with
    | x0 => (x8, false)
    | x1 => (x9, false)
    | x2 => (xa, false)
    | x3 => (xb, false)
    | x4 => (xc, false)
    | x5 => (xd, false)
    | x6 => (xe, false)
    | x7 => (xf, false)
    | x8 => (x0, true)
    | x9 => (x1, true)
    | xa => (x2, true)
    | xb => (x3, true)
    | xc => (x4, true)
    | xd => (x5, true)
    | xe => (x6, true)
    | xf => (x7, true)
    end
  | x9 =>
    match b with
    | x0 => (x9, false)
    | x1 => (xa, false)
    | x2 => (xb, false)
    | x3 => (xc, false)
    | x4 => (xd, false)
    | x5 => (xe, false)
    | x6 => (xf, false)
    | x7 => (x0, true)
    | x8 => (x1, true)
    | x9 => (x2, true)
    | xa => (x3, true)
    | xb => (x4, true)
    | xc => (x5, true)
    | xd => (x6, true)
    | xe => (x7, true)
    | xf => (x8, true)
    end
  | xa =>
    match b with
    | x0 => (xa, false)
    | x1 => (xb, false)
    | x2 => (xc, false)
    | x3 => (xd, false)
    | x4 => (xe, false)
    | x5 => (xf, false)
    | x6 => (x0, true)
    | x7 => (x1, true)
    | x8 => (x2, true)
    | x9 => (x3, true)
    | xa => (x4, true)
    | xb => (x5, true)
    | xc => (x6, true)
    | xd => (x7, true)
    | xe => (x8, true)
    | xf => (x9, true)
    end
  | xb =>
    match b with
    | x0 => (xb, false)
    | x1 => (xc, false)
    | x2 => (xd, false)
    | x3 => (xe, false)
    | x4 => (xf, false)
    | x5 => (x0, true)
    | x6 => (x1, true)
    | x7 => (x2, true)
    | x8 => (x3, true)
    | x9 => (x4, true)
    | xa => (x5, true)
    | xb => (x6, true)
    | xc => (x7, true)
    | xd => (x8, true)
    | xe => (x9, true)
    | xf => (xa, true)
    end
  | xc =>
    match b with
    | x0 => (xc, false)
    | x1 => (xd, false)
    | x2 => (xe, false)
    | x3 => (xf, false)
    | x4 => (x0, true)
    | x5 => (x1, true)
    | x6 => (x2, true)
    | x7 => (x3, true)
    | x8 => (x4, true)
    | x9 => (x5, true)
    | xa => (x6, true)
    | xb => (x7, true)
    | xc => (x8, true)
    | xd => (x9, true)
    | xe => (xa, true)
    | xf => (xb, true)
    end
  | xd =>
    match b with
    | x0 => (xd, false)
    | x1 => (xe, false)
    | x2 => (xf, false)
    | x3 => (x0, true)
    | x4 => (x1, true)
    | x5 => (x2, true)
    | x6 => (x3, true)
    | x7 => (x4, true)
    | x8 => (x5, true)
    | x9 => (x6, true)
    | xa => (x7, true)
    | xb => (x8, true)
    | xc => (x9, true)
    | xd => (xa, true)
    | xe => (xb, true)
    | xf => (xc, true)
    end
  | xe =>
    match b with
    | x0 => (xe, false)
    | x1 => (xf, false)
    | x2 => (x0, true)
    | x3 => (x1, true)
    | x4 => (x2, true)
    | x5 => (x3, true)
    | x6 => (x4, true)
    | x7 => (x5, true)
    | x8 => (x6, true)
    | x9 => (x7, true)
    | xa => (x8, true)
    | xb => (x9, true)
    | xc => (xa, true)
    | xd => (xb, true)
    | xe => (xc, true)
    | xf => (xd, true)
    end
  | xf =>
    match b with
    | x0 => (xf, false)
    | x1 => (x0, true)
    | x2 => (x1, true)
    | x3 => (x2, true)
    | x4 => (x3, true)
    | x5 => (x4, true)
    | x6 => (x5, true)
    | x7 => (x6, true)
    | x8 => (x7, true)
    | x9 => (x8, true)
    | xa => (x9, true)
    | xb => (xa, true)
    | xc => (xb, true)
    | xd => (xc, true)
    | xe => (xd, true)
    | xf => (xe, true)
    end
  end.

Lemma u4_inc_and_add x : u4_inc x = u4_add x x1.
Proof. destruct x; reflexivity. Qed.

Definition u4_adc (a: u4) (b: u4) (c: bool): (u4 * bool) :=
  match c with
  | false => u4_add a b
  | true =>
    match u4_inc a with
    | (a', c') =>
      match u4_add a' b with
      | (s, c'') => (s, c' || c'')
      end
    end
  end.

Lemma u4_add_adc_no_carry a b : u4_adc a b false = u4_add a b.
Proof. reflexivity. Qed.

Definition u4_sub (a: u4) (b: u4): (u4 * bool) :=
  match b with
  | x0 =>
    match a with
    | x0 => (x0, false)
    | x1 => (x1, false)
    | x2 => (x2, false)
    | x3 => (x3, false)
    | x4 => (x4, false)
    | x5 => (x5, false)
    | x6 => (x6, false)
    | x7 => (x7, false)
    | x8 => (x8, false)
    | x9 => (x9, false)
    | xa => (xa, false)
    | xb => (xb, false)
    | xc => (xc, false)
    | xd => (xd, false)
    | xe => (xe, false)
    | xf => (xf, false)
    end
  | x1 =>
    match a with
    | x0 => (xf, true)
    | x1 => (x0, false)
    | x2 => (x1, false)
    | x3 => (x2, false)
    | x4 => (x3, false)
    | x5 => (x4, false)
    | x6 => (x5, false)
    | x7 => (x6, false)
    | x8 => (x7, false)
    | x9 => (x8, false)
    | xa => (x9, false)
    | xb => (xa, false)
    | xc => (xb, false)
    | xd => (xc, false)
    | xe => (xd, false)
    | xf => (xe, false)
    end
  | x2 =>
    match a with
    | x0 => (xe, true)
    | x1 => (xf, true)
    | x2 => (x0, false)
    | x3 => (x1, false)
    | x4 => (x2, false)
    | x5 => (x3, false)
    | x6 => (x4, false)
    | x7 => (x5, false)
    | x8 => (x6, false)
    | x9 => (x7, false)
    | xa => (x8, false)
    | xb => (x9, false)
    | xc => (xa, false)
    | xd => (xb, false)
    | xe => (xc, false)
    | xf => (xd, false)
    end
  | x3 =>
    match a with
    | x0 => (xd, true)
    | x1 => (xe, true)
    | x2 => (xf, true)
    | x3 => (x0, false)
    | x4 => (x1, false)
    | x5 => (x2, false)
    | x6 => (x3, false)
    | x7 => (x4, false)
    | x8 => (x5, false)
    | x9 => (x6, false)
    | xa => (x7, false)
    | xb => (x8, false)
    | xc => (x9, false)
    | xd => (xa, false)
    | xe => (xb, false)
    | xf => (xc, false)
    end
  | x4 =>
    match a with
    | x0 => (xc, true)
    | x1 => (xd, true)
    | x2 => (xe, true)
    | x3 => (xf, true)
    | x4 => (x0, false)
    | x5 => (x1, false)
    | x6 => (x2, false)
    | x7 => (x3, false)
    | x8 => (x4, false)
    | x9 => (x5, false)
    | xa => (x6, false)
    | xb => (x7, false)
    | xc => (x8, false)
    | xd => (x9, false)
    | xe => (xa, false)
    | xf => (xb, false)
    end
  | x5 =>
    match a with
    | x0 => (xb, true)
    | x1 => (xc, true)
    | x2 => (xd, true)
    | x3 => (xe, true)
    | x4 => (xf, true)
    | x5 => (x0, false)
    | x6 => (x1, false)
    | x7 => (x2, false)
    | x8 => (x3, false)
    | x9 => (x4, false)
    | xa => (x5, false)
    | xb => (x6, false)
    | xc => (x7, false)
    | xd => (x8, false)
    | xe => (x9, false)
    | xf => (xa, false)
    end
  | x6 =>
    match a with
    | x0 => (xa, true)
    | x1 => (xb, true)
    | x2 => (xc, true)
    | x3 => (xd, true)
    | x4 => (xe, true)
    | x5 => (xf, true)
    | x6 => (x0, false)
    | x7 => (x1, false)
    | x8 => (x2, false)
    | x9 => (x3, false)
    | xa => (x4, false)
    | xb => (x5, false)
    | xc => (x6, false)
    | xd => (x7, false)
    | xe => (x8, false)
    | xf => (x9, false)
    end
  | x7 =>
    match a with
    | x0 => (x9, true)
    | x1 => (xa, true)
    | x2 => (xb, true)
    | x3 => (xc, true)
    | x4 => (xd, true)
    | x5 => (xe, true)
    | x6 => (xf, true)
    | x7 => (x0, false)
    | x8 => (x1, false)
    | x9 => (x2, false)
    | xa => (x3, false)
    | xb => (x4, false)
    | xc => (x5, false)
    | xd => (x6, false)
    | xe => (x7, false)
    | xf => (x8, false)
    end
  | x8 =>
    match a with
    | x0 => (x8, true)
    | x1 => (x9, true)
    | x2 => (xa, true)
    | x3 => (xb, true)
    | x4 => (xc, true)
    | x5 => (xd, true)
    | x6 => (xe, true)
    | x7 => (xf, true)
    | x8 => (x0, false)
    | x9 => (x1, false)
    | xa => (x2, false)
    | xb => (x3, false)
    | xc => (x4, false)
    | xd => (x5, false)
    | xe => (x6, false)
    | xf => (x7, false)
    end
  | x9 =>
    match a with
    | x0 => (x7, true)
    | x1 => (x8, true)
    | x2 => (x9, true)
    | x3 => (xa, true)
    | x4 => (xb, true)
    | x5 => (xc, true)
    | x6 => (xd, true)
    | x7 => (xe, true)
    | x8 => (xf, true)
    | x9 => (x0, false)
    | xa => (x1, false)
    | xb => (x2, false)
    | xc => (x3, false)
    | xd => (x4, false)
    | xe => (x5, false)
    | xf => (x6, false)
    end
  | xa =>
    match a with
    | x0 => (x6, true)
    | x1 => (x7, true)
    | x2 => (x8, true)
    | x3 => (x9, true)
    | x4 => (xa, true)
    | x5 => (xb, true)
    | x6 => (xc, true)
    | x7 => (xd, true)
    | x8 => (xe, true)
    | x9 => (xf, true)
    | xa => (x0, false)
    | xb => (x1, false)
    | xc => (x2, false)
    | xd => (x3, false)
    | xe => (x4, false)
    | xf => (x5, false)
    end
  | xb =>
    match a with
    | x0 => (x5, true)
    | x1 => (x6, true)
    | x2 => (x7, true)
    | x3 => (x8, true)
    | x4 => (x9, true)
    | x5 => (xa, true)
    | x6 => (xb, true)
    | x7 => (xc, true)
    | x8 => (xd, true)
    | x9 => (xe, true)
    | xa => (xf, true)
    | xb => (x0, false)
    | xc => (x1, false)
    | xd => (x2, false)
    | xe => (x3, false)
    | xf => (x4, false)
    end
  | xc =>
    match a with
    | x0 => (x4, true)
    | x1 => (x5, true)
    | x2 => (x6, true)
    | x3 => (x7, true)
    | x4 => (x8, true)
    | x5 => (x9, true)
    | x6 => (xa, true)
    | x7 => (xb, true)
    | x8 => (xc, true)
    | x9 => (xd, true)
    | xa => (xe, true)
    | xb => (xf, true)
    | xc => (x0, false)
    | xd => (x1, false)
    | xe => (x2, false)
    | xf => (x3, false)
    end
  | xd =>
    match a with
    | x0 => (x3, true)
    | x1 => (x4, true)
    | x2 => (x5, true)
    | x3 => (x6, true)
    | x4 => (x7, true)
    | x5 => (x8, true)
    | x6 => (x9, true)
    | x7 => (xa, true)
    | x8 => (xb, true)
    | x9 => (xc, true)
    | xa => (xd, true)
    | xb => (xe, true)
    | xc => (xf, true)
    | xd => (x0, false)
    | xe => (x1, false)
    | xf => (x2, false)
    end
  | xe =>
    match a with
    | x0 => (x2, true)
    | x1 => (x3, true)
    | x2 => (x4, true)
    | x3 => (x5, true)
    | x4 => (x6, true)
    | x5 => (x7, true)
    | x6 => (x8, true)
    | x7 => (x9, true)
    | x8 => (xa, true)
    | x9 => (xb, true)
    | xa => (xc, true)
    | xb => (xd, true)
    | xc => (xe, true)
    | xd => (xf, true)
    | xe => (x0, false)
    | xf => (x1, false)
    end
  | xf =>
    match a with
    | x0 => (x1, true)
    | x1 => (x2, true)
    | x2 => (x3, true)
    | x3 => (x4, true)
    | x4 => (x5, true)
    | x5 => (x6, true)
    | x6 => (x7, true)
    | x7 => (x8, true)
    | x8 => (x9, true)
    | x9 => (xa, true)
    | xa => (xb, true)
    | xb => (xc, true)
    | xc => (xd, true)
    | xd => (xe, true)
    | xe => (xf, true)
    | xf => (x0, false)
    end
  end.

Lemma u4_dec_and_sub x : u4_dec x = u4_sub x x1.
Proof. destruct x; reflexivity. Qed.

Definition u4_sbc (a: u4) (b: u4) (c: bool): (u4 * bool) :=
  match c with
  | false => u4_sub a b
  | true =>
    match u4_dec a with
    | (a', c') =>
      match u4_sub a' b with
      | (s, c'') => (s, c' || c'')
      end
    end
  end.

Lemma u4_sub_sbc_no_carry a b : u4_sbc a b false = u4_sub a b.
Proof. reflexivity. Qed.

Definition u4_add_wrap (a: u4) (b: u4) :=
  match u4_add a b with
  | (s, _) => s
  end.

Definition u4_is_zero (a: u4): bool :=
  match a with
  | x0 => true
  | _ => false
  end.

Definition u4_msb (a: u4): bool :=
  match u4_to_bits a with
  | (_, _, _, b) => b
  end.

Definition u4_lsb (a: u4): bool :=
  match u4_to_bits a with
  | (b, _, _, _) => b
  end.


Definition u4_and (a: u4) (b: u4): u4 :=
  match u4_to_bits a with
  | (a0, a1, a2, a3) =>
    match u4_to_bits b with
    | (b0, b1, b2, b3) =>
      u4_of_bits (andb a0 b0, andb a1 b1, andb a2 b2, andb a3 b3)
    end
  end.

Definition u4_xor (a: u4) (b: u4): u4 :=
  match u4_to_bits a with
  | (a0, a1, a2, a3) =>
    match u4_to_bits b with
    | (b0, b1, b2, b3) =>
      u4_of_bits (xorb a0 b0, xorb a1 b1, xorb a2 b2, xorb a3 b3)
    end
  end.

Definition u4_or (a: u4) (b: u4): u4 :=
  match u4_to_bits a with
  | (a0, a1, a2, a3) =>
    match u4_to_bits b with
    | (b0, b1, b2, b3) =>
      u4_of_bits (orb a0 b0, orb a1 b1, orb a2 b2, orb a3 b3)
    end
  end.

Inductive cmp: Type := Lt | Eq | Gt.

Definition u4_cmp (a: u4) (b: u4): cmp :=
  match a with
  | x0 =>
    match b with
    | x0 => Eq
    | x1 => Lt
    | x2 => Lt
    | x3 => Lt
    | x4 => Lt
    | x5 => Lt
    | x6 => Lt
    | x7 => Lt
    | x8 => Lt
    | x9 => Lt
    | xa => Lt
    | xb => Lt
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | x1 =>
    match b with
    | x0 => Gt
    | x1 => Eq
    | x2 => Lt
    | x3 => Lt
    | x4 => Lt
    | x5 => Lt
    | x6 => Lt
    | x7 => Lt
    | x8 => Lt
    | x9 => Lt
    | xa => Lt
    | xb => Lt
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | x2 =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Eq
    | x3 => Lt
    | x4 => Lt
    | x5 => Lt
    | x6 => Lt
    | x7 => Lt
    | x8 => Lt
    | x9 => Lt
    | xa => Lt
    | xb => Lt
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | x3 =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Eq
    | x4 => Lt
    | x5 => Lt
    | x6 => Lt
    | x7 => Lt
    | x8 => Lt
    | x9 => Lt
    | xa => Lt
    | xb => Lt
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | x4 =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Eq
    | x5 => Lt
    | x6 => Lt
    | x7 => Lt
    | x8 => Lt
    | x9 => Lt
    | xa => Lt
    | xb => Lt
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | x5 =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Gt
    | x5 => Eq
    | x6 => Lt
    | x7 => Lt
    | x8 => Lt
    | x9 => Lt
    | xa => Lt
    | xb => Lt
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | x6 =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Gt
    | x5 => Gt
    | x6 => Eq
    | x7 => Lt
    | x8 => Lt
    | x9 => Lt
    | xa => Lt
    | xb => Lt
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | x7 =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Gt
    | x5 => Gt
    | x6 => Gt
    | x7 => Eq
    | x8 => Lt
    | x9 => Lt
    | xa => Lt
    | xb => Lt
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | x8 =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Gt
    | x5 => Gt
    | x6 => Gt
    | x7 => Gt
    | x8 => Eq
    | x9 => Lt
    | xa => Lt
    | xb => Lt
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | x9 =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Gt
    | x5 => Gt
    | x6 => Gt
    | x7 => Gt
    | x8 => Gt
    | x9 => Eq
    | xa => Lt
    | xb => Lt
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | xa =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Gt
    | x5 => Gt
    | x6 => Gt
    | x7 => Gt
    | x8 => Gt
    | x9 => Gt
    | xa => Eq
    | xb => Lt
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | xb =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Gt
    | x5 => Gt
    | x6 => Gt
    | x7 => Gt
    | x8 => Gt
    | x9 => Gt
    | xa => Gt
    | xb => Eq
    | xc => Lt
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | xc =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Gt
    | x5 => Gt
    | x6 => Gt
    | x7 => Gt
    | x8 => Gt
    | x9 => Gt
    | xa => Gt
    | xb => Gt
    | xc => Eq
    | xd => Lt
    | xe => Lt
    | xf => Lt
    end
  | xd =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Gt
    | x5 => Gt
    | x6 => Gt
    | x7 => Gt
    | x8 => Gt
    | x9 => Gt
    | xa => Gt
    | xb => Gt
    | xc => Gt
    | xd => Eq
    | xe => Lt
    | xf => Lt
    end
  | xe =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Gt
    | x5 => Gt
    | x6 => Gt
    | x7 => Gt
    | x8 => Gt
    | x9 => Gt
    | xa => Gt
    | xb => Gt
    | xc => Gt
    | xd => Gt
    | xe => Eq
    | xf => Lt
    end
  | xf =>
    match b with
    | x0 => Gt
    | x1 => Gt
    | x2 => Gt
    | x3 => Gt
    | x4 => Gt
    | x5 => Gt
    | x6 => Gt
    | x7 => Gt
    | x8 => Gt
    | x9 => Gt
    | xa => Gt
    | xb => Gt
    | xc => Gt
    | xd => Gt
    | xe => Gt
    | xf => Eq
    end
  end.

Definition u4_gt (a: u4) (b: u4): bool :=
  match u4_cmp a b with
  | Lt => false
  | Eq => false
  | Gt => true
  end.

Definition u4_eq (a: u4) (b: u4): bool :=
  match u4_cmp a b with
  | Lt => false
  | Eq => true
  | Gt => true
  end.

Definition u4_cpl (a: u4): u4 :=
  match a with
  | x0 => xf
  | x1 => xe
  | x2 => xd
  | x3 => xc
  | x4 => xb
  | x5 => xa
  | x6 => x9
  | x7 => x8
  | x8 => x7
  | x9 => x6
  | xa => x5
  | xb => x4
  | xc => x3
  | xd => x2
  | xe => x1
  | xf => x0
  end.

Section Nat.
  Definition to_nat (v: u4): nat :=
    match v with
    | x0 => 0
    | x1 => 1
    | x2 => 2
    | x3 => 3
    | x4 => 4
    | x5 => 5
    | x6 => 6
    | x7 => 7
    | x8 => 8
    | x9 => 9
    | xa => 10
    | xb => 11
    | xc => 12
    | xd => 13
    | xe => 14
    | xf => 15
    end.

  Definition of_nat (n: nat): option u4 :=
    match n with
    | 0 => Some x0
    | 1 => Some x1
    | 2 => Some x2
    | 3 => Some x3
    | 4 => Some x4
    | 5 => Some x5
    | 6 => Some x6
    | 7 => Some x7
    | 8 => Some x8
    | 9 => Some x9
    | 10 => Some xa
    | 11 => Some xb
    | 12 => Some xc
    | 13 => Some xd
    | 14 => Some xe
    | 15 => Some xf
    | _ => None
    end.

  Lemma of_to_nat x : of_nat (to_nat x) = Some x.
  Proof. destruct x; reflexivity. Qed.

  Lemma to_of_nat x y : of_nat x = Some y -> to_nat y = x.
  Proof.
    cbv [of_nat];
      repeat match goal with
        | [ |- context[match ?x with _ => _ end] ] => is_var x; destruct x
        | _ => intro
        | _ => reflexivity
        | _ => progress subst
        | [ H : Some ?a = Some ?b |- _ ] => assert (a = b) by refine match H with eq_refl => eq_refl end; clear H
        | [ H : None = Some _ |- _ ] => solve [ inversion H ]
      end.
  Qed.

  Lemma to_of_nat_option_map x : option_map to_nat (of_nat x) = if Nat.leb x 15 then Some x else None.
  Proof.
    cbv [of_nat];
      repeat match goal with
        | [ |- context[match ?x with _ => _ end] ] => is_var x; destruct x
        end;
      reflexivity.
  Qed.

  Lemma to_nat_bounded x : to_nat x <= 15.
  Proof.
    generalize (to_of_nat_option_map (to_nat x)).
    rewrite of_to_nat; cbn [option_map].
    destruct (Nat.leb (to_nat x) 15) eqn:H; [ | congruence ].
    rewrite (Nat.leb_le (to_nat x) 15) in H.
    intro; assumption.
  Qed.

  Lemma gt_nat x y : u4_gt x y = true -> to_nat x > to_nat y.
  Proof.
    intros H.
    destruct x; destruct y; try (inversion H); try (simpl; omega).
  Qed.
End Nat.
