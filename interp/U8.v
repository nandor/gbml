(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

Require Import Coq.Bool.Bool.
Require Import Coq.Init.Nat.
Require Import Coq.Arith.PeanoNat.
Require Import Omega.

From GB Require Import U4.



Definition u8 : Type := (u4 * u4).

Definition u8_to_bits (v: u8) :=
  match v with
  | (lo, hi) =>
    match u4_to_bits lo with
    | (l0, l1, l2, l3) =>
      match u4_to_bits hi with
      | (h0, h1, h2, h3) =>
        (l0, l1, l2, l3, h0, h1, h2, h3)
      end
    end
  end.

Definition u8_of_bits v :=
  match v with
  | (l0, l1, l2, l3, h0, h1, h2, h3) =>
    (u4_of_bits (l0, l1, l2, l3), u4_of_bits (h0, h1, h2, h3))
  end.

Definition u8_hi (v: u8): u4 :=
  match v with
  | (_, hi) => hi
  end.

Definition u8_lo (v: u8): u4 :=
  match v with
  | (lo, _) => lo
  end.

Definition u8_inc (v: u8): (u8 * bool) :=
  match v with
  | (lo, hi) =>
    match u4_inc lo with
    | (lo', false) => ((lo', hi), false)
    | (lo', true) =>
      match u4_inc hi with
      | (hi', c) => ((lo', hi'), c)
      end
    end
  end.

Definition u8_dec (v: u8): (u8 * bool) :=
  match v with
  | (lo, hi) =>
    match u4_dec lo with
    | (lo', false) => ((lo', hi), false)
    | (lo', true) =>
      match u4_dec hi with
      | (hi', c) => ((lo', hi'), c)
      end
    end
  end.

Definition u8_add (a: u8) (b: u8): (u8 * bool) :=
  match a with
  | (a_lo, a_hi) =>
    match b with
    | (b_lo, b_hi) =>
      match u4_add a_lo b_lo with
      | (lo, false) =>
        match u4_add a_hi b_hi with
        | (hi, false) => ((lo, hi), false)
        | (hi, true) => ((lo, hi), true)
        end
      | (lo, true) =>
        match u4_add a_hi b_hi with
        | (hi0, c) =>
          match u4_inc hi0 with
          | (hi, c') => ((lo, hi), c || c')
          end
        end
      end
    end
  end.

Lemma u8_inc_and_add x : u8_inc x = u8_add x (x1, x0).
Proof. destruct x as [lo hi]. destruct lo; destruct hi; reflexivity. Qed.

Definition u8_adc (a: u8) (b: u8) (c: bool): (u8 * bool) :=
  match c with
  | false => u8_add a b
  | true =>
    match u8_inc a with
    | (a', c') =>
      match u8_add a' b with
      | (s, c'') => (s, c' || c'')
      end
    end
  end.

Definition u8_sub (a: u8) (b: u8) :=
 match a with
  | (a_lo, a_hi) =>
    match b with
    | (b_lo, b_hi) =>
      match u4_sub a_lo b_lo with
      | (lo, false) =>
        match u4_sub a_hi b_hi with
        | (hi, c) => ((lo, hi), c)
        end
      | (lo, true) =>
        match u4_sub a_hi b_hi with
        | (hi0, c) =>
          match u4_dec hi0 with
          | (hi, c') => ((lo, hi), c || c')
          end
        end
      end
    end
  end.

Lemma u8_dec_and_sub x : u8_dec x = u8_sub x (x1, x0).
Proof. destruct x as [lo hi]. destruct lo; destruct hi; reflexivity. Qed.

Definition u8_is_zero (a: u8): bool :=
  match a with
  | (x0, x0) => true
  | _ => false
  end.

Definition u8_msb (a: u8): bool := u4_msb (u8_hi a).
Definition u8_lsb (a: u8): bool := u4_lsb (u8_lo a).

Definition u8_and (a: u8) (b: u8) :=
  match a with
  | (a_lo, a_hi) =>
    match b with
    | (b_lo, b_hi) =>
      (u4_and a_lo b_lo, u4_and a_hi b_hi)
    end
  end.

Definition u8_xor (a: u8) (b: u8) :=
  match a with
  | (a_lo, a_hi) =>
    match b with
    | (b_lo, b_hi) =>
      (u4_xor a_lo b_lo, u4_xor a_hi b_hi)
    end
  end.

Definition u8_or (a: u8) (b: u8) :=
  match a with
  | (a_lo, a_hi) =>
    match b with
    | (b_lo, b_hi) =>
      (u4_or a_lo b_lo, u4_or a_hi b_hi)
    end
  end.


Definition u8_cpl (a: u8) :=
  match a with
  | (a_lo, a_hi) => (u4_cpl a_lo, u4_cpl a_hi)
  end.

Section Nat.

  Definition to_nat (v: u8): nat :=
    match v with
    | (lo, hi) => to_nat lo + to_nat hi * 16
    end.

  Definition of_nat (n: nat): option u8 :=
    match of_nat (n mod 16) with
    | None => None
    | Some lo =>
      match of_nat (n / 16) with
      | None => None
      | Some hi => Some (lo, hi)
      end
    end.

  Lemma of_to_nat x : of_nat (to_nat x) = Some x.
  Proof.
    destruct x as [lo hi].
    unfold to_nat.
    unfold of_nat.
    rewrite Nat.div_add.
    rewrite Nat.mod_add.
    destruct lo; try (destruct hi; reflexivity).
    omega.
    omega.
  Qed.

End Nat.
