(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

Require Import Coq.Bool.Bool.
Require Import Coq.Lists.List.
Require Coq.Init.Nat.

From GB Require Import U4.
From GB Require Import U8.
From GB Require Import U16.

Import ListNotations.


(******************************************************************************)

Inductive interrupt : Type :=
  | Int_VBlank
  | Int_Stat
  | Int_Timer
  | Int_Serial
  | Int_Pins
  .

Inductive System T : Type :=
  | S : (T -> interrupt -> bool)
     -> (T -> interrupt -> bool)
     -> (T -> interrupt -> option (System T))
     -> (T -> u16 -> option (u8 * (System T)))
     -> (T -> u16 -> u8 -> option (System T))
     -> T
     -> System T
  .

Definition sys_is_interrupt_pending {T: Type} (s: System T) (int: interrupt) :=
  match s with
  | S _ pending _ _ _ _ t => pending t int
  end.

Definition sys_is_interrupt_enabled {T: Type} (s: System T) (int: interrupt) :=
  match s with
  | S _ _ enabled _ _ _ t => enabled t int
  end.

Definition sys_clear_interrupt {T: Type} (s: System T) (int: interrupt) :=
  match s with
  | S _ _ _ clear _ _ t => clear t int
  end.

Definition sys_read {T: Type} (s: System T) (addr: u16): option (u8 * (System T)) :=
  match s with
  | S _ _ _ _ read _ t => read t addr
  end.

Definition sys_write {T: Type} (s: System T) (addr: u16) (v: u8): option (System T) :=
  match s with
  | S _ _ _ _ _ write t => write t addr v
  end.


(******************************************************************************)

Inductive reg_8 : Type :=
  | R8_B
  | R8_C
  | R8_D
  | R8_E
  | R8_H
  | R8_L
  | R8_A
  .

Inductive reg_16 : Type :=
  | R16_BC
  | R16_DE
  | R16_HL
  .

Inductive reg_16s : Type :=
  | R16S_BC
  | R16S_DE
  | R16S_HL
  | R16S_SP
  .

Inductive reg_16p : Type :=
  | R16P_BC
  | R16P_DE
  | R16P_HL
  | R16P_AF
  .

Inductive cc : Type :=
  | CC_A
  | CC_Z
  | CC_NZ
  | CC_C
  | CC_NC
  .

Inductive rcc : Type :=
  | RCC_Z
  | RCC_NZ
  | RCC_C
  | RCC_NC
  .

Inductive dir : Type :=
  | Dir_Nop
  | Dir_Inc
  | Dir_Dec
  .

Inductive bit_8 : Type :=
  | B0
  | B1
  | B2
  | B3
  | B4
  | B5
  | B6
  | B7
  .

Inductive alu8_op : Type :=
  | A8_Add
  | A8_Adc
  | A8_Sub
  | A8_Sbc
  | A8_And
  | A8_Xor
  | A8_Or
  | A8_Cp
  | A8_Mov
  | A8_Inc
  | A8_Dec
  | A8_Rlc
  | A8_Rrc
  | A8_Rl
  | A8_Rr
  | A8_Sla
  | A8_Sra
  | A8_Swap
  | A8_Srl
  | A8_Bit (n : bit_8)
  | A8_Res (n : bit_8)
  | A8_Set (n : bit_8)
  | A8_Cpl
  | A8_Ccf
  | A8_Scf
  | A8_Daa
  .

Inductive alu16_op : Type :=
  | A16_Inc
  | A16_Dec
  | A16_Add
  | A16_Mov
  .

Inductive rw : Type :=
  | R
  | W
  .

Inductive Uop : Type :=
  | U_FETCH
  | U_CB
  (* ld r16, d16 *)
  | U_LD_R16_D16_M2 (dst: reg_16s)
  | U_LD_R16_D16_M3 (dst: reg_16s)
  (* ld r8, (r16) *)
  | U_MOV_R8_R16_M2_R (dst: reg_8) (addr: reg_16)
  | U_MOV_R8_R16_M2_W (dst: reg_8) (addr: reg_16)
  (* jr *)
  | U_JR_M2 (cc: cc)
  | U_JR_M3 (ofs: u8)
  (* ld (HL±), r8 *)
  | U_MOV_HL_M2_R (dir: dir) (src: reg_8)
  | U_MOV_HL_M2_W (dir: dir) (src: reg_8)
  (* alu (HL) *)
  | U_ALU8_HL_M2 (op: alu8_op)
  | U_ALU8_HL_M3 (v: u8)
  (* ld r8, d8 *)
  | U_LD_R8_D8_M2 (dst:reg_8)
  (* ld (c), a *)
  | U_MOV_C_M2_R
  | U_MOV_C_M2_W
  (* ld (d8), a *)
  | U_MOV_D8_M2 (op: rw)
  | U_MOV_D8_M3_R (addr: u8)
  | U_MOV_D8_M3_W (addr: u8)
  (* call/call[cc] *)
  | U_CALL_M2 (cc: cc)
  | U_CALL_M3 (cc: cc) (lo: u8)
  | U_CALL_M4 (lo: u8) (hi: u8)
  | U_CALL_M5 (lo: u8) (hi: u8)
  | U_CALL_M6 (lo: u8)
  (* jp (cc), d16 *)
  | U_JP_D16_M2 (cc: cc)
  | U_JP_D16_M3 (cc: cc) (lo: u8)
  | U_JP_D16_M4 (lo: u8) (hi: u8)
  (* push *)
  | U_PUSH_M2 (reg: reg_16p)
  | U_PUSH_M3 (reg: reg_16p)
  | U_PUSH_M4 (reg: reg_16p)
  (* pop *)
  | U_POP_M2 (reg: reg_16p)
  | U_POP_M3 (reg: reg_16p)
  (* ret *)
  | U_RET_M2 (cc: rcc)
  | U_RET_M3 (ime: bool)
  | U_RET_M4 (lo: u8)
  | U_RET_M5 (lo: u8) (hi: u8)
  (* alu16 *)
  | U_ALU16_M2 (op: alu16_op) (dst: reg_16s) (src: reg_16s)
  (* alu8 r8, d8 *)
  | U_ALU8_D8_M2 (op: alu8_op) (dst: reg_8)
  (* alu8 r8, (HL) *)
  | U_ALU8_HL_R8_M2 (op: alu8_op) (dst: reg_8)
  (* bit (HL) *)
  | U_ALU8_BIT_M2 (n: bit_8)
  (* ld (d16), r8 *)
  | U_MOV_D16_R8_M2   (src:reg_8) (op: rw)
  | U_MOV_D16_R8_M3   (src:reg_8) (lo: u8) (op: rw)
  | U_MOV_D16_R8_M4_R (src:reg_8) (lo: u8) (hi: u8)
  | U_MOV_D16_R8_M4_W (src:reg_8) (lo: u8) (hi: u8)
  (* ld (a16), SP *)
  | U_ST_D16_R16_M2 (addr: reg_16s)
  | U_ST_D16_R12_M3 (addr: reg_16s) (lo: u8)
  | U_ST_D16_R12_M4 (addr: reg_16s) (lo: u8) (hi: u8)
  | U_ST_D16_R12_M5 (addr: reg_16s) (lo: u8) (hi: u8)
  (* ld (hl), d8 *)
  | U_ST_R16_D8_M2 (dst: reg_16)
  | U_ST_R16_D8_M3 (dst: reg_16) (v: u8)
  (* ld r16, sp+i8 *)
  | U_ADD_SP_D8_M2 (dst: reg_16s)
  | U_ADD_SP_D8_M3 (dst: reg_16s) (off: u8)
  | U_ADD_SP_D8_M4
  (* rst *)
  | U_RST_M2 (addr: u8)
  | U_RST_M3 (addr: u8)
  | U_RST_M4 (addr: u8)
  (* interrupt *)
  | U_INT_M2 (v: u8)
  | U_INT_M3 (v: u8)
  | U_INT_M4 (v: u8)
  | U_INT_M5 (v: u8)
  .

(******************************************************************************)

Record Regs :=
  { pc: u16
  ; sp: u16
  ; a: u8
  ; b: u8
  ; c: u8
  ; d: u8
  ; e: u8
  ; h: u8
  ; l: u8
  }.

Record Flags :=
  { zf: bool
  ; nf: bool
  ; hf: bool
  ; cf: bool
  }.

Record Cpu :=
  { r: Regs
  ; f: Flags
  ; ime: bool
  ; halted: bool
  ; uop: Uop
  }.


(******************************************************************************)

Definition get_reg_8 r reg :=
  match reg with
  | R8_B => r.(b)
  | R8_C => r.(c)
  | R8_D => r.(d)
  | R8_E => r.(e)
  | R8_H => r.(h)
  | R8_L => r.(l)
  | R8_A => r.(a)
  end.

Definition get_reg_16 r reg :=
  match reg with
  | R16_BC => (r.(c), r.(b))
  | R16_DE => (r.(e), r.(d))
  | R16_HL => (r.(l), r.(h))
  end.

Definition get_reg_hl r := (r.(l), r.(h)).

Definition set_reg_hl r hl :=
  match hl with
  | (lo, hi) => {| pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); d := r.(d); e := r.(e); h := hi; l := lo; a := r.(a) |}
  end.

Definition get_reg_16s r reg :=
  match reg with
  | R16S_BC => (r.(c), r.(b))
  | R16S_DE => (r.(e), r.(d))
  | R16S_HL => (r.(l), r.(h))
  | R16S_SP => r.(sp)
  end.

Definition set_reg_16s_lo r reg v :=
  match reg with
  | R16S_BC => {| pc := r.(pc); sp := r.(sp); b := r.(b); c := v; d := r.(d); e := r.(e); h := r.(h); l := r.(l); a := r.(a) |}
  | R16S_DE => {| pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); d := r.(d); e := v; h := r.(h); l := r.(l); a := r.(a) |}
  | R16S_HL => {| pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); d := r.(d); e := r.(e); h := r.(h); l := v; a := r.(a) |}
  | R16S_SP =>
    match r.(sp) with
    | (lo, hi) => {| pc := r.(pc); sp := (v, hi); b := r.(b); c := r.(c); d := r.(d); e := r.(e); h := r.(h); l := r.(l); a := r.(a) |}
    end
  end.

Definition set_reg_16s_hi r reg v :=
  match reg with
  | R16S_BC => {| pc := r.(pc); sp := r.(sp); b := v; c := r.(c); d := r.(d); e := r.(e); h := r.(h); l := r.(l); a := r.(a) |}
  | R16S_DE => {| pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); d := v; e := r.(e); h := r.(h); l := r.(l); a := r.(a) |}
  | R16S_HL => {| pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); d := r.(d); e := r.(e); h := v; l := r.(l); a := r.(a) |}
  | R16S_SP =>
    match r.(sp) with
    | (lo, hi) => {| pc := r.(pc); sp := (lo, v); b := r.(b); c := r.(c); d := r.(d); e := r.(e); h := r.(h); l := r.(l); a := r.(a) |}
    end
  end.

Definition set_reg_16s r reg v :=
  match v with
  | (lo, hi) =>
    match reg with
    | R16S_BC => {| pc := r.(pc); sp := r.(sp); b := hi; c := lo; d := r.(d); e := r.(e); h := r.(h); l := r.(l); a := r.(a) |}
    | R16S_DE => {| pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); d := hi; e := lo; h := r.(h); l := r.(l); a := r.(a) |}
    | R16S_HL => {| pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); d := r.(d); e := r.(e); h := hi; l := lo; a := r.(a) |}
    | R16S_SP => {| pc := r.(pc); sp := v; b := r.(b); c := r.(c); d := r.(d); e := r.(e); h := r.(h); l := r.(l); a := r.(a) |}
    end
  end.

Definition set_reg_8 r reg v :=
  match reg with
  | R8_B => {| b := v; pc := r.(pc); sp := r.(sp); c := r.(c); d := r.(d); e := r.(e); h := r.(h); l := r.(l); a := r.(a) |}
  | R8_C => {| c := v; pc := r.(pc); sp := r.(sp); b := r.(b); d := r.(d); e := r.(e); h := r.(h); l := r.(l); a := r.(a) |}
  | R8_D => {| d := v; pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); e := r.(e); h := r.(h); l := r.(l); a := r.(a) |}
  | R8_E => {| e := v; pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); d := r.(d); h := r.(h); l := r.(l); a := r.(a) |}
  | R8_H => {| h := v; pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); d := r.(d); e := r.(e); l := r.(l); a := r.(a) |}
  | R8_L => {| l := v; pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); d := r.(d); e := r.(e); h := r.(h); a := r.(a) |}
  | R8_A => {| a := v; pc := r.(pc); sp := r.(sp); b := r.(b); c := r.(c); d := r.(d); e := r.(e); h := r.(h); l := r.(l) |}
  end.

Definition is_taken cc f :=
  match cc with
  | CC_Z  => f.(zf)
  | CC_NZ => negb f.(zf)
  | CC_C  => f.(cf)
  | CC_NC => negb f.(cf)
  | CC_A  => true
  end.

Definition reg_with_pc r pc :=
  {| pc := pc
   ; sp := r.(sp)
   ; a := r.(a)
   ; b := r.(b)
   ; c := r.(c)
   ; d := r.(d)
   ; e := r.(e)
   ; h := r.(h)
   ; l := r.(l)
   |}.

Definition reg_with_pc_lo r lo :=
  match r.(pc) with
  | (_, hi) =>
    {| pc := (lo, hi)
     ; sp := r.(sp)
     ; a := r.(a)
     ; b := r.(b)
     ; c := r.(c)
     ; d := r.(d)
     ; e := r.(e)
     ; h := r.(h)
     ; l := r.(l)
     |}
  end.

Definition reg_with_pc_hi r hi :=
  match r.(pc) with
  | (lo, _) =>
    {| pc := (lo, hi)
     ; sp := r.(sp)
     ; a := r.(a)
     ; b := r.(b)
     ; c := r.(c)
     ; d := r.(d)
     ; e := r.(e)
     ; h := r.(h)
     ; l := r.(l)
     |}
  end.

Definition reg_with_sp r sp :=
  {| pc := r.(pc)
   ; sp := sp
   ; a := r.(a)
   ; b := r.(b)
   ; c := r.(c)
   ; d := r.(d)
   ; e := r.(e)
   ; h := r.(h)
   ; l := r.(l)
   |}.

Definition cpu_with_reg cpu r :=
  {| r := r
   ; f := cpu.(f)
   ; ime := cpu.(ime)
   ; halted := cpu.(halted)
   ; uop := cpu.(uop)
   |}.

Definition cpu_with_flags cpu f :=
  {| r := cpu.(r)
   ; f := f
   ; ime := cpu.(ime)
   ; halted := cpu.(halted)
   ; uop := cpu.(uop)
   |}.

Definition cpu_with_reg_flags cpu r f :=
  {| r := r
   ; f := f
   ; ime := cpu.(ime)
   ; halted := cpu.(halted)
   ; uop := cpu.(uop)
   |}.

Definition cpu_with_uop cpu uop :=
  {| r := cpu.(r)
   ; f := cpu.(f)
   ; ime := cpu.(ime)
   ; halted := cpu.(halted)
   ; uop := uop
   |}.

Definition cpu_with_ime cpu ime :=
  {| r := cpu.(r)
   ; f := cpu.(f)
   ; ime := ime
   ; halted := cpu.(halted)
   ; uop := cpu.(uop)
   |}.

Definition cpu_with_halted cpu halted :=
  {| r := cpu.(r)
   ; f := cpu.(f)
   ; ime := cpu.(ime)
   ; halted := halted
   ; uop := cpu.(uop)
   |}.

Definition cpu_with_next_pc cpu :=
  {| r := reg_with_pc cpu.(r) (inc_wrap_u16 cpu.(r).(pc))
   ; f := cpu.(f)
   ; ime := cpu.(ime)
   ; halted := cpu.(halted)
   ; uop := cpu.(uop)
   |}.

Definition cpu_with_prev_sp cpu :=
  {| r := reg_with_sp cpu.(r) (dec_wrap_u16 cpu.(r).(sp))
   ; f := cpu.(f)
   ; ime := cpu.(ime)
   ; halted := cpu.(halted)
   ; uop := cpu.(uop)
   |}.

Definition cpu_with_next_sp cpu :=
  {| r := reg_with_sp cpu.(r) (inc_wrap_u16 cpu.(r).(sp))
   ; f := cpu.(f)
   ; ime := cpu.(ime)
   ; halted := cpu.(halted)
   ; uop := cpu.(uop)
   |}.

Definition flags_from_bits v :=
  match u4_to_bits (u8_hi v) with
  | (c, h, n, z) =>
    {| zf := z; nf := n; hf := h; cf := c |}
  end.


(******************************************************************************)

Definition execute_alu8 (op: alu8_op) (f: Flags) (op0: u8) (op1: u8): (u8 * Flags) :=
  match op with
  | A8_Adc =>
    match op0 with
    | (lo0, hi0) =>
      match op1 with
      | (lo1, hi1) =>
        match u4_adc lo0 lo1 f.(cf) with
        | (lov, h) =>
          match u4_adc hi0 hi1 h with
          | (hiv, c) =>
            let v := (lov, hiv) in
            (v, {| zf := u8_is_zero v; nf := false; hf := h; cf := c |})
          end
        end
      end
    end

  | A8_Add =>
    match op0 with
    | (lo0, hi0) =>
      match op1 with
      | (lo1, hi1) =>
        match u4_add lo0 lo1 with
        | (lov, h) =>
          match u4_adc hi0 hi1 h with
          | (hiv, c) =>
            let v := (lov, hiv) in
            (v, {| zf := u8_is_zero v; nf := false; hf := h; cf := c |})
          end
        end
      end
    end

  | A8_Sub =>
    match op0 with
    | (lo0, hi0) =>
      match op1 with
      | (lo1, hi1) =>
        match u4_sub lo0 lo1 with
        | (lov, h) =>
          match u4_sbc hi0 hi1 h with
          | (hiv, c) =>
            let v := (lov, hiv) in
            (v, {| zf := u8_is_zero v; nf := true; hf := h; cf := c |})
          end
        end
      end
    end

  | A8_Sbc =>
    match op0 with
    | (lo0, hi0) =>
      match op1 with
      | (lo1, hi1) =>
        match u4_sbc lo0 lo1 f.(cf) with
        | (lov, h) =>
          match u4_sbc hi0 hi1 h with
          | (hiv, c) =>
            let v := (lov, hiv) in
            (v, {| zf := u8_is_zero v; nf := true; hf := h; cf := c |})
          end
        end
      end
    end

  | A8_And =>
    let v := u8_and op0 op1 in
    (v, {| zf := u8_is_zero v; nf := false; hf := true; cf := false |})

  | A8_Xor =>
    let v := u8_xor op0 op1 in
    (v, {| zf := u8_is_zero v; nf := false; hf := false; cf := false |})

  | A8_Or =>
    let v := u8_or op0 op1 in
    (v, {| zf := u8_is_zero v; nf := false; hf := false; cf := false |})

  | A8_Cp =>
    match op0 with
    | (lo0, hi0) =>
      match op1 with
      | (lo1, hi1) =>
        match u4_sub lo0 lo1 with
        | (lov, h) =>
          match u4_sbc hi0 hi1 h with
          | (hiv, c) =>
            (op0, {| zf := u8_is_zero (lov, hiv); nf := true; hf := h; cf := c |})
          end
        end
      end
    end

  | A8_Mov =>
    (op1, f)

  | A8_Inc =>
    match u8_inc op1 with
    | (v, _) => (v, {| zf := u8_is_zero v; nf := false; hf := u4_is_zero (u8_lo v); cf := f.(cf) |})
    end

  | A8_Dec =>
    match u8_dec op1 with
    | (v, _) => (v, {| zf := u8_is_zero v; nf := true; hf := u4_eq (u8_lo v) xf; cf := f.(cf) |})
    end

  | A8_Rlc =>
    let c := u8_msb op1 in
    let v :=
      match u8_to_bits op1 with
      | (b0, b1, b2, b3, b4, b5, b6, b7) =>
        u8_of_bits (c, b0, b1, b2, b3, b4, b5, b6)
      end
    in
    (v, {| zf := u8_is_zero v; nf := false; hf := false; cf := c |})

  | A8_Rrc =>
    let c := u8_lsb op1 in
    let v :=
      match u8_to_bits op1 with
      | (b0, b1, b2, b3, b4, b5, b6, b7) =>
        u8_of_bits (b1, b2, b3, b4, b5, b6, b7, c)
      end
    in
    (v, {| zf := u8_is_zero v; nf := false; hf := false; cf := c |})

  | A8_Rl =>
    let c := u8_msb op1 in
    let v :=
      match u8_to_bits op1 with
      | (b0, b1, b2, b3, b4, b5, b6, b7) =>
        u8_of_bits (f.(cf), b0, b1, b2, b3, b4, b5, b6)
      end
    in
    (v, {| zf := u8_is_zero v; nf := false; hf := false; cf := c |})

  | A8_Rr =>
    let c := u8_lsb op1 in
    let v :=
      match u8_to_bits op1 with
      | (b0, b1, b2, b3, b4, b5, b6, b7) =>
        u8_of_bits (b1, b2, b3, b4, b5, b6, b7, f.(cf))
      end
    in
    (v, {| zf := u8_is_zero v; nf := false; hf := false; cf := c |})


  | A8_Sla =>
    let c := u8_msb op1 in
    let v :=
      match u8_to_bits op1 with
      | (b0, b1, b2, b3, b4, b5, b6, b7) =>
        u8_of_bits (false, b0, b1, b2, b3, b4, b5, b6)
      end
    in
    (v, {| zf := u8_is_zero v; nf := false; hf := false; cf := c |})

  | A8_Sra =>
    let c := u8_lsb op1 in
    let v :=
      match u8_to_bits op1 with
      | (b0, b1, b2, b3, b4, b5, b6, b7) =>
        u8_of_bits (b1, b2, b3, b4, b5, b6, b7, b7)
      end
    in
    (v, {| zf := u8_is_zero v; nf := false; hf := false; cf := c |})

  | A8_Swap =>
    match op1 with
    | (lo, hi) =>
      ((hi, lo), {| zf := u8_is_zero op1; nf := false; hf := false; cf := false |})
    end

  | A8_Srl =>
    let c := u8_lsb op1 in
    let v :=
      match u8_to_bits op1 with
      | (b0, b1, b2, b3, b4, b5, b6, b7) =>
        u8_of_bits (b1, b2, b3, b4, b5, b6, b7, false)
      end
    in
    (v, {| zf := u8_is_zero v; nf := false; hf := false; cf := c |})

  | A8_Bit n =>
    let bit :=
      match u8_to_bits op1 with
      | (b0, b1, b2, b3, b4, b5, b6, b7) =>
        match n with
        | B0 => b0
        | B1 => b1
        | B2 => b2
        | B3 => b3
        | B4 => b4
        | B5 => b5
        | B6 => b6
        | B7 => b7
        end
      end
    in
    (op1, {| zf := negb bit; nf := false; hf := true; cf := f.(cf) |})

  | A8_Res n =>
    let v :=
      match u8_to_bits op1 with
      | (b0, b1, b2, b3, b4, b5, b6, b7) =>
        match n with
        | B0 => (false, b1, b2, b3, b4, b5, b6, b7)
        | B1 => (b0, false, b2, b3, b4, b5, b6, b7)
        | B2 => (b0, b1, false, b3, b4, b5, b6, b7)
        | B3 => (b0, b1, b2, false, b4, b5, b6, b7)
        | B4 => (b0, b1, b2, b3, false, b5, b6, b7)
        | B5 => (b0, b1, b2, b3, b4, false, b6, b7)
        | B6 => (b0, b1, b2, b3, b4, b5, false, b7)
        | B7 => (b0, b1, b2, b3, b4, b5, b6, false)
        end
      end
    in
    (u8_of_bits v, f)

  | A8_Set n =>
    let v :=
      match u8_to_bits op1 with
      | (b0, b1, b2, b3, b4, b5, b6, b7) =>
        match n with
        | B0 => (true, b1, b2, b3, b4, b5, b6, b7)
        | B1 => (b0, true, b2, b3, b4, b5, b6, b7)
        | B2 => (b0, b1, true, b3, b4, b5, b6, b7)
        | B3 => (b0, b1, b2, true, b4, b5, b6, b7)
        | B4 => (b0, b1, b2, b3, true, b5, b6, b7)
        | B5 => (b0, b1, b2, b3, b4, true, b6, b7)
        | B6 => (b0, b1, b2, b3, b4, b5, true, b7)
        | B7 => (b0, b1, b2, b3, b4, b5, b6, true)
        end
      end
    in
    (u8_of_bits v, f)

  | A8_Cpl =>
    (u8_cpl op1, {| zf := f.(zf); nf := true; hf := true; cf := f.(cf) |})

  | A8_Ccf =>
    (op0, {| zf := f.(zf); nf := false; hf := false; cf := negb f.(cf) |})

  | A8_Scf =>
    (op0, {| zf := f.(zf); nf := false; hf := false; cf := true |})

  | A8_Daa =>
    match op1 with
    | (lo, hi) =>
      match f.(nf) with
      | true =>
        match u4_sub lo (if f.(hf) then x6 else x0) with
        | (lo', h) =>
          match u4_sbc hi (if f.(cf) then x6 else x0) h with
          | (hi', _) =>
            let v := (lo', hi') in
            (v, {| zf := u8_is_zero v; nf := true; hf := false; cf := f.(cf) |})
          end
        end
      | false =>
        let adj_lo := f.(hf) || u4_gt lo x9 in
        match u4_add lo (if adj_lo then x6 else x0) with
        | (lo', h) =>
          let adj_hi := f.(cf) || u4_gt hi (if h then x8 else x9) in
          match u4_adc hi (if adj_hi then x6 else x0) h with
          | (hi', c) =>
            let v := (lo', hi') in
            (v, {| zf := u8_is_zero v; nf := false; hf := false; cf := adj_hi || c |})
          end
        end
      end
    end
  end.

(******************************************************************************)

Definition mem_reader {T: Type} (cpu: Cpu) (sys: System T) (addr: u16) (fn: Cpu -> u8 -> Cpu): option (Cpu * System T) :=
  match sys_read sys addr with
  | None => None
  | Some (v, sys) => Some (fn cpu v, sys)
  end.

Definition mem_writer {T: Type} (cpu: Cpu) (sys: System T) (addr: u16) (val: u8) (fn: Cpu -> Cpu): option (Cpu * System T) :=
  match sys_write sys addr val with
  | None => None
  | Some sys => Some (fn cpu, sys)
  end.

Definition imm_reader {T: Type} (cpu: Cpu) (sys: System T) (fn: Cpu -> u8 -> Cpu): option (Cpu * System T) :=
  match sys_read sys cpu.(r).(pc) with
  | None => None
  | Some (v, sys) => Some (fn (cpu_with_next_pc cpu) v, sys)
  end.


(******************************************************************************)

Definition handle (cpu: Cpu) (uop: Uop): option Cpu :=
  Some (cpu_with_uop (cpu_with_next_pc cpu) uop).

Definition handle_alu8 (cpu: Cpu) (op: alu8_op) (dst: reg_8) (src: reg_8): option Cpu :=
  let v0 := get_reg_8 cpu.(r) dst in
  let v1 := get_reg_8 cpu.(r) src in
  match execute_alu8 op cpu.(f) v0 v1 with
  | (v, f) =>
    handle (cpu_with_reg_flags cpu (set_reg_8 cpu.(r) dst v) f) U_FETCH
  end.

Definition handle_alu8_rot (cpu: Cpu) (op: alu8_op) (dst: reg_8) (src: reg_8): option Cpu :=
  let v0 := get_reg_8 cpu.(r) dst in
  let v1 := get_reg_8 cpu.(r) src in
  match execute_alu8 op cpu.(f) v0 v1 with
  | (v, f) =>
    let f' := {| zf := false; nf := false; hf := false; cf := f.(cf) |} in
    handle (cpu_with_reg_flags cpu (set_reg_8 cpu.(r) dst v) f') U_FETCH
  end.

Definition handle_alu8_hl (cpu: Cpu) (op: alu8_op): option Cpu :=
  handle cpu (U_ALU8_HL_M2 op).

Definition handle_alu8_bit (cpu: Cpu) (bit: bit_8): option Cpu :=
  handle cpu (U_ALU8_BIT_M2 bit).

Definition handle_alu8_hl_r8 (cpu: Cpu) (op: alu8_op) (dst: reg_8): option Cpu :=
  handle cpu (U_ALU8_HL_R8_M2 op dst).

Definition handle_mov_hl (cpu: Cpu) (rw: rw) (dir: dir) (reg: reg_8): option Cpu :=
  handle cpu (match rw with
    | R => U_MOV_HL_M2_R dir reg
    | W => U_MOV_HL_M2_W dir reg
    end).

Definition handle_add_sp_d8 (cpu: Cpu) (dst: reg_16s): option Cpu :=
  handle cpu (U_ADD_SP_D8_M2 dst).

Definition handle_alu16 (cpu: Cpu) (op: alu16_op) (dst: reg_16s) (src: reg_16s): option Cpu :=
  handle cpu (U_ALU16_M2 op dst src).

Definition handle_alu8_d8 (cpu: Cpu) (op: alu8_op) (dst: reg_8): option Cpu :=
  handle cpu (U_ALU8_D8_M2 op dst).

Definition handle_call (cpu: Cpu) (cc: cc): option Cpu :=
  handle cpu (U_CALL_M2 cc).

Definition handle_jp (cpu: Cpu) (cc: cc): option Cpu :=
  handle cpu (U_JP_D16_M2 cc).

Definition handle_jp_hl (cpu: Cpu): option Cpu :=
  let pc := (cpu.(r).(l), cpu.(r).(h)) in
  Some (cpu_with_uop (cpu_with_reg cpu (reg_with_pc cpu.(r) pc)) U_FETCH).

Definition handle_jr (cpu: Cpu) (cc: cc): option Cpu :=
  handle cpu (U_JR_M2 cc).

Definition handle_ld_r16_d16 (cpu: Cpu) (dst: reg_16s): option Cpu :=
  handle cpu (U_LD_R16_D16_M2 dst).

Definition handle_ld_r8_d8 (cpu: Cpu) (dst: reg_8): option Cpu :=
  handle cpu (U_LD_R8_D8_M2 dst).

Definition handle_mov_c (cpu: Cpu) (rw: rw): option Cpu :=
  handle cpu (match rw with
    | R => U_MOV_C_M2_R
    | W => U_MOV_C_M2_W
    end).

Definition handle_mov_d16_r8 (cpu: Cpu) (reg: reg_8) (rw: rw): option Cpu :=
  handle cpu (U_MOV_D16_R8_M2 reg rw).

Definition handle_mov_r8_r16 (cpu: Cpu) (rw: rw) (dst: reg_8) (src: reg_16): option Cpu :=
  handle cpu (match rw with
    | R => U_MOV_R8_R16_M2_R dst src
    | W => U_MOV_R8_R16_M2_W dst src
    end).

Definition handle_movh (cpu: Cpu) (rw: rw): option Cpu :=
  handle cpu (U_MOV_D8_M2 rw).

Definition handle_pop (cpu: Cpu) (reg: reg_16p): option Cpu :=
  handle cpu (U_POP_M2 reg).

Definition handle_push (cpu: Cpu) (reg: reg_16p): option Cpu :=
  handle cpu (U_PUSH_M2 reg).

Definition handle_ret (cpu: Cpu) (cc: cc) (ie: bool): option Cpu :=
  handle cpu (match cc with
    | CC_A  => U_RET_M3 ie
    | CC_Z  => U_RET_M2 RCC_Z
    | CC_NZ => U_RET_M2 RCC_NZ
    | CC_C  => U_RET_M2 RCC_C
    | CC_NC => U_RET_M2 RCC_NC
    end).

Definition handle_rst (cpu: Cpu) (addr: u8): option Cpu :=
  handle cpu (U_RST_M2 addr).

Definition handle_st_d16_r16 (cpu: Cpu) (src: reg_16s): option Cpu :=
  handle cpu (U_ST_D16_R16_M2 src).

Definition handle_st_r16_d8 (cpu: Cpu) (reg: reg_16): option Cpu :=
  handle cpu (U_ST_R16_D8_M2 reg).


(******************************************************************************)

Definition handle_op (cpu: Cpu) (op: u8) : option Cpu :=
  match op with
  | (x0, x0) => Some (cpu_with_uop (cpu_with_next_pc cpu) U_FETCH)
  | (x1, x0) => handle_ld_r16_d16 cpu R16S_BC
  | (x2, x0) => handle_mov_r8_r16 cpu W R8_A R16_BC
  | (x3, x0) => handle_alu16      cpu A16_Inc R16S_BC R16S_BC
  | (x4, x0) => handle_alu8       cpu A8_Inc R8_B R8_B
  | (x5, x0) => handle_alu8       cpu A8_Dec R8_B R8_B
  | (x6, x0) => handle_ld_r8_d8   cpu R8_B
  | (x7, x0) => handle_alu8_rot   cpu A8_Rlc R8_A R8_A
  | (x8, x0) => handle_st_d16_r16 cpu R16S_SP
  | (x9, x0) => handle_alu16      cpu A16_Add R16S_HL R16S_BC
  | (xa, x0) => handle_mov_r8_r16 cpu R R8_A R16_BC
  | (xb, x0) => handle_alu16      cpu A16_Dec R16S_BC R16S_BC
  | (xc, x0) => handle_alu8       cpu A8_Inc R8_C R8_C
  | (xd, x0) => handle_alu8       cpu A8_Dec R8_C R8_C
  | (xe, x0) => handle_ld_r8_d8   cpu R8_C
  | (xf, x0) => handle_alu8_rot   cpu A8_Rrc R8_A R8_A
  | (x0, x1) => None
  | (x1, x1) => handle_ld_r16_d16 cpu R16S_DE
  | (x2, x1) => handle_mov_r8_r16 cpu W R8_A R16_DE
  | (x3, x1) => handle_alu16      cpu A16_Inc R16S_DE R16S_DE
  | (x4, x1) => handle_alu8       cpu A8_Inc R8_D R8_D
  | (x5, x1) => handle_alu8       cpu A8_Dec R8_D R8_D
  | (x6, x1) => handle_ld_r8_d8   cpu R8_D
  | (x7, x1) => handle_alu8_rot   cpu A8_Rl R8_A R8_A
  | (x8, x1) => handle_jr         cpu CC_A
  | (x9, x1) => handle_alu16      cpu A16_Add R16S_HL R16S_DE
  | (xa, x1) => handle_mov_r8_r16 cpu R R8_A R16_DE
  | (xb, x1) => handle_alu16      cpu A16_Dec R16S_DE R16S_DE
  | (xc, x1) => handle_alu8       cpu A8_Inc R8_E R8_E
  | (xd, x1) => handle_alu8       cpu A8_Dec R8_E R8_E
  | (xe, x1) => handle_ld_r8_d8   cpu R8_E
  | (xf, x1) => handle_alu8_rot   cpu A8_Rr R8_A R8_A
  | (x0, x2) => handle_jr         cpu CC_NZ
  | (x1, x2) => handle_ld_r16_d16 cpu R16S_HL
  | (x2, x2) => handle_mov_hl     cpu W Dir_Inc R8_A
  | (x3, x2) => handle_alu16      cpu A16_Inc R16S_HL R16S_HL
  | (x4, x2) => handle_alu8       cpu A8_Inc R8_H R8_H
  | (x5, x2) => handle_alu8       cpu A8_Dec R8_H R8_H
  | (x6, x2) => handle_ld_r8_d8   cpu R8_H
  | (x7, x2) => handle_alu8       cpu A8_Daa R8_A R8_A
  | (x8, x2) => handle_jr         cpu CC_Z
  | (x9, x2) => handle_alu16      cpu A16_Add R16S_HL R16S_HL
  | (xa, x2) => handle_mov_hl     cpu R Dir_Inc R8_A
  | (xb, x2) => handle_alu16      cpu A16_Dec R16S_HL R16S_HL
  | (xc, x2) => handle_alu8       cpu A8_Inc R8_L R8_L
  | (xd, x2) => handle_alu8       cpu A8_Dec R8_L R8_L
  | (xe, x2) => handle_ld_r8_d8   cpu R8_L
  | (xf, x2) => handle_alu8       cpu A8_Cpl R8_A R8_A
  | (x0, x3) => handle_jr         cpu CC_NC
  | (x1, x3) => handle_ld_r16_d16 cpu R16S_SP
  | (x2, x3) => handle_mov_hl     cpu W Dir_Dec R8_A
  | (x3, x3) => handle_alu16      cpu A16_Inc R16S_SP R16S_SP
  | (x4, x3) => handle_alu8_hl    cpu A8_Inc
  | (x5, x3) => handle_alu8_hl    cpu A8_Dec
  | (x6, x3) => handle_st_r16_d8  cpu R16_HL
  | (x7, x3) => handle_alu8       cpu A8_Scf R8_A R8_A
  | (x8, x3) => handle_jr         cpu CC_C
  | (x9, x3) => handle_alu16      cpu A16_Add R16S_HL R16S_SP
  | (xa, x3) => handle_mov_hl     cpu R Dir_Dec R8_A
  | (xb, x3) => handle_alu16      cpu A16_Dec R16S_SP R16S_SP
  | (xc, x3) => handle_alu8       cpu A8_Inc R8_A R8_A
  | (xd, x3) => handle_alu8       cpu A8_Dec R8_A R8_A
  | (xe, x3) => handle_ld_r8_d8   cpu R8_A
  | (xf, x3) => handle_alu8       cpu A8_Ccf R8_A R8_A
  | (x0, x4) => handle_alu8       cpu A8_Mov R8_B R8_B
  | (x1, x4) => handle_alu8       cpu A8_Mov R8_B R8_C
  | (x2, x4) => handle_alu8       cpu A8_Mov R8_B R8_D
  | (x3, x4) => handle_alu8       cpu A8_Mov R8_B R8_E
  | (x4, x4) => handle_alu8       cpu A8_Mov R8_B R8_H
  | (x5, x4) => handle_alu8       cpu A8_Mov R8_B R8_L
  | (x6, x4) => handle_alu8_hl_r8 cpu A8_Mov R8_B
  | (x7, x4) => handle_alu8       cpu A8_Mov R8_B R8_A
  | (x8, x4) => handle_alu8       cpu A8_Mov R8_C R8_B
  | (x9, x4) => handle_alu8       cpu A8_Mov R8_C R8_C
  | (xa, x4) => handle_alu8       cpu A8_Mov R8_C R8_D
  | (xb, x4) => handle_alu8       cpu A8_Mov R8_C R8_E
  | (xc, x4) => handle_alu8       cpu A8_Mov R8_C R8_H
  | (xd, x4) => handle_alu8       cpu A8_Mov R8_C R8_L
  | (xe, x4) => handle_alu8_hl_r8 cpu A8_Mov R8_C
  | (xf, x4) => handle_alu8       cpu A8_Mov R8_C R8_A
  | (x0, x5) => handle_alu8       cpu A8_Mov R8_D R8_B
  | (x1, x5) => handle_alu8       cpu A8_Mov R8_D R8_C
  | (x2, x5) => handle_alu8       cpu A8_Mov R8_D R8_D
  | (x3, x5) => handle_alu8       cpu A8_Mov R8_D R8_E
  | (x4, x5) => handle_alu8       cpu A8_Mov R8_D R8_H
  | (x5, x5) => handle_alu8       cpu A8_Mov R8_D R8_L
  | (x6, x5) => handle_alu8_hl_r8 cpu A8_Mov R8_D
  | (x7, x5) => handle_alu8       cpu A8_Mov R8_D R8_A
  | (x8, x5) => handle_alu8       cpu A8_Mov R8_E R8_B
  | (x9, x5) => handle_alu8       cpu A8_Mov R8_E R8_C
  | (xa, x5) => handle_alu8       cpu A8_Mov R8_E R8_D
  | (xb, x5) => handle_alu8       cpu A8_Mov R8_E R8_E
  | (xc, x5) => handle_alu8       cpu A8_Mov R8_E R8_H
  | (xd, x5) => handle_alu8       cpu A8_Mov R8_E R8_L
  | (xe, x5) => handle_alu8_hl_r8 cpu A8_Mov R8_E
  | (xf, x5) => handle_alu8       cpu A8_Mov R8_E R8_A
  | (x0, x6) => handle_alu8       cpu A8_Mov R8_H R8_B
  | (x1, x6) => handle_alu8       cpu A8_Mov R8_H R8_C
  | (x2, x6) => handle_alu8       cpu A8_Mov R8_H R8_D
  | (x3, x6) => handle_alu8       cpu A8_Mov R8_H R8_E
  | (x4, x6) => handle_alu8       cpu A8_Mov R8_H R8_H
  | (x5, x6) => handle_alu8       cpu A8_Mov R8_H R8_L
  | (x6, x6) => handle_alu8_hl_r8 cpu A8_Mov R8_H
  | (x7, x6) => handle_alu8       cpu A8_Mov R8_H R8_A
  | (x8, x6) => handle_alu8       cpu A8_Mov R8_L R8_B
  | (x9, x6) => handle_alu8       cpu A8_Mov R8_L R8_C
  | (xa, x6) => handle_alu8       cpu A8_Mov R8_L R8_D
  | (xb, x6) => handle_alu8       cpu A8_Mov R8_L R8_E
  | (xc, x6) => handle_alu8       cpu A8_Mov R8_L R8_H
  | (xd, x6) => handle_alu8       cpu A8_Mov R8_L R8_L
  | (xe, x6) => handle_alu8_hl_r8 cpu A8_Mov R8_L
  | (xf, x6) => handle_alu8       cpu A8_Mov R8_L R8_A
  | (x0, x7) => handle_mov_hl     cpu W Dir_Nop R8_B
  | (x1, x7) => handle_mov_hl     cpu W Dir_Nop R8_C
  | (x2, x7) => handle_mov_hl     cpu W Dir_Nop R8_D
  | (x3, x7) => handle_mov_hl     cpu W Dir_Nop R8_E
  | (x4, x7) => handle_mov_hl     cpu W Dir_Nop R8_H
  | (x5, x7) => handle_mov_hl     cpu W Dir_Nop R8_L
  | (x6, x7) => Some (cpu_with_uop (cpu_with_halted (cpu_with_next_pc cpu) true) U_FETCH)
  | (x7, x7) => handle_mov_hl     cpu W Dir_Nop R8_A
  | (x8, x7) => handle_alu8       cpu A8_Mov R8_A R8_B
  | (x9, x7) => handle_alu8       cpu A8_Mov R8_A R8_C
  | (xa, x7) => handle_alu8       cpu A8_Mov R8_A R8_D
  | (xb, x7) => handle_alu8       cpu A8_Mov R8_A R8_E
  | (xc, x7) => handle_alu8       cpu A8_Mov R8_A R8_H
  | (xd, x7) => handle_alu8       cpu A8_Mov R8_A R8_L
  | (xe, x7) => handle_alu8_hl_r8 cpu A8_Mov R8_A
  | (xf, x7) => handle_alu8       cpu A8_Mov R8_A R8_A
  | (x0, x8) => handle_alu8       cpu A8_Add R8_A R8_B
  | (x1, x8) => handle_alu8       cpu A8_Add R8_A R8_C
  | (x2, x8) => handle_alu8       cpu A8_Add R8_A R8_D
  | (x3, x8) => handle_alu8       cpu A8_Add R8_A R8_E
  | (x4, x8) => handle_alu8       cpu A8_Add R8_A R8_H
  | (x5, x8) => handle_alu8       cpu A8_Add R8_A R8_L
  | (x6, x8) => handle_alu8_hl_r8 cpu A8_Add R8_A
  | (x7, x8) => handle_alu8       cpu A8_Add R8_A R8_A
  | (x8, x8) => handle_alu8       cpu A8_Adc R8_A R8_B
  | (x9, x8) => handle_alu8       cpu A8_Adc R8_A R8_C
  | (xa, x8) => handle_alu8       cpu A8_Adc R8_A R8_D
  | (xb, x8) => handle_alu8       cpu A8_Adc R8_A R8_E
  | (xc, x8) => handle_alu8       cpu A8_Adc R8_A R8_H
  | (xd, x8) => handle_alu8       cpu A8_Adc R8_A R8_L
  | (xe, x8) => handle_alu8_hl_r8 cpu A8_Adc R8_A
  | (xf, x8) => handle_alu8       cpu A8_Adc R8_A R8_A
  | (x0, x9) => handle_alu8       cpu A8_Sub R8_A R8_B
  | (x1, x9) => handle_alu8       cpu A8_Sub R8_A R8_C
  | (x2, x9) => handle_alu8       cpu A8_Sub R8_A R8_D
  | (x3, x9) => handle_alu8       cpu A8_Sub R8_A R8_E
  | (x4, x9) => handle_alu8       cpu A8_Sub R8_A R8_H
  | (x5, x9) => handle_alu8       cpu A8_Sub R8_A R8_L
  | (x6, x9) => handle_alu8_hl_r8 cpu A8_Sub R8_A
  | (x7, x9) => handle_alu8       cpu A8_Sub R8_A R8_A
  | (x8, x9) => handle_alu8       cpu A8_Sbc R8_A R8_B
  | (x9, x9) => handle_alu8       cpu A8_Sbc R8_A R8_C
  | (xa, x9) => handle_alu8       cpu A8_Sbc R8_A R8_D
  | (xb, x9) => handle_alu8       cpu A8_Sbc R8_A R8_E
  | (xc, x9) => handle_alu8       cpu A8_Sbc R8_A R8_H
  | (xd, x9) => handle_alu8       cpu A8_Sbc R8_A R8_L
  | (xe, x9) => handle_alu8_hl_r8 cpu A8_Sbc R8_A
  | (xf, x9) => handle_alu8       cpu A8_Sbc R8_A R8_A
  | (x0, xa) => handle_alu8       cpu A8_And R8_A R8_B
  | (x1, xa) => handle_alu8       cpu A8_And R8_A R8_C
  | (x2, xa) => handle_alu8       cpu A8_And R8_A R8_D
  | (x3, xa) => handle_alu8       cpu A8_And R8_A R8_E
  | (x4, xa) => handle_alu8       cpu A8_And R8_A R8_H
  | (x5, xa) => handle_alu8       cpu A8_And R8_A R8_L
  | (x6, xa) => handle_alu8_hl_r8 cpu A8_And R8_A
  | (x7, xa) => handle_alu8       cpu A8_And R8_A R8_A
  | (x8, xa) => handle_alu8       cpu A8_Xor R8_A R8_B
  | (x9, xa) => handle_alu8       cpu A8_Xor R8_A R8_C
  | (xa, xa) => handle_alu8       cpu A8_Xor R8_A R8_D
  | (xb, xa) => handle_alu8       cpu A8_Xor R8_A R8_E
  | (xc, xa) => handle_alu8       cpu A8_Xor R8_A R8_H
  | (xd, xa) => handle_alu8       cpu A8_Xor R8_A R8_L
  | (xe, xa) => handle_alu8_hl_r8 cpu A8_Xor R8_A
  | (xf, xa) => handle_alu8       cpu A8_Xor R8_A R8_A
  | (x0, xb) => handle_alu8       cpu A8_Or  R8_A R8_B
  | (x1, xb) => handle_alu8       cpu A8_Or  R8_A R8_C
  | (x2, xb) => handle_alu8       cpu A8_Or  R8_A R8_D
  | (x3, xb) => handle_alu8       cpu A8_Or  R8_A R8_E
  | (x4, xb) => handle_alu8       cpu A8_Or  R8_A R8_H
  | (x5, xb) => handle_alu8       cpu A8_Or  R8_A R8_L
  | (x6, xb) => handle_alu8_hl_r8 cpu A8_Or  R8_A
  | (x7, xb) => handle_alu8       cpu A8_Or  R8_A R8_A
  | (x8, xb) => handle_alu8       cpu A8_Cp  R8_A R8_B
  | (x9, xb) => handle_alu8       cpu A8_Cp  R8_A R8_C
  | (xa, xb) => handle_alu8       cpu A8_Cp  R8_A R8_D
  | (xb, xb) => handle_alu8       cpu A8_Cp  R8_A R8_E
  | (xc, xb) => handle_alu8       cpu A8_Cp  R8_A R8_H
  | (xd, xb) => handle_alu8       cpu A8_Cp  R8_A R8_L
  | (xe, xb) => handle_alu8_hl_r8 cpu A8_Cp  R8_A
  | (xf, xb) => handle_alu8       cpu A8_Cp  R8_A R8_A
  | (x0, xc) => handle_ret        cpu CC_NZ false
  | (x1, xc) => handle_pop        cpu R16P_BC
  | (x2, xc) => handle_jp         cpu CC_NZ
  | (x3, xc) => handle_jp         cpu CC_A
  | (x4, xc) => handle_call       cpu CC_NZ
  | (x5, xc) => handle_push       cpu R16P_BC
  | (x6, xc) => handle_alu8_d8    cpu A8_Add R8_A
  | (x7, xc) => handle_rst        cpu (x0, x0)
  | (x8, xc) => handle_ret        cpu CC_Z false
  | (x9, xc) => handle_ret        cpu CC_A false
  | (xa, xc) => handle_jp         cpu CC_Z
  | (xb, xc) => Some (cpu_with_uop (cpu_with_next_pc cpu) U_CB)
  | (xc, xc) => handle_call       cpu CC_Z
  | (xd, xc) => handle_call       cpu CC_A
  | (xe, xc) => handle_alu8_d8    cpu A8_Adc R8_A
  | (xf, xc) => handle_rst        cpu (x8, x0)
  | (x0, xd) => handle_ret        cpu CC_NC false
  | (x1, xd) => handle_pop        cpu R16P_DE
  | (x2, xd) => handle_jp         cpu CC_NC
  | (x3, xd) => None
  | (x4, xd) => handle_call       cpu CC_NC
  | (x5, xd) => handle_push       cpu R16P_DE
  | (x6, xd) => handle_alu8_d8    cpu A8_Sub R8_A
  | (x7, xd) => handle_rst        cpu (x0, x1)
  | (x8, xd) => handle_ret        cpu CC_C false
  | (x9, xd) => handle_ret        cpu CC_A true
  | (xa, xd) => handle_jp         cpu CC_C
  | (xb, xd) => None
  | (xc, xd) => handle_call       cpu CC_C
  | (xd, xd) => None
  | (xe, xd) => handle_alu8_d8    cpu A8_Sbc R8_A
  | (xf, xd) => handle_rst        cpu (x8, x1)
  | (x0, xe) => handle_movh       cpu W
  | (x1, xe) => handle_pop        cpu R16P_HL
  | (x2, xe) => handle_mov_c      cpu W
  | (x3, xe) => None
  | (x4, xe) => None
  | (x5, xe) => handle_push       cpu R16P_HL
  | (x6, xe) => handle_alu8_d8    cpu A8_And R8_A
  | (x7, xe) => handle_rst        cpu (x0, x2)
  | (x8, xe) => handle_add_sp_d8  cpu R16S_SP
  | (x9, xe) => handle_jp_hl      cpu
  | (xa, xe) => handle_mov_d16_r8 cpu R8_A W
  | (xb, xe) => None
  | (xc, xe) => None
  | (xd, xe) => None
  | (xe, xe) => handle_alu8_d8    cpu A8_Xor R8_A
  | (xf, xe) => handle_rst        cpu (x8, x2)
  | (x0, xf) => handle_movh       cpu R
  | (x1, xf) => handle_pop        cpu R16P_AF
  | (x2, xf) => handle_mov_c      cpu R
  | (x3, xf) => Some (cpu_with_uop (cpu_with_next_pc (cpu_with_ime cpu false)) U_FETCH)
  | (x4, xf) => None
  | (x5, xf) => handle_push       cpu R16P_AF
  | (x6, xf) => handle_alu8_d8    cpu A8_Or R8_A
  | (x7, xf) => handle_rst        cpu (x0, x3)
  | (x8, xf) => handle_add_sp_d8  cpu R16S_HL
  | (x9, xf) => handle_alu16      cpu A16_Mov R16S_SP R16S_HL
  | (xa, xf) => handle_mov_d16_r8 cpu R8_A R
  | (xb, xf) => Some (cpu_with_uop (cpu_with_next_pc (cpu_with_ime cpu true)) U_FETCH)
  | (xc, xf) => None
  | (xd, xf) => None
  | (xe, xf) => handle_alu8_d8    cpu A8_Cp R8_A
  | (xf, xf) => handle_rst        cpu (x8, x3)
  end.

Definition handle_cb_alu8 (cpu: Cpu) (op: alu8_op) (dst: reg_8) (src: reg_8): Cpu :=
  let v0 := get_reg_8 cpu.(r) dst in
  let v1 := get_reg_8 cpu.(r) src in
  match execute_alu8 op cpu.(f) v0 v1 with
  | (v, f) =>
    let r := set_reg_8 cpu.(r) dst v in
    cpu_with_uop (cpu_with_next_pc (cpu_with_reg_flags cpu r f)) U_FETCH
  end.

Definition handle_cb_alu8_hl (cpu: Cpu) (op: alu8_op): Cpu :=
  cpu_with_uop (cpu_with_next_pc cpu) (U_ALU8_HL_M2 op).

Definition handle_cb_alu8_bit (cpu: Cpu) (bit: bit_8): Cpu :=
  cpu_with_uop (cpu_with_next_pc cpu) (U_ALU8_BIT_M2 bit).

Definition handle_cb_op (cpu: Cpu) (op: u8) : Cpu :=
  match op with
  | (x0, x0) => handle_cb_alu8     cpu A8_Rlc R8_B R8_B
  | (x1, x0) => handle_cb_alu8     cpu A8_Rlc R8_C R8_C
  | (x2, x0) => handle_cb_alu8     cpu A8_Rlc R8_D R8_D
  | (x3, x0) => handle_cb_alu8     cpu A8_Rlc R8_E R8_E
  | (x4, x0) => handle_cb_alu8     cpu A8_Rlc R8_H R8_H
  | (x5, x0) => handle_cb_alu8     cpu A8_Rlc R8_L R8_L
  | (x6, x0) => handle_cb_alu8_hl  cpu A8_Rlc
  | (x7, x0) => handle_cb_alu8     cpu A8_Rlc R8_A R8_A
  | (x8, x0) => handle_cb_alu8     cpu A8_Rrc R8_B R8_B
  | (x9, x0) => handle_cb_alu8     cpu A8_Rrc R8_C R8_C
  | (xa, x0) => handle_cb_alu8     cpu A8_Rrc R8_D R8_D
  | (xb, x0) => handle_cb_alu8     cpu A8_Rrc R8_E R8_E
  | (xc, x0) => handle_cb_alu8     cpu A8_Rrc R8_H R8_H
  | (xd, x0) => handle_cb_alu8     cpu A8_Rrc R8_L R8_L
  | (xe, x0) => handle_cb_alu8_hl  cpu A8_Rrc
  | (xf, x0) => handle_cb_alu8     cpu A8_Rrc R8_A R8_A
  | (x0, x1) => handle_cb_alu8     cpu A8_Rl R8_B R8_B
  | (x1, x1) => handle_cb_alu8     cpu A8_Rl R8_C R8_C
  | (x2, x1) => handle_cb_alu8     cpu A8_Rl R8_D R8_D
  | (x3, x1) => handle_cb_alu8     cpu A8_Rl R8_E R8_E
  | (x4, x1) => handle_cb_alu8     cpu A8_Rl R8_H R8_H
  | (x5, x1) => handle_cb_alu8     cpu A8_Rl R8_L R8_L
  | (x6, x1) => handle_cb_alu8_hl  cpu A8_Rl
  | (x7, x1) => handle_cb_alu8     cpu A8_Rl R8_A R8_A
  | (x8, x1) => handle_cb_alu8     cpu A8_Rr R8_B R8_B
  | (x9, x1) => handle_cb_alu8     cpu A8_Rr R8_C R8_C
  | (xa, x1) => handle_cb_alu8     cpu A8_Rr R8_D R8_D
  | (xb, x1) => handle_cb_alu8     cpu A8_Rr R8_E R8_E
  | (xc, x1) => handle_cb_alu8     cpu A8_Rr R8_H R8_H
  | (xd, x1) => handle_cb_alu8     cpu A8_Rr R8_L R8_L
  | (xe, x1) => handle_cb_alu8_hl  cpu A8_Rr
  | (xf, x1) => handle_cb_alu8     cpu A8_Rr R8_A R8_A
  | (x0, x2) => handle_cb_alu8     cpu A8_Sla R8_B R8_B
  | (x1, x2) => handle_cb_alu8     cpu A8_Sla R8_C R8_C
  | (x2, x2) => handle_cb_alu8     cpu A8_Sla R8_D R8_D
  | (x3, x2) => handle_cb_alu8     cpu A8_Sla R8_E R8_E
  | (x4, x2) => handle_cb_alu8     cpu A8_Sla R8_H R8_H
  | (x5, x2) => handle_cb_alu8     cpu A8_Sla R8_L R8_L
  | (x6, x2) => handle_cb_alu8_hl  cpu A8_Sla
  | (x7, x2) => handle_cb_alu8     cpu A8_Sla R8_A R8_A
  | (x8, x2) => handle_cb_alu8     cpu A8_Sra R8_B R8_B
  | (x9, x2) => handle_cb_alu8     cpu A8_Sra R8_C R8_C
  | (xa, x2) => handle_cb_alu8     cpu A8_Sra R8_D R8_D
  | (xb, x2) => handle_cb_alu8     cpu A8_Sra R8_E R8_E
  | (xc, x2) => handle_cb_alu8     cpu A8_Sra R8_H R8_H
  | (xd, x2) => handle_cb_alu8     cpu A8_Sra R8_L R8_L
  | (xe, x2) => handle_cb_alu8_hl  cpu A8_Sra
  | (xf, x2) => handle_cb_alu8     cpu A8_Sra R8_A R8_A
  | (x0, x3) => handle_cb_alu8     cpu A8_Swap R8_B R8_B
  | (x1, x3) => handle_cb_alu8     cpu A8_Swap R8_C R8_C
  | (x2, x3) => handle_cb_alu8     cpu A8_Swap R8_D R8_D
  | (x3, x3) => handle_cb_alu8     cpu A8_Swap R8_E R8_E
  | (x4, x3) => handle_cb_alu8     cpu A8_Swap R8_H R8_H
  | (x5, x3) => handle_cb_alu8     cpu A8_Swap R8_L R8_L
  | (x6, x3) => handle_cb_alu8_hl  cpu A8_Swap
  | (x7, x3) => handle_cb_alu8     cpu A8_Swap R8_A R8_A
  | (x8, x3) => handle_cb_alu8     cpu A8_Srl R8_B R8_B
  | (x9, x3) => handle_cb_alu8     cpu A8_Srl R8_C R8_C
  | (xa, x3) => handle_cb_alu8     cpu A8_Srl R8_D R8_D
  | (xb, x3) => handle_cb_alu8     cpu A8_Srl R8_E R8_E
  | (xc, x3) => handle_cb_alu8     cpu A8_Srl R8_H R8_H
  | (xd, x3) => handle_cb_alu8     cpu A8_Srl R8_L R8_L
  | (xe, x3) => handle_cb_alu8_hl  cpu A8_Srl
  | (xf, x3) => handle_cb_alu8     cpu A8_Srl R8_A R8_A
  | (x0, x4) => handle_cb_alu8     cpu (A8_Bit B0) R8_B R8_B
  | (x1, x4) => handle_cb_alu8     cpu (A8_Bit B0) R8_C R8_C
  | (x2, x4) => handle_cb_alu8     cpu (A8_Bit B0) R8_D R8_D
  | (x3, x4) => handle_cb_alu8     cpu (A8_Bit B0) R8_E R8_E
  | (x4, x4) => handle_cb_alu8     cpu (A8_Bit B0) R8_H R8_H
  | (x5, x4) => handle_cb_alu8     cpu (A8_Bit B0) R8_L R8_L
  | (x6, x4) => handle_cb_alu8_bit cpu B0
  | (x7, x4) => handle_cb_alu8     cpu (A8_Bit B0) R8_A R8_A
  | (x8, x4) => handle_cb_alu8     cpu (A8_Bit B1) R8_B R8_B
  | (x9, x4) => handle_cb_alu8     cpu (A8_Bit B1) R8_C R8_C
  | (xa, x4) => handle_cb_alu8     cpu (A8_Bit B1) R8_D R8_D
  | (xb, x4) => handle_cb_alu8     cpu (A8_Bit B1) R8_E R8_E
  | (xc, x4) => handle_cb_alu8     cpu (A8_Bit B1) R8_H R8_H
  | (xd, x4) => handle_cb_alu8     cpu (A8_Bit B1) R8_L R8_L
  | (xe, x4) => handle_cb_alu8_bit cpu B1
  | (xf, x4) => handle_cb_alu8     cpu (A8_Bit B1) R8_A R8_A
  | (x0, x5) => handle_cb_alu8     cpu (A8_Bit B2) R8_B R8_B
  | (x1, x5) => handle_cb_alu8     cpu (A8_Bit B2) R8_C R8_C
  | (x2, x5) => handle_cb_alu8     cpu (A8_Bit B2) R8_D R8_D
  | (x3, x5) => handle_cb_alu8     cpu (A8_Bit B2) R8_E R8_E
  | (x4, x5) => handle_cb_alu8     cpu (A8_Bit B2) R8_H R8_H
  | (x5, x5) => handle_cb_alu8     cpu (A8_Bit B2) R8_L R8_L
  | (x6, x5) => handle_cb_alu8_bit cpu B2
  | (x7, x5) => handle_cb_alu8     cpu (A8_Bit B2) R8_A R8_A
  | (x8, x5) => handle_cb_alu8     cpu (A8_Bit B3) R8_B R8_B
  | (x9, x5) => handle_cb_alu8     cpu (A8_Bit B3) R8_C R8_C
  | (xa, x5) => handle_cb_alu8     cpu (A8_Bit B3) R8_D R8_D
  | (xb, x5) => handle_cb_alu8     cpu (A8_Bit B3) R8_E R8_E
  | (xc, x5) => handle_cb_alu8     cpu (A8_Bit B3) R8_H R8_H
  | (xd, x5) => handle_cb_alu8     cpu (A8_Bit B3) R8_L R8_L
  | (xe, x5) => handle_cb_alu8_bit cpu B3
  | (xf, x5) => handle_cb_alu8     cpu (A8_Bit B3) R8_A R8_A
  | (x0, x6) => handle_cb_alu8     cpu (A8_Bit B4) R8_B R8_B
  | (x1, x6) => handle_cb_alu8     cpu (A8_Bit B4) R8_C R8_C
  | (x2, x6) => handle_cb_alu8     cpu (A8_Bit B4) R8_D R8_D
  | (x3, x6) => handle_cb_alu8     cpu (A8_Bit B4) R8_E R8_E
  | (x4, x6) => handle_cb_alu8     cpu (A8_Bit B4) R8_H R8_H
  | (x5, x6) => handle_cb_alu8     cpu (A8_Bit B4) R8_L R8_L
  | (x6, x6) => handle_cb_alu8_bit cpu B4
  | (x7, x6) => handle_cb_alu8     cpu (A8_Bit B4) R8_A R8_A
  | (x8, x6) => handle_cb_alu8     cpu (A8_Bit B5) R8_B R8_B
  | (x9, x6) => handle_cb_alu8     cpu (A8_Bit B5) R8_C R8_C
  | (xa, x6) => handle_cb_alu8     cpu (A8_Bit B5) R8_D R8_D
  | (xb, x6) => handle_cb_alu8     cpu (A8_Bit B5) R8_E R8_E
  | (xc, x6) => handle_cb_alu8     cpu (A8_Bit B5) R8_H R8_H
  | (xd, x6) => handle_cb_alu8     cpu (A8_Bit B5) R8_L R8_L
  | (xe, x6) => handle_cb_alu8_bit cpu B5
  | (xf, x6) => handle_cb_alu8     cpu (A8_Bit B5) R8_A R8_A
  | (x0, x7) => handle_cb_alu8     cpu (A8_Bit B6) R8_B R8_B
  | (x1, x7) => handle_cb_alu8     cpu (A8_Bit B6) R8_C R8_C
  | (x2, x7) => handle_cb_alu8     cpu (A8_Bit B6) R8_D R8_D
  | (x3, x7) => handle_cb_alu8     cpu (A8_Bit B6) R8_E R8_E
  | (x4, x7) => handle_cb_alu8     cpu (A8_Bit B6) R8_H R8_H
  | (x5, x7) => handle_cb_alu8     cpu (A8_Bit B6) R8_L R8_L
  | (x6, x7) => handle_cb_alu8_bit cpu B6
  | (x7, x7) => handle_cb_alu8     cpu (A8_Bit B6) R8_A R8_A
  | (x8, x7) => handle_cb_alu8     cpu (A8_Bit B7) R8_B R8_B
  | (x9, x7) => handle_cb_alu8     cpu (A8_Bit B7) R8_C R8_C
  | (xa, x7) => handle_cb_alu8     cpu (A8_Bit B7) R8_D R8_D
  | (xb, x7) => handle_cb_alu8     cpu (A8_Bit B7) R8_E R8_E
  | (xc, x7) => handle_cb_alu8     cpu (A8_Bit B7) R8_H R8_H
  | (xd, x7) => handle_cb_alu8     cpu (A8_Bit B7) R8_L R8_L
  | (xe, x7) => handle_cb_alu8_bit cpu B7
  | (xf, x7) => handle_cb_alu8     cpu (A8_Bit B7) R8_A R8_A
  | (x0, x8) => handle_cb_alu8     cpu (A8_Res B0) R8_B R8_B
  | (x1, x8) => handle_cb_alu8     cpu (A8_Res B0) R8_C R8_C
  | (x2, x8) => handle_cb_alu8     cpu (A8_Res B0) R8_D R8_D
  | (x3, x8) => handle_cb_alu8     cpu (A8_Res B0) R8_E R8_E
  | (x4, x8) => handle_cb_alu8     cpu (A8_Res B0) R8_H R8_H
  | (x5, x8) => handle_cb_alu8     cpu (A8_Res B0) R8_L R8_L
  | (x6, x8) => handle_cb_alu8_hl  cpu (A8_Res B0)
  | (x7, x8) => handle_cb_alu8     cpu (A8_Res B0) R8_A R8_A
  | (x8, x8) => handle_cb_alu8     cpu (A8_Res B1) R8_B R8_B
  | (x9, x8) => handle_cb_alu8     cpu (A8_Res B1) R8_C R8_C
  | (xa, x8) => handle_cb_alu8     cpu (A8_Res B1) R8_D R8_D
  | (xb, x8) => handle_cb_alu8     cpu (A8_Res B1) R8_E R8_E
  | (xc, x8) => handle_cb_alu8     cpu (A8_Res B1) R8_H R8_H
  | (xd, x8) => handle_cb_alu8     cpu (A8_Res B1) R8_L R8_L
  | (xe, x8) => handle_cb_alu8_hl  cpu (A8_Res B1)
  | (xf, x8) => handle_cb_alu8     cpu (A8_Res B1) R8_A R8_A
  | (x0, x9) => handle_cb_alu8     cpu (A8_Res B2) R8_B R8_B
  | (x1, x9) => handle_cb_alu8     cpu (A8_Res B2) R8_C R8_C
  | (x2, x9) => handle_cb_alu8     cpu (A8_Res B2) R8_D R8_D
  | (x3, x9) => handle_cb_alu8     cpu (A8_Res B2) R8_E R8_E
  | (x4, x9) => handle_cb_alu8     cpu (A8_Res B2) R8_H R8_H
  | (x5, x9) => handle_cb_alu8     cpu (A8_Res B2) R8_L R8_L
  | (x6, x9) => handle_cb_alu8_hl  cpu (A8_Res B2)
  | (x7, x9) => handle_cb_alu8     cpu (A8_Res B2) R8_A R8_A
  | (x8, x9) => handle_cb_alu8     cpu (A8_Res B3) R8_B R8_B
  | (x9, x9) => handle_cb_alu8     cpu (A8_Res B3) R8_C R8_C
  | (xa, x9) => handle_cb_alu8     cpu (A8_Res B3) R8_D R8_D
  | (xb, x9) => handle_cb_alu8     cpu (A8_Res B3) R8_E R8_E
  | (xc, x9) => handle_cb_alu8     cpu (A8_Res B3) R8_H R8_H
  | (xd, x9) => handle_cb_alu8     cpu (A8_Res B3) R8_L R8_L
  | (xe, x9) => handle_cb_alu8_hl  cpu (A8_Res B3)
  | (xf, x9) => handle_cb_alu8     cpu (A8_Res B3) R8_A R8_A
  | (x0, xa) => handle_cb_alu8     cpu (A8_Res B4) R8_B R8_B
  | (x1, xa) => handle_cb_alu8     cpu (A8_Res B4) R8_C R8_C
  | (x2, xa) => handle_cb_alu8     cpu (A8_Res B4) R8_D R8_D
  | (x3, xa) => handle_cb_alu8     cpu (A8_Res B4) R8_E R8_E
  | (x4, xa) => handle_cb_alu8     cpu (A8_Res B4) R8_H R8_H
  | (x5, xa) => handle_cb_alu8     cpu (A8_Res B4) R8_L R8_L
  | (x6, xa) => handle_cb_alu8_hl  cpu (A8_Res B4)
  | (x7, xa) => handle_cb_alu8     cpu (A8_Res B4) R8_A R8_A
  | (x8, xa) => handle_cb_alu8     cpu (A8_Res B5) R8_B R8_B
  | (x9, xa) => handle_cb_alu8     cpu (A8_Res B5) R8_C R8_C
  | (xa, xa) => handle_cb_alu8     cpu (A8_Res B5) R8_D R8_D
  | (xb, xa) => handle_cb_alu8     cpu (A8_Res B5) R8_E R8_E
  | (xc, xa) => handle_cb_alu8     cpu (A8_Res B5) R8_H R8_H
  | (xd, xa) => handle_cb_alu8     cpu (A8_Res B5) R8_L R8_L
  | (xe, xa) => handle_cb_alu8_hl  cpu (A8_Res B5)
  | (xf, xa) => handle_cb_alu8     cpu (A8_Res B5) R8_A R8_A
  | (x0, xb) => handle_cb_alu8     cpu (A8_Res B6) R8_B R8_B
  | (x1, xb) => handle_cb_alu8     cpu (A8_Res B6) R8_C R8_C
  | (x2, xb) => handle_cb_alu8     cpu (A8_Res B6) R8_D R8_D
  | (x3, xb) => handle_cb_alu8     cpu (A8_Res B6) R8_E R8_E
  | (x4, xb) => handle_cb_alu8     cpu (A8_Res B6) R8_H R8_H
  | (x5, xb) => handle_cb_alu8     cpu (A8_Res B6) R8_L R8_L
  | (x6, xb) => handle_cb_alu8_hl  cpu (A8_Res B6)
  | (x7, xb) => handle_cb_alu8     cpu (A8_Res B6) R8_A R8_A
  | (x8, xb) => handle_cb_alu8     cpu (A8_Res B7) R8_B R8_B
  | (x9, xb) => handle_cb_alu8     cpu (A8_Res B7) R8_C R8_C
  | (xa, xb) => handle_cb_alu8     cpu (A8_Res B7) R8_D R8_D
  | (xb, xb) => handle_cb_alu8     cpu (A8_Res B7) R8_E R8_E
  | (xc, xb) => handle_cb_alu8     cpu (A8_Res B7) R8_H R8_H
  | (xd, xb) => handle_cb_alu8     cpu (A8_Res B7) R8_L R8_L
  | (xe, xb) => handle_cb_alu8_hl  cpu (A8_Res B7)
  | (xf, xb) => handle_cb_alu8     cpu (A8_Res B7) R8_A R8_A
  | (x0, xc) => handle_cb_alu8     cpu (A8_Set B0) R8_B R8_B
  | (x1, xc) => handle_cb_alu8     cpu (A8_Set B0) R8_C R8_C
  | (x2, xc) => handle_cb_alu8     cpu (A8_Set B0) R8_D R8_D
  | (x3, xc) => handle_cb_alu8     cpu (A8_Set B0) R8_E R8_E
  | (x4, xc) => handle_cb_alu8     cpu (A8_Set B0) R8_H R8_H
  | (x5, xc) => handle_cb_alu8     cpu (A8_Set B0) R8_L R8_L
  | (x6, xc) => handle_cb_alu8_hl  cpu (A8_Set B0)
  | (x7, xc) => handle_cb_alu8     cpu (A8_Set B0) R8_A R8_A
  | (x8, xc) => handle_cb_alu8     cpu (A8_Set B1) R8_B R8_B
  | (x9, xc) => handle_cb_alu8     cpu (A8_Set B1) R8_C R8_C
  | (xa, xc) => handle_cb_alu8     cpu (A8_Set B1) R8_D R8_D
  | (xb, xc) => handle_cb_alu8     cpu (A8_Set B1) R8_E R8_E
  | (xc, xc) => handle_cb_alu8     cpu (A8_Set B1) R8_H R8_H
  | (xd, xc) => handle_cb_alu8     cpu (A8_Set B1) R8_L R8_L
  | (xe, xc) => handle_cb_alu8_hl  cpu (A8_Set B1)
  | (xf, xc) => handle_cb_alu8     cpu (A8_Set B1) R8_A R8_A
  | (x0, xd) => handle_cb_alu8     cpu (A8_Set B2) R8_B R8_B
  | (x1, xd) => handle_cb_alu8     cpu (A8_Set B2) R8_C R8_C
  | (x2, xd) => handle_cb_alu8     cpu (A8_Set B2) R8_D R8_D
  | (x3, xd) => handle_cb_alu8     cpu (A8_Set B2) R8_E R8_E
  | (x4, xd) => handle_cb_alu8     cpu (A8_Set B2) R8_H R8_H
  | (x5, xd) => handle_cb_alu8     cpu (A8_Set B2) R8_L R8_L
  | (x6, xd) => handle_cb_alu8_hl  cpu (A8_Set B2)
  | (x7, xd) => handle_cb_alu8     cpu (A8_Set B2) R8_A R8_A
  | (x8, xd) => handle_cb_alu8     cpu (A8_Set B3) R8_B R8_B
  | (x9, xd) => handle_cb_alu8     cpu (A8_Set B3) R8_C R8_C
  | (xa, xd) => handle_cb_alu8     cpu (A8_Set B3) R8_D R8_D
  | (xb, xd) => handle_cb_alu8     cpu (A8_Set B3) R8_E R8_E
  | (xc, xd) => handle_cb_alu8     cpu (A8_Set B3) R8_H R8_H
  | (xd, xd) => handle_cb_alu8     cpu (A8_Set B3) R8_L R8_L
  | (xe, xd) => handle_cb_alu8_hl  cpu (A8_Set B3)
  | (xf, xd) => handle_cb_alu8     cpu (A8_Set B3) R8_A R8_A
  | (x0, xe) => handle_cb_alu8     cpu (A8_Set B4) R8_B R8_B
  | (x1, xe) => handle_cb_alu8     cpu (A8_Set B4) R8_C R8_C
  | (x2, xe) => handle_cb_alu8     cpu (A8_Set B4) R8_D R8_D
  | (x3, xe) => handle_cb_alu8     cpu (A8_Set B4) R8_E R8_E
  | (x4, xe) => handle_cb_alu8     cpu (A8_Set B4) R8_H R8_H
  | (x5, xe) => handle_cb_alu8     cpu (A8_Set B4) R8_L R8_L
  | (x6, xe) => handle_cb_alu8_hl  cpu (A8_Set B4)
  | (x7, xe) => handle_cb_alu8     cpu (A8_Set B4) R8_A R8_A
  | (x8, xe) => handle_cb_alu8     cpu (A8_Set B5) R8_B R8_B
  | (x9, xe) => handle_cb_alu8     cpu (A8_Set B5) R8_C R8_C
  | (xa, xe) => handle_cb_alu8     cpu (A8_Set B5) R8_D R8_D
  | (xb, xe) => handle_cb_alu8     cpu (A8_Set B5) R8_E R8_E
  | (xc, xe) => handle_cb_alu8     cpu (A8_Set B5) R8_H R8_H
  | (xd, xe) => handle_cb_alu8     cpu (A8_Set B5) R8_L R8_L
  | (xe, xe) => handle_cb_alu8_hl  cpu (A8_Set B5)
  | (xf, xe) => handle_cb_alu8     cpu (A8_Set B5) R8_A R8_A
  | (x0, xf) => handle_cb_alu8     cpu (A8_Set B6) R8_B R8_B
  | (x1, xf) => handle_cb_alu8     cpu (A8_Set B6) R8_C R8_C
  | (x2, xf) => handle_cb_alu8     cpu (A8_Set B6) R8_D R8_D
  | (x3, xf) => handle_cb_alu8     cpu (A8_Set B6) R8_E R8_E
  | (x4, xf) => handle_cb_alu8     cpu (A8_Set B6) R8_H R8_H
  | (x5, xf) => handle_cb_alu8     cpu (A8_Set B6) R8_L R8_L
  | (x6, xf) => handle_cb_alu8_hl  cpu (A8_Set B6)
  | (x7, xf) => handle_cb_alu8     cpu (A8_Set B6) R8_A R8_A
  | (x8, xf) => handle_cb_alu8     cpu (A8_Set B7) R8_B R8_B
  | (x9, xf) => handle_cb_alu8     cpu (A8_Set B7) R8_C R8_C
  | (xa, xf) => handle_cb_alu8     cpu (A8_Set B7) R8_D R8_D
  | (xb, xf) => handle_cb_alu8     cpu (A8_Set B7) R8_E R8_E
  | (xc, xf) => handle_cb_alu8     cpu (A8_Set B7) R8_H R8_H
  | (xd, xf) => handle_cb_alu8     cpu (A8_Set B7) R8_L R8_L
  | (xe, xf) => handle_cb_alu8_hl  cpu (A8_Set B7)
  | (xf, xf) => handle_cb_alu8     cpu (A8_Set B7) R8_A R8_A
  end.

(******************************************************************************)

Definition all_interrupts :=
  [ Int_VBlank
  ; Int_Stat
  ; Int_Timer
  ; Int_Serial
  ; Int_Pins
  ].

Definition has_pending_interrupts {T: Type} (sys: System T) :=
  existsb (fun int => sys_is_interrupt_pending sys int) all_interrupts.

Definition cpu_is_halted {T: Type} (cpu: Cpu) (sys: System T) :=
  cpu.(halted) && negb (has_pending_interrupts sys).

Definition cpu_interrupted {T: Type} (cpu: Cpu) (sys: System T) (int: interrupt) :=
  cpu.(ime) &&
  sys_is_interrupt_pending sys int &&
  sys_is_interrupt_enabled sys int.

Definition handle_interrupt {T: Type} (cpu: Cpu) (sys: System T) (addr: u8) (int: interrupt) : option (Cpu * System T) :=
  match sys_clear_interrupt sys int with
  | None => None
  | Some sys => Some (cpu_with_uop (cpu_with_ime cpu false) (U_INT_M2 addr), sys)
  end.


(******************************************************************************)

Definition tick {T: Type} (cpu: Cpu) (sys: System T): option (Cpu * System T) :=
  match cpu.(uop) with
  | U_FETCH =>
    (* Check for interrupts and enter the ISR if any are pending. *)
    (* If there are no interrupts and the CPU is halted, spin. *)
    (* Otherwise, fetch an opcode and start decoding the instruction. *)
    if cpu_interrupted cpu sys Int_VBlank then
      handle_interrupt cpu sys (x0, x4) Int_VBlank
    else if cpu_interrupted cpu sys Int_Stat then
      handle_interrupt cpu sys (x8, x4) Int_Stat
    else if cpu_interrupted cpu sys Int_Timer then
      handle_interrupt cpu sys (x0, x5) Int_Timer
    else if cpu_interrupted cpu sys Int_Serial then
      handle_interrupt cpu sys (x8, x5) Int_Serial
    else if cpu_interrupted cpu sys Int_Pins then
      handle_interrupt cpu sys (x0, x6) Int_Pins
    else if cpu_is_halted cpu sys then
      Some (cpu, sys)
    else
      match sys_read sys cpu.(r).(pc) with
      | None => None
      | Some (op, sys) =>
        match handle_op cpu op with
        | None => None
        | Some cpu => Some (cpu, sys)
        end
      end
  | U_CB =>
    (* Read the opcode after the prefix and handle it. *)
    match sys_read sys cpu.(r).(pc) with
    | None => None
    | Some (op, sys) => Some (handle_cb_op cpu op, sys)
    end

  (* ld r16, d16 *)
  | U_LD_R16_D16_M2 dst =>
    imm_reader cpu sys (fun cpu v =>
      cpu_with_uop (cpu_with_reg cpu (set_reg_16s_lo cpu.(r) dst v)) (U_LD_R16_D16_M3 dst)
    )
  | U_LD_R16_D16_M3 dst =>
    imm_reader cpu sys (fun cpu v =>
      cpu_with_uop (cpu_with_reg cpu (set_reg_16s_hi cpu.(r) dst v)) U_FETCH
    )

  (* ld r8, (r16) *)
  | U_MOV_R8_R16_M2_R reg_dst reg_addr =>
    let addr := get_reg_16 cpu.(r) reg_addr in
    mem_reader cpu sys addr (fun cpu v =>
      cpu_with_uop (cpu_with_reg cpu (set_reg_8 cpu.(r) reg_dst v)) U_FETCH
    )
  (* ld (r16), r8 *)
  | U_MOV_R8_R16_M2_W dst src =>
    let addr := get_reg_16 cpu.(r) src in
    let v := get_reg_8 cpu.(r) dst in
    mem_writer cpu sys addr v (fun cpu => cpu_with_uop cpu U_FETCH)

  (* jr *)
  | U_JR_M2 cc =>
    imm_reader cpu sys (fun cpu off =>
      cpu_with_uop cpu (if is_taken cc cpu.(f) then U_JR_M3 off else U_FETCH)
    )
  | U_JR_M3 off =>
    let pc := u16_add_wrap cpu.(r).(pc) (u8_sext_u16 off) in
    Some (cpu_with_uop (cpu_with_reg cpu (reg_with_pc cpu.(r) pc)) U_FETCH, sys)

  (* ld (HL±), r8 *)
  | U_MOV_HL_M2_W op src =>
    let v := get_reg_8 cpu.(r) src in
    let addr := get_reg_hl cpu.(r) in
    mem_writer cpu sys addr v (fun cpu =>
      let new_addr :=
        match op with
        | Dir_Nop => addr
        | Dir_Inc => inc_wrap_u16 addr
        | Dir_Dec => dec_wrap_u16 addr
        end
      in
      cpu_with_uop (cpu_with_reg cpu (set_reg_hl cpu.(r) new_addr)) U_FETCH
    )

  (* ld r8, (HL±) *)
  | U_MOV_HL_M2_R op dst =>
    let addr := get_reg_hl cpu.(r) in
    mem_reader cpu sys addr (fun cpu v =>
      let new_addr :=
        match op with
        | Dir_Nop => addr
        | Dir_Inc => inc_wrap_u16 addr
        | Dir_Dec => dec_wrap_u16 addr
        end
      in
      let r := set_reg_8 (set_reg_hl cpu.(r) new_addr) dst v in
      cpu_with_uop (cpu_with_reg cpu r) U_FETCH
    )

  (* ld r8, d8 *)
  | U_LD_R8_D8_M2 dst =>
    imm_reader cpu sys (fun cpu v =>
      cpu_with_uop (cpu_with_reg cpu (set_reg_8 cpu.(r) dst v)) U_FETCH
    )

  (* ld (c), a *)
  | U_MOV_C_M2_R =>
    mem_reader cpu sys (cpu.(r).(c), (xf, xf)) (fun cpu v =>
      cpu_with_uop (cpu_with_reg cpu (set_reg_8 cpu.(r) R8_A v)) U_FETCH
    )
  | U_MOV_C_M2_W =>
    mem_writer cpu sys (cpu.(r).(c), (xf, xf)) cpu.(r).(a) (fun cpu =>
      cpu_with_uop cpu U_FETCH
    )

  (* ld (d8), a *)
  | U_MOV_D8_M2 rw =>
    imm_reader cpu sys (fun cpu v =>
      match rw with
      | R => cpu_with_uop cpu (U_MOV_D8_M3_R v)
      | W => cpu_with_uop cpu (U_MOV_D8_M3_W v)
      end
    )

  | U_MOV_D8_M3_W lo =>
    mem_writer cpu sys (lo, (xf, xf)) cpu.(r).(a) (
      fun cpu => cpu_with_uop cpu U_FETCH
    )

  | U_MOV_D8_M3_R lo =>
    mem_reader cpu sys (lo, (xf, xf)) (fun cpu v =>
      cpu_with_uop (cpu_with_reg cpu (set_reg_8 cpu.(r) R8_A v)) U_FETCH
    )

  (* call/call[cc] *)
  | U_CALL_M2 cc =>
    imm_reader cpu sys (fun cpu lo =>
      cpu_with_uop cpu (U_CALL_M3 cc lo)
    )
  | U_CALL_M3 cc lo =>
    imm_reader cpu sys (fun cpu hi =>
      cpu_with_uop cpu (if is_taken cc cpu.(f) then U_CALL_M4 lo hi else U_FETCH)
    )
  | U_CALL_M4 lo hi =>
    Some (cpu_with_uop (cpu_with_prev_sp cpu) (U_CALL_M5 lo hi), sys)
  | U_CALL_M5 lo hi =>
    match cpu.(r).(pc) with
    | (_, pc_hi) =>
      mem_writer cpu sys cpu.(r).(sp) pc_hi (fun cpu =>
        let sp := dec_wrap_u16 cpu.(r).(sp) in
        let reg := reg_with_pc_hi (reg_with_sp cpu.(r) sp) hi in
        cpu_with_uop (cpu_with_reg cpu reg) (U_CALL_M6 lo)
      )
    end
  | U_CALL_M6 lo =>
    match cpu.(r).(pc) with
    | (pc_lo, _) =>
      mem_writer cpu sys cpu.(r).(sp) pc_lo (fun cpu =>
        cpu_with_uop (cpu_with_reg cpu (reg_with_pc_lo cpu.(r) lo)) U_FETCH
      )
    end

  (* jp d16 *)
  | U_JP_D16_M2 cc =>
    imm_reader cpu sys (fun cpu lo =>
      cpu_with_uop cpu (U_JP_D16_M3 cc lo)
    )
  | U_JP_D16_M3 cc lo =>
    imm_reader cpu sys (fun cpu hi =>
      cpu_with_uop cpu (if is_taken cc cpu.(f) then U_JP_D16_M4 lo hi else U_FETCH)
    )
  | U_JP_D16_M4 lo hi =>
    Some (cpu_with_uop (cpu_with_reg cpu (reg_with_pc cpu.(r) (lo, hi))) U_FETCH, sys)

  (* push *)
  | U_PUSH_M2 dd =>
    Some (cpu_with_uop (cpu_with_prev_sp cpu) (U_PUSH_M3 dd), sys)
  | U_PUSH_M3 dd =>
    let hi := match dd with
      | R16P_BC => cpu.(r).(b)
      | R16P_DE => cpu.(r).(d)
      | R16P_HL => cpu.(r).(h)
      | R16P_AF => cpu.(r).(a)
      end
    in
    mem_writer cpu sys cpu.(r).(sp) hi (fun cpu =>
      cpu_with_uop (cpu_with_prev_sp cpu) (U_PUSH_M4 dd)
    )
  | U_PUSH_M4 dd =>
    let lo := match dd with
      | R16P_BC => cpu.(r).(c)
      | R16P_DE => cpu.(r).(e)
      | R16P_HL => cpu.(r).(l)
      | R16P_AF =>
        (x0, u4_of_bits (cpu.(f).(cf), cpu.(f).(hf), cpu.(f).(nf), cpu.(f).(zf)))
      end
    in
    mem_writer cpu sys cpu.(r).(sp) lo (fun cpu =>
      cpu_with_uop cpu U_FETCH
    )

  (* pop *)
  | U_POP_M2 dd =>
    mem_reader cpu sys cpu.(r).(sp) (fun cpu lo =>
      let r' :=
        match dd with
        | R16P_BC => set_reg_8 cpu.(r) R8_C lo
        | R16P_DE => set_reg_8 cpu.(r) R8_E lo
        | R16P_HL => set_reg_8 cpu.(r) R8_L lo
        | R16P_AF => cpu.(r)
        end
      in
      let f' :=
        match dd with
        | R16P_BC => cpu.(f)
        | R16P_DE => cpu.(f)
        | R16P_HL => cpu.(f)
        | R16P_AF => flags_from_bits lo
        end
      in
      cpu_with_uop (cpu_with_next_sp (cpu_with_reg_flags cpu r' f')) (U_POP_M3 dd)
    )
  | U_POP_M3 dd =>
    mem_reader cpu sys cpu.(r).(sp) (fun cpu hi =>
      let r' :=
        match dd with
        | R16P_BC => set_reg_8 cpu.(r) R8_B hi
        | R16P_DE => set_reg_8 cpu.(r) R8_D hi
        | R16P_HL => set_reg_8 cpu.(r) R8_H hi
        | R16P_AF => set_reg_8 cpu.(r) R8_A hi
        end
      in
      cpu_with_uop (cpu_with_next_sp (cpu_with_reg cpu r')) U_FETCH
    )

  (* alu16 reg16 *)
  | U_ALU16_M2 op dst src =>
    let v0 := get_reg_16s cpu.(r) dst in
    let v1 := get_reg_16s cpu.(r) src in
    let vf := match op with
      | A16_Inc => (inc_wrap_u16 v1, cpu.(f))
      | A16_Dec => (dec_wrap_u16 v1, cpu.(f))
      | A16_Mov => (v1, cpu.(f))
      | A16_Add =>
        match v0 with
        | (l0, (lh0, hh0)) =>
          match v1 with
          | (l1, (lh1, hh1)) =>
            match u8_add l0 l1 with
            | (lr, cl) =>
              match u4_adc lh0 lh1 cl with
              | (lhr, h) =>
                match u4_adc hh0 hh1 h with
                | (hhr, c) =>
                  let v := (lr, (lhr, hhr)) in
                  (v, {| zf := cpu.(f).(zf); nf := false; hf := h; cf := c |})
                end
              end
            end
          end
        end
      end
    in
    match vf with
    | (v, f) =>
      let r := set_reg_16s cpu.(r) dst v in
      Some (cpu_with_uop (cpu_with_reg_flags cpu r f) U_FETCH, sys)
    end

  (* alu8 r8, d8 *)
  | U_ALU8_D8_M2 op dst =>
    imm_reader cpu sys (fun cpu v1 =>
      let v0 := get_reg_8 cpu.(r) dst in
      match execute_alu8 op cpu.(f) v0 v1 with
      | (v, f) =>
        cpu_with_uop (cpu_with_reg_flags cpu (set_reg_8 cpu.(r) dst v) f) U_FETCH
      end
    )

  (* alu8 (HL) *)
  | U_ALU8_HL_R8_M2 op reg =>
    let v0 := get_reg_8 cpu.(r) reg in
    mem_reader cpu sys (cpu.(r).(l), cpu.(r).(h)) (fun cpu v1 =>
      match execute_alu8 op cpu.(f) v0 v1 with
      | (v, f) =>
        cpu_with_uop (cpu_with_reg_flags cpu (set_reg_8 cpu.(r) reg v) f) U_FETCH
      end
    )

  (* bit (HL), n *)
  | U_ALU8_BIT_M2 n =>
    mem_reader cpu sys (cpu.(r).(l), cpu.(r).(h)) (fun cpu v =>
      match execute_alu8 (A8_Bit n) cpu.(f) v v with
      | (_, f) =>
        cpu_with_uop (cpu_with_flags cpu f) U_FETCH
      end
    )

  (* ld (d16), r8 *)
  | U_MOV_D16_R8_M2 src rw =>
    imm_reader cpu sys (fun cpu lo =>
      cpu_with_uop cpu (U_MOV_D16_R8_M3 src lo rw)
    )
  | U_MOV_D16_R8_M3 src lo rw =>
    imm_reader cpu sys (fun cpu hi =>
      cpu_with_uop cpu (match rw with
        | R => U_MOV_D16_R8_M4_R src lo hi
        | W => U_MOV_D16_R8_M4_W src lo hi
        end)
    )
  | U_MOV_D16_R8_M4_R reg lo hi =>
    mem_reader cpu sys (lo, hi) (fun cpu v =>
      cpu_with_uop (cpu_with_reg cpu (set_reg_8 cpu.(r) reg v)) U_FETCH
    )
  | U_MOV_D16_R8_M4_W reg lo hi =>
    mem_writer cpu sys (lo, hi) (get_reg_8 cpu.(r) reg) (fun cpu =>
      cpu_with_uop cpu U_FETCH
    )

  | U_ALU8_HL_M2 op =>
    mem_reader cpu sys (cpu.(r).(l), cpu.(r).(h)) (fun cpu v =>
      match execute_alu8 op cpu.(f) v v with
      | (r, f) => cpu_with_uop (cpu_with_flags cpu f) (U_ALU8_HL_M3 r)
      end
    )
  | U_ALU8_HL_M3 v =>
    mem_writer cpu sys (cpu.(r).(l), cpu.(r).(h)) v (fun cpu =>
      cpu_with_uop cpu U_FETCH
    )

  | U_ST_D16_R16_M2 reg =>
    imm_reader cpu sys (fun cpu lo => cpu_with_uop cpu (U_ST_D16_R12_M3 reg lo))
  | U_ST_D16_R12_M3 reg lo =>
    imm_reader cpu sys (fun cpu hi => cpu_with_uop cpu (U_ST_D16_R12_M4 reg lo hi))
  | U_ST_D16_R12_M4 reg lo hi =>
    let addr := (lo, hi) in
    let v := u16_lo (get_reg_16s cpu.(r) reg) in
    mem_writer cpu sys addr v (fun cpu => cpu_with_uop cpu (U_ST_D16_R12_M5 reg lo hi))

  | U_ST_D16_R12_M5 reg lo hi =>
    let addr := inc_wrap_u16 (lo, hi) in
    let v := u16_hi (get_reg_16s cpu.(r) reg) in
    mem_writer cpu sys addr v (fun cpu => cpu_with_uop cpu U_FETCH)

  (* rst c8 *)
  | U_RST_M2 h =>
    Some (cpu_with_uop (cpu_with_prev_sp cpu) (U_RST_M3 h), sys)
  | U_RST_M3 h =>
    mem_writer cpu sys cpu.(r).(sp) (u16_hi cpu.(r).(pc)) (fun cpu =>
      let sp := dec_wrap_u16 cpu.(r).(sp) in
      let r := reg_with_sp (reg_with_pc_hi cpu.(r) (x0, x0)) sp in
      cpu_with_uop (cpu_with_reg cpu r) (U_RST_M4 h)
    )
  | U_RST_M4 h =>
    mem_writer cpu sys cpu.(r).(sp) (u16_lo cpu.(r).(pc)) (fun cpu =>
      let r := reg_with_pc_lo cpu.(r) h in
      cpu_with_uop (cpu_with_reg cpu r) U_FETCH
    )

  (* ld (hl), d8 *)
  | U_ST_R16_D8_M2 reg =>
    imm_reader cpu sys (fun cpu v => cpu_with_uop cpu (U_ST_R16_D8_M3 reg v))
  | U_ST_R16_D8_M3 reg v =>
    mem_writer cpu sys (get_reg_16 cpu.(r) reg) v (fun cpu => cpu_with_uop cpu U_FETCH)

  (* ld r16, sp+i8 *)
  | U_ADD_SP_D8_M2 reg =>
    imm_reader cpu sys (fun cpu v => cpu_with_uop cpu (U_ADD_SP_D8_M3 reg v))
  | U_ADD_SP_D8_M3 reg v =>
    match cpu.(r).(sp) with
    | ((sll, slh), sh) =>
      match u8_sext_u16 v with
      | ((oll, olh), oh) =>
        match u4_add sll oll with
        | (rll, h) =>
          match u4_adc slh olh h with
          | (rlh, c) =>
            match u8_adc sh oh c with
            | (rh, _) =>
              let r := set_reg_16s cpu.(r) reg ((rll, rlh), rh) in
              let f := {| zf := false; nf := false; hf := h; cf := c |} in
              let uop := match reg with
                | R16S_BC => U_FETCH
                | R16S_DE => U_FETCH
                | R16S_HL => U_FETCH
                | R16S_SP => U_ADD_SP_D8_M4
                end
              in
              Some (cpu_with_uop (cpu_with_reg_flags cpu r f) uop, sys)
            end
          end
        end
      end
    end

  | U_ADD_SP_D8_M4 =>
    Some (cpu_with_uop cpu U_FETCH, sys)

  | U_INT_M2 addr =>
    Some (cpu_with_uop (cpu_with_prev_sp cpu) (U_INT_M3 addr), sys)
  | U_INT_M3 addr =>
    Some (cpu_with_uop cpu (U_INT_M4 addr), sys)
  | U_INT_M4 addr =>
    match cpu.(r).(pc) with
    | (_, hi) =>
      mem_writer cpu sys cpu.(r).(sp) hi (fun cpu =>
        let sp := dec_wrap_u16 cpu.(r).(sp) in
        let r := reg_with_sp (reg_with_pc_hi cpu.(r) (x0, x0)) sp in
        cpu_with_uop (cpu_with_reg cpu r) (U_INT_M5 addr)
      )
    end
  | U_INT_M5 addr =>
    match cpu.(r).(pc) with
    | (lo, _) =>
      mem_writer cpu sys cpu.(r).(sp) lo (fun cpu =>
        cpu_with_uop (cpu_with_reg cpu (reg_with_pc_lo cpu.(r) addr)) U_FETCH
      )
    end

  | U_RET_M2 cc =>
    let taken :=
      match cc with
      | RCC_Z  => cpu.(f).(zf)
      | RCC_NZ => negb cpu.(f).(zf)
      | RCC_C  => cpu.(f).(cf)
      | RCC_NC => negb cpu.(f).(cf)
      end
    in
    Some (cpu_with_uop cpu (if taken then U_RET_M3 false else U_FETCH), sys)
  | U_RET_M3 ie =>
    mem_reader cpu sys cpu.(r).(sp) (fun cpu lo =>
      {| r := reg_with_sp cpu.(r) (inc_wrap_u16 cpu.(r).(sp))
       ; f := cpu.(f)
       ; ime := if ie then true else cpu.(ime)
       ; halted := cpu.(halted)
       ; uop := U_RET_M4 lo
       |}
    )
  | U_RET_M4 lo =>
    mem_reader cpu sys cpu.(r).(sp) (fun cpu hi =>
      cpu_with_uop (cpu_with_next_sp cpu) (U_RET_M5 lo hi)
    )
  | U_RET_M5 lo hi =>
    let r := reg_with_pc cpu.(r) (lo, hi) in
    Some (cpu_with_uop (cpu_with_reg cpu r) U_FETCH, sys)
  end.

(******************************************************************************)

Definition create (_: unit) :=
  {| r :=
    {| pc := ((x0, x0), (x0, x0))
     ; sp := ((x0, x0), (x0, x0))
     ; a := (x0, x0)
     ; b := (x0, x0)
     ; c := (x0, x0)
     ; d := (x0, x0)
     ; e := (x0, x0)
     ; h := (x0, x0)
     ; l := (x0, x0)
     |}
   ; f :=
    {| zf := false
     ; nf := false
     ; hf := false
     ; cf := false
     |}
   ; ime := false
   ; halted := false
   ; uop := U_FETCH
   |}.

(******************************************************************************)

Fixpoint Reads {T: Type} (sys sys': System T) : Prop :=
  match sys with
  | S _ _ _ clear read _ s =>
    (exists int,
      match clear s int with
      | None => False
      | Some s' => Reads s' sys'
      end)
    \/
    (exists addr,
      match read s addr with
      | None => False
      | Some (v, s') => s' = sys'
      end)
  end.

Fixpoint Writes {T: Type} (sys sys': System T) : Prop :=
  match sys with
  | S _ _ _ clear _ write s =>
    (exists int,
      match clear s int with
      | None => False
      | Some s' => Reads s' sys'
      end)
    \/
    (exists addr v,
      match write s addr v with
      | None => False
      | Some s' => s' = sys'
      end)
  end.

Theorem one_mem_per_cycle {T: Type} (c c': Cpu) (s s': System T):
  tick c s = Some (c', s') -> s = s' \/ Reads s s' \/ Writes s s'.
Admitted.
