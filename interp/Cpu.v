(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

Require Import Coq.Numbers.BinNums.
Require Import Coq.Init.Byte.
Require Import Coq.Bool.Bool.
Require Import Coq.Lists.List.

Import ListNotations.


(******************************************************************************)

Definition u8 : Type := byte.
Definition i8 : Type := byte.

Definition inc_u8 (v: u8) :=
  match v with
  | x00 => (x01, false) | x01 => (x02, false)
  | x02 => (x03, false) | x03 => (x04, false)
  | x04 => (x05, false) | x05 => (x06, false)
  | x06 => (x07, false) | x07 => (x08, false)
  | x08 => (x09, false) | x09 => (x0a, false)
  | x0a => (x0b, false) | x0b => (x0c, false)
  | x0c => (x0d, false) | x0d => (x0e, false)
  | x0e => (x0f, false) | x0f => (x10, false)
  | x10 => (x11, false) | x11 => (x12, false)
  | x12 => (x13, false) | x13 => (x14, false)
  | x14 => (x15, false) | x15 => (x16, false)
  | x16 => (x17, false) | x17 => (x18, false)
  | x18 => (x19, false) | x19 => (x1a, false)
  | x1a => (x1b, false) | x1b => (x1c, false)
  | x1c => (x1d, false) | x1d => (x1e, false)
  | x1e => (x1f, false) | x1f => (x20, false)
  | x20 => (x21, false) | x21 => (x22, false)
  | x22 => (x23, false) | x23 => (x24, false)
  | x24 => (x25, false) | x25 => (x26, false)
  | x26 => (x27, false) | x27 => (x28, false)
  | x28 => (x29, false) | x29 => (x2a, false)
  | x2a => (x2b, false) | x2b => (x2c, false)
  | x2c => (x2d, false) | x2d => (x2e, false)
  | x2e => (x2f, false) | x2f => (x30, false)
  | x30 => (x31, false) | x31 => (x32, false)
  | x32 => (x33, false) | x33 => (x34, false)
  | x34 => (x35, false) | x35 => (x36, false)
  | x36 => (x37, false) | x37 => (x38, false)
  | x38 => (x39, false) | x39 => (x3a, false)
  | x3a => (x3b, false) | x3b => (x3c, false)
  | x3c => (x3d, false) | x3d => (x3e, false)
  | x3e => (x3f, false) | x3f => (x40, false)
  | x40 => (x41, false) | x41 => (x42, false)
  | x42 => (x43, false) | x43 => (x44, false)
  | x44 => (x45, false) | x45 => (x46, false)
  | x46 => (x47, false) | x47 => (x48, false)
  | x48 => (x49, false) | x49 => (x4a, false)
  | x4a => (x4b, false) | x4b => (x4c, false)
  | x4c => (x4d, false) | x4d => (x4e, false)
  | x4e => (x4f, false) | x4f => (x50, false)
  | x50 => (x51, false) | x51 => (x52, false)
  | x52 => (x53, false) | x53 => (x54, false)
  | x54 => (x55, false) | x55 => (x56, false)
  | x56 => (x57, false) | x57 => (x58, false)
  | x58 => (x59, false) | x59 => (x5a, false)
  | x5a => (x5b, false) | x5b => (x5c, false)
  | x5c => (x5d, false) | x5d => (x5e, false)
  | x5e => (x5f, false) | x5f => (x60, false)
  | x60 => (x61, false) | x61 => (x62, false)
  | x62 => (x63, false) | x63 => (x64, false)
  | x64 => (x65, false) | x65 => (x66, false)
  | x66 => (x67, false) | x67 => (x68, false)
  | x68 => (x69, false) | x69 => (x6a, false)
  | x6a => (x6b, false) | x6b => (x6c, false)
  | x6c => (x6d, false) | x6d => (x6e, false)
  | x6e => (x6f, false) | x6f => (x70, false)
  | x70 => (x71, false) | x71 => (x72, false)
  | x72 => (x73, false) | x73 => (x74, false)
  | x74 => (x75, false) | x75 => (x76, false)
  | x76 => (x77, false) | x77 => (x78, false)
  | x78 => (x79, false) | x79 => (x7a, false)
  | x7a => (x7b, false) | x7b => (x7c, false)
  | x7c => (x7d, false) | x7d => (x7e, false)
  | x7e => (x7f, false) | x7f => (x80, false)
  | x80 => (x81, false) | x81 => (x82, false)
  | x82 => (x83, false) | x83 => (x84, false)
  | x84 => (x85, false) | x85 => (x86, false)
  | x86 => (x87, false) | x87 => (x88, false)
  | x88 => (x89, false) | x89 => (x8a, false)
  | x8a => (x8b, false) | x8b => (x8c, false)
  | x8c => (x8d, false) | x8d => (x8e, false)
  | x8e => (x8f, false) | x8f => (x90, false)
  | x90 => (x91, false) | x91 => (x92, false)
  | x92 => (x93, false) | x93 => (x94, false)
  | x94 => (x95, false) | x95 => (x96, false)
  | x96 => (x97, false) | x97 => (x98, false)
  | x98 => (x99, false) | x99 => (x9a, false)
  | x9a => (x9b, false) | x9b => (x9c, false)
  | x9c => (x9d, false) | x9d => (x9e, false)
  | x9e => (x9f, false) | x9f => (xa0, false)
  | xa0 => (xa1, false) | xa1 => (xa2, false)
  | xa2 => (xa3, false) | xa3 => (xa4, false)
  | xa4 => (xa5, false) | xa5 => (xa6, false)
  | xa6 => (xa7, false) | xa7 => (xa8, false)
  | xa8 => (xa9, false) | xa9 => (xaa, false)
  | xaa => (xab, false) | xab => (xac, false)
  | xac => (xad, false) | xad => (xae, false)
  | xae => (xaf, false) | xaf => (xb0, false)
  | xb0 => (xb1, false) | xb1 => (xb2, false)
  | xb2 => (xb3, false) | xb3 => (xb4, false)
  | xb4 => (xb5, false) | xb5 => (xb6, false)
  | xb6 => (xb7, false) | xb7 => (xb8, false)
  | xb8 => (xb9, false) | xb9 => (xba, false)
  | xba => (xbb, false) | xbb => (xbc, false)
  | xbc => (xbd, false) | xbd => (xbe, false)
  | xbe => (xbf, false) | xbf => (xc0, false)
  | xc0 => (xc1, false) | xc1 => (xc2, false)
  | xc2 => (xc3, false) | xc3 => (xc4, false)
  | xc4 => (xc5, false) | xc5 => (xc6, false)
  | xc6 => (xc7, false) | xc7 => (xc8, false)
  | xc8 => (xc9, false) | xc9 => (xca, false)
  | xca => (xcb, false) | xcb => (xcc, false)
  | xcc => (xcd, false) | xcd => (xce, false)
  | xce => (xcf, false) | xcf => (xd0, false)
  | xd0 => (xd1, false) | xd1 => (xd2, false)
  | xd2 => (xd3, false) | xd3 => (xd4, false)
  | xd4 => (xd5, false) | xd5 => (xd6, false)
  | xd6 => (xd7, false) | xd7 => (xd8, false)
  | xd8 => (xd9, false) | xd9 => (xda, false)
  | xda => (xdb, false) | xdb => (xdc, false)
  | xdc => (xdd, false) | xdd => (xde, false)
  | xde => (xdf, false) | xdf => (xe0, false)
  | xe0 => (xe1, false) | xe1 => (xe2, false)
  | xe2 => (xe3, false) | xe3 => (xe4, false)
  | xe4 => (xe5, false) | xe5 => (xe6, false)
  | xe6 => (xe7, false) | xe7 => (xe8, false)
  | xe8 => (xe9, false) | xe9 => (xea, false)
  | xea => (xeb, false) | xeb => (xec, false)
  | xec => (xed, false) | xed => (xee, false)
  | xee => (xef, false) | xef => (xf0, false)
  | xf0 => (xf1, false) | xf1 => (xf2, false)
  | xf2 => (xf3, false) | xf3 => (xf4, false)
  | xf4 => (xf5, false) | xf5 => (xf6, false)
  | xf6 => (xf7, false) | xf7 => (xf8, false)
  | xf8 => (xf9, false) | xf9 => (xfa, false)
  | xfa => (xfb, false) | xfb => (xfc, false)
  | xfc => (xfd, false) | xfd => (xfe, false)
  | xfe => (xff, false) | xff => (x00, true)
  end.

(******************************************************************************)

Definition u16 : Type := byte * byte.

Definition inc_u16_wrap (v: u16) :=
  match v with
  | (lo, hi) =>
    match inc_u8 lo with
    | (lo', c) =>
      match c with
      | false => (lo', hi)
      | true =>
        match inc_u8 hi with
        | (hi', _) => (lo', hi')
        end
      end
    end
  end.

(******************************************************************************)

Inductive interrupt : Type :=
  | Int_VBlank
  | Int_Stat
  | Int_Timer
  | Int_Serial
  | Int_Pins
  .

Axiom System: Type.
Axiom sys_tick : System -> option System.
Axiom sys_is_interrupt_pending : System -> interrupt -> bool.
Axiom sys_is_interrupt_enabled : System -> interrupt -> bool.
Axiom sys_clear_interrupt : System -> interrupt -> option System.
Axiom sys_read : System -> u16 -> option (u8 * System).

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
  | B8
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
  | A16_Inc16
  | A16_Dec16
  | A16_Add16
  | A16_Mov16
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
  | U_MOV_R8_R16_M2_R (dest: reg_8) (addr: reg_16)
  | U_MOV_R8_R16_M2_W (dest: reg_8) (addr: reg_16)
  (* jr *)
  | U_JR_M2 (cc: cc)
  | U_JR_M3 (ofs: i8)
  (* ld (HL±), r8 *)
  | U_MOV_HL_M2_R (dir: dir) (src: reg_8)
  | U_MOV_HL_M2_W (dir: dir) (src: reg_8)
  (* inc/dec (HL) *)
  | U_ALU_HL_M2 (op: alu8_op)
  | U_ALU_HL_M3 (v: u8)
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
  | U_ALU8_HL_M2 (op: alu8_op) (dst: reg_8)
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
  | U_ST_HL_D8_M2 (dst: reg_16)
  | U_ST_HL_D8_M3 (dst: reg_16) (v: u8)
  (* ld r16, sp+i8 *)
  | U_ADD_SP_D8_M2 (dst: reg_16s)
  | U_ADD_SP_D8_M3 (dst: reg_16s) (off: i8)
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
  ; sys: System
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

Definition get_reg_16s r reg :=
  match reg with
  | R16S_BC => (r.(c), r.(b))
  | R16S_DE => (r.(e), r.(d))
  | R16S_HL => (r.(l), r.(h))
  | R16S_SP => r.(sp)
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

Definition reg_with_next_pc r :=
  {| pc := inc_u16_wrap r.(pc)
   ; sp := r.(sp)
   ; a := r.(a)
   ; b := r.(b)
   ; c := r.(c)
   ; d := r.(d)
   ; e := r.(e)
   ; h := r.(h)
   ; l := r.(l)
   |}.

Definition cpu_with_sys cpu sys :=
  {| r := cpu.(r)
   ; f := cpu.(f)
   ; sys := sys
   ; ime := cpu.(ime)
   ; halted := cpu.(halted)
   ; uop := cpu.(uop)
   |}.

Definition cpu_with_reg cpu r :=
  {| r := r
   ; f := cpu.(f)
   ; sys := cpu.(sys)
   ; ime := cpu.(ime)
   ; halted := cpu.(halted)
   ; uop := cpu.(uop)
   |}.

Definition cpu_with_uop cpu uop :=
  {| r := cpu.(r)
   ; f := cpu.(f)
   ; sys := cpu.(sys)
   ; ime := cpu.(ime)
   ; halted := cpu.(halted)
   ; uop := uop
   |}.

Definition cpu_with_ime cpu ime :=
  {| r := cpu.(r)
   ; f := cpu.(f)
   ; sys := cpu.(sys)
   ; ime := ime
   ; halted := cpu.(halted)
   ; uop := cpu.(uop)
   |}.

Definition cpu_with_halted cpu halted :=
  {| r := cpu.(r)
   ; f := cpu.(f)
   ; sys := cpu.(sys)
   ; ime := cpu.(ime)
   ; halted := halted
   ; uop := cpu.(uop)
   |}.

Definition cpu_with_next_pc cpu :=
  {| r := reg_with_next_pc cpu.(r)
   ; f := cpu.(f)
   ; sys := cpu.(sys)
   ; ime := cpu.(ime)
   ; halted := cpu.(halted)
   ; uop := cpu.(uop)
   |}.

(******************************************************************************)

Definition sys_reader (cpu: Cpu) (addr: u16) (fn: Cpu -> u8 -> Cpu): option Cpu :=
  match sys_read cpu.(sys) addr with
  | None => None
  | Some (v, sys) => Some (fn cpu v)
  end.

(******************************************************************************)

Definition handle_alu8 (cpu: Cpu) (op: alu8_op) (dst: reg_8) (src: reg_8): option Cpu :=
  None
  .

Definition handle_alu8_hl (cpu: Cpu) (op: alu8_op): option Cpu :=
  None
  .

Definition handle_alu8_bit (cpu: Cpu) (bit: bit_8): option Cpu :=
  None
  .

Definition handle_alu8_hl_r8 (cpu: Cpu) (op: alu8_op) (dst: reg_8): option Cpu :=
  None
  .

Definition handle_mov_hl (cpu: Cpu) (rw: rw) (dir: dir) (reg: reg_8): option Cpu :=
  None
  .

Definition handle_add_sp_d8  (cpu: Cpu) (dst: reg_16s): option Cpu :=
  None.

Definition handle_alu16 (cpu: Cpu) (op: alu16_op) (dst: reg_16s) (src: reg_16s): option Cpu :=
  None.

Definition handle_alu8_d8 (cpu: Cpu) (op: alu8_op) (dst: reg_8): option Cpu :=
  None.

Definition handle_alu8_rot (cpu: Cpu) (op: alu8_op) (dst: reg_8) (src: reg_8): option Cpu :=
  None.

Definition handle_call (cpu: Cpu) (cc: cc): option Cpu :=
  None.

Definition handle_jp (cpu: Cpu) (cc: cc): option Cpu :=
  None.

Definition handle_jp_hl (cpu: Cpu): option Cpu :=
  None.

Definition handle_jr (cpu: Cpu) (cc: cc): option Cpu :=
  None.

Definition handle_ld_r16_d16 (cpu: Cpu) (dst: reg_16s): option Cpu :=
  None.

Definition handle_ld_r8_d8 (cpu: Cpu) (dst: reg_8): option Cpu :=
  None.

Definition handle_mov_c (cpu: Cpu) (rw: rw): option Cpu :=
  None.

Definition handle_mov_d16_r8 (cpu: Cpu) (reg: reg_8) (rw: rw): option Cpu :=
  None.

Definition handle_mov_r8_r16 (cpu: Cpu) (rw: rw) (dst: reg_8) (src: reg_16): option Cpu :=
  None.

Definition handle_movh (cpu: Cpu) (rw: rw): option Cpu :=
  None.

Definition handle_pop (cpu: Cpu) (reg: reg_16p): option Cpu :=
  None.

Definition handle_push (cpu: Cpu) (reg: reg_16p): option Cpu :=
  None.

Definition handle_ret (cpu: Cpu) (cc: cc) (ie: bool): option Cpu :=
  None.

Definition handle_rst (cpu: Cpu) (addr: u8): option Cpu :=
  None.

Definition handle_st_d16_r16 (cpu: Cpu) (src: reg_16s): option Cpu :=
  None.

Definition handle_st_r16_d8 (cpu: Cpu) (reg: reg_16): option Cpu :=
  None.


(******************************************************************************)

Definition handle_op (cpu: Cpu) (op: u8) : option Cpu :=
  match op with
  | x00 => Some (cpu_with_uop (cpu_with_next_pc cpu) U_FETCH)
  | x01 => handle_ld_r16_d16 cpu R16S_BC
  | x02 => handle_mov_r8_r16 cpu W R8_A R16_BC
  | x03 => handle_alu16      cpu A16_Inc16 R16S_BC R16S_BC
  | x04 => handle_alu8       cpu A8_Inc R8_B R8_B
  | x05 => handle_alu8       cpu A8_Dec R8_B R8_B
  | x06 => handle_ld_r8_d8   cpu R8_B
  | x07 => handle_alu8_rot   cpu A8_Rlc R8_A R8_A
  | x08 => handle_st_d16_r16 cpu R16S_SP
  | x09 => handle_alu16      cpu A16_Add16 R16S_HL R16S_BC
  | x0a => handle_mov_r8_r16 cpu R R8_A R16_BC
  | x0b => handle_alu16      cpu A16_Dec16 R16S_BC R16S_BC
  | x0c => handle_alu8       cpu A8_Inc R8_C R8_C
  | x0d => handle_alu8       cpu A8_Dec R8_C R8_C
  | x0e => handle_ld_r8_d8   cpu R8_C
  | x0f => handle_alu8_rot   cpu A8_Rrc R8_A R8_A
  | x10 => None
  | x11 => handle_ld_r16_d16 cpu R16S_DE
  | x12 => handle_mov_r8_r16 cpu W R8_A R16_DE
  | x13 => handle_alu16      cpu A16_Inc16 R16S_DE R16S_DE
  | x14 => handle_alu8       cpu A8_Inc R8_D R8_D
  | x15 => handle_alu8       cpu A8_Dec R8_D R8_D
  | x16 => handle_ld_r8_d8   cpu R8_D
  | x17 => handle_alu8_rot   cpu A8_Rl R8_A R8_A
  | x18 => handle_jr         cpu CC_A
  | x19 => handle_alu16      cpu A16_Add16 R16S_HL R16S_DE
  | x1a => handle_mov_r8_r16 cpu R R8_A R16_DE
  | x1b => handle_alu16      cpu A16_Dec16 R16S_DE R16S_DE
  | x1c => handle_alu8       cpu A8_Inc R8_E R8_E
  | x1d => handle_alu8       cpu A8_Dec R8_E R8_E
  | x1e => handle_ld_r8_d8   cpu R8_E
  | x1f => handle_alu8_rot   cpu A8_Rr R8_A R8_A
  | x20 => handle_jr         cpu CC_NZ
  | x21 => handle_ld_r16_d16 cpu R16S_HL
  | x22 => handle_mov_hl     cpu W Dir_Inc R8_A
  | x23 => handle_alu16      cpu A16_Inc16 R16S_HL R16S_HL
  | x24 => handle_alu8       cpu A8_Inc R8_H R8_H
  | x25 => handle_alu8       cpu A8_Dec R8_H R8_H
  | x26 => handle_ld_r8_d8   cpu R8_H
  | x27 => handle_alu8       cpu A8_Daa R8_A R8_A
  | x28 => handle_jr         cpu CC_Z
  | x29 => handle_alu16      cpu A16_Add16 R16S_HL R16S_HL
  | x2a => handle_mov_hl     cpu R Dir_Inc R8_A
  | x2b => handle_alu16      cpu A16_Dec16 R16S_HL R16S_HL
  | x2c => handle_alu8       cpu A8_Inc R8_L R8_L
  | x2d => handle_alu8       cpu A8_Dec R8_L R8_L
  | x2e => handle_ld_r8_d8   cpu R8_L
  | x2f => handle_alu8       cpu A8_Cpl R8_A R8_A
  | x30 => handle_jr         cpu CC_NC
  | x31 => handle_ld_r16_d16 cpu R16S_SP
  | x32 => handle_mov_hl     cpu W Dir_Dec R8_A
  | x33 => handle_alu16      cpu A16_Inc16 R16S_SP R16S_SP
  | x34 => handle_alu8_hl    cpu A8_Inc
  | x35 => handle_alu8_hl    cpu A8_Dec
  | x36 => handle_st_r16_d8  cpu R16_HL
  | x37 => handle_alu8       cpu A8_Scf R8_A R8_A
  | x38 => handle_jr         cpu CC_C
  | x39 => handle_alu16      cpu A16_Add16 R16S_HL R16S_SP
  | x3a => handle_mov_hl     cpu R Dir_Dec R8_A
  | x3b => handle_alu16      cpu A16_Dec16 R16S_SP R16S_SP
  | x3c => handle_alu8       cpu A8_Inc R8_A R8_A
  | x3d => handle_alu8       cpu A8_Dec R8_A R8_A
  | x3e => handle_ld_r8_d8   cpu R8_A
  | x3f => handle_alu8       cpu A8_Ccf R8_A R8_A
  | x40 => handle_alu8       cpu A8_Mov R8_B R8_B
  | x41 => handle_alu8       cpu A8_Mov R8_B R8_C
  | x42 => handle_alu8       cpu A8_Mov R8_B R8_D
  | x43 => handle_alu8       cpu A8_Mov R8_B R8_E
  | x44 => handle_alu8       cpu A8_Mov R8_B R8_H
  | x45 => handle_alu8       cpu A8_Mov R8_B R8_L
  | x46 => handle_alu8_hl_r8 cpu A8_Mov R8_B
  | x47 => handle_alu8       cpu A8_Mov R8_B R8_A
  | x48 => handle_alu8       cpu A8_Mov R8_C R8_B
  | x49 => handle_alu8       cpu A8_Mov R8_C R8_C
  | x4a => handle_alu8       cpu A8_Mov R8_C R8_D
  | x4b => handle_alu8       cpu A8_Mov R8_C R8_E
  | x4c => handle_alu8       cpu A8_Mov R8_C R8_H
  | x4d => handle_alu8       cpu A8_Mov R8_C R8_L
  | x4e => handle_alu8_hl_r8 cpu A8_Mov R8_C
  | x4f => handle_alu8       cpu A8_Mov R8_C R8_A
  | x50 => handle_alu8       cpu A8_Mov R8_D R8_B
  | x51 => handle_alu8       cpu A8_Mov R8_D R8_C
  | x52 => handle_alu8       cpu A8_Mov R8_D R8_D
  | x53 => handle_alu8       cpu A8_Mov R8_D R8_E
  | x54 => handle_alu8       cpu A8_Mov R8_D R8_H
  | x55 => handle_alu8       cpu A8_Mov R8_D R8_L
  | x56 => handle_alu8_hl_r8 cpu A8_Mov R8_D
  | x57 => handle_alu8       cpu A8_Mov R8_D R8_A
  | x58 => handle_alu8       cpu A8_Mov R8_E R8_B
  | x59 => handle_alu8       cpu A8_Mov R8_E R8_C
  | x5a => handle_alu8       cpu A8_Mov R8_E R8_D
  | x5b => handle_alu8       cpu A8_Mov R8_E R8_E
  | x5c => handle_alu8       cpu A8_Mov R8_E R8_H
  | x5d => handle_alu8       cpu A8_Mov R8_E R8_L
  | x5e => handle_alu8_hl_r8 cpu A8_Mov R8_E
  | x5f => handle_alu8       cpu A8_Mov R8_E R8_A
  | x60 => handle_alu8       cpu A8_Mov R8_H R8_B
  | x61 => handle_alu8       cpu A8_Mov R8_H R8_C
  | x62 => handle_alu8       cpu A8_Mov R8_H R8_D
  | x63 => handle_alu8       cpu A8_Mov R8_H R8_E
  | x64 => handle_alu8       cpu A8_Mov R8_H R8_H
  | x65 => handle_alu8       cpu A8_Mov R8_H R8_L
  | x66 => handle_alu8_hl_r8 cpu A8_Mov R8_H
  | x67 => handle_alu8       cpu A8_Mov R8_H R8_A
  | x68 => handle_alu8       cpu A8_Mov R8_L R8_B
  | x69 => handle_alu8       cpu A8_Mov R8_L R8_C
  | x6a => handle_alu8       cpu A8_Mov R8_L R8_D
  | x6b => handle_alu8       cpu A8_Mov R8_L R8_E
  | x6c => handle_alu8       cpu A8_Mov R8_L R8_H
  | x6d => handle_alu8       cpu A8_Mov R8_L R8_L
  | x6e => handle_alu8_hl_r8 cpu A8_Mov R8_L
  | x6f => handle_alu8       cpu A8_Mov R8_L R8_A
  | x70 => handle_mov_hl     cpu W Dir_Nop R8_B
  | x71 => handle_mov_hl     cpu W Dir_Nop R8_C
  | x72 => handle_mov_hl     cpu W Dir_Nop R8_D
  | x73 => handle_mov_hl     cpu W Dir_Nop R8_E
  | x74 => handle_mov_hl     cpu W Dir_Nop R8_H
  | x75 => handle_mov_hl     cpu W Dir_Nop R8_L
  | x76 => Some (cpu_with_uop (cpu_with_halted (cpu_with_next_pc cpu) true) U_FETCH)
  | x77 => handle_mov_hl     cpu W Dir_Nop R8_A
  | x78 => handle_alu8       cpu A8_Mov R8_A R8_B
  | x79 => handle_alu8       cpu A8_Mov R8_A R8_C
  | x7a => handle_alu8       cpu A8_Mov R8_A R8_D
  | x7b => handle_alu8       cpu A8_Mov R8_A R8_E
  | x7c => handle_alu8       cpu A8_Mov R8_A R8_H
  | x7d => handle_alu8       cpu A8_Mov R8_A R8_L
  | x7e => handle_alu8_hl_r8 cpu A8_Mov R8_A
  | x7f => handle_alu8       cpu A8_Mov R8_A R8_A
  | x80 => handle_alu8       cpu A8_Add R8_A R8_B
  | x81 => handle_alu8       cpu A8_Add R8_A R8_C
  | x82 => handle_alu8       cpu A8_Add R8_A R8_D
  | x83 => handle_alu8       cpu A8_Add R8_A R8_E
  | x84 => handle_alu8       cpu A8_Add R8_A R8_H
  | x85 => handle_alu8       cpu A8_Add R8_A R8_L
  | x86 => handle_alu8_hl_r8 cpu A8_Add R8_A
  | x87 => handle_alu8       cpu A8_Add R8_A R8_A
  | x88 => handle_alu8       cpu A8_Adc R8_A R8_B
  | x89 => handle_alu8       cpu A8_Adc R8_A R8_C
  | x8a => handle_alu8       cpu A8_Adc R8_A R8_D
  | x8b => handle_alu8       cpu A8_Adc R8_A R8_E
  | x8c => handle_alu8       cpu A8_Adc R8_A R8_H
  | x8d => handle_alu8       cpu A8_Adc R8_A R8_L
  | x8e => handle_alu8_hl_r8 cpu A8_Adc R8_A
  | x8f => handle_alu8       cpu A8_Adc R8_A R8_A
  | x90 => handle_alu8       cpu A8_Sub R8_A R8_B
  | x91 => handle_alu8       cpu A8_Sub R8_A R8_C
  | x92 => handle_alu8       cpu A8_Sub R8_A R8_D
  | x93 => handle_alu8       cpu A8_Sub R8_A R8_E
  | x94 => handle_alu8       cpu A8_Sub R8_A R8_H
  | x95 => handle_alu8       cpu A8_Sub R8_A R8_L
  | x96 => handle_alu8_hl_r8 cpu A8_Sub R8_A
  | x97 => handle_alu8       cpu A8_Sub R8_A R8_A
  | x98 => handle_alu8       cpu A8_Sbc R8_A R8_B
  | x99 => handle_alu8       cpu A8_Sbc R8_A R8_C
  | x9a => handle_alu8       cpu A8_Sbc R8_A R8_D
  | x9b => handle_alu8       cpu A8_Sbc R8_A R8_E
  | x9c => handle_alu8       cpu A8_Sbc R8_A R8_H
  | x9d => handle_alu8       cpu A8_Sbc R8_A R8_L
  | x9e => handle_alu8_hl_r8 cpu A8_Sbc R8_A
  | x9f => handle_alu8       cpu A8_Sbc R8_A R8_A
  | xa0 => handle_alu8       cpu A8_And R8_A R8_B
  | xa1 => handle_alu8       cpu A8_And R8_A R8_C
  | xa2 => handle_alu8       cpu A8_And R8_A R8_D
  | xa3 => handle_alu8       cpu A8_And R8_A R8_E
  | xa4 => handle_alu8       cpu A8_And R8_A R8_H
  | xa5 => handle_alu8       cpu A8_And R8_A R8_L
  | xa6 => handle_alu8_hl_r8 cpu A8_And R8_A
  | xa7 => handle_alu8       cpu A8_And R8_A R8_A
  | xa8 => handle_alu8       cpu A8_Xor R8_A R8_B
  | xa9 => handle_alu8       cpu A8_Xor R8_A R8_C
  | xaa => handle_alu8       cpu A8_Xor R8_A R8_D
  | xab => handle_alu8       cpu A8_Xor R8_A R8_E
  | xac => handle_alu8       cpu A8_Xor R8_A R8_H
  | xad => handle_alu8       cpu A8_Xor R8_A R8_L
  | xae => handle_alu8_hl_r8 cpu A8_Xor R8_A
  | xaf => handle_alu8       cpu A8_Xor R8_A R8_A
  | xb0 => handle_alu8       cpu A8_Or  R8_A R8_B
  | xb1 => handle_alu8       cpu A8_Or  R8_A R8_C
  | xb2 => handle_alu8       cpu A8_Or  R8_A R8_D
  | xb3 => handle_alu8       cpu A8_Or  R8_A R8_E
  | xb4 => handle_alu8       cpu A8_Or  R8_A R8_H
  | xb5 => handle_alu8       cpu A8_Or  R8_A R8_L
  | xb6 => handle_alu8_hl_r8 cpu A8_Or  R8_A
  | xb7 => handle_alu8       cpu A8_Or  R8_A R8_A
  | xb8 => handle_alu8       cpu A8_Cp  R8_A R8_B
  | xb9 => handle_alu8       cpu A8_Cp  R8_A R8_C
  | xba => handle_alu8       cpu A8_Cp  R8_A R8_D
  | xbb => handle_alu8       cpu A8_Cp  R8_A R8_E
  | xbc => handle_alu8       cpu A8_Cp  R8_A R8_H
  | xbd => handle_alu8       cpu A8_Cp  R8_A R8_L
  | xbe => handle_alu8_hl_r8 cpu A8_Cp  R8_A
  | xbf => handle_alu8       cpu A8_Cp  R8_A R8_A
  | xc0 => handle_ret        cpu CC_NZ false
  | xc1 => handle_pop        cpu R16P_BC
  | xc2 => handle_jp         cpu CC_NZ
  | xc3 => handle_jp         cpu CC_A
  | xc4 => handle_call       cpu CC_NZ
  | xc5 => handle_push       cpu R16P_BC
  | xc6 => handle_alu8_d8    cpu A8_Add R8_A
  | xc7 => handle_rst        cpu x00
  | xc8 => handle_ret        cpu CC_Z false
  | xc9 => handle_ret        cpu CC_A false
  | xca => handle_jp         cpu CC_Z
  | xcb => Some (cpu_with_uop (cpu_with_next_pc cpu) U_CB)
  | xcc => handle_call       cpu CC_Z
  | xcd => handle_call       cpu CC_A
  | xce => handle_alu8_d8    cpu A8_Adc R8_A
  | xcf => handle_rst        cpu x08
  | xd0 => handle_ret        cpu CC_NC false
  | xd1 => handle_pop        cpu R16P_DE
  | xd2 => handle_jp         cpu CC_NC
  | xd3 => None
  | xd4 => handle_call       cpu CC_NC
  | xd5 => handle_push       cpu R16P_DE
  | xd6 => handle_alu8_d8    cpu A8_Sub R8_A
  | xd7 => handle_rst        cpu x10
  | xd8 => handle_ret        cpu CC_C false
  | xd9 => handle_ret        cpu CC_A true
  | xda => handle_jp         cpu CC_C
  | xdb => None
  | xdc => handle_call       cpu CC_C
  | xdd => None
  | xde => handle_alu8_d8    cpu A8_Sbc R8_A
  | xdf => handle_rst        cpu x18
  | xe0 => handle_movh       cpu W
  | xe1 => handle_pop        cpu R16P_HL
  | xe2 => handle_mov_c      cpu W
  | xe3 => None
  | xe4 => None
  | xe5 => handle_push       cpu R16P_HL
  | xe6 => handle_alu8_d8    cpu A8_And R8_A
  | xe7 => handle_rst        cpu x20
  | xe8 => handle_add_sp_d8  cpu R16S_SP
  | xe9 => handle_jp_hl      cpu
  | xea => handle_mov_d16_r8 cpu R8_A W
  | xeb => None
  | xec => None
  | xed => None
  | xee => handle_alu8_d8    cpu A8_Xor R8_A
  | xef => handle_rst        cpu x28
  | xf0 => handle_movh       cpu R
  | xf1 => handle_pop        cpu R16P_AF
  | xf2 => handle_mov_c      cpu R
  | xf3 => Some (cpu_with_uop (cpu_with_next_pc (cpu_with_ime cpu false)) U_FETCH)
  | xf4 => None
  | xf5 => handle_push       cpu R16P_AF
  | xf6 => handle_alu8_d8    cpu A8_Or R8_A
  | xf7 => handle_rst        cpu x30
  | xf8 => handle_add_sp_d8  cpu R16S_HL
  | xf9 => handle_alu16      cpu A16_Mov16 R16S_SP R16S_HL
  | xfa => handle_mov_d16_r8 cpu R8_A R
  | xfb => Some (cpu_with_uop (cpu_with_next_pc (cpu_with_ime cpu true)) U_FETCH)
  | xfc => None
  | xfd => None
  | xfe => handle_alu8_d8    cpu A8_Cp R8_A
  | xff => handle_rst        cpu x38
  end.

Definition handle_cb_op (cpu: Cpu) (op: u8) : option Cpu :=
  match op with
  | x00 => handle_alu8     cpu A8_Rlc R8_B R8_B
  | x01 => handle_alu8     cpu A8_Rlc R8_C R8_C
  | x02 => handle_alu8     cpu A8_Rlc R8_D R8_D
  | x03 => handle_alu8     cpu A8_Rlc R8_E R8_E
  | x04 => handle_alu8     cpu A8_Rlc R8_H R8_H
  | x05 => handle_alu8     cpu A8_Rlc R8_L R8_L
  | x06 => handle_alu8_hl  cpu A8_Rlc
  | x07 => handle_alu8     cpu A8_Rlc R8_A R8_A
  | x08 => handle_alu8     cpu A8_Rrc R8_B R8_B
  | x09 => handle_alu8     cpu A8_Rrc R8_C R8_C
  | x0a => handle_alu8     cpu A8_Rrc R8_D R8_D
  | x0b => handle_alu8     cpu A8_Rrc R8_E R8_E
  | x0c => handle_alu8     cpu A8_Rrc R8_H R8_H
  | x0d => handle_alu8     cpu A8_Rrc R8_L R8_L
  | x0e => handle_alu8_hl  cpu A8_Rrc
  | x0f => handle_alu8     cpu A8_Rrc R8_A R8_A
  | x10 => handle_alu8     cpu A8_Rl R8_B R8_B
  | x11 => handle_alu8     cpu A8_Rl R8_C R8_C
  | x12 => handle_alu8     cpu A8_Rl R8_D R8_D
  | x13 => handle_alu8     cpu A8_Rl R8_E R8_E
  | x14 => handle_alu8     cpu A8_Rl R8_H R8_H
  | x15 => handle_alu8     cpu A8_Rl R8_L R8_L
  | x16 => handle_alu8_hl  cpu A8_Rl
  | x17 => handle_alu8     cpu A8_Rl R8_A R8_A
  | x18 => handle_alu8     cpu A8_Rr R8_B R8_B
  | x19 => handle_alu8     cpu A8_Rr R8_C R8_C
  | x1a => handle_alu8     cpu A8_Rr R8_D R8_D
  | x1b => handle_alu8     cpu A8_Rr R8_E R8_E
  | x1c => handle_alu8     cpu A8_Rr R8_H R8_H
  | x1d => handle_alu8     cpu A8_Rr R8_L R8_L
  | x1e => handle_alu8_hl  cpu A8_Rr
  | x1f => handle_alu8     cpu A8_Rr R8_A R8_A
  | x20 => handle_alu8     cpu A8_Sla R8_B R8_B
  | x21 => handle_alu8     cpu A8_Sla R8_C R8_C
  | x22 => handle_alu8     cpu A8_Sla R8_D R8_D
  | x23 => handle_alu8     cpu A8_Sla R8_E R8_E
  | x24 => handle_alu8     cpu A8_Sla R8_H R8_H
  | x25 => handle_alu8     cpu A8_Sla R8_L R8_L
  | x26 => handle_alu8_hl  cpu A8_Sla
  | x27 => handle_alu8     cpu A8_Sla R8_A R8_A
  | x28 => handle_alu8     cpu A8_Sra R8_B R8_B
  | x29 => handle_alu8     cpu A8_Sra R8_C R8_C
  | x2a => handle_alu8     cpu A8_Sra R8_D R8_D
  | x2b => handle_alu8     cpu A8_Sra R8_E R8_E
  | x2c => handle_alu8     cpu A8_Sra R8_H R8_H
  | x2d => handle_alu8     cpu A8_Sra R8_L R8_L
  | x2e => handle_alu8_hl  cpu A8_Sra
  | x2f => handle_alu8     cpu A8_Sra R8_A R8_A
  | x30 => handle_alu8     cpu A8_Swap R8_B R8_B
  | x31 => handle_alu8     cpu A8_Swap R8_C R8_C
  | x32 => handle_alu8     cpu A8_Swap R8_D R8_D
  | x33 => handle_alu8     cpu A8_Swap R8_E R8_E
  | x34 => handle_alu8     cpu A8_Swap R8_H R8_H
  | x35 => handle_alu8     cpu A8_Swap R8_L R8_L
  | x36 => handle_alu8_hl  cpu A8_Swap
  | x37 => handle_alu8     cpu A8_Swap R8_A R8_A
  | x38 => handle_alu8     cpu A8_Srl R8_B R8_B
  | x39 => handle_alu8     cpu A8_Srl R8_C R8_C
  | x3a => handle_alu8     cpu A8_Srl R8_D R8_D
  | x3b => handle_alu8     cpu A8_Srl R8_E R8_E
  | x3c => handle_alu8     cpu A8_Srl R8_H R8_H
  | x3d => handle_alu8     cpu A8_Srl R8_L R8_L
  | x3e => handle_alu8_hl  cpu A8_Srl
  | x3f => handle_alu8     cpu A8_Srl R8_A R8_A
  | x40 => handle_alu8     cpu (A8_Bit B0) R8_B R8_B
  | x41 => handle_alu8     cpu (A8_Bit B0) R8_C R8_C
  | x42 => handle_alu8     cpu (A8_Bit B0) R8_D R8_D
  | x43 => handle_alu8     cpu (A8_Bit B0) R8_E R8_E
  | x44 => handle_alu8     cpu (A8_Bit B0) R8_H R8_H
  | x45 => handle_alu8     cpu (A8_Bit B0) R8_L R8_L
  | x46 => handle_alu8_bit cpu B0
  | x47 => handle_alu8     cpu (A8_Bit B0) R8_A R8_A
  | x48 => handle_alu8     cpu (A8_Bit B1) R8_B R8_B
  | x49 => handle_alu8     cpu (A8_Bit B1) R8_C R8_C
  | x4a => handle_alu8     cpu (A8_Bit B1) R8_D R8_D
  | x4b => handle_alu8     cpu (A8_Bit B1) R8_E R8_E
  | x4c => handle_alu8     cpu (A8_Bit B1) R8_H R8_H
  | x4d => handle_alu8     cpu (A8_Bit B1) R8_L R8_L
  | x4e => handle_alu8_bit cpu B1
  | x4f => handle_alu8     cpu (A8_Bit B1) R8_A R8_A
  | x50 => handle_alu8     cpu (A8_Bit B2) R8_B R8_B
  | x51 => handle_alu8     cpu (A8_Bit B2) R8_C R8_C
  | x52 => handle_alu8     cpu (A8_Bit B2) R8_D R8_D
  | x53 => handle_alu8     cpu (A8_Bit B2) R8_E R8_E
  | x54 => handle_alu8     cpu (A8_Bit B2) R8_H R8_H
  | x55 => handle_alu8     cpu (A8_Bit B2) R8_L R8_L
  | x56 => handle_alu8_bit cpu B2
  | x57 => handle_alu8     cpu (A8_Bit B2) R8_A R8_A
  | x58 => handle_alu8     cpu (A8_Bit B3) R8_B R8_B
  | x59 => handle_alu8     cpu (A8_Bit B3) R8_C R8_C
  | x5a => handle_alu8     cpu (A8_Bit B3) R8_D R8_D
  | x5b => handle_alu8     cpu (A8_Bit B3) R8_E R8_E
  | x5c => handle_alu8     cpu (A8_Bit B3) R8_H R8_H
  | x5d => handle_alu8     cpu (A8_Bit B3) R8_L R8_L
  | x5e => handle_alu8_bit cpu B3
  | x5f => handle_alu8     cpu (A8_Bit B3) R8_A R8_A
  | x60 => handle_alu8     cpu (A8_Bit B4) R8_B R8_B
  | x61 => handle_alu8     cpu (A8_Bit B4) R8_C R8_C
  | x62 => handle_alu8     cpu (A8_Bit B4) R8_D R8_D
  | x63 => handle_alu8     cpu (A8_Bit B4) R8_E R8_E
  | x64 => handle_alu8     cpu (A8_Bit B4) R8_H R8_H
  | x65 => handle_alu8     cpu (A8_Bit B4) R8_L R8_L
  | x66 => handle_alu8_bit cpu B4
  | x67 => handle_alu8     cpu (A8_Bit B4) R8_A R8_A
  | x68 => handle_alu8     cpu (A8_Bit B5) R8_B R8_B
  | x69 => handle_alu8     cpu (A8_Bit B5) R8_C R8_C
  | x6a => handle_alu8     cpu (A8_Bit B5) R8_D R8_D
  | x6b => handle_alu8     cpu (A8_Bit B5) R8_E R8_E
  | x6c => handle_alu8     cpu (A8_Bit B5) R8_H R8_H
  | x6d => handle_alu8     cpu (A8_Bit B5) R8_L R8_L
  | x6e => handle_alu8_bit cpu B5
  | x6f => handle_alu8     cpu (A8_Bit B5) R8_A R8_A
  | x70 => handle_alu8     cpu (A8_Bit B6) R8_B R8_B
  | x71 => handle_alu8     cpu (A8_Bit B6) R8_C R8_C
  | x72 => handle_alu8     cpu (A8_Bit B6) R8_D R8_D
  | x73 => handle_alu8     cpu (A8_Bit B6) R8_E R8_E
  | x74 => handle_alu8     cpu (A8_Bit B6) R8_H R8_H
  | x75 => handle_alu8     cpu (A8_Bit B6) R8_L R8_L
  | x76 => handle_alu8_bit cpu B6
  | x77 => handle_alu8     cpu (A8_Bit B6) R8_A R8_A
  | x78 => handle_alu8     cpu (A8_Bit B7) R8_B R8_B
  | x79 => handle_alu8     cpu (A8_Bit B7) R8_C R8_C
  | x7a => handle_alu8     cpu (A8_Bit B7) R8_D R8_D
  | x7b => handle_alu8     cpu (A8_Bit B7) R8_E R8_E
  | x7c => handle_alu8     cpu (A8_Bit B7) R8_H R8_H
  | x7d => handle_alu8     cpu (A8_Bit B7) R8_L R8_L
  | x7e => handle_alu8_bit cpu B7
  | x7f => handle_alu8     cpu (A8_Bit B7) R8_A R8_A
  | x80 => handle_alu8     cpu (A8_Res B0) R8_B R8_B
  | x81 => handle_alu8     cpu (A8_Res B0) R8_C R8_C
  | x82 => handle_alu8     cpu (A8_Res B0) R8_D R8_D
  | x83 => handle_alu8     cpu (A8_Res B0) R8_E R8_E
  | x84 => handle_alu8     cpu (A8_Res B0) R8_H R8_H
  | x85 => handle_alu8     cpu (A8_Res B0) R8_L R8_L
  | x86 => handle_alu8_hl  cpu (A8_Res B0)
  | x87 => handle_alu8     cpu (A8_Res B0) R8_A R8_A
  | x88 => handle_alu8     cpu (A8_Res B1) R8_B R8_B
  | x89 => handle_alu8     cpu (A8_Res B1) R8_C R8_C
  | x8a => handle_alu8     cpu (A8_Res B1) R8_D R8_D
  | x8b => handle_alu8     cpu (A8_Res B1) R8_E R8_E
  | x8c => handle_alu8     cpu (A8_Res B1) R8_H R8_H
  | x8d => handle_alu8     cpu (A8_Res B1) R8_L R8_L
  | x8e => handle_alu8_hl  cpu (A8_Res B1)
  | x8f => handle_alu8     cpu (A8_Res B1) R8_A R8_A
  | x90 => handle_alu8     cpu (A8_Res B2) R8_B R8_B
  | x91 => handle_alu8     cpu (A8_Res B2) R8_C R8_C
  | x92 => handle_alu8     cpu (A8_Res B2) R8_D R8_D
  | x93 => handle_alu8     cpu (A8_Res B2) R8_E R8_E
  | x94 => handle_alu8     cpu (A8_Res B2) R8_H R8_H
  | x95 => handle_alu8     cpu (A8_Res B2) R8_L R8_L
  | x96 => handle_alu8_hl  cpu (A8_Res B2)
  | x97 => handle_alu8     cpu (A8_Res B2) R8_A R8_A
  | x98 => handle_alu8     cpu (A8_Res B3) R8_B R8_B
  | x99 => handle_alu8     cpu (A8_Res B3) R8_C R8_C
  | x9a => handle_alu8     cpu (A8_Res B3) R8_D R8_D
  | x9b => handle_alu8     cpu (A8_Res B3) R8_E R8_E
  | x9c => handle_alu8     cpu (A8_Res B3) R8_H R8_H
  | x9d => handle_alu8     cpu (A8_Res B3) R8_L R8_L
  | x9e => handle_alu8_hl  cpu (A8_Res B3)
  | x9f => handle_alu8     cpu (A8_Res B3) R8_A R8_A
  | xa0 => handle_alu8     cpu (A8_Res B4) R8_B R8_B
  | xa1 => handle_alu8     cpu (A8_Res B4) R8_C R8_C
  | xa2 => handle_alu8     cpu (A8_Res B4) R8_D R8_D
  | xa3 => handle_alu8     cpu (A8_Res B4) R8_E R8_E
  | xa4 => handle_alu8     cpu (A8_Res B4) R8_H R8_H
  | xa5 => handle_alu8     cpu (A8_Res B4) R8_L R8_L
  | xa6 => handle_alu8_hl  cpu (A8_Res B4)
  | xa7 => handle_alu8     cpu (A8_Res B4) R8_A R8_A
  | xa8 => handle_alu8     cpu (A8_Res B5) R8_B R8_B
  | xa9 => handle_alu8     cpu (A8_Res B5) R8_C R8_C
  | xaa => handle_alu8     cpu (A8_Res B5) R8_D R8_D
  | xab => handle_alu8     cpu (A8_Res B5) R8_E R8_E
  | xac => handle_alu8     cpu (A8_Res B5) R8_H R8_H
  | xad => handle_alu8     cpu (A8_Res B5) R8_L R8_L
  | xae => handle_alu8_hl  cpu (A8_Res B5)
  | xaf => handle_alu8     cpu (A8_Res B5) R8_A R8_A
  | xb0 => handle_alu8     cpu (A8_Res B6) R8_B R8_B
  | xb1 => handle_alu8     cpu (A8_Res B6) R8_C R8_C
  | xb2 => handle_alu8     cpu (A8_Res B6) R8_D R8_D
  | xb3 => handle_alu8     cpu (A8_Res B6) R8_E R8_E
  | xb4 => handle_alu8     cpu (A8_Res B6) R8_H R8_H
  | xb5 => handle_alu8     cpu (A8_Res B6) R8_L R8_L
  | xb6 => handle_alu8_hl  cpu (A8_Res B6)
  | xb7 => handle_alu8     cpu (A8_Res B6) R8_A R8_A
  | xb8 => handle_alu8     cpu (A8_Res B7) R8_B R8_B
  | xb9 => handle_alu8     cpu (A8_Res B7) R8_C R8_C
  | xba => handle_alu8     cpu (A8_Res B7) R8_D R8_D
  | xbb => handle_alu8     cpu (A8_Res B7) R8_E R8_E
  | xbc => handle_alu8     cpu (A8_Res B7) R8_H R8_H
  | xbd => handle_alu8     cpu (A8_Res B7) R8_L R8_L
  | xbe => handle_alu8_hl  cpu (A8_Res B7)
  | xbf => handle_alu8     cpu (A8_Res B7) R8_A R8_A
  | xc0 => handle_alu8     cpu (A8_Set B0) R8_B R8_B
  | xc1 => handle_alu8     cpu (A8_Set B0) R8_C R8_C
  | xc2 => handle_alu8     cpu (A8_Set B0) R8_D R8_D
  | xc3 => handle_alu8     cpu (A8_Set B0) R8_E R8_E
  | xc4 => handle_alu8     cpu (A8_Set B0) R8_H R8_H
  | xc5 => handle_alu8     cpu (A8_Set B0) R8_L R8_L
  | xc6 => handle_alu8_hl  cpu (A8_Set B0)
  | xc7 => handle_alu8     cpu (A8_Set B0) R8_A R8_A
  | xc8 => handle_alu8     cpu (A8_Set B1) R8_B R8_B
  | xc9 => handle_alu8     cpu (A8_Set B1) R8_C R8_C
  | xca => handle_alu8     cpu (A8_Set B1) R8_D R8_D
  | xcb => handle_alu8     cpu (A8_Set B1) R8_E R8_E
  | xcc => handle_alu8     cpu (A8_Set B1) R8_H R8_H
  | xcd => handle_alu8     cpu (A8_Set B1) R8_L R8_L
  | xce => handle_alu8_hl  cpu (A8_Set B1)
  | xcf => handle_alu8     cpu (A8_Set B1) R8_A R8_A
  | xd0 => handle_alu8     cpu (A8_Set B2) R8_B R8_B
  | xd1 => handle_alu8     cpu (A8_Set B2) R8_C R8_C
  | xd2 => handle_alu8     cpu (A8_Set B2) R8_D R8_D
  | xd3 => handle_alu8     cpu (A8_Set B2) R8_E R8_E
  | xd4 => handle_alu8     cpu (A8_Set B2) R8_H R8_H
  | xd5 => handle_alu8     cpu (A8_Set B2) R8_L R8_L
  | xd6 => handle_alu8_hl  cpu (A8_Set B2)
  | xd7 => handle_alu8     cpu (A8_Set B2) R8_A R8_A
  | xd8 => handle_alu8     cpu (A8_Set B3) R8_B R8_B
  | xd9 => handle_alu8     cpu (A8_Set B3) R8_C R8_C
  | xda => handle_alu8     cpu (A8_Set B3) R8_D R8_D
  | xdb => handle_alu8     cpu (A8_Set B3) R8_E R8_E
  | xdc => handle_alu8     cpu (A8_Set B3) R8_H R8_H
  | xdd => handle_alu8     cpu (A8_Set B3) R8_L R8_L
  | xde => handle_alu8_hl  cpu (A8_Set B3)
  | xdf => handle_alu8     cpu (A8_Set B3) R8_A R8_A
  | xe0 => handle_alu8     cpu (A8_Set B4) R8_B R8_B
  | xe1 => handle_alu8     cpu (A8_Set B4) R8_C R8_C
  | xe2 => handle_alu8     cpu (A8_Set B4) R8_D R8_D
  | xe3 => handle_alu8     cpu (A8_Set B4) R8_E R8_E
  | xe4 => handle_alu8     cpu (A8_Set B4) R8_H R8_H
  | xe5 => handle_alu8     cpu (A8_Set B4) R8_L R8_L
  | xe6 => handle_alu8_hl  cpu (A8_Set B4)
  | xe7 => handle_alu8     cpu (A8_Set B4) R8_A R8_A
  | xe8 => handle_alu8     cpu (A8_Set B5) R8_B R8_B
  | xe9 => handle_alu8     cpu (A8_Set B5) R8_C R8_C
  | xea => handle_alu8     cpu (A8_Set B5) R8_D R8_D
  | xeb => handle_alu8     cpu (A8_Set B5) R8_E R8_E
  | xec => handle_alu8     cpu (A8_Set B5) R8_H R8_H
  | xed => handle_alu8     cpu (A8_Set B5) R8_L R8_L
  | xee => handle_alu8_hl  cpu (A8_Set B5)
  | xef => handle_alu8     cpu (A8_Set B5) R8_A R8_A
  | xf0 => handle_alu8     cpu (A8_Set B6) R8_B R8_B
  | xf1 => handle_alu8     cpu (A8_Set B6) R8_C R8_C
  | xf2 => handle_alu8     cpu (A8_Set B6) R8_D R8_D
  | xf3 => handle_alu8     cpu (A8_Set B6) R8_E R8_E
  | xf4 => handle_alu8     cpu (A8_Set B6) R8_H R8_H
  | xf5 => handle_alu8     cpu (A8_Set B6) R8_L R8_L
  | xf6 => handle_alu8_hl  cpu (A8_Set B6)
  | xf7 => handle_alu8     cpu (A8_Set B6) R8_A R8_A
  | xf8 => handle_alu8     cpu (A8_Set B7) R8_B R8_B
  | xf9 => handle_alu8     cpu (A8_Set B7) R8_C R8_C
  | xfa => handle_alu8     cpu (A8_Set B7) R8_D R8_D
  | xfb => handle_alu8     cpu (A8_Set B7) R8_E R8_E
  | xfc => handle_alu8     cpu (A8_Set B7) R8_H R8_H
  | xfd => handle_alu8     cpu (A8_Set B7) R8_L R8_L
  | xfe => handle_alu8_hl  cpu (A8_Set B7)
  | xff => handle_alu8     cpu (A8_Set B7) R8_A R8_A
  end.

(******************************************************************************)

Definition all_interrupts :=
  [ Int_VBlank
  ; Int_Stat
  ; Int_Timer
  ; Int_Serial
  ; Int_Pins
  ].

Definition cpu_has_pending_interrupts cpu :=
  existsb (fun int => sys_is_interrupt_pending cpu.(sys) int) all_interrupts.

Definition cpu_is_halted cpu :=
  cpu.(halted) && negb (cpu_has_pending_interrupts cpu).

Definition cpu_interrupted cpu int :=
  cpu.(ime) &&
  sys_is_interrupt_pending cpu.(sys) int &&
  sys_is_interrupt_enabled cpu.(sys) int.

Definition handle_interrupt (cpu: Cpu) (addr: u8) (int: interrupt) : option Cpu :=
  None.

(******************************************************************************)

Definition step (cpu: Cpu) : option Cpu :=
  match cpu.(uop) with
  | U_FETCH =>
    (* Check for interrupts and enter the ISR if any are pending. *)
    (* If there are no interrupts and the CPU is halted, spin. *)
    (* Otherwise, fetch an opcode and start decoding the instruction. *)
    if cpu_interrupted cpu Int_VBlank then
      handle_interrupt cpu x40 Int_VBlank
    else if cpu_interrupted cpu Int_Stat then
      handle_interrupt cpu x48 Int_Stat
    else if cpu_interrupted cpu Int_Timer then
      handle_interrupt cpu x50 Int_Timer
    else if cpu_interrupted cpu Int_Serial then
      handle_interrupt cpu x58 Int_Serial
    else if cpu_interrupted cpu Int_Pins then
      handle_interrupt cpu x60 Int_Pins
    else if cpu_is_halted cpu then
      Some cpu
    else
      match sys_read cpu.(sys) cpu.(r).(pc) with
      | None => None
      | Some (op, sys) => handle_op (cpu_with_sys cpu sys) op
      end
  | U_CB =>
    (* Read the opcode after the prefix and handle it. *)
    match sys_read cpu.(sys) cpu.(r).(pc) with
    | None => None
    | Some (op, sys) => handle_cb_op (cpu_with_sys cpu sys) op
    end
  | U_MOV_R8_R16_M2_R reg_dst reg_addr =>
      let addr := get_reg_16 cpu.(r) reg_addr in
      sys_reader cpu addr (fun cpu v =>
        cpu_with_uop (cpu_with_reg cpu (set_reg_8 cpu.(r) reg_dst v)) U_FETCH
      )
  | _ =>
    None
  end.

(******************************************************************************)
Definition create (sys: System) :=
  {| r :=
    {| pc := (x00, x00)
     ; sp := (x00, x00)
     ; a := x00
     ; b := x00
     ; c := x00
     ; d := x00
     ; e := x00
     ; h := x00
     ; l := x00
     |}
   ; f :=
    {| zf := false
     ; nf := false
     ; hf := false
     ; cf := false
     |}
   ; sys := sys
   ; ime := false
   ; halted := false
   ; uop := U_FETCH
   |}.

(******************************************************************************)
Definition tick (cpu: Cpu) :=
  match sys_tick cpu.(sys) with
  | None => None
  | Some sys => step (cpu_with_sys cpu sys)
  end.
