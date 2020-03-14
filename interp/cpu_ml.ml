(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

type r =
  { pc: u16
  ; sp: u16
  ; a: u8
  ; f: u8
  ; b: u8
  ; c: u8
  ; d: u8
  ; e: u8
  ; h: u8
  ; l: u8
  }

type f =
  { z: bool
  ; n: bool
  ; h: bool
  ; c: bool
  }

type reg_8 =
  | R8_B
  | R8_C
  | R8_D
  | R8_E
  | R8_H
  | R8_L
  | R8_A

type reg_16 =
  | R16_BC
  | R16_DE
  | R16_HL

type reg_16s =
  | R16S_BC
  | R16S_DE
  | R16S_HL
  | R16S_SP

type reg_16p =
  | R16P_BC
  | R16P_DE
  | R16P_HL
  | R16P_AF

type cc =
  | CC_A
  | CC_Z
  | CC_NZ
  | CC_C
  | CC_NC

type ret_cc =
  | RCC_Z
  | RCC_NZ
  | RCC_C
  | RCC_NC

type dir =
  | D_Nop
  | D_Inc
  | D_Dec

type alu8_op =
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
  | A8_Bit of int
  | A8_Res of int
  | A8_Set of int
  | A8_Cpl
  | A8_Ccf
  | A8_Scf
  | A8_Daa

type alu16_op =
  | Inc16
  | Dec16
  | Add16
  | Mov16

type rw =
  | R
  | W

type uop =
  | U_FETCH
  | U_CB
  (* ld r16, d16 *)
  | U_LD_R16_D16_M2 of reg_16s
  | U_LD_R16_D16_M3 of reg_16s
  (* ld r8, (r16) *)
  | U_MOV_R8_R16_M2_R of reg_8 * reg_16
  | U_MOV_R8_R16_M2_W of reg_8 * reg_16
  (* jr *)
  | U_JR_M2 of cc
  | U_JR_M3 of u8
  (* ld (HL±), r8 *)
  | U_MOV_HL_M2_R of dir * reg_8
  | U_MOV_HL_M2_W of dir * reg_8
  (* inc/dec (HL) *)
  | U_ALU8_HL_M2 of alu8_op
  | U_ALU8_HL_M3 of u8
  (* ld r8, d8 *)
  | U_LD_R8_D8_M2 of reg_8
  (* ld (c), a *)
  | U_MOV_C_M2_R
  | U_MOV_C_M2_W
  (* ld (d8), a *)
  | U_MOV_D8_M2 of rw
  | U_MOV_D8_M3_R of u8
  | U_MOV_D8_M3_W of u8
  (* call/call[cc] *)
  | U_CALL_M2 of cc
  | U_CALL_M3 of cc * u8
  | U_CALL_M4 of u8 * u8
  | U_CALL_M5 of u8 * u8
  | U_CALL_M6 of u8
  (* jp (cc), d16 *)
  | U_JP_D16_M2 of cc
  | U_JP_D16_M3 of cc * u8
  | U_JP_D16_M4 of u8 * u8
  (* push *)
  | U_PUSH_M2 of reg_16p
  | U_PUSH_M3 of reg_16p
  | U_PUSH_M4 of reg_16p
  (* pop *)
  | U_POP_M2 of reg_16p
  | U_POP_M3 of reg_16p
  (* ret *)
  | U_RET_M2 of ret_cc
  | U_RET_M3 of bool
  | U_RET_M4 of u8
  | U_RET_M5 of u8 * u8
  (* alu16 *)
  | U_ALU16_M2 of alu16_op * reg_16s * reg_16s
  (* alu8 r8, d8 *)
  | U_ALU8_D8_M2 of alu8_op * reg_8
  (* alu8 r8, (HL) *)
  | U_ALU8_HL_R8_M2 of alu8_op * reg_8
  (* bit (HL) *)
  | U_ALU8_BIT_M2 of int
  (* ld (d16), r8 *)
  | U_MOV_D16_R8_M2 of reg_8 * rw
  | U_MOV_D16_R8_M3 of reg_8 * u8 * rw
  | U_MOV_D16_R8_M4_R of reg_8 * u8 * u8
  | U_MOV_D16_R8_M4_W of reg_8 * u8 * u8
  (* ld (a16), SP *)
  | U_ST_D16_R16_M2 of reg_16s
  | U_ST_D16_R12_M3 of reg_16s * u8
  | U_ST_D16_R12_M4 of reg_16s * u8 * u8
  | U_ST_D16_R12_M5 of reg_16s * u8 * u8
  (* rst *)
  | U_RST_M2 of u8
  | U_RST_M3 of u8
  | U_RST_M4 of u8
  (* ld (hl), d8 *)
  | U_ST_HL_D8_M2 of reg_16
  | U_ST_HL_D8_M3 of reg_16 * u8
  (* ld r16, sp+i8 *)
  | U_ADD_SP_D8_M2 of reg_16s
  | U_ADD_SP_D8_M3 of reg_16s * u8
  | U_ADD_SP_D8_M4
  (* interrupt *)
  | U_INT_M2 of u8
  | U_INT_M3 of u8
  | U_INT_M4 of u8
  | U_INT_M5 of u8

type t =
  { r: r
  ; f: f
  ; s: System.t
  ; ime: bool
  ; halted: bool
  ; uop: uop
  }

let get_reg_8 r reg =
  match reg with
  | R8_B -> r.b
  | R8_C -> r.c
  | R8_D -> r.d
  | R8_E -> r.e
  | R8_H -> r.h
  | R8_L -> r.l
  | R8_A -> r.a

let get_reg_16 r reg =
  match reg with
  | R16_BC -> (r.b lsl 8) lor r.c
  | R16_DE -> (r.d lsl 8) lor r.e
  | R16_HL -> (r.h lsl 8) lor r.l

let get_reg_16s r reg =
  match reg with
  | R16S_BC -> (r.b lsl 8) lor r.c
  | R16S_DE -> (r.d lsl 8) lor r.e
  | R16S_HL -> (r.h lsl 8) lor r.l
  | R16S_SP -> r.sp

let set_reg_8 r reg v =
  match reg with
  | R8_B -> { r with b = v }
  | R8_C -> { r with c = v }
  | R8_D -> { r with d = v }
  | R8_E -> { r with e = v }
  | R8_H -> { r with h = v }
  | R8_L -> { r with l = v }
  | R8_A -> { r with a = v }

let is_taken cc f =
  match cc with
  | CC_Z -> f.z
  | CC_NZ -> not f.z
  | CC_C -> f.c
  | CC_NC -> not f.c
  | CC_A -> true

let inc_pc r =
  { r with pc = (r.pc + 1) land 0xFFFF }

let read_imm cpu f =
  System.read cpu.s cpu.r.pc |> Option.map (fun imm ->
    f { cpu with r = inc_pc cpu.r } imm
  )

let execute_alu8 op f op0 op1 =
  let { z; n; h; c } = f in
  match op with
  | A8_Add ->
    let v = op0 + op1 in
    let z = (v land 0xFF) = 0x00 in
    let h = (v land 0xF) < (op1 land 0xF) in
    let c = v > 0xFF in
    v land 0xFF, { z; n = false; h; c }

  | A8_Adc ->
    let v = op0 + op1 + if c then 1 else 0 in
    let z = (v land 0xFF) == 0x00 in
    let h = (op0 land 0x0F) + (op1 land 0x0F) + (if c then 1 else 0) > 0x0F in
    let c = v > 0xFF in
    v land 0xFF, { z; n = false; h; c }

  | A8_Sub ->
    let v = op0 - op1 in
    let z = (v land 0xFF) = 0 in
    let h = (op0 land 0x0F) < (v land 0xF) in
    let c = v < 0x00 in
    v land 0xFF, { z; n = true; h; c }

  | A8_Sbc ->
    let v = op0 - op1 - if c then 1 else 0 in
    let h = (op0 land 0x0F) - (op1 land 0x0F) - (if c then 1 else 0) < 0x00 in
    v land 0xFF, { z = (v land 0xFF) = 0; n = true; h; c = v < 0x00 }

  | A8_And ->
    let v = op0 land op1 in
    v, { z = v = 0; n = false; h = true; c = false }

  | A8_Xor ->
    let v = op0 lxor op1 in
    v, { z = v = 0; n = false; h = false; c = false }

  | A8_Or ->
    let v = op0 lor op1 in
    v, { z = v = 0; n = false; h = false; c = false }

  | A8_Cp ->
    let v = op0 - op1 in
    op0, { z = v = 0; n = true; h = (v land 0x0F) > (op0 land 0x0F); c = v < 0 }

  | A8_Bit n ->
    let v = op1 in
    v, { z = v land (1 lsl n) = 0; n = false; h = true; c }

  | A8_Mov ->
    let v = op1 in
    v, f

  | A8_Rlc ->
    let l = (op1 land 0x80) lsr 7 in
    let c = (op1 land 0x80) <> 0 in
    let v = ((op1 lsl 1) lor l) land 0xFF in
    v, { z = v = 0; n = false; h = false; c }

  | A8_Rrc ->
    let l = (op1 land 0x01) lsl 7 in
    let c = (op1 land 0x01) <> 0 in
    let v = ((op1 lsr 1) lor l) land 0xFF in
    v, { z = v = 0; n = false; h = false; c }

  | A8_Rl ->
    let l = if c then 1 else 0 in
    let c = (op1 land 0x80) <> 0 in
    let v = ((op1 lsl 1) lor l) land 0xFF in
    v, { z = v = 0; n = false; h = false; c }

  | A8_Rr ->
    let l = if c then 0x80 else 0x00 in
    let c = (op1 land 0x01) <> 0 in
    let v = ((op1 lsr 1) lor l) land 0xFF in
    v, { z = v = 0; n = false; h = false; c }

  | A8_Sla ->
    let c = (op1 land 0x80) != 0x00 in
    let v = (op1 lsl 1) land 0xFF in
    v, { z = v = 0; n = false; h = false; c }

  | A8_Sra ->
    let c = (op1 land 0x01) != 0x00 in
    let v = ((op1 lsr 1) lor (op1 land 0x80)) land 0xFF in
    v, { z = v = 0; n = false; h = false; c }

  | A8_Swap ->
    let v = ((op1 land 0x0F ) lsl 4) lor ((op1 land 0xF0) lsr 4) in
    v, { z = v = 0; n = false; h = false; c = false }

  | A8_Srl ->
    let c = (op1 land 0x01) <> 0 in
    let v = (op1 lsr 1) land 0xFF in
    v, { z = v = 0; n = false; h = false; c }

  | A8_Res n ->
    op1 land (lnot (1 lsl n)), f

  | A8_Set n ->
    op1 lor (1 lsl n), f

  | A8_Inc ->
    let v = (op1 + 1) land 0xFF in
    v, { z = v = 0; n = false; h = (v land 0x0F) = 0; c }

  | A8_Dec ->
    let v = (op1 - 1) land 0xFF in
    v, { z = v = 0; n = true; h = (v land 0x0F) = 0x0F; c }

  | A8_Cpl ->
    let v = (lnot op1) land 0xFF in
    v, { z; n = true; h = true; c }

  | A8_Ccf ->
    op0, { z; n = false; h = false; c = not c }

  | A8_Scf ->
    op0, { z; n = false; h = false; c = true }

  | A8_Daa ->
    let v, c =
      if n then
        let adj =
          if c && h then 0x9A
          else if c then 0xA0
          else if h then 0xFA
          else 0x00
        in
        (op1 + adj) land 0xFF, c
      else
        let a, c =
          if c || op1 > 0x99 then (op1 + 0x60) land 0xFF, true else op1, c
        in
        if h || (a land 0x0F) > 0x09 then (a + 0x06) land 0xFF, c else a, c
    in
    v, { z = v = 0; n; h = false; c }

let decode_alu8 cpu op op0 op1 =
  let v0 = get_reg_8 cpu.r op0 in
  let v1 = get_reg_8 cpu.r op1 in
  let v, f = execute_alu8 op cpu.f v0 v1 in
  Some { cpu with r = (set_reg_8 (inc_pc cpu.r) op0 v); f; uop = U_FETCH }

let decode_alu8_rot cpu op op0 op1 =
  let v0 = get_reg_8 cpu.r op0 in
  let v1 = get_reg_8 cpu.r op1 in
  let v, { c } = execute_alu8 op cpu.f v0 v1 in
  Some { cpu with
    r = (set_reg_8 (inc_pc cpu.r) op0 v);
    f = { z = false; n = false; h = false; c };
    uop = U_FETCH
  }

let decode_alu8_d8 cpu op dst =
  Some { cpu with r = inc_pc cpu.r; uop = U_ALU8_D8_M2(op, dst) }

let decode_alu8_hl_r8 cpu op dst =
  Some { cpu with r = inc_pc cpu.r; uop = U_ALU8_HL_R8_M2(op, dst) }

let decode_alu16 cpu op dst src =
  Some { cpu with r = inc_pc cpu.r; uop = U_ALU16_M2(op, dst, src) }

let decode_ld_r8_d8 cpu dst =
  Some { cpu with r = inc_pc cpu.r; uop = U_LD_R8_D8_M2 dst }

let decode_ld_r16_d16 cpu reg =
  Some { cpu with r = inc_pc cpu.r; uop = U_LD_R16_D16_M2 reg }

let decode_st_d16_r16 cpu reg =
  Some { cpu with r = inc_pc cpu.r; uop = U_ST_D16_R16_M2 reg }

let decode_mov_r8_r16 cpu rw dst src =
  Some { cpu with r = inc_pc cpu.r; uop = match rw with
      | R -> U_MOV_R8_R16_M2_R(dst, src)
      | W -> U_MOV_R8_R16_M2_W(dst, src)
  }

let decode_mov_hl cpu rw dir reg =
  Some { cpu with r = inc_pc cpu.r; uop = match rw with
      | R -> U_MOV_HL_M2_R(dir, reg)
      | W -> U_MOV_HL_M2_W(dir, reg)
  }

let decode_alu8_hl cpu op =
  Some { cpu with r = inc_pc cpu.r; uop = U_ALU8_HL_M2 op }

let decode_alu8_bit cpu n =
  Some { cpu with r = inc_pc cpu.r; uop = U_ALU8_BIT_M2 n }

let decode_jr cpu cc =
  Some { cpu with r = inc_pc cpu.r; uop = U_JR_M2 cc }

let decode_ret cpu cc ime =
  Some { cpu with r = inc_pc cpu.r; uop = match cc with
      | CC_A  -> U_RET_M3 ime
      | CC_Z  -> U_RET_M2 RCC_Z
      | CC_NZ -> U_RET_M2 RCC_NZ
      | CC_C  -> U_RET_M2 RCC_C
      | CC_NC -> U_RET_M2 RCC_NC
  }

let decode_push cpu reg =
  Some { cpu with r = inc_pc cpu.r; uop = U_PUSH_M2 reg }

let decode_pop cpu reg =
  Some { cpu with r = inc_pc cpu.r; uop = U_POP_M2 reg }

let decode_call cpu cc =
  Some { cpu with r = inc_pc cpu.r; uop = U_CALL_M2 cc }

let decode_jp cpu cc =
  Some { cpu with r = inc_pc cpu.r; uop = U_JP_D16_M2 cc }

let decode_movh cpu rw =
  Some { cpu with r = inc_pc cpu.r; uop = U_MOV_D8_M2 rw }

let decode_mov_c cpu rw =
  Some { cpu with
    r = inc_pc cpu.r;
    uop = match rw with
    | R -> U_MOV_C_M2_R
    | W -> U_MOV_C_M2_W
  }

let decode_mov_d16_r8 cpu reg rw =
  Some { cpu with r = inc_pc cpu.r; uop = U_MOV_D16_R8_M2(reg, rw) }

let decode_jp_hl cpu =
  Some { cpu with r = { cpu.r with pc = (cpu.r.h lsl 8) lor cpu.r.l }; uop = U_FETCH }

let decode_rst cpu i =
  Some { cpu with r = inc_pc cpu.r; uop = U_RST_M2 i }

let decode_st_hl_d8 cpu r =
  Some { cpu with r = inc_pc cpu.r; uop = U_ST_HL_D8_M2 r }

let decode_add_sp_d8 cpu r =
  Some { cpu with r = inc_pc cpu.r; uop = U_ADD_SP_D8_M2 r }

let create s =
  { r =
    { pc = 0x0000
    ; sp = 0x0000
    ; a = 0x00
    ; f = 0x00
    ; b = 0x00
    ; c = 0x00
    ; d = 0x00
    ; e = 0x00
    ; h = 0x00
    ; l = 0x00
    }
  ; f =
    { z = false
    ; n = false
    ; h = false
    ; c = false
    }
  ; s
  ; ime = false
  ; halted = false
  ; uop = U_FETCH
  }

let interrupt cpu addr i =
  System.clear_interrupt cpu.s i |> Option.map (fun s ->
    { cpu with ime = false; s; uop = U_INT_M2 addr }
  )

let step cpu =
  (* Update the halted flag is an interrupt was triggered. *)
  let halted =
    cpu.halted && not (
      List.exists (System.is_interrupt_pending cpu.s)
        [ Int_VBlank
        ; Int_Stat
        ; Int_Timer
        ; Int_Serial
        ; Int_Pins
        ]
    )
  in
  let cpu = { cpu with halted } in
  let { uop; r; s; f; halted; ime } = cpu in

  (* Helper to check if an ISR should be entered. *)
  let has_interrupt i =
    ime && System.is_interrupt_pending s i && System.is_interrupt_enabled s i
  in
  match uop with
  | U_FETCH when has_interrupt Int_VBlank -> interrupt cpu 0x40 Int_VBlank
  | U_FETCH when has_interrupt Int_Stat   -> interrupt cpu 0x48 Int_Stat
  | U_FETCH when has_interrupt Int_Timer  -> interrupt cpu 0x50 Int_Timer
  | U_FETCH when has_interrupt Int_Serial -> interrupt cpu 0x58 Int_Serial
  | U_FETCH when has_interrupt Int_Pins   -> interrupt cpu 0x60 Int_Pins
  | U_FETCH when halted -> Some cpu
  | U_FETCH ->
    (match System.read s r.pc with
    | None -> None
    | Some op ->
      (match op with
      | 0x00 -> Some { cpu with r = inc_pc r; uop = U_FETCH }
      | 0x01 -> decode_ld_r16_d16 cpu R16S_BC
      | 0x02 -> decode_mov_r8_r16 cpu W R8_A R16_BC
      | 0x03 -> decode_alu16      cpu Inc16 R16S_BC R16S_BC
      | 0x04 -> decode_alu8       cpu A8_Inc R8_B R8_B
      | 0x05 -> decode_alu8       cpu A8_Dec R8_B R8_B
      | 0x06 -> decode_ld_r8_d8   cpu R8_B
      | 0x07 -> decode_alu8_rot   cpu A8_Rlc R8_A R8_A
      | 0x08 -> decode_st_d16_r16 cpu R16S_SP
      | 0x09 -> decode_alu16      cpu Add16 R16S_HL R16S_BC
      | 0x0A -> decode_mov_r8_r16 cpu R R8_A R16_BC
      | 0x0B -> decode_alu16      cpu Dec16 R16S_BC R16S_BC
      | 0x0C -> decode_alu8       cpu A8_Inc R8_C R8_C
      | 0x0D -> decode_alu8       cpu A8_Dec R8_C R8_C
      | 0x0E -> decode_ld_r8_d8   cpu R8_C
      | 0x0F -> decode_alu8_rot   cpu A8_Rrc R8_A R8_A
      | 0x10 -> None
      | 0x11 -> decode_ld_r16_d16 cpu R16S_DE
      | 0x12 -> decode_mov_r8_r16 cpu W R8_A R16_DE
      | 0x13 -> decode_alu16      cpu Inc16 R16S_DE R16S_DE
      | 0x14 -> decode_alu8       cpu A8_Inc R8_D R8_D
      | 0x15 -> decode_alu8       cpu A8_Dec R8_D R8_D
      | 0x16 -> decode_ld_r8_d8   cpu R8_D
      | 0x17 -> decode_alu8_rot   cpu A8_Rl R8_A R8_A
      | 0x18 -> decode_jr         cpu CC_A
      | 0x19 -> decode_alu16      cpu Add16 R16S_HL R16S_DE
      | 0x1A -> decode_mov_r8_r16 cpu R R8_A R16_DE
      | 0x1B -> decode_alu16      cpu Dec16 R16S_DE R16S_DE
      | 0x1C -> decode_alu8       cpu A8_Inc R8_E R8_E
      | 0x1D -> decode_alu8       cpu A8_Dec R8_E R8_E
      | 0x1E -> decode_ld_r8_d8   cpu R8_E
      | 0x1F -> decode_alu8_rot   cpu A8_Rr R8_A R8_A
      | 0x20 -> decode_jr         cpu CC_NZ
      | 0x21 -> decode_ld_r16_d16 cpu R16S_HL
      | 0x22 -> decode_mov_hl     cpu W D_Inc R8_A
      | 0x23 -> decode_alu16      cpu Inc16 R16S_HL R16S_HL
      | 0x24 -> decode_alu8       cpu A8_Inc R8_H R8_H
      | 0x25 -> decode_alu8       cpu A8_Dec R8_H R8_H
      | 0x26 -> decode_ld_r8_d8   cpu R8_H
      | 0x27 -> decode_alu8       cpu A8_Daa R8_A R8_A
      | 0x28 -> decode_jr         cpu CC_Z
      | 0x29 -> decode_alu16      cpu Add16 R16S_HL R16S_HL
      | 0x2A -> decode_mov_hl     cpu R D_Inc R8_A
      | 0x2B -> decode_alu16      cpu Dec16 R16S_HL R16S_HL
      | 0x2C -> decode_alu8       cpu A8_Inc R8_L R8_L
      | 0x2D -> decode_alu8       cpu A8_Dec R8_L R8_L
      | 0x2E -> decode_ld_r8_d8   cpu R8_L
      | 0x2F -> decode_alu8       cpu A8_Cpl R8_A R8_A
      | 0x30 -> decode_jr         cpu CC_NC
      | 0x31 -> decode_ld_r16_d16 cpu R16S_SP
      | 0x32 -> decode_mov_hl     cpu W D_Dec R8_A
      | 0x33 -> decode_alu16      cpu Inc16 R16S_SP R16S_SP
      | 0x34 -> decode_alu8_hl    cpu A8_Inc
      | 0x35 -> decode_alu8_hl    cpu A8_Dec
      | 0x36 -> decode_st_hl_d8   cpu R16_HL
      | 0x37 -> decode_alu8       cpu A8_Scf R8_A R8_A
      | 0x38 -> decode_jr         cpu CC_C
      | 0x39 -> decode_alu16      cpu Add16 R16S_HL R16S_SP
      | 0x3A -> decode_mov_hl     cpu R D_Dec R8_A
      | 0x3B -> decode_alu16      cpu Dec16 R16S_SP R16S_SP
      | 0x3C -> decode_alu8       cpu A8_Inc R8_A R8_A
      | 0x3D -> decode_alu8       cpu A8_Dec R8_A R8_A
      | 0x3E -> decode_ld_r8_d8   cpu R8_A
      | 0x3F -> decode_alu8       cpu A8_Ccf R8_A R8_A
      | 0x40 -> decode_alu8       cpu A8_Mov R8_B R8_B
      | 0x41 -> decode_alu8       cpu A8_Mov R8_B R8_C
      | 0x42 -> decode_alu8       cpu A8_Mov R8_B R8_D
      | 0x43 -> decode_alu8       cpu A8_Mov R8_B R8_E
      | 0x44 -> decode_alu8       cpu A8_Mov R8_B R8_H
      | 0x45 -> decode_alu8       cpu A8_Mov R8_B R8_L
      | 0x46 -> decode_alu8_hl_r8 cpu A8_Mov R8_B
      | 0x47 -> decode_alu8       cpu A8_Mov R8_B R8_A
      | 0x48 -> decode_alu8       cpu A8_Mov R8_C R8_B
      | 0x49 -> decode_alu8       cpu A8_Mov R8_C R8_C
      | 0x4A -> decode_alu8       cpu A8_Mov R8_C R8_D
      | 0x4B -> decode_alu8       cpu A8_Mov R8_C R8_E
      | 0x4C -> decode_alu8       cpu A8_Mov R8_C R8_H
      | 0x4D -> decode_alu8       cpu A8_Mov R8_C R8_L
      | 0x4E -> decode_alu8_hl_r8 cpu A8_Mov R8_C
      | 0x4F -> decode_alu8       cpu A8_Mov R8_C R8_A
      | 0x50 -> decode_alu8       cpu A8_Mov R8_D R8_B
      | 0x51 -> decode_alu8       cpu A8_Mov R8_D R8_C
      | 0x52 -> decode_alu8       cpu A8_Mov R8_D R8_D
      | 0x53 -> decode_alu8       cpu A8_Mov R8_D R8_E
      | 0x54 -> decode_alu8       cpu A8_Mov R8_D R8_H
      | 0x55 -> decode_alu8       cpu A8_Mov R8_D R8_L
      | 0x56 -> decode_alu8_hl_r8 cpu A8_Mov R8_D
      | 0x57 -> decode_alu8       cpu A8_Mov R8_D R8_A
      | 0x58 -> decode_alu8       cpu A8_Mov R8_E R8_B
      | 0x59 -> decode_alu8       cpu A8_Mov R8_E R8_C
      | 0x5A -> decode_alu8       cpu A8_Mov R8_E R8_D
      | 0x5B -> decode_alu8       cpu A8_Mov R8_E R8_E
      | 0x5C -> decode_alu8       cpu A8_Mov R8_E R8_H
      | 0x5D -> decode_alu8       cpu A8_Mov R8_E R8_L
      | 0x5E -> decode_alu8_hl_r8 cpu A8_Mov R8_E
      | 0x5F -> decode_alu8       cpu A8_Mov R8_E R8_A
      | 0x60 -> decode_alu8       cpu A8_Mov R8_H R8_B
      | 0x61 -> decode_alu8       cpu A8_Mov R8_H R8_C
      | 0x62 -> decode_alu8       cpu A8_Mov R8_H R8_D
      | 0x63 -> decode_alu8       cpu A8_Mov R8_H R8_E
      | 0x64 -> decode_alu8       cpu A8_Mov R8_H R8_H
      | 0x65 -> decode_alu8       cpu A8_Mov R8_H R8_L
      | 0x66 -> decode_alu8_hl_r8 cpu A8_Mov R8_H
      | 0x67 -> decode_alu8       cpu A8_Mov R8_H R8_A
      | 0x68 -> decode_alu8       cpu A8_Mov R8_L R8_B
      | 0x69 -> decode_alu8       cpu A8_Mov R8_L R8_C
      | 0x6A -> decode_alu8       cpu A8_Mov R8_L R8_D
      | 0x6B -> decode_alu8       cpu A8_Mov R8_L R8_E
      | 0x6C -> decode_alu8       cpu A8_Mov R8_L R8_H
      | 0x6D -> decode_alu8       cpu A8_Mov R8_L R8_L
      | 0x6E -> decode_alu8_hl_r8 cpu A8_Mov R8_L
      | 0x6F -> decode_alu8       cpu A8_Mov R8_L R8_A
      | 0x70 -> decode_mov_hl     cpu W D_Nop R8_B
      | 0x71 -> decode_mov_hl     cpu W D_Nop R8_C
      | 0x72 -> decode_mov_hl     cpu W D_Nop R8_D
      | 0x73 -> decode_mov_hl     cpu W D_Nop R8_E
      | 0x74 -> decode_mov_hl     cpu W D_Nop R8_H
      | 0x75 -> decode_mov_hl     cpu W D_Nop R8_L
      | 0x76 -> Some { cpu with r = inc_pc r; halted = true; uop = U_FETCH }
      | 0x77 -> decode_mov_hl     cpu W D_Nop R8_A
      | 0x78 -> decode_alu8       cpu A8_Mov R8_A R8_B
      | 0x79 -> decode_alu8       cpu A8_Mov R8_A R8_C
      | 0x7A -> decode_alu8       cpu A8_Mov R8_A R8_D
      | 0x7B -> decode_alu8       cpu A8_Mov R8_A R8_E
      | 0x7C -> decode_alu8       cpu A8_Mov R8_A R8_H
      | 0x7D -> decode_alu8       cpu A8_Mov R8_A R8_L
      | 0x7E -> decode_alu8_hl_r8 cpu A8_Mov R8_A
      | 0x7F -> decode_alu8       cpu A8_Mov R8_A R8_A
      | 0x80 -> decode_alu8       cpu A8_Add R8_A R8_B
      | 0x81 -> decode_alu8       cpu A8_Add R8_A R8_C
      | 0x82 -> decode_alu8       cpu A8_Add R8_A R8_D
      | 0x83 -> decode_alu8       cpu A8_Add R8_A R8_E
      | 0x84 -> decode_alu8       cpu A8_Add R8_A R8_H
      | 0x85 -> decode_alu8       cpu A8_Add R8_A R8_L
      | 0x86 -> decode_alu8_hl_r8 cpu A8_Add R8_A
      | 0x87 -> decode_alu8       cpu A8_Add R8_A R8_A
      | 0x88 -> decode_alu8       cpu A8_Adc R8_A R8_B
      | 0x89 -> decode_alu8       cpu A8_Adc R8_A R8_C
      | 0x8A -> decode_alu8       cpu A8_Adc R8_A R8_D
      | 0x8B -> decode_alu8       cpu A8_Adc R8_A R8_E
      | 0x8C -> decode_alu8       cpu A8_Adc R8_A R8_H
      | 0x8D -> decode_alu8       cpu A8_Adc R8_A R8_L
      | 0x8E -> decode_alu8_hl_r8 cpu A8_Adc R8_A
      | 0x8F -> decode_alu8       cpu A8_Adc R8_A R8_A
      | 0x90 -> decode_alu8       cpu A8_Sub R8_A R8_B
      | 0x91 -> decode_alu8       cpu A8_Sub R8_A R8_C
      | 0x92 -> decode_alu8       cpu A8_Sub R8_A R8_D
      | 0x93 -> decode_alu8       cpu A8_Sub R8_A R8_E
      | 0x94 -> decode_alu8       cpu A8_Sub R8_A R8_H
      | 0x95 -> decode_alu8       cpu A8_Sub R8_A R8_L
      | 0x96 -> decode_alu8_hl_r8 cpu A8_Sub R8_A
      | 0x97 -> decode_alu8       cpu A8_Sub R8_A R8_A
      | 0x98 -> decode_alu8       cpu A8_Sbc R8_A R8_B
      | 0x99 -> decode_alu8       cpu A8_Sbc R8_A R8_C
      | 0x9A -> decode_alu8       cpu A8_Sbc R8_A R8_D
      | 0x9B -> decode_alu8       cpu A8_Sbc R8_A R8_E
      | 0x9C -> decode_alu8       cpu A8_Sbc R8_A R8_H
      | 0x9D -> decode_alu8       cpu A8_Sbc R8_A R8_L
      | 0x9E -> decode_alu8_hl_r8 cpu A8_Sbc R8_A
      | 0x9F -> decode_alu8       cpu A8_Sbc R8_A R8_A
      | 0xA0 -> decode_alu8       cpu A8_And R8_A R8_B
      | 0xA1 -> decode_alu8       cpu A8_And R8_A R8_C
      | 0xA2 -> decode_alu8       cpu A8_And R8_A R8_D
      | 0xA3 -> decode_alu8       cpu A8_And R8_A R8_E
      | 0xA4 -> decode_alu8       cpu A8_And R8_A R8_H
      | 0xA5 -> decode_alu8       cpu A8_And R8_A R8_L
      | 0xA6 -> decode_alu8_hl_r8 cpu A8_And R8_A
      | 0xA7 -> decode_alu8       cpu A8_And R8_A R8_A
      | 0xA8 -> decode_alu8       cpu A8_Xor R8_A R8_B
      | 0xA9 -> decode_alu8       cpu A8_Xor R8_A R8_C
      | 0xAA -> decode_alu8       cpu A8_Xor R8_A R8_D
      | 0xAB -> decode_alu8       cpu A8_Xor R8_A R8_E
      | 0xAC -> decode_alu8       cpu A8_Xor R8_A R8_H
      | 0xAD -> decode_alu8       cpu A8_Xor R8_A R8_L
      | 0xAE -> decode_alu8_hl_r8 cpu A8_Xor R8_A
      | 0xAF -> decode_alu8       cpu A8_Xor R8_A R8_A
      | 0xB0 -> decode_alu8       cpu A8_Or  R8_A R8_B
      | 0xB1 -> decode_alu8       cpu A8_Or  R8_A R8_C
      | 0xB2 -> decode_alu8       cpu A8_Or  R8_A R8_D
      | 0xB3 -> decode_alu8       cpu A8_Or  R8_A R8_E
      | 0xB4 -> decode_alu8       cpu A8_Or  R8_A R8_H
      | 0xB5 -> decode_alu8       cpu A8_Or  R8_A R8_L
      | 0xB6 -> decode_alu8_hl_r8 cpu A8_Or  R8_A
      | 0xB7 -> decode_alu8       cpu A8_Or  R8_A R8_A
      | 0xB8 -> decode_alu8       cpu A8_Cp  R8_A R8_B
      | 0xB9 -> decode_alu8       cpu A8_Cp  R8_A R8_C
      | 0xBA -> decode_alu8       cpu A8_Cp  R8_A R8_D
      | 0xBB -> decode_alu8       cpu A8_Cp  R8_A R8_E
      | 0xBC -> decode_alu8       cpu A8_Cp  R8_A R8_H
      | 0xBD -> decode_alu8       cpu A8_Cp  R8_A R8_L
      | 0xBE -> decode_alu8_hl_r8 cpu A8_Cp  R8_A
      | 0xBF -> decode_alu8       cpu A8_Cp  R8_A R8_A
      | 0xC0 -> decode_ret        cpu CC_NZ false
      | 0xC1 -> decode_pop        cpu R16P_BC
      | 0xC2 -> decode_jp         cpu CC_NZ
      | 0xC3 -> decode_jp         cpu CC_A
      | 0xC4 -> decode_call       cpu CC_NZ
      | 0xC5 -> decode_push       cpu R16P_BC
      | 0xC6 -> decode_alu8_d8    cpu A8_Add R8_A
      | 0xC7 -> decode_rst        cpu 0x00
      | 0xC8 -> decode_ret        cpu CC_Z false
      | 0xC9 -> decode_ret        cpu CC_A false
      | 0xCA -> decode_jp         cpu CC_Z
      | 0xCB -> Some { cpu with r = inc_pc r; uop = U_CB }
      | 0xCC -> decode_call       cpu CC_Z
      | 0xCD -> decode_call       cpu CC_A
      | 0xCE -> decode_alu8_d8    cpu A8_Adc R8_A
      | 0xCF -> decode_rst        cpu 0x08
      | 0xD0 -> decode_ret        cpu CC_NC false
      | 0xD1 -> decode_pop        cpu R16P_DE
      | 0xD2 -> decode_jp         cpu CC_NC
      | 0xD3 -> failwith "0xD3"
      | 0xD4 -> decode_call       cpu CC_NC
      | 0xD5 -> decode_push       cpu R16P_DE
      | 0xD6 -> decode_alu8_d8    cpu A8_Sub R8_A
      | 0xD7 -> decode_rst        cpu 0x10
      | 0xD8 -> decode_ret        cpu CC_C false
      | 0xD9 -> decode_ret        cpu CC_A true
      | 0xDA -> decode_jp         cpu CC_C
      | 0xDB -> failwith "0xDB"
      | 0xDC -> decode_call       cpu CC_C
      | 0xDE -> decode_alu8_d8    cpu A8_Sbc R8_A
      | 0xDF -> decode_rst        cpu 0x18
      | 0xE0 -> decode_movh       cpu W
      | 0xE1 -> decode_pop        cpu R16P_HL
      | 0xE2 -> decode_mov_c      cpu W
      | 0xE3 -> failwith "0xE3"
      | 0xE4 -> failwith "0xE4"
      | 0xE5 -> decode_push       cpu R16P_HL
      | 0xE6 -> decode_alu8_d8    cpu A8_And R8_A
      | 0xE7 -> decode_rst        cpu 0x20
      | 0xE8 -> decode_add_sp_d8  cpu R16S_SP
      | 0xE9 -> decode_jp_hl      cpu
      | 0xEA -> decode_mov_d16_r8 cpu R8_A W
      | 0xEB -> failwith "0xEB"
      | 0xEC -> failwith "0xEC"
      | 0xED -> failwith "0xED"
      | 0xEE -> decode_alu8_d8    cpu A8_Xor R8_A
      | 0xEF -> decode_rst        cpu 0x28
      | 0xF0 -> decode_movh       cpu R
      | 0xF1 -> decode_pop        cpu R16P_AF
      | 0xF2 -> decode_mov_c      cpu R
      | 0xF3 -> Some { cpu with r = inc_pc r; ime = false; uop = U_FETCH }
      | 0xF4 -> failwith "0xF4"
      | 0xF5 -> decode_push       cpu R16P_AF
      | 0xF6 -> decode_alu8_d8    cpu A8_Or R8_A
      | 0xF7 -> decode_rst        cpu 0x30
      | 0xF8 -> decode_add_sp_d8  cpu R16S_HL
      | 0xF9 -> decode_alu16      cpu Mov16 R16S_SP R16S_HL
      | 0xFA -> decode_mov_d16_r8 cpu R8_A R
      | 0xFB -> Some { cpu with r = inc_pc r; ime = true; uop = U_FETCH }
      | 0xFC -> failwith "0xFC"
      | 0xFD -> failwith "0xFD"
      | 0xFE -> decode_alu8_d8    cpu A8_Cp R8_A
      | 0xFF -> decode_rst        cpu 0x38
      | _ -> failwith "unreachable"
      )
    )
  | U_CB ->
    (match System.read s r.pc with
    | None -> None
    | Some op ->
      (match op with
      | 0x00 -> decode_alu8     cpu A8_Rlc R8_B R8_B
      | 0x01 -> decode_alu8     cpu A8_Rlc R8_C R8_C
      | 0x02 -> decode_alu8     cpu A8_Rlc R8_D R8_D
      | 0x03 -> decode_alu8     cpu A8_Rlc R8_E R8_E
      | 0x04 -> decode_alu8     cpu A8_Rlc R8_H R8_H
      | 0x05 -> decode_alu8     cpu A8_Rlc R8_L R8_L
      | 0x06 -> decode_alu8_hl  cpu A8_Rlc
      | 0x07 -> decode_alu8     cpu A8_Rlc R8_A R8_A
      | 0x08 -> decode_alu8     cpu A8_Rrc R8_B R8_B
      | 0x09 -> decode_alu8     cpu A8_Rrc R8_C R8_C
      | 0x0A -> decode_alu8     cpu A8_Rrc R8_D R8_D
      | 0x0B -> decode_alu8     cpu A8_Rrc R8_E R8_E
      | 0x0C -> decode_alu8     cpu A8_Rrc R8_H R8_H
      | 0x0D -> decode_alu8     cpu A8_Rrc R8_L R8_L
      | 0x0E -> decode_alu8_hl  cpu A8_Rrc
      | 0x0F -> decode_alu8     cpu A8_Rrc R8_A R8_A
      | 0x10 -> decode_alu8     cpu A8_Rl R8_B R8_B
      | 0x11 -> decode_alu8     cpu A8_Rl R8_C R8_C
      | 0x12 -> decode_alu8     cpu A8_Rl R8_D R8_D
      | 0x13 -> decode_alu8     cpu A8_Rl R8_E R8_E
      | 0x14 -> decode_alu8     cpu A8_Rl R8_H R8_H
      | 0x15 -> decode_alu8     cpu A8_Rl R8_L R8_L
      | 0x16 -> decode_alu8_hl  cpu A8_Rl
      | 0x17 -> decode_alu8     cpu A8_Rl R8_A R8_A
      | 0x18 -> decode_alu8     cpu A8_Rr R8_B R8_B
      | 0x19 -> decode_alu8     cpu A8_Rr R8_C R8_C
      | 0x1A -> decode_alu8     cpu A8_Rr R8_D R8_D
      | 0x1B -> decode_alu8     cpu A8_Rr R8_E R8_E
      | 0x1C -> decode_alu8     cpu A8_Rr R8_H R8_H
      | 0x1D -> decode_alu8     cpu A8_Rr R8_L R8_L
      | 0x1E -> decode_alu8_hl  cpu A8_Rr
      | 0x1F -> decode_alu8     cpu A8_Rr R8_A R8_A
      | 0x20 -> decode_alu8     cpu A8_Sla R8_B R8_B
      | 0x21 -> decode_alu8     cpu A8_Sla R8_C R8_C
      | 0x22 -> decode_alu8     cpu A8_Sla R8_D R8_D
      | 0x23 -> decode_alu8     cpu A8_Sla R8_E R8_E
      | 0x24 -> decode_alu8     cpu A8_Sla R8_H R8_H
      | 0x25 -> decode_alu8     cpu A8_Sla R8_L R8_L
      | 0x26 -> decode_alu8_hl  cpu A8_Sla
      | 0x27 -> decode_alu8     cpu A8_Sla R8_A R8_A
      | 0x28 -> decode_alu8     cpu A8_Sra R8_B R8_B
      | 0x29 -> decode_alu8     cpu A8_Sra R8_C R8_C
      | 0x2A -> decode_alu8     cpu A8_Sra R8_D R8_D
      | 0x2B -> decode_alu8     cpu A8_Sra R8_E R8_E
      | 0x2C -> decode_alu8     cpu A8_Sra R8_H R8_H
      | 0x2D -> decode_alu8     cpu A8_Sra R8_L R8_L
      | 0x2E -> decode_alu8_hl  cpu A8_Sra
      | 0x2F -> decode_alu8     cpu A8_Sra R8_A R8_A
      | 0x30 -> decode_alu8     cpu A8_Swap R8_B R8_B
      | 0x31 -> decode_alu8     cpu A8_Swap R8_C R8_C
      | 0x32 -> decode_alu8     cpu A8_Swap R8_D R8_D
      | 0x33 -> decode_alu8     cpu A8_Swap R8_E R8_E
      | 0x34 -> decode_alu8     cpu A8_Swap R8_H R8_H
      | 0x35 -> decode_alu8     cpu A8_Swap R8_L R8_L
      | 0x36 -> decode_alu8_hl  cpu A8_Swap
      | 0x37 -> decode_alu8     cpu A8_Swap R8_A R8_A
      | 0x38 -> decode_alu8     cpu A8_Srl R8_B R8_B
      | 0x39 -> decode_alu8     cpu A8_Srl R8_C R8_C
      | 0x3A -> decode_alu8     cpu A8_Srl R8_D R8_D
      | 0x3B -> decode_alu8     cpu A8_Srl R8_E R8_E
      | 0x3C -> decode_alu8     cpu A8_Srl R8_H R8_H
      | 0x3D -> decode_alu8     cpu A8_Srl R8_L R8_L
      | 0x3E -> decode_alu8_hl  cpu A8_Srl
      | 0x3F -> decode_alu8     cpu A8_Srl R8_A R8_A
      | 0x40 -> decode_alu8     cpu (A8_Bit 0) R8_B R8_B
      | 0x41 -> decode_alu8     cpu (A8_Bit 0) R8_C R8_C
      | 0x42 -> decode_alu8     cpu (A8_Bit 0) R8_D R8_D
      | 0x43 -> decode_alu8     cpu (A8_Bit 0) R8_E R8_E
      | 0x44 -> decode_alu8     cpu (A8_Bit 0) R8_H R8_H
      | 0x45 -> decode_alu8     cpu (A8_Bit 0) R8_L R8_L
      | 0x46 -> decode_alu8_bit cpu 0
      | 0x47 -> decode_alu8     cpu (A8_Bit 0) R8_A R8_A
      | 0x48 -> decode_alu8     cpu (A8_Bit 1) R8_B R8_B
      | 0x49 -> decode_alu8     cpu (A8_Bit 1) R8_C R8_C
      | 0x4A -> decode_alu8     cpu (A8_Bit 1) R8_D R8_D
      | 0x4B -> decode_alu8     cpu (A8_Bit 1) R8_E R8_E
      | 0x4C -> decode_alu8     cpu (A8_Bit 1) R8_H R8_H
      | 0x4D -> decode_alu8     cpu (A8_Bit 1) R8_L R8_L
      | 0x4E -> decode_alu8_bit cpu 1
      | 0x4F -> decode_alu8     cpu (A8_Bit 1) R8_A R8_A
      | 0x50 -> decode_alu8     cpu (A8_Bit 2) R8_B R8_B
      | 0x51 -> decode_alu8     cpu (A8_Bit 2) R8_C R8_C
      | 0x52 -> decode_alu8     cpu (A8_Bit 2) R8_D R8_D
      | 0x53 -> decode_alu8     cpu (A8_Bit 2) R8_E R8_E
      | 0x54 -> decode_alu8     cpu (A8_Bit 2) R8_H R8_H
      | 0x55 -> decode_alu8     cpu (A8_Bit 2) R8_L R8_L
      | 0x56 -> decode_alu8_bit cpu 2
      | 0x57 -> decode_alu8     cpu (A8_Bit 2) R8_A R8_A
      | 0x58 -> decode_alu8     cpu (A8_Bit 3) R8_B R8_B
      | 0x59 -> decode_alu8     cpu (A8_Bit 3) R8_C R8_C
      | 0x5A -> decode_alu8     cpu (A8_Bit 3) R8_D R8_D
      | 0x5B -> decode_alu8     cpu (A8_Bit 3) R8_E R8_E
      | 0x5C -> decode_alu8     cpu (A8_Bit 3) R8_H R8_H
      | 0x5D -> decode_alu8     cpu (A8_Bit 3) R8_L R8_L
      | 0x5E -> decode_alu8_bit cpu 3
      | 0x5F -> decode_alu8     cpu (A8_Bit 3) R8_A R8_A
      | 0x60 -> decode_alu8     cpu (A8_Bit 4) R8_B R8_B
      | 0x61 -> decode_alu8     cpu (A8_Bit 4) R8_C R8_C
      | 0x62 -> decode_alu8     cpu (A8_Bit 4) R8_D R8_D
      | 0x63 -> decode_alu8     cpu (A8_Bit 4) R8_E R8_E
      | 0x64 -> decode_alu8     cpu (A8_Bit 4) R8_H R8_H
      | 0x65 -> decode_alu8     cpu (A8_Bit 4) R8_L R8_L
      | 0x66 -> decode_alu8_bit cpu 4
      | 0x67 -> decode_alu8     cpu (A8_Bit 4) R8_A R8_A
      | 0x68 -> decode_alu8     cpu (A8_Bit 5) R8_B R8_B
      | 0x69 -> decode_alu8     cpu (A8_Bit 5) R8_C R8_C
      | 0x6A -> decode_alu8     cpu (A8_Bit 5) R8_D R8_D
      | 0x6B -> decode_alu8     cpu (A8_Bit 5) R8_E R8_E
      | 0x6C -> decode_alu8     cpu (A8_Bit 5) R8_H R8_H
      | 0x6D -> decode_alu8     cpu (A8_Bit 5) R8_L R8_L
      | 0x6E -> decode_alu8_bit cpu 5
      | 0x6F -> decode_alu8     cpu (A8_Bit 5) R8_A R8_A
      | 0x70 -> decode_alu8     cpu (A8_Bit 6) R8_B R8_B
      | 0x71 -> decode_alu8     cpu (A8_Bit 6) R8_C R8_C
      | 0x72 -> decode_alu8     cpu (A8_Bit 6) R8_D R8_D
      | 0x73 -> decode_alu8     cpu (A8_Bit 6) R8_E R8_E
      | 0x74 -> decode_alu8     cpu (A8_Bit 6) R8_H R8_H
      | 0x75 -> decode_alu8     cpu (A8_Bit 6) R8_L R8_L
      | 0x76 -> decode_alu8_bit cpu 6
      | 0x77 -> decode_alu8     cpu (A8_Bit 6) R8_A R8_A
      | 0x78 -> decode_alu8     cpu (A8_Bit 7) R8_B R8_B
      | 0x79 -> decode_alu8     cpu (A8_Bit 7) R8_C R8_C
      | 0x7A -> decode_alu8     cpu (A8_Bit 7) R8_D R8_D
      | 0x7B -> decode_alu8     cpu (A8_Bit 7) R8_E R8_E
      | 0x7C -> decode_alu8     cpu (A8_Bit 7) R8_H R8_H
      | 0x7D -> decode_alu8     cpu (A8_Bit 7) R8_L R8_L
      | 0x7E -> decode_alu8_bit cpu 7
      | 0x7F -> decode_alu8     cpu (A8_Bit 7) R8_A R8_A
      | 0x80 -> decode_alu8     cpu (A8_Res 0) R8_B R8_B
      | 0x81 -> decode_alu8     cpu (A8_Res 0) R8_C R8_C
      | 0x82 -> decode_alu8     cpu (A8_Res 0) R8_D R8_D
      | 0x83 -> decode_alu8     cpu (A8_Res 0) R8_E R8_E
      | 0x84 -> decode_alu8     cpu (A8_Res 0) R8_H R8_H
      | 0x85 -> decode_alu8     cpu (A8_Res 0) R8_L R8_L
      | 0x86 -> decode_alu8_hl  cpu (A8_Res 0)
      | 0x87 -> decode_alu8     cpu (A8_Res 0) R8_A R8_A
      | 0x88 -> decode_alu8     cpu (A8_Res 1) R8_B R8_B
      | 0x89 -> decode_alu8     cpu (A8_Res 1) R8_C R8_C
      | 0x8A -> decode_alu8     cpu (A8_Res 1) R8_D R8_D
      | 0x8B -> decode_alu8     cpu (A8_Res 1) R8_E R8_E
      | 0x8C -> decode_alu8     cpu (A8_Res 1) R8_H R8_H
      | 0x8D -> decode_alu8     cpu (A8_Res 1) R8_L R8_L
      | 0x8E -> decode_alu8_hl  cpu (A8_Res 1)
      | 0x8F -> decode_alu8     cpu (A8_Res 1) R8_A R8_A
      | 0x90 -> decode_alu8     cpu (A8_Res 2) R8_B R8_B
      | 0x91 -> decode_alu8     cpu (A8_Res 2) R8_C R8_C
      | 0x92 -> decode_alu8     cpu (A8_Res 2) R8_D R8_D
      | 0x93 -> decode_alu8     cpu (A8_Res 2) R8_E R8_E
      | 0x94 -> decode_alu8     cpu (A8_Res 2) R8_H R8_H
      | 0x95 -> decode_alu8     cpu (A8_Res 2) R8_L R8_L
      | 0x96 -> decode_alu8_hl  cpu (A8_Res 2)
      | 0x97 -> decode_alu8     cpu (A8_Res 2) R8_A R8_A
      | 0x98 -> decode_alu8     cpu (A8_Res 3) R8_B R8_B
      | 0x99 -> decode_alu8     cpu (A8_Res 3) R8_C R8_C
      | 0x9A -> decode_alu8     cpu (A8_Res 3) R8_D R8_D
      | 0x9B -> decode_alu8     cpu (A8_Res 3) R8_E R8_E
      | 0x9C -> decode_alu8     cpu (A8_Res 3) R8_H R8_H
      | 0x9D -> decode_alu8     cpu (A8_Res 3) R8_L R8_L
      | 0x9E -> decode_alu8_hl  cpu (A8_Res 3)
      | 0x9F -> decode_alu8     cpu (A8_Res 3) R8_A R8_A
      | 0xA0 -> decode_alu8     cpu (A8_Res 4) R8_B R8_B
      | 0xA1 -> decode_alu8     cpu (A8_Res 4) R8_C R8_C
      | 0xA2 -> decode_alu8     cpu (A8_Res 4) R8_D R8_D
      | 0xA3 -> decode_alu8     cpu (A8_Res 4) R8_E R8_E
      | 0xA4 -> decode_alu8     cpu (A8_Res 4) R8_H R8_H
      | 0xA5 -> decode_alu8     cpu (A8_Res 4) R8_L R8_L
      | 0xA6 -> decode_alu8_hl  cpu (A8_Res 4)
      | 0xA7 -> decode_alu8     cpu (A8_Res 4) R8_A R8_A
      | 0xA8 -> decode_alu8     cpu (A8_Res 5) R8_B R8_B
      | 0xA9 -> decode_alu8     cpu (A8_Res 5) R8_C R8_C
      | 0xAA -> decode_alu8     cpu (A8_Res 5) R8_D R8_D
      | 0xAB -> decode_alu8     cpu (A8_Res 5) R8_E R8_E
      | 0xAC -> decode_alu8     cpu (A8_Res 5) R8_H R8_H
      | 0xAD -> decode_alu8     cpu (A8_Res 5) R8_L R8_L
      | 0xAE -> decode_alu8_hl  cpu (A8_Res 5)
      | 0xAF -> decode_alu8     cpu (A8_Res 5) R8_A R8_A
      | 0xB0 -> decode_alu8     cpu (A8_Res 6) R8_B R8_B
      | 0xB1 -> decode_alu8     cpu (A8_Res 6) R8_C R8_C
      | 0xB2 -> decode_alu8     cpu (A8_Res 6) R8_D R8_D
      | 0xB3 -> decode_alu8     cpu (A8_Res 6) R8_E R8_E
      | 0xB4 -> decode_alu8     cpu (A8_Res 6) R8_H R8_H
      | 0xB5 -> decode_alu8     cpu (A8_Res 6) R8_L R8_L
      | 0xB6 -> decode_alu8_hl  cpu (A8_Res 6)
      | 0xB7 -> decode_alu8     cpu (A8_Res 6) R8_A R8_A
      | 0xB8 -> decode_alu8     cpu (A8_Res 7) R8_B R8_B
      | 0xB9 -> decode_alu8     cpu (A8_Res 7) R8_C R8_C
      | 0xBA -> decode_alu8     cpu (A8_Res 7) R8_D R8_D
      | 0xBB -> decode_alu8     cpu (A8_Res 7) R8_E R8_E
      | 0xBC -> decode_alu8     cpu (A8_Res 7) R8_H R8_H
      | 0xBD -> decode_alu8     cpu (A8_Res 7) R8_L R8_L
      | 0xBE -> decode_alu8_hl  cpu (A8_Res 7)
      | 0xBF -> decode_alu8     cpu (A8_Res 7) R8_A R8_A
      | 0xC0 -> decode_alu8     cpu (A8_Set 0) R8_B R8_B
      | 0xC1 -> decode_alu8     cpu (A8_Set 0) R8_C R8_C
      | 0xC2 -> decode_alu8     cpu (A8_Set 0) R8_D R8_D
      | 0xC3 -> decode_alu8     cpu (A8_Set 0) R8_E R8_E
      | 0xC4 -> decode_alu8     cpu (A8_Set 0) R8_H R8_H
      | 0xC5 -> decode_alu8     cpu (A8_Set 0) R8_L R8_L
      | 0xC6 -> decode_alu8_hl  cpu (A8_Set 0)
      | 0xC7 -> decode_alu8     cpu (A8_Set 0) R8_A R8_A
      | 0xC8 -> decode_alu8     cpu (A8_Set 1) R8_B R8_B
      | 0xC9 -> decode_alu8     cpu (A8_Set 1) R8_C R8_C
      | 0xCA -> decode_alu8     cpu (A8_Set 1) R8_D R8_D
      | 0xCB -> decode_alu8     cpu (A8_Set 1) R8_E R8_E
      | 0xCC -> decode_alu8     cpu (A8_Set 1) R8_H R8_H
      | 0xCD -> decode_alu8     cpu (A8_Set 1) R8_L R8_L
      | 0xCE -> decode_alu8_hl  cpu (A8_Set 1)
      | 0xCF -> decode_alu8     cpu (A8_Set 1) R8_A R8_A
      | 0xD0 -> decode_alu8     cpu (A8_Set 2) R8_B R8_B
      | 0xD1 -> decode_alu8     cpu (A8_Set 2) R8_C R8_C
      | 0xD2 -> decode_alu8     cpu (A8_Set 2) R8_D R8_D
      | 0xD3 -> decode_alu8     cpu (A8_Set 2) R8_E R8_E
      | 0xD4 -> decode_alu8     cpu (A8_Set 2) R8_H R8_H
      | 0xD5 -> decode_alu8     cpu (A8_Set 2) R8_L R8_L
      | 0xD6 -> decode_alu8_hl  cpu (A8_Set 2)
      | 0xD7 -> decode_alu8     cpu (A8_Set 2) R8_A R8_A
      | 0xD8 -> decode_alu8     cpu (A8_Set 3) R8_B R8_B
      | 0xD9 -> decode_alu8     cpu (A8_Set 3) R8_C R8_C
      | 0xDA -> decode_alu8     cpu (A8_Set 3) R8_D R8_D
      | 0xDB -> decode_alu8     cpu (A8_Set 3) R8_E R8_E
      | 0xDC -> decode_alu8     cpu (A8_Set 3) R8_H R8_H
      | 0xDD -> decode_alu8     cpu (A8_Set 3) R8_L R8_L
      | 0xDE -> decode_alu8_hl  cpu (A8_Set 3)
      | 0xDF -> decode_alu8     cpu (A8_Set 3) R8_A R8_A
      | 0xE0 -> decode_alu8     cpu (A8_Set 4) R8_B R8_B
      | 0xE1 -> decode_alu8     cpu (A8_Set 4) R8_C R8_C
      | 0xE2 -> decode_alu8     cpu (A8_Set 4) R8_D R8_D
      | 0xE3 -> decode_alu8     cpu (A8_Set 4) R8_E R8_E
      | 0xE4 -> decode_alu8     cpu (A8_Set 4) R8_H R8_H
      | 0xE5 -> decode_alu8     cpu (A8_Set 4) R8_L R8_L
      | 0xE6 -> decode_alu8_hl  cpu (A8_Set 4)
      | 0xE7 -> decode_alu8     cpu (A8_Set 4) R8_A R8_A
      | 0xE8 -> decode_alu8     cpu (A8_Set 5) R8_B R8_B
      | 0xE9 -> decode_alu8     cpu (A8_Set 5) R8_C R8_C
      | 0xEA -> decode_alu8     cpu (A8_Set 5) R8_D R8_D
      | 0xEB -> decode_alu8     cpu (A8_Set 5) R8_E R8_E
      | 0xEC -> decode_alu8     cpu (A8_Set 5) R8_H R8_H
      | 0xED -> decode_alu8     cpu (A8_Set 5) R8_L R8_L
      | 0xEE -> decode_alu8_hl  cpu (A8_Set 5)
      | 0xEF -> decode_alu8     cpu (A8_Set 5) R8_A R8_A
      | 0xF0 -> decode_alu8     cpu (A8_Set 6) R8_B R8_B
      | 0xF1 -> decode_alu8     cpu (A8_Set 6) R8_C R8_C
      | 0xF2 -> decode_alu8     cpu (A8_Set 6) R8_D R8_D
      | 0xF3 -> decode_alu8     cpu (A8_Set 6) R8_E R8_E
      | 0xF4 -> decode_alu8     cpu (A8_Set 6) R8_H R8_H
      | 0xF5 -> decode_alu8     cpu (A8_Set 6) R8_L R8_L
      | 0xF6 -> decode_alu8_hl  cpu (A8_Set 6)
      | 0xF7 -> decode_alu8     cpu (A8_Set 6) R8_A R8_A
      | 0xF8 -> decode_alu8     cpu (A8_Set 7) R8_B R8_B
      | 0xF9 -> decode_alu8     cpu (A8_Set 7) R8_C R8_C
      | 0xFA -> decode_alu8     cpu (A8_Set 7) R8_D R8_D
      | 0xFB -> decode_alu8     cpu (A8_Set 7) R8_E R8_E
      | 0xFC -> decode_alu8     cpu (A8_Set 7) R8_H R8_H
      | 0xFD -> decode_alu8     cpu (A8_Set 7) R8_L R8_L
      | 0xFE -> decode_alu8_hl  cpu (A8_Set 7)
      | 0xFF -> decode_alu8     cpu (A8_Set 7) R8_A R8_A
      | _ -> failwith "unreachable"
      )
    )
  | U_MOV_R8_R16_M2_R(r8, r16) ->
      let addr = get_reg_16 r r16 in
      System.read s addr |> Option.map (fun v ->
        { cpu with r = (set_reg_8 r r8 v); uop = U_FETCH }
      )
  | U_MOV_R8_R16_M2_W(r8, r16) ->
    let addr = get_reg_16 r r16 in
    let v = get_reg_8 r r8 in
    System.write s addr v |> Option.map (fun s ->
      { cpu with s; uop = U_FETCH }
    )
  | U_LD_R16_D16_M2 reg ->
    read_imm cpu (fun cpu v ->
      let r = match reg with
        | R16S_BC -> { cpu.r with c = v }
        | R16S_DE -> { cpu.r with e = v }
        | R16S_HL -> { cpu.r with l = v }
        | R16S_SP -> { cpu.r with sp = (r.sp land 0xFF00) lor v }
      in
      { cpu with r; uop = U_LD_R16_D16_M3 reg }
    )
  | U_LD_R16_D16_M3 reg ->
    read_imm cpu (fun cpu v ->
      let r = match reg with
        | R16S_BC -> { cpu.r with b = v }
        | R16S_DE -> { cpu.r with d = v }
        | R16S_HL -> { cpu.r with h = v }
        | R16S_SP -> { cpu.r with sp = (r.sp land 0x00FF) lor (v lsl 8) }
      in
      { cpu with r; uop = U_FETCH }
    )
  | U_JR_M2 cc ->
    read_imm cpu (fun cpu off ->
      { cpu with uop = if is_taken cc f then U_JR_M3 off else U_FETCH }
    )
  | U_JR_M3 off ->
    Some({
      cpu with
        r = { r with pc = (r.pc + i8_of_u8 off) land 0xFFFF };
        uop = U_FETCH
    })
  | U_MOV_HL_M2_W(op, d) ->
    let v = get_reg_8 r d in
    let addr = (r.h lsl 8) lor r.l in
    System.write s addr v |> Option.map (fun s ->
      let hl =
        match op with
        | D_Nop -> addr
        | D_Inc -> (addr + 1) land 0xFFFF
        | D_Dec -> (addr - 1) land 0xFFFF
      in
      let h = (hl land 0xFF00) lsr 8 in
      let l = (hl land 0x00FF) lsr 0 in
      { cpu with
        r = { r with h; l };
        s;
        uop = U_FETCH
      }
    )
  | U_MOV_HL_M2_R(op, d) ->
    let addr = (r.h lsl 8) lor r.l in
    System.read s addr |> Option.map (fun v ->
      let hl =
        match op with
        | D_Nop -> addr
        | D_Inc -> (addr + 1) land 0xFFFF
        | D_Dec -> (addr - 1) land 0xFFFF
      in
      let h = (hl land 0xFF00) lsr 8 in
      let l = (hl land 0x00FF) lsr 0 in
      { cpu with r = { (set_reg_8 r d v) with h; l }; s; uop = U_FETCH }
    )
  | U_LD_R8_D8_M2 d ->
    read_imm cpu (fun cpu v ->
      { cpu with r = set_reg_8 cpu.r d v; uop = U_FETCH; }
    )

  | U_MOV_C_M2_R ->
    System.read s (0xFF00 + r.c) |> Option.map (fun a ->
      { cpu with r = { r with a }; s; uop = U_FETCH }
    )
  | U_MOV_C_M2_W ->
    System.write s (0xFF00 + r.c) r.a |> Option.map (fun s ->
      { cpu with s; uop = U_FETCH }
    )

  | U_MOV_D8_M2 rw ->
    read_imm cpu (fun cpu a ->
      { cpu with
        uop = match rw with
          | R -> U_MOV_D8_M3_R a
          | W -> U_MOV_D8_M3_W a
      }
    )
  | U_MOV_D8_M3_W a ->
    System.write s (0xFF00 + a) r.a |> Option.map (fun s ->
      { cpu with s; uop = U_FETCH }
    )
  | U_MOV_D8_M3_R a ->
    System.read s (0xFF00 + a) |> Option.map (fun a ->
      { cpu with r = { r with a }; uop = U_FETCH }
    )
  | U_CALL_M2 cc ->
    read_imm cpu (fun cpu lo -> { cpu with uop = U_CALL_M3(cc, lo) })
  | U_CALL_M3(cc, lo) ->
    read_imm cpu (fun cpu hi ->
      { cpu with uop = if is_taken cc f then U_CALL_M4(lo, hi) else U_FETCH }
    )
  | U_CALL_M4(lo, hi) ->
    let sp = (r.sp - 1) land 0xFFFF in
    Some { cpu with r = { r with sp }; uop = U_CALL_M5(lo, hi) }
  | U_CALL_M5(lo, hi) ->
    let pc_hi = (r.pc lsr 8) land 0xFF in
    let pc_lo = (r.pc lsr 0) land 0xFF in
    System.write s r.sp pc_hi |> Option.map (fun s ->
      let sp = (r.sp - 1) land 0xFFFF in
      let pc = (hi lsl 8) lor pc_lo in
      { cpu with r = { r with pc; sp}; s; uop = U_CALL_M6 lo }
    )
  | U_CALL_M6 lo ->
    let pc_hi = (r.pc lsr 8) land 0xFF in
    let pc_lo = (r.pc lsr 0) land 0xFF in
    System.write s r.sp pc_lo |> Option.map (fun s ->
      let pc = (pc_hi lsl 8) lor lo in
      { cpu with r = { r with pc }; s; uop = U_FETCH }
    )

  | U_JP_D16_M2 cc ->
    read_imm cpu (fun cpu lo -> { cpu with uop = U_JP_D16_M3(cc, lo) })
  | U_JP_D16_M3(cc, lo) ->
    read_imm cpu (fun cpu hi ->
      { cpu with uop = if is_taken cc f then U_JP_D16_M4(lo, hi) else U_FETCH }
    )
  | U_JP_D16_M4(lo, hi) ->
    Some { cpu with r = { r with pc = (hi lsl 8) lor lo }; uop = U_FETCH }

  | U_PUSH_M2 dd ->
    let sp = (r.sp - 1) land 0xFFFF in
    Some { cpu with r = { r with sp }; uop = U_PUSH_M3 dd }
  | U_PUSH_M3 dd ->
    let v = match dd with
      | R16P_BC -> r.b
      | R16P_DE -> r.d
      | R16P_HL -> r.h
      | R16P_AF -> r.a
    in
    System.write s r.sp v |> Option.map (fun s ->
      { cpu with
        r = { r with sp = (r.sp - 1) land 0xFFFF };
        s;
        uop = U_PUSH_M4 dd;
      }
    )
  | U_PUSH_M4 dd ->
    let v = match dd with
      | R16P_BC -> r.c
      | R16P_DE -> r.e
      | R16P_HL -> r.l
      | R16P_AF ->
        (if f.z then 0x80 else 0x00)
        lor
        (if f.n then 0x40 else 0x00)
        lor
        (if f.h then 0x20 else 0x00)
        lor
        (if f.c then 0x10 else 0x00)
    in
    System.write s r.sp v |> Option.map (fun s ->
      { cpu with s; uop = U_FETCH }
    )
  | U_POP_M2 dd ->
    System.read s r.sp |> Option.map (fun lo ->
      let r, f = match dd with
        | R16P_BC -> { r with c = lo }, f
        | R16P_DE -> { r with e = lo }, f
        | R16P_HL -> { r with l = lo }, f
        | R16P_AF ->
          r,
          { z = (lo land 0x80) <> 0
          ; n = (lo land 0x40) <> 0
          ; h = (lo land 0x20) <> 0
          ; c = (lo land 0x10) <> 0
          }
      in
      { cpu with
        r = { r with sp = (r.sp + 1) land 0xFFFF };
        f;
        uop = U_POP_M3 dd
      }
    )
  | U_POP_M3 dd ->
    System.read s r.sp |> Option.map (fun hi ->
      let r = match dd with
        | R16P_BC -> { r with b = hi }
        | R16P_DE -> { r with d = hi }
        | R16P_HL -> { r with h = hi }
        | R16P_AF -> { r with a = hi }
      in
      { cpu with
        r = { r with sp = (r.sp + 1) land 0xFFFF };
        uop = U_FETCH
      }
    )
  | U_RET_M2 cc ->
    let taken =
      match cc with
      | RCC_Z  -> f.z
      | RCC_NZ -> not f.z
      | RCC_C  -> f.c
      | RCC_NC -> not f.c
    in
    Some { cpu with uop = if taken then U_RET_M3 false else U_FETCH }
  | U_RET_M3 ime ->
    System.read s r.sp |> Option.map (fun lo ->
      { cpu with
        r = { r with sp = (r.sp + 1) land 0xFFFF };
        ime = if ime then true else cpu.ime;
        uop = U_RET_M4 lo;
      }
    )
  | U_RET_M4 lo ->
    System.read s r.sp |> Option.map (fun hi ->
      { cpu with
        r = { r with sp = (r.sp + 1) land 0xFFFF };
        uop = U_RET_M5(lo, hi);
      }
    )
  | U_RET_M5(lo, hi) ->
    Some { cpu with
      r = { r with pc = (hi lsl 8) lor lo };
      uop = U_FETCH
    }
  | U_ALU16_M2(op, reg0, reg1) ->
    let v0 = get_reg_16s r reg0 in
    let v1 = get_reg_16s r reg1 in
    let v, f = match op with
      | Inc16 -> (v1 + 1) land 0xFFFF, f
      | Dec16 -> (v1 - 1) land 0xFFFF, f
      | Add16 ->
        let v = v0 + v1 in
        let h = (v0 land 0x0FFF ) > (v land 0x0FFF) in
        let c = v > 0xFFFF in
        v land 0xFFFF, { z = f.z; n = false; h; c}
      | Mov16 -> v1, f
    in
    let hi = (v land 0xFF00) lsr 8 in
    let lo = (v land 0x00FF) lsr 0 in
    let r = match reg0 with
      | R16S_BC -> { r with b = hi; c = lo }
      | R16S_DE -> { r with d = hi; e = lo }
      | R16S_HL -> { r with h = hi; l = lo }
      | R16S_SP -> { r with sp = v }
    in
    Some { cpu with r; f; uop = U_FETCH }
  | U_ALU8_D8_M2(op, dst) ->
    read_imm cpu (fun cpu v1 ->
      let v0 = get_reg_8 cpu.r dst in
      let v, f = execute_alu8 op cpu.f v0 v1 in
      { cpu with r = set_reg_8 cpu.r dst v; f; uop = U_FETCH }
    )
  | U_ALU8_HL_R8_M2(op, dst) ->
    let v0 = get_reg_8 r dst in
    System.read s ((r.h lsl 8) lor r.l) |> Option.map (fun v1 ->
      let v, f = execute_alu8 op cpu.f v0 v1 in
      let r = set_reg_8 r dst v in
      { cpu with r; f; uop = U_FETCH }
    )
  | U_ALU8_BIT_M2 n ->
    System.read s ((r.h lsl 8) lor r.l) |> Option.map (fun v ->
      { cpu with
        f = { f with z = v land (1 lsl n) = 0; n = false; h = true };
        uop = U_FETCH
      }
    )
  | U_MOV_D16_R8_M2(src, rw) ->
    read_imm cpu (fun cpu lo ->
      { cpu with uop = U_MOV_D16_R8_M3(src, lo, rw) }
    )
  | U_MOV_D16_R8_M3(src, lo, rw) ->
    read_imm cpu (fun cpu hi ->
      { cpu with
        uop = match rw with
          | R -> U_MOV_D16_R8_M4_R(src, lo, hi)
          | W -> U_MOV_D16_R8_M4_W(src, lo, hi)
      }
    )
  | U_MOV_D16_R8_M4_R(reg, lo, hi) ->
    let addr = (hi lsl 8) lor lo in
    System.read s addr |> Option.map (fun v ->
      { cpu with r = set_reg_8 r reg v; uop = U_FETCH }
    )
  | U_MOV_D16_R8_M4_W(reg, lo, hi) ->
    let addr = (hi lsl 8) lor lo in
    let v = get_reg_8 r reg in
    System.write s addr v |> Option.map (fun s ->
      { cpu with s; uop = U_FETCH }
    )

  | U_ALU8_HL_M2(op) ->
    System.read s ((r.h lsl 8) lor r.l) |> Option.map (fun v ->
      let v, f = execute_alu8 op f v v in
      { cpu with s; f; uop = U_ALU8_HL_M3 v }
    )
  | U_ALU8_HL_M3(v) ->
    System.write s ((r.h lsl 8) lor r.l) v |> Option.map (fun s ->
      { cpu with s; uop = U_FETCH }
    )

  | U_ST_D16_R16_M2 reg ->
    read_imm cpu (fun cpu lo -> { cpu with uop = U_ST_D16_R12_M3(reg, lo) })
  | U_ST_D16_R12_M3(reg, lo) ->
    read_imm cpu (fun cpu hi -> { cpu with uop = U_ST_D16_R12_M4(reg, lo, hi) })
  | U_ST_D16_R12_M4(reg, lo, hi) ->
    let addr = (((hi lsl 8) lor lo) + 0) land 0xFFFF in
    let v = match reg with
      | R16S_BC -> r.c
      | R16S_DE -> r.e
      | R16S_HL -> r.l
      | R16S_SP -> ((r.sp land 0x00FF) lsr 0)
    in
    System.write s addr v |> Option.map (fun s ->
      { cpu with s; uop = U_ST_D16_R12_M5(reg, lo, hi) }
    )
  | U_ST_D16_R12_M5(reg, lo, hi) ->
    let addr = (((hi lsl 8) lor lo) + 1) land 0xFFFF in
    let v = match reg with
      | R16S_BC -> r.b
      | R16S_DE -> r.d
      | R16S_HL -> r.h
      | R16S_SP -> ((r.sp land 0xFF00) lsr 8)
    in
    System.write s addr v |> Option.map (fun s -> { cpu with s; uop = U_FETCH })
  | U_RST_M2 h ->
    Some { cpu with
      r = { r with sp = (r.sp - 1) land 0xFFFF };
      uop = U_RST_M3 h
    }
  | U_RST_M3 h ->
    System.write s r.sp ((r.pc land 0xFF00) lsr 8) |> Option.map (fun s ->
      { cpu with r =
        { r with
          sp = (r.sp - 1) land 0xFFFF;
          pc = r.pc land 0xFF
        };
        uop = U_RST_M4 h
      }
    )
  | U_RST_M4 h ->
    System.write s r.sp ((r.pc land 0x00FF) lsr 0) |> Option.map (fun s ->
      { cpu with r = { r with pc = h }; uop = U_FETCH }
    )

  | U_ST_HL_D8_M2 reg ->
    read_imm cpu (fun cpu v -> { cpu with uop = U_ST_HL_D8_M3(reg, v) })
  | U_ST_HL_D8_M3(reg, v) ->
    System.write s (get_reg_16 cpu.r reg) v |> Option.map (fun s ->
      { cpu with s; uop = U_FETCH }
    )

  | U_ADD_SP_D8_M2 reg ->
    read_imm cpu (fun cpu v -> { cpu with uop = U_ADD_SP_D8_M3(reg, v) })

  | U_ADD_SP_D8_M3(reg, v) ->
    let i = i8_of_u8 v in
    let v = (r.sp + i) land 0xFFFF in
    let c = r.sp lxor i lxor v in
    let f =
      { z = false
      ; n = false
      ; h = (c land 0x0010) = 0x0010
      ; c = (c land 0x100) = 0x0100
      }
    in
    let hi = (v land 0xFF00) lsr 8 in
    let lo = (v land 0x00FF) lsr 0 in
    (match reg with
    | R16S_BC -> Some { cpu with r = { r with b = hi; c = lo }; f; uop = U_FETCH }
    | R16S_DE -> Some { cpu with r = { r with d = hi; e = lo }; f; uop = U_FETCH }
    | R16S_HL -> Some { cpu with r = { r with h = hi; l = lo }; f; uop = U_FETCH }
    | R16S_SP -> Some { cpu with r = { r with sp = v }; f; uop = U_ADD_SP_D8_M4 }
    )
  | U_ADD_SP_D8_M4 ->
    Some { cpu with uop = U_FETCH }

  | U_INT_M2 addr ->
    Some { cpu with
      r = { r with sp = (r.sp - 1) land 0xFFFF };
      uop = U_INT_M3 addr;
    }
  | U_INT_M3 addr ->
    Some { cpu with uop = U_INT_M4 addr }
  | U_INT_M4 addr ->
    System.write s r.sp ((r.pc land 0xFF00) lsr 8) |> Option.map (fun s ->
      { cpu with s;
        r = { r with sp = (r.sp - 1) land 0xFFFF; pc = r.pc land 0x00FF; };
        uop = U_INT_M5 addr
      }
    )

  | U_INT_M5 addr ->
    System.write s r.sp ((r.pc land 0x00FF) lsr 0) |> Option.map (fun s ->
      { cpu with s; r = { r with pc = addr; }; uop = U_FETCH }
    )

let tick cpu =
  match System.tick cpu.s with
  | None -> None
  | Some s -> step { cpu with s }

let dump cpu =
  Printf.eprintf "%04x %04x %02x %02x %02x %02x %02x %02x %02x %b %b %b %b %b\n"
      cpu.r.pc cpu.r.sp
      cpu.r.b cpu.r.c cpu.r.d cpu.r.e cpu.r.h cpu.r.l cpu.r.a
      cpu.f.z cpu.f.n cpu.f.h cpu.f.c
      cpu.ime;

