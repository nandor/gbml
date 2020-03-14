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

let mem_reader cpu sys addr f =
  match System.read sys addr with
  | None -> None
  | Some imm -> Some (f imm, sys)

let mem_writer cpu sys addr v f =
  match System.write sys addr v with
  | None -> None
  | Some sys' -> Some (f (), sys')

let read_imm cpu sys f =
  match System.read sys cpu.r.pc with
  | None -> None
  | Some imm -> Some (f { cpu with r = inc_pc cpu.r } imm, sys)

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

let handle_op cpu sys op =
  let handle_alu8 op op0 op1 =
    let v0 = get_reg_8 cpu.r op0 in
    let v1 = get_reg_8 cpu.r op1 in
    let v, f = execute_alu8 op cpu.f v0 v1 in
    Some { cpu with r = (set_reg_8 (inc_pc cpu.r) op0 v); f; uop = U_FETCH }
  in
  let handle_alu8_rot op op0 op1 =
    let v0 = get_reg_8 cpu.r op0 in
    let v1 = get_reg_8 cpu.r op1 in
    let v, { c } = execute_alu8 op cpu.f v0 v1 in
    Some { cpu with
      r = (set_reg_8 (inc_pc cpu.r) op0 v);
      f = { z = false; n = false; h = false; c };
      uop = U_FETCH
    }
  in
  let handle_alu8_d8 op dst =
    Some { cpu with r = inc_pc cpu.r; uop = U_ALU8_D8_M2(op, dst) }
  in
  let handle_alu8_hl_r8 op dst =
    Some { cpu with r = inc_pc cpu.r; uop = U_ALU8_HL_R8_M2(op, dst) }
  in
  let handle_alu16 op dst src =
    Some { cpu with r = inc_pc cpu.r; uop = U_ALU16_M2(op, dst, src) }
  in
  let handle_ld_r8_d8 dst =
    Some { cpu with r = inc_pc cpu.r; uop = U_LD_R8_D8_M2 dst }
  in
  let handle_ld_r16_d16 reg =
    Some { cpu with r = inc_pc cpu.r; uop = U_LD_R16_D16_M2 reg }
  in
  let handle_st_d16_r16 reg =
    Some { cpu with r = inc_pc cpu.r; uop = U_ST_D16_R16_M2 reg }
  in
  let handle_mov_r8_r16 rw dst src =
    Some { cpu with r = inc_pc cpu.r; uop = match rw with
        | R -> U_MOV_R8_R16_M2_R(dst, src)
        | W -> U_MOV_R8_R16_M2_W(dst, src)
    }
  in
  let handle_mov_hl rw dir reg =
    Some { cpu with r = inc_pc cpu.r; uop = match rw with
        | R -> U_MOV_HL_M2_R(dir, reg)
        | W -> U_MOV_HL_M2_W(dir, reg)
    }
  in
  let handle_alu8_hl op =
    Some { cpu with r = inc_pc cpu.r; uop = U_ALU8_HL_M2 op }
  in
  let handle_jr cc =
    Some { cpu with r = inc_pc cpu.r; uop = U_JR_M2 cc }
  in
  let handle_ret cc ime =
    Some { cpu with r = inc_pc cpu.r; uop = match cc with
        | CC_A  -> U_RET_M3 ime
        | CC_Z  -> U_RET_M2 RCC_Z
        | CC_NZ -> U_RET_M2 RCC_NZ
        | CC_C  -> U_RET_M2 RCC_C
        | CC_NC -> U_RET_M2 RCC_NC
    }
  in
  let handle_push reg =
    Some { cpu with r = inc_pc cpu.r; uop = U_PUSH_M2 reg }
  in
  let handle_pop reg =
    Some { cpu with r = inc_pc cpu.r; uop = U_POP_M2 reg }
  in
  let handle_call cc =
    Some { cpu with r = inc_pc cpu.r; uop = U_CALL_M2 cc }
  in
  let handle_jp cc =
    Some { cpu with r = inc_pc cpu.r; uop = U_JP_D16_M2 cc }
  in
  let handle_movh rw =
    Some { cpu with r = inc_pc cpu.r; uop = U_MOV_D8_M2 rw }
  in
  let handle_mov_c rw =
    Some { cpu with
      r = inc_pc cpu.r;
      uop = match rw with
      | R -> U_MOV_C_M2_R
      | W -> U_MOV_C_M2_W
    }
  in
  let handle_mov_d16_r8 reg rw =
    Some { cpu with r = inc_pc cpu.r; uop = U_MOV_D16_R8_M2(reg, rw) }
  in
  let handle_rst i =
    Some { cpu with r = inc_pc cpu.r; uop = U_RST_M2 i }
  in
  let handle_st_hl_d8 r =
    Some { cpu with r = inc_pc cpu.r; uop = U_ST_HL_D8_M2 r }
  in
  let handle_add_sp_d8 r =
    Some { cpu with r = inc_pc cpu.r; uop = U_ADD_SP_D8_M2 r }
  in
  match op with
  | 0x00 -> Some { cpu with r = inc_pc cpu.r; uop = U_FETCH }
  | 0x01 -> handle_ld_r16_d16 R16S_BC
  | 0x02 -> handle_mov_r8_r16 W R8_A R16_BC
  | 0x03 -> handle_alu16      Inc16 R16S_BC R16S_BC
  | 0x04 -> handle_alu8       A8_Inc R8_B R8_B
  | 0x05 -> handle_alu8       A8_Dec R8_B R8_B
  | 0x06 -> handle_ld_r8_d8   R8_B
  | 0x07 -> handle_alu8_rot   A8_Rlc R8_A R8_A
  | 0x08 -> handle_st_d16_r16 R16S_SP
  | 0x09 -> handle_alu16      Add16 R16S_HL R16S_BC
  | 0x0A -> handle_mov_r8_r16 R R8_A R16_BC
  | 0x0B -> handle_alu16      Dec16 R16S_BC R16S_BC
  | 0x0C -> handle_alu8       A8_Inc R8_C R8_C
  | 0x0D -> handle_alu8       A8_Dec R8_C R8_C
  | 0x0E -> handle_ld_r8_d8   R8_C
  | 0x0F -> handle_alu8_rot   A8_Rrc R8_A R8_A
  | 0x10 -> None
  | 0x11 -> handle_ld_r16_d16 R16S_DE
  | 0x12 -> handle_mov_r8_r16 W R8_A R16_DE
  | 0x13 -> handle_alu16      Inc16 R16S_DE R16S_DE
  | 0x14 -> handle_alu8       A8_Inc R8_D R8_D
  | 0x15 -> handle_alu8       A8_Dec R8_D R8_D
  | 0x16 -> handle_ld_r8_d8   R8_D
  | 0x17 -> handle_alu8_rot   A8_Rl R8_A R8_A
  | 0x18 -> handle_jr         CC_A
  | 0x19 -> handle_alu16      Add16 R16S_HL R16S_DE
  | 0x1A -> handle_mov_r8_r16 R R8_A R16_DE
  | 0x1B -> handle_alu16      Dec16 R16S_DE R16S_DE
  | 0x1C -> handle_alu8       A8_Inc R8_E R8_E
  | 0x1D -> handle_alu8       A8_Dec R8_E R8_E
  | 0x1E -> handle_ld_r8_d8   R8_E
  | 0x1F -> handle_alu8_rot   A8_Rr R8_A R8_A
  | 0x20 -> handle_jr         CC_NZ
  | 0x21 -> handle_ld_r16_d16 R16S_HL
  | 0x22 -> handle_mov_hl     W D_Inc R8_A
  | 0x23 -> handle_alu16      Inc16 R16S_HL R16S_HL
  | 0x24 -> handle_alu8       A8_Inc R8_H R8_H
  | 0x25 -> handle_alu8       A8_Dec R8_H R8_H
  | 0x26 -> handle_ld_r8_d8   R8_H
  | 0x27 -> handle_alu8       A8_Daa R8_A R8_A
  | 0x28 -> handle_jr         CC_Z
  | 0x29 -> handle_alu16      Add16 R16S_HL R16S_HL
  | 0x2A -> handle_mov_hl     R D_Inc R8_A
  | 0x2B -> handle_alu16      Dec16 R16S_HL R16S_HL
  | 0x2C -> handle_alu8       A8_Inc R8_L R8_L
  | 0x2D -> handle_alu8       A8_Dec R8_L R8_L
  | 0x2E -> handle_ld_r8_d8   R8_L
  | 0x2F -> handle_alu8       A8_Cpl R8_A R8_A
  | 0x30 -> handle_jr         CC_NC
  | 0x31 -> handle_ld_r16_d16 R16S_SP
  | 0x32 -> handle_mov_hl     W D_Dec R8_A
  | 0x33 -> handle_alu16      Inc16 R16S_SP R16S_SP
  | 0x34 -> handle_alu8_hl    A8_Inc
  | 0x35 -> handle_alu8_hl    A8_Dec
  | 0x36 -> handle_st_hl_d8   R16_HL
  | 0x37 -> handle_alu8       A8_Scf R8_A R8_A
  | 0x38 -> handle_jr         CC_C
  | 0x39 -> handle_alu16      Add16 R16S_HL R16S_SP
  | 0x3A -> handle_mov_hl     R D_Dec R8_A
  | 0x3B -> handle_alu16      Dec16 R16S_SP R16S_SP
  | 0x3C -> handle_alu8       A8_Inc R8_A R8_A
  | 0x3D -> handle_alu8       A8_Dec R8_A R8_A
  | 0x3E -> handle_ld_r8_d8   R8_A
  | 0x3F -> handle_alu8       A8_Ccf R8_A R8_A
  | 0x40 -> handle_alu8       A8_Mov R8_B R8_B
  | 0x41 -> handle_alu8       A8_Mov R8_B R8_C
  | 0x42 -> handle_alu8       A8_Mov R8_B R8_D
  | 0x43 -> handle_alu8       A8_Mov R8_B R8_E
  | 0x44 -> handle_alu8       A8_Mov R8_B R8_H
  | 0x45 -> handle_alu8       A8_Mov R8_B R8_L
  | 0x46 -> handle_alu8_hl_r8 A8_Mov R8_B
  | 0x47 -> handle_alu8       A8_Mov R8_B R8_A
  | 0x48 -> handle_alu8       A8_Mov R8_C R8_B
  | 0x49 -> handle_alu8       A8_Mov R8_C R8_C
  | 0x4A -> handle_alu8       A8_Mov R8_C R8_D
  | 0x4B -> handle_alu8       A8_Mov R8_C R8_E
  | 0x4C -> handle_alu8       A8_Mov R8_C R8_H
  | 0x4D -> handle_alu8       A8_Mov R8_C R8_L
  | 0x4E -> handle_alu8_hl_r8 A8_Mov R8_C
  | 0x4F -> handle_alu8       A8_Mov R8_C R8_A
  | 0x50 -> handle_alu8       A8_Mov R8_D R8_B
  | 0x51 -> handle_alu8       A8_Mov R8_D R8_C
  | 0x52 -> handle_alu8       A8_Mov R8_D R8_D
  | 0x53 -> handle_alu8       A8_Mov R8_D R8_E
  | 0x54 -> handle_alu8       A8_Mov R8_D R8_H
  | 0x55 -> handle_alu8       A8_Mov R8_D R8_L
  | 0x56 -> handle_alu8_hl_r8 A8_Mov R8_D
  | 0x57 -> handle_alu8       A8_Mov R8_D R8_A
  | 0x58 -> handle_alu8       A8_Mov R8_E R8_B
  | 0x59 -> handle_alu8       A8_Mov R8_E R8_C
  | 0x5A -> handle_alu8       A8_Mov R8_E R8_D
  | 0x5B -> handle_alu8       A8_Mov R8_E R8_E
  | 0x5C -> handle_alu8       A8_Mov R8_E R8_H
  | 0x5D -> handle_alu8       A8_Mov R8_E R8_L
  | 0x5E -> handle_alu8_hl_r8 A8_Mov R8_E
  | 0x5F -> handle_alu8       A8_Mov R8_E R8_A
  | 0x60 -> handle_alu8       A8_Mov R8_H R8_B
  | 0x61 -> handle_alu8       A8_Mov R8_H R8_C
  | 0x62 -> handle_alu8       A8_Mov R8_H R8_D
  | 0x63 -> handle_alu8       A8_Mov R8_H R8_E
  | 0x64 -> handle_alu8       A8_Mov R8_H R8_H
  | 0x65 -> handle_alu8       A8_Mov R8_H R8_L
  | 0x66 -> handle_alu8_hl_r8 A8_Mov R8_H
  | 0x67 -> handle_alu8       A8_Mov R8_H R8_A
  | 0x68 -> handle_alu8       A8_Mov R8_L R8_B
  | 0x69 -> handle_alu8       A8_Mov R8_L R8_C
  | 0x6A -> handle_alu8       A8_Mov R8_L R8_D
  | 0x6B -> handle_alu8       A8_Mov R8_L R8_E
  | 0x6C -> handle_alu8       A8_Mov R8_L R8_H
  | 0x6D -> handle_alu8       A8_Mov R8_L R8_L
  | 0x6E -> handle_alu8_hl_r8 A8_Mov R8_L
  | 0x6F -> handle_alu8       A8_Mov R8_L R8_A
  | 0x70 -> handle_mov_hl     W D_Nop R8_B
  | 0x71 -> handle_mov_hl     W D_Nop R8_C
  | 0x72 -> handle_mov_hl     W D_Nop R8_D
  | 0x73 -> handle_mov_hl     W D_Nop R8_E
  | 0x74 -> handle_mov_hl     W D_Nop R8_H
  | 0x75 -> handle_mov_hl     W D_Nop R8_L
  | 0x76 -> Some { cpu with r = inc_pc cpu.r; halted = true; uop = U_FETCH }
  | 0x77 -> handle_mov_hl     W D_Nop R8_A
  | 0x78 -> handle_alu8       A8_Mov R8_A R8_B
  | 0x79 -> handle_alu8       A8_Mov R8_A R8_C
  | 0x7A -> handle_alu8       A8_Mov R8_A R8_D
  | 0x7B -> handle_alu8       A8_Mov R8_A R8_E
  | 0x7C -> handle_alu8       A8_Mov R8_A R8_H
  | 0x7D -> handle_alu8       A8_Mov R8_A R8_L
  | 0x7E -> handle_alu8_hl_r8 A8_Mov R8_A
  | 0x7F -> handle_alu8       A8_Mov R8_A R8_A
  | 0x80 -> handle_alu8       A8_Add R8_A R8_B
  | 0x81 -> handle_alu8       A8_Add R8_A R8_C
  | 0x82 -> handle_alu8       A8_Add R8_A R8_D
  | 0x83 -> handle_alu8       A8_Add R8_A R8_E
  | 0x84 -> handle_alu8       A8_Add R8_A R8_H
  | 0x85 -> handle_alu8       A8_Add R8_A R8_L
  | 0x86 -> handle_alu8_hl_r8 A8_Add R8_A
  | 0x87 -> handle_alu8       A8_Add R8_A R8_A
  | 0x88 -> handle_alu8       A8_Adc R8_A R8_B
  | 0x89 -> handle_alu8       A8_Adc R8_A R8_C
  | 0x8A -> handle_alu8       A8_Adc R8_A R8_D
  | 0x8B -> handle_alu8       A8_Adc R8_A R8_E
  | 0x8C -> handle_alu8       A8_Adc R8_A R8_H
  | 0x8D -> handle_alu8       A8_Adc R8_A R8_L
  | 0x8E -> handle_alu8_hl_r8 A8_Adc R8_A
  | 0x8F -> handle_alu8       A8_Adc R8_A R8_A
  | 0x90 -> handle_alu8       A8_Sub R8_A R8_B
  | 0x91 -> handle_alu8       A8_Sub R8_A R8_C
  | 0x92 -> handle_alu8       A8_Sub R8_A R8_D
  | 0x93 -> handle_alu8       A8_Sub R8_A R8_E
  | 0x94 -> handle_alu8       A8_Sub R8_A R8_H
  | 0x95 -> handle_alu8       A8_Sub R8_A R8_L
  | 0x96 -> handle_alu8_hl_r8 A8_Sub R8_A
  | 0x97 -> handle_alu8       A8_Sub R8_A R8_A
  | 0x98 -> handle_alu8       A8_Sbc R8_A R8_B
  | 0x99 -> handle_alu8       A8_Sbc R8_A R8_C
  | 0x9A -> handle_alu8       A8_Sbc R8_A R8_D
  | 0x9B -> handle_alu8       A8_Sbc R8_A R8_E
  | 0x9C -> handle_alu8       A8_Sbc R8_A R8_H
  | 0x9D -> handle_alu8       A8_Sbc R8_A R8_L
  | 0x9E -> handle_alu8_hl_r8 A8_Sbc R8_A
  | 0x9F -> handle_alu8       A8_Sbc R8_A R8_A
  | 0xA0 -> handle_alu8       A8_And R8_A R8_B
  | 0xA1 -> handle_alu8       A8_And R8_A R8_C
  | 0xA2 -> handle_alu8       A8_And R8_A R8_D
  | 0xA3 -> handle_alu8       A8_And R8_A R8_E
  | 0xA4 -> handle_alu8       A8_And R8_A R8_H
  | 0xA5 -> handle_alu8       A8_And R8_A R8_L
  | 0xA6 -> handle_alu8_hl_r8 A8_And R8_A
  | 0xA7 -> handle_alu8       A8_And R8_A R8_A
  | 0xA8 -> handle_alu8       A8_Xor R8_A R8_B
  | 0xA9 -> handle_alu8       A8_Xor R8_A R8_C
  | 0xAA -> handle_alu8       A8_Xor R8_A R8_D
  | 0xAB -> handle_alu8       A8_Xor R8_A R8_E
  | 0xAC -> handle_alu8       A8_Xor R8_A R8_H
  | 0xAD -> handle_alu8       A8_Xor R8_A R8_L
  | 0xAE -> handle_alu8_hl_r8 A8_Xor R8_A
  | 0xAF -> handle_alu8       A8_Xor R8_A R8_A
  | 0xB0 -> handle_alu8       A8_Or  R8_A R8_B
  | 0xB1 -> handle_alu8       A8_Or  R8_A R8_C
  | 0xB2 -> handle_alu8       A8_Or  R8_A R8_D
  | 0xB3 -> handle_alu8       A8_Or  R8_A R8_E
  | 0xB4 -> handle_alu8       A8_Or  R8_A R8_H
  | 0xB5 -> handle_alu8       A8_Or  R8_A R8_L
  | 0xB6 -> handle_alu8_hl_r8 A8_Or  R8_A
  | 0xB7 -> handle_alu8       A8_Or  R8_A R8_A
  | 0xB8 -> handle_alu8       A8_Cp  R8_A R8_B
  | 0xB9 -> handle_alu8       A8_Cp  R8_A R8_C
  | 0xBA -> handle_alu8       A8_Cp  R8_A R8_D
  | 0xBB -> handle_alu8       A8_Cp  R8_A R8_E
  | 0xBC -> handle_alu8       A8_Cp  R8_A R8_H
  | 0xBD -> handle_alu8       A8_Cp  R8_A R8_L
  | 0xBE -> handle_alu8_hl_r8 A8_Cp  R8_A
  | 0xBF -> handle_alu8       A8_Cp  R8_A R8_A
  | 0xC0 -> handle_ret        CC_NZ false
  | 0xC1 -> handle_pop        R16P_BC
  | 0xC2 -> handle_jp         CC_NZ
  | 0xC3 -> handle_jp         CC_A
  | 0xC4 -> handle_call       CC_NZ
  | 0xC5 -> handle_push       R16P_BC
  | 0xC6 -> handle_alu8_d8    A8_Add R8_A
  | 0xC7 -> handle_rst        0x00
  | 0xC8 -> handle_ret        CC_Z false
  | 0xC9 -> handle_ret        CC_A false
  | 0xCA -> handle_jp         CC_Z
  | 0xCB -> Some { cpu with r = inc_pc cpu.r; uop = U_CB }
  | 0xCC -> handle_call       CC_Z
  | 0xCD -> handle_call       CC_A
  | 0xCE -> handle_alu8_d8    A8_Adc R8_A
  | 0xCF -> handle_rst        0x08
  | 0xD0 -> handle_ret        CC_NC false
  | 0xD1 -> handle_pop        R16P_DE
  | 0xD2 -> handle_jp         CC_NC
  | 0xD3 -> failwith "0xD3"
  | 0xD4 -> handle_call       CC_NC
  | 0xD5 -> handle_push       R16P_DE
  | 0xD6 -> handle_alu8_d8    A8_Sub R8_A
  | 0xD7 -> handle_rst        0x10
  | 0xD8 -> handle_ret        CC_C false
  | 0xD9 -> handle_ret        CC_A true
  | 0xDA -> handle_jp         CC_C
  | 0xDB -> failwith "0xDB"
  | 0xDC -> handle_call       CC_C
  | 0xDE -> handle_alu8_d8    A8_Sbc R8_A
  | 0xDF -> handle_rst        0x18
  | 0xE0 -> handle_movh       W
  | 0xE1 -> handle_pop        R16P_HL
  | 0xE2 -> handle_mov_c      W
  | 0xE3 -> failwith "0xE3"
  | 0xE4 -> failwith "0xE4"
  | 0xE5 -> handle_push       R16P_HL
  | 0xE6 -> handle_alu8_d8    A8_And R8_A
  | 0xE7 -> handle_rst        0x20
  | 0xE8 -> handle_add_sp_d8  R16S_SP
  | 0xE9 -> Some { cpu with r = { cpu.r with pc = (cpu.r.h lsl 8) lor cpu.r.l }; uop = U_FETCH }
  | 0xEA -> handle_mov_d16_r8 R8_A W
  | 0xEB -> failwith "0xEB"
  | 0xEC -> failwith "0xEC"
  | 0xED -> failwith "0xED"
  | 0xEE -> handle_alu8_d8    A8_Xor R8_A
  | 0xEF -> handle_rst        0x28
  | 0xF0 -> handle_movh       R
  | 0xF1 -> handle_pop        R16P_AF
  | 0xF2 -> handle_mov_c      R
  | 0xF3 -> Some { cpu with r = inc_pc cpu.r; ime = false; uop = U_FETCH }
  | 0xF4 -> failwith "0xF4"
  | 0xF5 -> handle_push       R16P_AF
  | 0xF6 -> handle_alu8_d8    A8_Or R8_A
  | 0xF7 -> handle_rst        0x30
  | 0xF8 -> handle_add_sp_d8  R16S_HL
  | 0xF9 -> handle_alu16      Mov16 R16S_SP R16S_HL
  | 0xFA -> handle_mov_d16_r8 R8_A R
  | 0xFB -> Some { cpu with r = inc_pc cpu.r; ime = true; uop = U_FETCH }
  | 0xFC -> failwith "0xFC"
  | 0xFD -> failwith "0xFD"
  | 0xFE -> handle_alu8_d8    A8_Cp R8_A
  | 0xFF -> handle_rst        0x38
  | _ -> failwith "unreachable"

let handle_cb cpu sys op =
  let handle_alu8 op op0 op1 =
    let v0 = get_reg_8 cpu.r op0 in
    let v1 = get_reg_8 cpu.r op1 in
    let v, f = execute_alu8 op cpu.f v0 v1 in
    { cpu with r = (set_reg_8 (inc_pc cpu.r) op0 v); f; uop = U_FETCH }
  in
  let handle_alu8_hl op =
    { cpu with r = inc_pc cpu.r; uop = U_ALU8_HL_M2 op }
  in
  let handle_alu8_bit n =
    { cpu with r = inc_pc cpu.r; uop = U_ALU8_BIT_M2 n }
  in
  match op with
  | 0x00 -> handle_alu8     A8_Rlc R8_B R8_B
  | 0x01 -> handle_alu8     A8_Rlc R8_C R8_C
  | 0x02 -> handle_alu8     A8_Rlc R8_D R8_D
  | 0x03 -> handle_alu8     A8_Rlc R8_E R8_E
  | 0x04 -> handle_alu8     A8_Rlc R8_H R8_H
  | 0x05 -> handle_alu8     A8_Rlc R8_L R8_L
  | 0x06 -> handle_alu8_hl  A8_Rlc
  | 0x07 -> handle_alu8     A8_Rlc R8_A R8_A
  | 0x08 -> handle_alu8     A8_Rrc R8_B R8_B
  | 0x09 -> handle_alu8     A8_Rrc R8_C R8_C
  | 0x0A -> handle_alu8     A8_Rrc R8_D R8_D
  | 0x0B -> handle_alu8     A8_Rrc R8_E R8_E
  | 0x0C -> handle_alu8     A8_Rrc R8_H R8_H
  | 0x0D -> handle_alu8     A8_Rrc R8_L R8_L
  | 0x0E -> handle_alu8_hl  A8_Rrc
  | 0x0F -> handle_alu8     A8_Rrc R8_A R8_A
  | 0x10 -> handle_alu8     A8_Rl R8_B R8_B
  | 0x11 -> handle_alu8     A8_Rl R8_C R8_C
  | 0x12 -> handle_alu8     A8_Rl R8_D R8_D
  | 0x13 -> handle_alu8     A8_Rl R8_E R8_E
  | 0x14 -> handle_alu8     A8_Rl R8_H R8_H
  | 0x15 -> handle_alu8     A8_Rl R8_L R8_L
  | 0x16 -> handle_alu8_hl  A8_Rl
  | 0x17 -> handle_alu8     A8_Rl R8_A R8_A
  | 0x18 -> handle_alu8     A8_Rr R8_B R8_B
  | 0x19 -> handle_alu8     A8_Rr R8_C R8_C
  | 0x1A -> handle_alu8     A8_Rr R8_D R8_D
  | 0x1B -> handle_alu8     A8_Rr R8_E R8_E
  | 0x1C -> handle_alu8     A8_Rr R8_H R8_H
  | 0x1D -> handle_alu8     A8_Rr R8_L R8_L
  | 0x1E -> handle_alu8_hl  A8_Rr
  | 0x1F -> handle_alu8     A8_Rr R8_A R8_A
  | 0x20 -> handle_alu8     A8_Sla R8_B R8_B
  | 0x21 -> handle_alu8     A8_Sla R8_C R8_C
  | 0x22 -> handle_alu8     A8_Sla R8_D R8_D
  | 0x23 -> handle_alu8     A8_Sla R8_E R8_E
  | 0x24 -> handle_alu8     A8_Sla R8_H R8_H
  | 0x25 -> handle_alu8     A8_Sla R8_L R8_L
  | 0x26 -> handle_alu8_hl  A8_Sla
  | 0x27 -> handle_alu8     A8_Sla R8_A R8_A
  | 0x28 -> handle_alu8     A8_Sra R8_B R8_B
  | 0x29 -> handle_alu8     A8_Sra R8_C R8_C
  | 0x2A -> handle_alu8     A8_Sra R8_D R8_D
  | 0x2B -> handle_alu8     A8_Sra R8_E R8_E
  | 0x2C -> handle_alu8     A8_Sra R8_H R8_H
  | 0x2D -> handle_alu8     A8_Sra R8_L R8_L
  | 0x2E -> handle_alu8_hl  A8_Sra
  | 0x2F -> handle_alu8     A8_Sra R8_A R8_A
  | 0x30 -> handle_alu8     A8_Swap R8_B R8_B
  | 0x31 -> handle_alu8     A8_Swap R8_C R8_C
  | 0x32 -> handle_alu8     A8_Swap R8_D R8_D
  | 0x33 -> handle_alu8     A8_Swap R8_E R8_E
  | 0x34 -> handle_alu8     A8_Swap R8_H R8_H
  | 0x35 -> handle_alu8     A8_Swap R8_L R8_L
  | 0x36 -> handle_alu8_hl  A8_Swap
  | 0x37 -> handle_alu8     A8_Swap R8_A R8_A
  | 0x38 -> handle_alu8     A8_Srl R8_B R8_B
  | 0x39 -> handle_alu8     A8_Srl R8_C R8_C
  | 0x3A -> handle_alu8     A8_Srl R8_D R8_D
  | 0x3B -> handle_alu8     A8_Srl R8_E R8_E
  | 0x3C -> handle_alu8     A8_Srl R8_H R8_H
  | 0x3D -> handle_alu8     A8_Srl R8_L R8_L
  | 0x3E -> handle_alu8_hl  A8_Srl
  | 0x3F -> handle_alu8     A8_Srl R8_A R8_A
  | 0x40 -> handle_alu8     (A8_Bit 0) R8_B R8_B
  | 0x41 -> handle_alu8     (A8_Bit 0) R8_C R8_C
  | 0x42 -> handle_alu8     (A8_Bit 0) R8_D R8_D
  | 0x43 -> handle_alu8     (A8_Bit 0) R8_E R8_E
  | 0x44 -> handle_alu8     (A8_Bit 0) R8_H R8_H
  | 0x45 -> handle_alu8     (A8_Bit 0) R8_L R8_L
  | 0x46 -> handle_alu8_bit 0
  | 0x47 -> handle_alu8     (A8_Bit 0) R8_A R8_A
  | 0x48 -> handle_alu8     (A8_Bit 1) R8_B R8_B
  | 0x49 -> handle_alu8     (A8_Bit 1) R8_C R8_C
  | 0x4A -> handle_alu8     (A8_Bit 1) R8_D R8_D
  | 0x4B -> handle_alu8     (A8_Bit 1) R8_E R8_E
  | 0x4C -> handle_alu8     (A8_Bit 1) R8_H R8_H
  | 0x4D -> handle_alu8     (A8_Bit 1) R8_L R8_L
  | 0x4E -> handle_alu8_bit 1
  | 0x4F -> handle_alu8     (A8_Bit 1) R8_A R8_A
  | 0x50 -> handle_alu8     (A8_Bit 2) R8_B R8_B
  | 0x51 -> handle_alu8     (A8_Bit 2) R8_C R8_C
  | 0x52 -> handle_alu8     (A8_Bit 2) R8_D R8_D
  | 0x53 -> handle_alu8     (A8_Bit 2) R8_E R8_E
  | 0x54 -> handle_alu8     (A8_Bit 2) R8_H R8_H
  | 0x55 -> handle_alu8     (A8_Bit 2) R8_L R8_L
  | 0x56 -> handle_alu8_bit 2
  | 0x57 -> handle_alu8     (A8_Bit 2) R8_A R8_A
  | 0x58 -> handle_alu8     (A8_Bit 3) R8_B R8_B
  | 0x59 -> handle_alu8     (A8_Bit 3) R8_C R8_C
  | 0x5A -> handle_alu8     (A8_Bit 3) R8_D R8_D
  | 0x5B -> handle_alu8     (A8_Bit 3) R8_E R8_E
  | 0x5C -> handle_alu8     (A8_Bit 3) R8_H R8_H
  | 0x5D -> handle_alu8     (A8_Bit 3) R8_L R8_L
  | 0x5E -> handle_alu8_bit 3
  | 0x5F -> handle_alu8     (A8_Bit 3) R8_A R8_A
  | 0x60 -> handle_alu8     (A8_Bit 4) R8_B R8_B
  | 0x61 -> handle_alu8     (A8_Bit 4) R8_C R8_C
  | 0x62 -> handle_alu8     (A8_Bit 4) R8_D R8_D
  | 0x63 -> handle_alu8     (A8_Bit 4) R8_E R8_E
  | 0x64 -> handle_alu8     (A8_Bit 4) R8_H R8_H
  | 0x65 -> handle_alu8     (A8_Bit 4) R8_L R8_L
  | 0x66 -> handle_alu8_bit 4
  | 0x67 -> handle_alu8     (A8_Bit 4) R8_A R8_A
  | 0x68 -> handle_alu8     (A8_Bit 5) R8_B R8_B
  | 0x69 -> handle_alu8     (A8_Bit 5) R8_C R8_C
  | 0x6A -> handle_alu8     (A8_Bit 5) R8_D R8_D
  | 0x6B -> handle_alu8     (A8_Bit 5) R8_E R8_E
  | 0x6C -> handle_alu8     (A8_Bit 5) R8_H R8_H
  | 0x6D -> handle_alu8     (A8_Bit 5) R8_L R8_L
  | 0x6E -> handle_alu8_bit 5
  | 0x6F -> handle_alu8     (A8_Bit 5) R8_A R8_A
  | 0x70 -> handle_alu8     (A8_Bit 6) R8_B R8_B
  | 0x71 -> handle_alu8     (A8_Bit 6) R8_C R8_C
  | 0x72 -> handle_alu8     (A8_Bit 6) R8_D R8_D
  | 0x73 -> handle_alu8     (A8_Bit 6) R8_E R8_E
  | 0x74 -> handle_alu8     (A8_Bit 6) R8_H R8_H
  | 0x75 -> handle_alu8     (A8_Bit 6) R8_L R8_L
  | 0x76 -> handle_alu8_bit 6
  | 0x77 -> handle_alu8     (A8_Bit 6) R8_A R8_A
  | 0x78 -> handle_alu8     (A8_Bit 7) R8_B R8_B
  | 0x79 -> handle_alu8     (A8_Bit 7) R8_C R8_C
  | 0x7A -> handle_alu8     (A8_Bit 7) R8_D R8_D
  | 0x7B -> handle_alu8     (A8_Bit 7) R8_E R8_E
  | 0x7C -> handle_alu8     (A8_Bit 7) R8_H R8_H
  | 0x7D -> handle_alu8     (A8_Bit 7) R8_L R8_L
  | 0x7E -> handle_alu8_bit 7
  | 0x7F -> handle_alu8     (A8_Bit 7) R8_A R8_A
  | 0x80 -> handle_alu8     (A8_Res 0) R8_B R8_B
  | 0x81 -> handle_alu8     (A8_Res 0) R8_C R8_C
  | 0x82 -> handle_alu8     (A8_Res 0) R8_D R8_D
  | 0x83 -> handle_alu8     (A8_Res 0) R8_E R8_E
  | 0x84 -> handle_alu8     (A8_Res 0) R8_H R8_H
  | 0x85 -> handle_alu8     (A8_Res 0) R8_L R8_L
  | 0x86 -> handle_alu8_hl  (A8_Res 0)
  | 0x87 -> handle_alu8     (A8_Res 0) R8_A R8_A
  | 0x88 -> handle_alu8     (A8_Res 1) R8_B R8_B
  | 0x89 -> handle_alu8     (A8_Res 1) R8_C R8_C
  | 0x8A -> handle_alu8     (A8_Res 1) R8_D R8_D
  | 0x8B -> handle_alu8     (A8_Res 1) R8_E R8_E
  | 0x8C -> handle_alu8     (A8_Res 1) R8_H R8_H
  | 0x8D -> handle_alu8     (A8_Res 1) R8_L R8_L
  | 0x8E -> handle_alu8_hl  (A8_Res 1)
  | 0x8F -> handle_alu8     (A8_Res 1) R8_A R8_A
  | 0x90 -> handle_alu8     (A8_Res 2) R8_B R8_B
  | 0x91 -> handle_alu8     (A8_Res 2) R8_C R8_C
  | 0x92 -> handle_alu8     (A8_Res 2) R8_D R8_D
  | 0x93 -> handle_alu8     (A8_Res 2) R8_E R8_E
  | 0x94 -> handle_alu8     (A8_Res 2) R8_H R8_H
  | 0x95 -> handle_alu8     (A8_Res 2) R8_L R8_L
  | 0x96 -> handle_alu8_hl  (A8_Res 2)
  | 0x97 -> handle_alu8     (A8_Res 2) R8_A R8_A
  | 0x98 -> handle_alu8     (A8_Res 3) R8_B R8_B
  | 0x99 -> handle_alu8     (A8_Res 3) R8_C R8_C
  | 0x9A -> handle_alu8     (A8_Res 3) R8_D R8_D
  | 0x9B -> handle_alu8     (A8_Res 3) R8_E R8_E
  | 0x9C -> handle_alu8     (A8_Res 3) R8_H R8_H
  | 0x9D -> handle_alu8     (A8_Res 3) R8_L R8_L
  | 0x9E -> handle_alu8_hl  (A8_Res 3)
  | 0x9F -> handle_alu8     (A8_Res 3) R8_A R8_A
  | 0xA0 -> handle_alu8     (A8_Res 4) R8_B R8_B
  | 0xA1 -> handle_alu8     (A8_Res 4) R8_C R8_C
  | 0xA2 -> handle_alu8     (A8_Res 4) R8_D R8_D
  | 0xA3 -> handle_alu8     (A8_Res 4) R8_E R8_E
  | 0xA4 -> handle_alu8     (A8_Res 4) R8_H R8_H
  | 0xA5 -> handle_alu8     (A8_Res 4) R8_L R8_L
  | 0xA6 -> handle_alu8_hl  (A8_Res 4)
  | 0xA7 -> handle_alu8     (A8_Res 4) R8_A R8_A
  | 0xA8 -> handle_alu8     (A8_Res 5) R8_B R8_B
  | 0xA9 -> handle_alu8     (A8_Res 5) R8_C R8_C
  | 0xAA -> handle_alu8     (A8_Res 5) R8_D R8_D
  | 0xAB -> handle_alu8     (A8_Res 5) R8_E R8_E
  | 0xAC -> handle_alu8     (A8_Res 5) R8_H R8_H
  | 0xAD -> handle_alu8     (A8_Res 5) R8_L R8_L
  | 0xAE -> handle_alu8_hl  (A8_Res 5)
  | 0xAF -> handle_alu8     (A8_Res 5) R8_A R8_A
  | 0xB0 -> handle_alu8     (A8_Res 6) R8_B R8_B
  | 0xB1 -> handle_alu8     (A8_Res 6) R8_C R8_C
  | 0xB2 -> handle_alu8     (A8_Res 6) R8_D R8_D
  | 0xB3 -> handle_alu8     (A8_Res 6) R8_E R8_E
  | 0xB4 -> handle_alu8     (A8_Res 6) R8_H R8_H
  | 0xB5 -> handle_alu8     (A8_Res 6) R8_L R8_L
  | 0xB6 -> handle_alu8_hl  (A8_Res 6)
  | 0xB7 -> handle_alu8     (A8_Res 6) R8_A R8_A
  | 0xB8 -> handle_alu8     (A8_Res 7) R8_B R8_B
  | 0xB9 -> handle_alu8     (A8_Res 7) R8_C R8_C
  | 0xBA -> handle_alu8     (A8_Res 7) R8_D R8_D
  | 0xBB -> handle_alu8     (A8_Res 7) R8_E R8_E
  | 0xBC -> handle_alu8     (A8_Res 7) R8_H R8_H
  | 0xBD -> handle_alu8     (A8_Res 7) R8_L R8_L
  | 0xBE -> handle_alu8_hl  (A8_Res 7)
  | 0xBF -> handle_alu8     (A8_Res 7) R8_A R8_A
  | 0xC0 -> handle_alu8     (A8_Set 0) R8_B R8_B
  | 0xC1 -> handle_alu8     (A8_Set 0) R8_C R8_C
  | 0xC2 -> handle_alu8     (A8_Set 0) R8_D R8_D
  | 0xC3 -> handle_alu8     (A8_Set 0) R8_E R8_E
  | 0xC4 -> handle_alu8     (A8_Set 0) R8_H R8_H
  | 0xC5 -> handle_alu8     (A8_Set 0) R8_L R8_L
  | 0xC6 -> handle_alu8_hl  (A8_Set 0)
  | 0xC7 -> handle_alu8     (A8_Set 0) R8_A R8_A
  | 0xC8 -> handle_alu8     (A8_Set 1) R8_B R8_B
  | 0xC9 -> handle_alu8     (A8_Set 1) R8_C R8_C
  | 0xCA -> handle_alu8     (A8_Set 1) R8_D R8_D
  | 0xCB -> handle_alu8     (A8_Set 1) R8_E R8_E
  | 0xCC -> handle_alu8     (A8_Set 1) R8_H R8_H
  | 0xCD -> handle_alu8     (A8_Set 1) R8_L R8_L
  | 0xCE -> handle_alu8_hl  (A8_Set 1)
  | 0xCF -> handle_alu8     (A8_Set 1) R8_A R8_A
  | 0xD0 -> handle_alu8     (A8_Set 2) R8_B R8_B
  | 0xD1 -> handle_alu8     (A8_Set 2) R8_C R8_C
  | 0xD2 -> handle_alu8     (A8_Set 2) R8_D R8_D
  | 0xD3 -> handle_alu8     (A8_Set 2) R8_E R8_E
  | 0xD4 -> handle_alu8     (A8_Set 2) R8_H R8_H
  | 0xD5 -> handle_alu8     (A8_Set 2) R8_L R8_L
  | 0xD6 -> handle_alu8_hl  (A8_Set 2)
  | 0xD7 -> handle_alu8     (A8_Set 2) R8_A R8_A
  | 0xD8 -> handle_alu8     (A8_Set 3) R8_B R8_B
  | 0xD9 -> handle_alu8     (A8_Set 3) R8_C R8_C
  | 0xDA -> handle_alu8     (A8_Set 3) R8_D R8_D
  | 0xDB -> handle_alu8     (A8_Set 3) R8_E R8_E
  | 0xDC -> handle_alu8     (A8_Set 3) R8_H R8_H
  | 0xDD -> handle_alu8     (A8_Set 3) R8_L R8_L
  | 0xDE -> handle_alu8_hl  (A8_Set 3)
  | 0xDF -> handle_alu8     (A8_Set 3) R8_A R8_A
  | 0xE0 -> handle_alu8     (A8_Set 4) R8_B R8_B
  | 0xE1 -> handle_alu8     (A8_Set 4) R8_C R8_C
  | 0xE2 -> handle_alu8     (A8_Set 4) R8_D R8_D
  | 0xE3 -> handle_alu8     (A8_Set 4) R8_E R8_E
  | 0xE4 -> handle_alu8     (A8_Set 4) R8_H R8_H
  | 0xE5 -> handle_alu8     (A8_Set 4) R8_L R8_L
  | 0xE6 -> handle_alu8_hl  (A8_Set 4)
  | 0xE7 -> handle_alu8     (A8_Set 4) R8_A R8_A
  | 0xE8 -> handle_alu8     (A8_Set 5) R8_B R8_B
  | 0xE9 -> handle_alu8     (A8_Set 5) R8_C R8_C
  | 0xEA -> handle_alu8     (A8_Set 5) R8_D R8_D
  | 0xEB -> handle_alu8     (A8_Set 5) R8_E R8_E
  | 0xEC -> handle_alu8     (A8_Set 5) R8_H R8_H
  | 0xED -> handle_alu8     (A8_Set 5) R8_L R8_L
  | 0xEE -> handle_alu8_hl  (A8_Set 5)
  | 0xEF -> handle_alu8     (A8_Set 5) R8_A R8_A
  | 0xF0 -> handle_alu8     (A8_Set 6) R8_B R8_B
  | 0xF1 -> handle_alu8     (A8_Set 6) R8_C R8_C
  | 0xF2 -> handle_alu8     (A8_Set 6) R8_D R8_D
  | 0xF3 -> handle_alu8     (A8_Set 6) R8_E R8_E
  | 0xF4 -> handle_alu8     (A8_Set 6) R8_H R8_H
  | 0xF5 -> handle_alu8     (A8_Set 6) R8_L R8_L
  | 0xF6 -> handle_alu8_hl  (A8_Set 6)
  | 0xF7 -> handle_alu8     (A8_Set 6) R8_A R8_A
  | 0xF8 -> handle_alu8     (A8_Set 7) R8_B R8_B
  | 0xF9 -> handle_alu8     (A8_Set 7) R8_C R8_C
  | 0xFA -> handle_alu8     (A8_Set 7) R8_D R8_D
  | 0xFB -> handle_alu8     (A8_Set 7) R8_E R8_E
  | 0xFC -> handle_alu8     (A8_Set 7) R8_H R8_H
  | 0xFD -> handle_alu8     (A8_Set 7) R8_L R8_L
  | 0xFE -> handle_alu8_hl  (A8_Set 7)
  | 0xFF -> handle_alu8     (A8_Set 7) R8_A R8_A
  | _ -> failwith "unreachable"

let create () =
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
  ; ime = false
  ; halted = false
  ; uop = U_FETCH
  }

let interrupt cpu sys addr i =
  System.clear_interrupt sys i |> Option.map (fun sys ->
    { cpu with ime = false; uop = U_INT_M2 addr }, sys
  )

let tick cpu sys =
  (* Update the halted flag is an interrupt was triggered. *)
  let halted =
    cpu.halted && not (
      List.exists (System.is_interrupt_pending sys)
        [ Int_VBlank
        ; Int_Stat
        ; Int_Timer
        ; Int_Serial
        ; Int_Pins
        ]
    )
  in
  let cpu = { cpu with halted } in
  let { uop; r; f; halted; ime } = cpu in

  (* Helper to check if an ISR should be entered. *)
  let has_interrupt i =
    ime && System.is_interrupt_pending sys i && System.is_interrupt_enabled sys i
  in
  match uop with
  | U_FETCH when has_interrupt Int_VBlank -> interrupt cpu sys 0x40 Int_VBlank
  | U_FETCH when has_interrupt Int_Stat   -> interrupt cpu sys 0x48 Int_Stat
  | U_FETCH when has_interrupt Int_Timer  -> interrupt cpu sys 0x50 Int_Timer
  | U_FETCH when has_interrupt Int_Serial -> interrupt cpu sys 0x58 Int_Serial
  | U_FETCH when has_interrupt Int_Pins   -> interrupt cpu sys 0x60 Int_Pins
  | U_FETCH when halted -> Some (cpu, sys)
  | U_FETCH ->
    (match System.read sys r.pc with
    | None -> None
    | Some op ->
      match handle_op cpu sys op with
      | None -> None
      | Some cpu' -> Some (cpu', sys)
    )
  | U_CB ->
    (match System.read sys r.pc with
    | None -> None
    | Some op -> Some (handle_cb cpu sys op, sys)
    )
  | U_MOV_R8_R16_M2_R(r8, r16) ->
      let addr = get_reg_16 r r16 in
      System.read sys addr |> Option.map (fun v ->
        { cpu with r = (set_reg_8 r r8 v); uop = U_FETCH }, sys
      )
  | U_MOV_R8_R16_M2_W(r8, r16) ->
    let addr = get_reg_16 r r16 in
    let v = get_reg_8 r r8 in
    System.write sys addr v |> Option.map (fun sys ->
      { cpu with uop = U_FETCH }, sys
    )
  | U_LD_R16_D16_M2 reg ->
    read_imm cpu sys (fun cpu v ->
      let r = match reg with
        | R16S_BC -> { cpu.r with c = v }
        | R16S_DE -> { cpu.r with e = v }
        | R16S_HL -> { cpu.r with l = v }
        | R16S_SP -> { cpu.r with sp = (r.sp land 0xFF00) lor v }
      in
      { cpu with r; uop = U_LD_R16_D16_M3 reg }
    )
  | U_LD_R16_D16_M3 reg ->
    read_imm cpu sys (fun cpu v ->
      let r = match reg with
        | R16S_BC -> { cpu.r with b = v }
        | R16S_DE -> { cpu.r with d = v }
        | R16S_HL -> { cpu.r with h = v }
        | R16S_SP -> { cpu.r with sp = (r.sp land 0x00FF) lor (v lsl 8) }
      in
      { cpu with r; uop = U_FETCH }
    )
  | U_JR_M2 cc ->
    read_imm cpu sys (fun cpu off ->
      { cpu with uop = if is_taken cc f then U_JR_M3 off else U_FETCH }
    )
  | U_JR_M3 off ->
    Some({
      cpu with
        r = { r with pc = (r.pc + i8_of_u8 off) land 0xFFFF };
        uop = U_FETCH
    }, sys)
  | U_MOV_HL_M2_W(op, d) ->
    let v = get_reg_8 r d in
    let addr = (r.h lsl 8) lor r.l in
    System.write sys addr v |> Option.map (fun sys ->
      let hl =
        match op with
        | D_Nop -> addr
        | D_Inc -> (addr + 1) land 0xFFFF
        | D_Dec -> (addr - 1) land 0xFFFF
      in
      let h = (hl land 0xFF00) lsr 8 in
      let l = (hl land 0x00FF) lsr 0 in
      { cpu with r = { r with h; l }; uop = U_FETCH }, sys
    )
  | U_MOV_HL_M2_R(op, d) ->
    let addr = (r.h lsl 8) lor r.l in
    System.read sys addr |> Option.map (fun v ->
      let hl =
        match op with
        | D_Nop -> addr
        | D_Inc -> (addr + 1) land 0xFFFF
        | D_Dec -> (addr - 1) land 0xFFFF
      in
      let h = (hl land 0xFF00) lsr 8 in
      let l = (hl land 0x00FF) lsr 0 in
      { cpu with r = { (set_reg_8 r d v) with h; l }; uop = U_FETCH }, sys
    )
  | U_LD_R8_D8_M2 d ->
    read_imm cpu sys (fun cpu v ->
      { cpu with r = set_reg_8 cpu.r d v; uop = U_FETCH; }
    )

  | U_MOV_C_M2_R ->
    System.read sys (0xFF00 + r.c) |> Option.map (fun a ->
      { cpu with r = { r with a }; uop = U_FETCH }, sys
    )
  | U_MOV_C_M2_W ->
    System.write sys (0xFF00 + r.c) r.a |> Option.map (fun sys ->
      { cpu with uop = U_FETCH }, sys
    )

  | U_MOV_D8_M2 rw ->
    read_imm cpu sys (fun cpu a ->
      let uop =
        match rw with
        | R -> U_MOV_D8_M3_R a
        | W -> U_MOV_D8_M3_W a
      in
      { cpu with uop }
    )
  | U_MOV_D8_M3_W a ->
    mem_writer cpu sys (0xFF00 + a) r.a (fun _ ->
      { cpu with uop = U_FETCH }
    )
  | U_MOV_D8_M3_R a ->
    mem_reader cpu sys (0xFF00 + a) (fun a ->
      { cpu with r = { r with a }; uop = U_FETCH }
    )
  | U_CALL_M2 cc ->
    read_imm cpu sys (fun cpu lo -> { cpu with uop = U_CALL_M3(cc, lo) })
  | U_CALL_M3(cc, lo) ->
    read_imm cpu sys (fun cpu hi ->
      { cpu with uop = if is_taken cc f then U_CALL_M4(lo, hi) else U_FETCH }
    )
  | U_CALL_M4(lo, hi) ->
    let sp = (r.sp - 1) land 0xFFFF in
    Some ({ cpu with r = { r with sp }; uop = U_CALL_M5(lo, hi) }, sys)
  | U_CALL_M5(lo, hi) ->
    let pc_hi = (r.pc lsr 8) land 0xFF in
    let pc_lo = (r.pc lsr 0) land 0xFF in
    mem_writer cpu sys r.sp pc_hi (fun _ ->
      let sp = (r.sp - 1) land 0xFFFF in
      let pc = (hi lsl 8) lor pc_lo in
      { cpu with r = { r with pc; sp}; uop = U_CALL_M6 lo }
    )
  | U_CALL_M6 lo ->
    let pc_hi = (r.pc lsr 8) land 0xFF in
    let pc_lo = (r.pc lsr 0) land 0xFF in
    mem_writer cpu sys r.sp pc_lo (fun _ ->
      let pc = (pc_hi lsl 8) lor lo in
      { cpu with r = { r with pc }; uop = U_FETCH }
    )

  | U_JP_D16_M2 cc ->
    read_imm cpu sys (fun cpu lo -> { cpu with uop = U_JP_D16_M3(cc, lo) })
  | U_JP_D16_M3(cc, lo) ->
    read_imm cpu sys (fun cpu hi ->
      { cpu with uop = if is_taken cc f then U_JP_D16_M4(lo, hi) else U_FETCH }
    )
  | U_JP_D16_M4(lo, hi) ->
    Some ({ cpu with r = { r with pc = (hi lsl 8) lor lo }; uop = U_FETCH }, sys)

  | U_PUSH_M2 dd ->
    let sp = (r.sp - 1) land 0xFFFF in
    Some ({ cpu with r = { r with sp }; uop = U_PUSH_M3 dd }, sys)
  | U_PUSH_M3 dd ->
    let v = match dd with
      | R16P_BC -> r.b
      | R16P_DE -> r.d
      | R16P_HL -> r.h
      | R16P_AF -> r.a
    in
    mem_writer cpu sys r.sp v (fun _ ->
      { cpu with
        r = { r with sp = (r.sp - 1) land 0xFFFF };
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
    mem_writer cpu sys r.sp v (fun _ ->
      { cpu with uop = U_FETCH }
    )
  | U_POP_M2 dd ->
    mem_reader cpu sys r.sp (fun lo ->
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
    mem_reader cpu sys r.sp (fun hi ->
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
    Some ({ cpu with uop = if taken then U_RET_M3 false else U_FETCH }, sys)
  | U_RET_M3 ime ->
    mem_reader cpu sys r.sp (fun lo ->
      { cpu with
        r = { r with sp = (r.sp + 1) land 0xFFFF };
        ime = if ime then true else cpu.ime;
        uop = U_RET_M4 lo;
      }
    )
  | U_RET_M4 lo ->
    mem_reader cpu sys r.sp (fun hi ->
      { cpu with
        r = { r with sp = (r.sp + 1) land 0xFFFF };
        uop = U_RET_M5(lo, hi);
      }
    )
  | U_RET_M5(lo, hi) ->
    let pc = (hi lsl 8) lor lo in
    Some ({ cpu with r = { r with pc }; uop = U_FETCH }, sys)

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
    Some ({ cpu with r; f; uop = U_FETCH }, sys)

  | U_ALU8_D8_M2(op, dst) ->
    read_imm cpu sys (fun cpu v1 ->
      let v0 = get_reg_8 cpu.r dst in
      let v, f = execute_alu8 op cpu.f v0 v1 in
      { cpu with r = set_reg_8 cpu.r dst v; f; uop = U_FETCH }
    )
  | U_ALU8_HL_R8_M2(op, dst) ->
    let v0 = get_reg_8 r dst in
    mem_reader cpu sys ((r.h lsl 8) lor r.l) (fun v1 ->
      let v, f = execute_alu8 op cpu.f v0 v1 in
      let r = set_reg_8 r dst v in
      { cpu with r; f; uop = U_FETCH }
    )
  | U_ALU8_BIT_M2 n ->
    mem_reader cpu sys ((r.h lsl 8) lor r.l) (fun v ->
      { cpu with
        f = { f with z = v land (1 lsl n) = 0; n = false; h = true };
        uop = U_FETCH
      }
    )
  | U_MOV_D16_R8_M2(src, rw) ->
    read_imm cpu sys (fun cpu lo ->
      { cpu with uop = U_MOV_D16_R8_M3(src, lo, rw) }
    )
  | U_MOV_D16_R8_M3(src, lo, rw) ->
    read_imm cpu sys (fun cpu hi ->
      { cpu with
        uop = match rw with
          | R -> U_MOV_D16_R8_M4_R(src, lo, hi)
          | W -> U_MOV_D16_R8_M4_W(src, lo, hi)
      }
    )
  | U_MOV_D16_R8_M4_R(reg, lo, hi) ->
    let addr = (hi lsl 8) lor lo in
    mem_reader cpu sys addr (fun v ->
      { cpu with r = set_reg_8 r reg v; uop = U_FETCH }
    )
  | U_MOV_D16_R8_M4_W(reg, lo, hi) ->
    let addr = (hi lsl 8) lor lo in
    let v = get_reg_8 r reg in
    mem_writer cpu sys addr v (fun _ ->
      { cpu with uop = U_FETCH }
    )

  | U_ALU8_HL_M2(op) ->
    mem_reader cpu sys ((r.h lsl 8) lor r.l) (fun v ->
      let v, f = execute_alu8 op f v v in
      { cpu with f; uop = U_ALU8_HL_M3 v }
    )
  | U_ALU8_HL_M3(v) ->
    mem_writer cpu sys ((r.h lsl 8) lor r.l) v (fun _ ->
      { cpu with uop = U_FETCH }
    )

  | U_ST_D16_R16_M2 reg ->
    read_imm cpu sys (fun cpu lo -> { cpu with uop = U_ST_D16_R12_M3(reg, lo) })
  | U_ST_D16_R12_M3(reg, lo) ->
    read_imm cpu sys (fun cpu hi -> { cpu with uop = U_ST_D16_R12_M4(reg, lo, hi) })
  | U_ST_D16_R12_M4(reg, lo, hi) ->
    let addr = (((hi lsl 8) lor lo) + 0) land 0xFFFF in
    let v = match reg with
      | R16S_BC -> r.c
      | R16S_DE -> r.e
      | R16S_HL -> r.l
      | R16S_SP -> ((r.sp land 0x00FF) lsr 0)
    in
    mem_writer cpu sys addr v (fun _ ->
      { cpu with uop = U_ST_D16_R12_M5(reg, lo, hi) }
    )
  | U_ST_D16_R12_M5(reg, lo, hi) ->
    let addr = (((hi lsl 8) lor lo) + 1) land 0xFFFF in
    let v = match reg with
      | R16S_BC -> r.b
      | R16S_DE -> r.d
      | R16S_HL -> r.h
      | R16S_SP -> ((r.sp land 0xFF00) lsr 8)
    in
    mem_writer cpu sys addr v (fun _ -> { cpu with uop = U_FETCH })
  | U_RST_M2 h ->
    let sp = (r.sp - 1) land 0xFFFF in
    Some ({ cpu with r = { r with sp }; uop = U_RST_M3 h }, sys)
  | U_RST_M3 h ->
    mem_writer cpu sys r.sp ((r.pc land 0xFF00) lsr 8) (fun _ ->
      { cpu with r =
        { r with
          sp = (r.sp - 1) land 0xFFFF;
          pc = r.pc land 0xFF
        };
        uop = U_RST_M4 h
      }
    )
  | U_RST_M4 h ->
    mem_writer cpu sys r.sp ((r.pc land 0x00FF) lsr 0) (fun _ ->
      { cpu with r = { r with pc = h }; uop = U_FETCH }
    )

  | U_ST_HL_D8_M2 reg ->
    read_imm cpu sys (fun cpu v -> { cpu with uop = U_ST_HL_D8_M3(reg, v) })
  | U_ST_HL_D8_M3(reg, v) ->
    mem_writer cpu sys (get_reg_16 cpu.r reg) v (fun _ ->
      { cpu with uop = U_FETCH }
    )

  | U_ADD_SP_D8_M2 reg ->
    read_imm cpu sys (fun cpu v -> { cpu with uop = U_ADD_SP_D8_M3(reg, v) })

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
    let r, uop =
      match reg with
      | R16S_BC -> { r with b = hi; c = lo }, U_FETCH
      | R16S_DE -> { r with d = hi; e = lo }, U_FETCH
      | R16S_HL -> { r with h = hi; l = lo }, U_FETCH
      | R16S_SP -> { r with sp = v }, U_ADD_SP_D8_M4
    in
    Some ({ cpu with r; f; uop }, sys)
  | U_ADD_SP_D8_M4 ->
    Some ({ cpu with uop = U_FETCH }, sys)

  | U_INT_M2 addr ->
    let sp = (r.sp - 1) land 0xFFFF in
    Some ({ cpu with r = { r with sp }; uop = U_INT_M3 addr; }, sys)
  | U_INT_M3 addr ->
    Some ({ cpu with uop = U_INT_M4 addr }, sys)
  | U_INT_M4 addr ->
    mem_writer cpu sys r.sp ((r.pc land 0xFF00) lsr 8) (fun _ ->
      { cpu with
        r = { r with sp = (r.sp - 1) land 0xFFFF; pc = r.pc land 0x00FF; };
        uop = U_INT_M5 addr
      }
    )
  | U_INT_M5 addr ->
    mem_writer cpu sys r.sp ((r.pc land 0x00FF) lsr 0) (fun _ ->
      { cpu with r = { r with pc = addr; }; uop = U_FETCH }
    )
