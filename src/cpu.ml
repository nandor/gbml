(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (R8_C) 2020 Nandor Licker. All rights reserved. *)

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

let get_reg_8 r reg =
  match reg with
  | R8_B -> r.b
  | R8_C -> r.c
  | R8_D -> r.d
  | R8_E -> r.e
  | R8_H -> r.h
  | R8_L -> r.l
  | R8_A -> r.a

let set_reg_8 r reg v =
  match reg with
  | R8_B -> { r with b = v }
  | R8_C -> { r with c = v }
  | R8_D -> { r with d = v }
  | R8_E -> { r with e = v }
  | R8_H -> { r with h = v }
  | R8_L -> { r with l = v }
  | R8_A -> { r with a = v }

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
  | Add
  | Adc
  | Sub
  | Sbc
  | And
  | Xor
  | Or
  | Cp
  | Mov
  | Inc
  | Dec
  | Rlc
  | Rrc
  | Rl
  | Rr
  | Sla
  | Sra
  | Swap
  | Srl
  | Bit of int
  | Res of int
  | Set of int

type alu16_op =
  | Inc
  | Dec

type rw =
  | R
  | W

type uop =
  | U_FETCH
  | U_CB
  (* ld r16, d16 *)
  | U_LD_R16_D16_M2 of reg_16s
  | U_LD_R16_D16_M3 of reg_16s
  (* ld r8, r16 *)
  | U_LD_R8_R16_M2 of reg_8 * reg_16
  (* jr *)
  | U_JR_M2 of cc
  | U_JR_M3 of u8
  (* ld (HL±), r8 *)
  | U_ST_HL_M2 of dir * reg_8
  (* ld r8, d8 *)
  | U_LD_R8_D8_M2 of reg_8
  (* ld (c), a *)
  | U_ST_C_M2
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
  | U_ALU8_M2 of alu8_op * reg_8
  (* alu8 r8, (HL) *)
  | U_ALU8_HL_M2 of alu8_op * reg_8
  (* ld (d16), r8 *)
  | U_MOV_D16_R8_M2 of reg_8 * rw
  | U_MOV_D16_R8_M3 of reg_8 * u8 * rw
  | U_MOV_D16_R8_M4_R of reg_8 * u8 * u8
  | U_MOV_D16_R8_M4_W of reg_8 * u8 * u8

type t =
  { r: r
  ; f: f
  ; s: System.t
  ; ei: bool
  ; uop: uop
  }

let execute_alu8 op f op0 op1 =
  let { z; n; h; c } = f in
  match op with
  | Add ->
    let v = op0 + op1 in
    let z = (v land 0xFF) = 0 in
    let h = (v land 0xF) < (op1 land 0xF) in
    let c = v > 0xFF in
    v land 0xFF, { z; n = false; h; c }

  | Adc -> failwith "not implemented: Adc"

  | Sub ->
    let v = op1 - op0 in
    let z = (v land 0xFF) = 0 in
    let h = (op0 land 0x0F) < (v land 0xF) in
    let c = v < 0x00 in
    v land 0xFF, { z; n = true; h; c }

  | Sbc -> failwith "not implemented: Sbc"

  | And -> failwith "not implemented: And"

  | Xor ->
    let v = op0 lxor op1 in
    v, { z = v = 0; n = false; h = false; c = false }

  | Or -> failwith "not implemented: Or"

  | Cp ->
    let v = op0 - op1 in
    op0, { z = v = 0; n = true; h = (v land 0x0F) > (op0 land 0x0F); c = v < 0 }

  | Bit n ->
    let v = op1 in
    v, { z = v land (1 lsl n) = 0; n = false; h = true; c }

  | Mov ->
    let v = op1 in
    v, f

  | Rlc -> failwith "not implemented: Rlc"
  | Rrc -> failwith "not implemented: Rrc"

  | Rl ->
    let l = if c then 1 else 0 in
    let c = (op1 land 0x80) <> 0 in
    let v = ((op1 lsl 1) lor l) land 0xFF in
    v, { z = v = 0; n = false; h = false; c }

  | Rr -> failwith "not implemented: Rr"
  | Sla -> failwith "not implemented: Sla"
  | Sra -> failwith "not implemented: Sra"
  | Swap -> failwith "not implemented: Swap"
  | Srl -> failwith "not implemented: Srl"
  | Res _ -> failwith "not implemented: Res"
  | Set _ -> failwith "not implemented: Set"

  | Inc ->
    let v = (op1 + 1) land 0xFF in
    v, { z = v = 0; n = false; h = (v land 0x0F) = 0; c }

  | Dec ->
    let v = (op1 - 1) land 0xFF in
    v, { z = v = 0; n = true; h = (v land 0x0F) = 0; c }

let decode_alu8 cpu op op0 op1 =
  let v0 = get_reg_8 cpu.r op0 in
  let v1 = get_reg_8 cpu.r op1 in
  let v, f = execute_alu8 op cpu.f v0 v1 in
  let pc = (cpu.r.pc + 1) land 0xFFFF in
  let r = { (set_reg_8 cpu.r op0 v) with pc } in
  Some { cpu with r; f; uop = U_FETCH }

let decode_alu8_d8 cpu op dst =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_ALU8_M2(op, dst)
  }

let decode_alu8_hl cpu op dst =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_ALU8_HL_M2(op, dst)
  }

let decode_alu16 cpu op dst src =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_ALU16_M2(op, dst, src)
  }

let decode_ld_r8_d8 cpu dst =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_LD_R8_D8_M2 dst
  }

let decode_ld_r16_d16 cpu reg =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_LD_R16_D16_M2 reg
  }

let decode_ld_r8_r16 cpu dst src =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_LD_R8_R16_M2(dst, src)
  }

let decode_st_hl cpu dir reg =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_ST_HL_M2(dir, reg)
  }

let decode_jr cpu cc =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_JR_M2 cc
  }

let decode_ret cpu cc ei =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = match cc with
      | CC_A  -> U_RET_M3 ei
      | CC_Z  -> U_RET_M2 RCC_Z
      | CC_NZ -> U_RET_M2 RCC_NZ
      | CC_C  -> U_RET_M2 RCC_C
      | CC_NC -> U_RET_M2 RCC_NC
  }

let decode_push cpu reg =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_PUSH_M2 reg
  }

let decode_pop cpu reg =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_POP_M2 reg
  }

let decode_call cpu cc =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_CALL_M2 cc
  }

let decode_movh cpu rw =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_MOV_D8_M2 rw
  }

let decode_st_c cpu =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_ST_C_M2
  }

let decode_mov_d16_r8 cpu reg rw =
  Some { cpu with
    r = { cpu.r with pc = cpu.r.pc + 1 };
    uop = U_MOV_D16_R8_M2(reg, rw)
  }

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
  ; ei = false
  ; uop = U_FETCH
  }

let step cpu =
  let { uop; r; s; f } = cpu in
  match uop with
  | U_FETCH ->
    (match System.read s r.pc with
    | None -> None
    | Some op ->
      (match op with
      | 0x01 -> decode_ld_r16_d16 cpu R16S_BC
      | 0x03 -> decode_alu16      cpu Inc R16S_BC R16S_BC
      | 0x04 -> decode_alu8       cpu Inc R8_B R8_B
      | 0x05 -> decode_alu8       cpu Dec R8_B R8_B
      | 0x06 -> decode_ld_r8_d8   cpu R8_B
      | 0x0A -> decode_ld_r8_r16  cpu R8_A R16_BC
      | 0x0B -> decode_alu16      cpu Dec R16S_BC R16S_BC
      | 0x0C -> decode_alu8       cpu Inc R8_C R8_C
      | 0x0D -> decode_alu8       cpu Dec R8_C R8_C
      | 0x0E -> decode_ld_r8_d8   cpu R8_C
      | 0x11 -> decode_ld_r16_d16 cpu R16S_DE
      | 0x13 -> decode_alu16      cpu Inc R16S_DE R16S_DE
      | 0x14 -> decode_alu8       cpu Inc R8_D R8_D
      | 0x15 -> decode_alu8       cpu Dec R8_D R8_D
      | 0x16 -> decode_ld_r8_d8   cpu R8_D
      | 0x17 -> decode_alu8       cpu Rl R8_A R8_A
      | 0x18 -> decode_jr         cpu CC_A
      | 0x1A -> decode_ld_r8_r16  cpu R8_A R16_DE
      | 0x1B -> decode_alu16      cpu Dec R16S_DE R16S_DE
      | 0x1C -> decode_alu8       cpu Inc R8_E R8_E
      | 0x1D -> decode_alu8       cpu Dec R8_E R8_E
      | 0x1E -> decode_ld_r8_d8   cpu R8_E
      | 0x20 -> decode_jr         cpu CC_NZ
      | 0x21 -> decode_ld_r16_d16 cpu R16S_HL
      | 0x23 -> decode_alu16      cpu Inc R16S_HL R16S_HL
      | 0x24 -> decode_alu8       cpu Inc R8_H R8_H
      | 0x25 -> decode_alu8       cpu Dec R8_H R8_H
      | 0x22 -> decode_st_hl      cpu D_Inc R8_A
      | 0x26 -> decode_ld_r8_d8   cpu R8_H
      | 0x28 -> decode_jr         cpu CC_Z
      | 0x2B -> decode_alu16      cpu Dec R16S_HL R16S_HL
      | 0x2C -> decode_alu8       cpu Inc R8_L R8_L
      | 0x2D -> decode_alu8       cpu Dec R8_L R8_L
      | 0x2E -> decode_ld_r8_d8   cpu R8_L
      | 0x30 -> decode_jr         cpu CC_NC
      | 0x31 -> decode_ld_r16_d16 cpu R16S_SP
      | 0x32 -> decode_st_hl      cpu D_Dec R8_A
      | 0x33 -> decode_alu16      cpu Inc R16S_SP R16S_SP
      | 0x38 -> decode_jr         cpu CC_C
      | 0x3B -> decode_alu16      cpu Dec R16S_SP R16S_SP
      | 0x3C -> decode_alu8       cpu Inc R8_A R8_A
      | 0x3D -> decode_alu8       cpu Dec R8_A R8_A
      | 0x3E -> decode_ld_r8_d8   cpu R8_A
      | 0x40 -> decode_alu8       cpu Mov R8_B R8_B
      | 0x41 -> decode_alu8       cpu Mov R8_B R8_C
      | 0x42 -> decode_alu8       cpu Mov R8_B R8_D
      | 0x43 -> decode_alu8       cpu Mov R8_B R8_E
      | 0x44 -> decode_alu8       cpu Mov R8_B R8_H
      | 0x45 -> decode_alu8       cpu Mov R8_B R8_L
      | 0x46 -> decode_alu8_hl    cpu Mov R8_B
      | 0x47 -> decode_alu8       cpu Mov R8_B R8_A
      | 0x48 -> decode_alu8       cpu Mov R8_C R8_B
      | 0x49 -> decode_alu8       cpu Mov R8_C R8_C
      | 0x4A -> decode_alu8       cpu Mov R8_C R8_D
      | 0x4B -> decode_alu8       cpu Mov R8_C R8_E
      | 0x4C -> decode_alu8       cpu Mov R8_C R8_H
      | 0x4D -> decode_alu8       cpu Mov R8_C R8_L
      | 0x4E -> decode_alu8_hl    cpu Mov R8_C
      | 0x4F -> decode_alu8       cpu Mov R8_C R8_A
      | 0x50 -> decode_alu8       cpu Mov R8_D R8_B
      | 0x51 -> decode_alu8       cpu Mov R8_D R8_C
      | 0x52 -> decode_alu8       cpu Mov R8_D R8_D
      | 0x53 -> decode_alu8       cpu Mov R8_D R8_E
      | 0x54 -> decode_alu8       cpu Mov R8_D R8_H
      | 0x55 -> decode_alu8       cpu Mov R8_D R8_L
      | 0x56 -> decode_alu8_hl    cpu Mov R8_D
      | 0x57 -> decode_alu8       cpu Mov R8_D R8_A
      | 0x58 -> decode_alu8       cpu Mov R8_E R8_B
      | 0x59 -> decode_alu8       cpu Mov R8_E R8_C
      | 0x5A -> decode_alu8       cpu Mov R8_E R8_D
      | 0x5B -> decode_alu8       cpu Mov R8_E R8_E
      | 0x5C -> decode_alu8       cpu Mov R8_E R8_H
      | 0x5D -> decode_alu8       cpu Mov R8_E R8_L
      | 0x5E -> decode_alu8_hl    cpu Mov R8_E
      | 0x5F -> decode_alu8       cpu Mov R8_E R8_A
      | 0x60 -> decode_alu8       cpu Mov R8_H R8_B
      | 0x61 -> decode_alu8       cpu Mov R8_H R8_C
      | 0x62 -> decode_alu8       cpu Mov R8_H R8_D
      | 0x63 -> decode_alu8       cpu Mov R8_H R8_E
      | 0x64 -> decode_alu8       cpu Mov R8_H R8_H
      | 0x65 -> decode_alu8       cpu Mov R8_H R8_L
      | 0x66 -> decode_alu8_hl    cpu Mov R8_H
      | 0x67 -> decode_alu8       cpu Mov R8_H R8_A
      | 0x68 -> decode_alu8       cpu Mov R8_L R8_B
      | 0x69 -> decode_alu8       cpu Mov R8_L R8_C
      | 0x6A -> decode_alu8       cpu Mov R8_L R8_D
      | 0x6B -> decode_alu8       cpu Mov R8_L R8_E
      | 0x6C -> decode_alu8       cpu Mov R8_L R8_H
      | 0x6D -> decode_alu8       cpu Mov R8_L R8_L
      | 0x6E -> decode_alu8_hl    cpu Mov R8_L
      | 0x6F -> decode_alu8       cpu Mov R8_L R8_A
      | 0x70 -> decode_st_hl      cpu D_Nop R8_B
      | 0x71 -> decode_st_hl      cpu D_Nop R8_C
      | 0x72 -> decode_st_hl      cpu D_Nop R8_D
      | 0x73 -> decode_st_hl      cpu D_Nop R8_E
      | 0x74 -> decode_st_hl      cpu D_Nop R8_H
      | 0x75 -> decode_st_hl      cpu D_Nop R8_L
      | 0x76 -> failwith "not implemented: HALT"
      | 0x77 -> decode_st_hl      cpu D_Nop R8_A
      | 0x78 -> decode_alu8       cpu Mov R8_A R8_B
      | 0x79 -> decode_alu8       cpu Mov R8_A R8_C
      | 0x7A -> decode_alu8       cpu Mov R8_A R8_D
      | 0x7B -> decode_alu8       cpu Mov R8_A R8_E
      | 0x7C -> decode_alu8       cpu Mov R8_A R8_H
      | 0x7D -> decode_alu8       cpu Mov R8_A R8_L
      | 0x7E -> decode_alu8_hl    cpu Mov R8_A
      | 0x7F -> decode_alu8       cpu Mov R8_A R8_A
      | 0x80 -> decode_alu8       cpu Add R8_A R8_B
      | 0x81 -> decode_alu8       cpu Add R8_A R8_C
      | 0x82 -> decode_alu8       cpu Add R8_A R8_D
      | 0x83 -> decode_alu8       cpu Add R8_A R8_E
      | 0x84 -> decode_alu8       cpu Add R8_A R8_H
      | 0x85 -> decode_alu8       cpu Add R8_A R8_L
      | 0x86 -> decode_alu8_hl    cpu Add R8_A
      | 0x87 -> decode_alu8       cpu Add R8_A R8_A
      | 0x88 -> decode_alu8       cpu Adc R8_A R8_B
      | 0x89 -> decode_alu8       cpu Adc R8_A R8_C
      | 0x8A -> decode_alu8       cpu Adc R8_A R8_D
      | 0x8B -> decode_alu8       cpu Adc R8_A R8_E
      | 0x8C -> decode_alu8       cpu Adc R8_A R8_H
      | 0x8D -> decode_alu8       cpu Adc R8_A R8_L
      | 0x8E -> decode_alu8_hl    cpu Adc R8_A
      | 0x8F -> decode_alu8       cpu Adc R8_A R8_A
      | 0x90 -> decode_alu8       cpu Sub R8_A R8_B
      | 0x91 -> decode_alu8       cpu Sub R8_A R8_C
      | 0x92 -> decode_alu8       cpu Sub R8_A R8_D
      | 0x93 -> decode_alu8       cpu Sub R8_A R8_E
      | 0x94 -> decode_alu8       cpu Sub R8_A R8_H
      | 0x95 -> decode_alu8       cpu Sub R8_A R8_L
      | 0x96 -> decode_alu8_hl    cpu Sub R8_A
      | 0x97 -> decode_alu8       cpu Sub R8_A R8_A
      | 0x98 -> decode_alu8       cpu Sbc R8_A R8_B
      | 0x99 -> decode_alu8       cpu Sbc R8_A R8_C
      | 0x9A -> decode_alu8       cpu Sbc R8_A R8_D
      | 0x9B -> decode_alu8       cpu Sbc R8_A R8_E
      | 0x9C -> decode_alu8       cpu Sbc R8_A R8_H
      | 0x9D -> decode_alu8       cpu Sbc R8_A R8_L
      | 0x9E -> decode_alu8_hl    cpu Sbc R8_A
      | 0x9F -> decode_alu8       cpu Sbc R8_A R8_A
      | 0xA0 -> decode_alu8       cpu And R8_A R8_B
      | 0xA1 -> decode_alu8       cpu And R8_A R8_C
      | 0xA2 -> decode_alu8       cpu And R8_A R8_D
      | 0xA3 -> decode_alu8       cpu And R8_A R8_E
      | 0xA4 -> decode_alu8       cpu And R8_A R8_H
      | 0xA5 -> decode_alu8       cpu And R8_A R8_L
      | 0xA6 -> decode_alu8_hl    cpu And R8_A
      | 0xA7 -> decode_alu8       cpu And R8_A R8_A
      | 0xA8 -> decode_alu8       cpu Xor R8_A R8_B
      | 0xA9 -> decode_alu8       cpu Xor R8_A R8_C
      | 0xAA -> decode_alu8       cpu Xor R8_A R8_D
      | 0xAB -> decode_alu8       cpu Xor R8_A R8_E
      | 0xAC -> decode_alu8       cpu Xor R8_A R8_H
      | 0xAD -> decode_alu8       cpu Xor R8_A R8_L
      | 0xAE -> decode_alu8_hl    cpu Xor R8_A
      | 0xAF -> decode_alu8       cpu Xor R8_A R8_A
      | 0xB0 -> decode_alu8       cpu Or  R8_A R8_B
      | 0xB1 -> decode_alu8       cpu Or  R8_A R8_C
      | 0xB2 -> decode_alu8       cpu Or  R8_A R8_D
      | 0xB3 -> decode_alu8       cpu Or  R8_A R8_E
      | 0xB4 -> decode_alu8       cpu Or  R8_A R8_H
      | 0xB5 -> decode_alu8       cpu Or  R8_A R8_L
      | 0xB6 -> decode_alu8_hl    cpu Or  R8_A
      | 0xB7 -> decode_alu8       cpu Or  R8_A R8_A
      | 0xB8 -> decode_alu8       cpu Cp  R8_A R8_B
      | 0xB9 -> decode_alu8       cpu Cp  R8_A R8_C
      | 0xBA -> decode_alu8       cpu Cp  R8_A R8_D
      | 0xBB -> decode_alu8       cpu Cp  R8_A R8_E
      | 0xBC -> decode_alu8       cpu Cp  R8_A R8_H
      | 0xBD -> decode_alu8       cpu Cp  R8_A R8_L
      | 0xBE -> decode_alu8_hl    cpu Cp  R8_A
      | 0xBF -> decode_alu8       cpu Cp  R8_A R8_A
      | 0xC0 -> decode_ret        cpu CC_NZ false
      | 0xC1 -> decode_pop        cpu R16P_BC
      | 0xC4 -> decode_call       cpu CC_NZ
      | 0xC5 -> decode_push       cpu R16P_BC
      | 0xC6 -> decode_alu8_d8    cpu Add R8_A
      | 0xC8 -> decode_ret        cpu CC_Z false
      | 0xC9 -> decode_ret        cpu CC_A false
      | 0xCB -> Some { cpu with r = { r with pc = r.pc + 1 }; uop = U_CB }
      | 0xCC -> decode_call       cpu CC_Z
      | 0xCD -> decode_call       cpu CC_A
      | 0xCE -> decode_alu8_d8    cpu Adc R8_A
      | 0xD0 -> decode_ret        cpu CC_NC false
      | 0xD1 -> decode_pop        cpu R16P_DE
      | 0xD4 -> decode_call       cpu CC_NC
      | 0xD5 -> decode_push       cpu R16P_DE
      | 0xD6 -> decode_alu8_d8    cpu Sub R8_A
      | 0xD8 -> decode_ret        cpu CC_C false
      | 0xD9 -> decode_ret        cpu CC_A true
      | 0xDC -> decode_call       cpu CC_C
      | 0xDE -> decode_alu8_d8    cpu Sbc R8_A
      | 0xE0 -> decode_movh       cpu W
      | 0xE1 -> decode_pop        cpu R16P_HL
      | 0xE2 -> decode_st_c       cpu
      | 0xE5 -> decode_push       cpu R16P_HL
      | 0xE6 -> decode_alu8_d8    cpu And R8_A
      | 0xEA -> decode_mov_d16_r8 cpu R8_A W
      | 0xEE -> decode_alu8_d8    cpu Xor R8_A
      | 0xF0 -> decode_movh       cpu R
      | 0xF1 -> decode_pop        cpu R16P_AF
      | 0xF5 -> decode_push       cpu R16P_AF
      | 0xF6 -> decode_alu8_d8    cpu Or R8_A
      | 0xFA -> decode_mov_d16_r8 cpu R8_A R
      | 0xFE -> decode_alu8_d8    cpu Cp R8_A
      | op ->
        Printf.eprintf "invalid opcode: %x\n" op;
        exit (-1)
      )
    )
  | U_CB ->
    (match System.read s r.pc with
    | None -> None
    | Some op ->
      (match op with
      | 0x10 -> decode_alu8 cpu Rl R8_B R8_B
      | 0x11 -> decode_alu8 cpu Rl R8_C R8_C
      | 0x12 -> decode_alu8 cpu Rl R8_D R8_D
      | 0x13 -> decode_alu8 cpu Rl R8_E R8_E
      | 0x14 -> decode_alu8 cpu Rl R8_H R8_H
      | 0x15 -> decode_alu8 cpu Rl R8_L R8_L
      | 0x16 -> failwith "not implemented: 0x16"
      | 0x17 -> decode_alu8 cpu Rl R8_A R8_A
      | 0x40 -> decode_alu8 cpu (Bit 0) R8_B R8_B
      | 0x44 -> decode_alu8 cpu (Bit 0) R8_H R8_H
      | 0x48 -> decode_alu8 cpu (Bit 1) R8_B R8_B
      | 0x4C -> decode_alu8 cpu (Bit 1) R8_H R8_H
      | 0x50 -> decode_alu8 cpu (Bit 2) R8_B R8_B
      | 0x54 -> decode_alu8 cpu (Bit 2) R8_H R8_H
      | 0x58 -> decode_alu8 cpu (Bit 3) R8_B R8_B
      | 0x5C -> decode_alu8 cpu (Bit 3) R8_H R8_H
      | 0x60 -> decode_alu8 cpu (Bit 4) R8_B R8_B
      | 0x64 -> decode_alu8 cpu (Bit 4) R8_H R8_H
      | 0x68 -> decode_alu8 cpu (Bit 5) R8_B R8_B
      | 0x6C -> decode_alu8 cpu (Bit 5) R8_H R8_H
      | 0x70 -> decode_alu8 cpu (Bit 6) R8_B R8_B
      | 0x74 -> decode_alu8 cpu (Bit 6) R8_H R8_H
      | 0x78 -> decode_alu8 cpu (Bit 7) R8_B R8_B
      | 0x7C -> decode_alu8 cpu (Bit 7) R8_H R8_H
      | op ->
        Printf.eprintf "invalid CB opcode: %x\n" op;
        exit (-1)
      )
    )
  | U_LD_R8_R16_M2(dst, src) ->
      let addr =
        match src with
        | R16_BC -> (r.b lsl 8) lor r.c
        | R16_DE -> (r.d lsl 8) lor r.e
        | R16_HL -> (r.h lsl 8) lor r.l
      in
      System.read s addr |> Option.map (fun v ->
        let r =
          match dst with
          | R8_B -> { r with b = v }
          | R8_C -> { r with c = v }
          | R8_D -> { r with d = v }
          | R8_E -> { r with e = v }
          | R8_H -> { r with h = v }
          | R8_L -> { r with l = v }
          | R8_A -> { r with a = v }
        in
        { cpu with r; uop = U_FETCH }
      )
  | U_LD_R16_D16_M2 reg ->
    System.read s r.pc |> Option.map (fun v ->
      let pc = r.pc + 1 in
      let r = match reg with
        | R16S_BC -> { r with pc; c = v }
        | R16S_DE -> { r with pc; e = v }
        | R16S_HL -> { r with pc; l = v }
        | R16S_SP -> { r with pc; sp = (r.sp land 0xFF00) lor v }
      in
      { cpu with r; uop = U_LD_R16_D16_M3 reg }
    )
  | U_LD_R16_D16_M3 reg ->
    System.read s r.pc |> Option.map (fun v ->
      let pc = r.pc + 1 in
      let r = match reg with
        | R16S_BC -> { r with pc; b = v }
        | R16S_DE -> { r with pc; d = v }
        | R16S_HL -> { r with pc; h = v }
        | R16S_SP -> { r with pc; sp = (r.sp land 0x00FF) lor (v lsl 8) }
      in
      { cpu with r; uop = U_FETCH }
    )
  | U_JR_M2 cc ->
    System.read s r.pc |> Option.map (fun off ->
      let taken = match cc with
        | CC_Z -> f.z
        | CC_NZ -> not f.z
        | CC_C -> f.c
        | CC_NC -> not f.c
        | CC_A -> true
      in
      { cpu with
        r = { r with pc = r.pc + 1};
        uop = if taken then U_JR_M3 off else U_FETCH
      }
    )
  | U_JR_M3 off ->
    Some({
      cpu with
        r = { r with pc = (r.pc + i8_of_u8 off) land 0xFF};
        uop = U_FETCH
    })
  | U_ST_HL_M2(op, d) ->
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
  | U_LD_R8_D8_M2 d ->
    System.read s r.pc |> Option.map (fun v ->
      let pc = r.pc + 1 in
      { cpu with
        uop = U_FETCH;
        r = { (set_reg_8 r d v) with pc }
      }
    )
  | U_ST_C_M2 ->
    System.write s (0xFF00 + r.c) r.a |> Option.map (fun s ->
      { cpu with s; uop = U_FETCH }
    )
  | U_MOV_D8_M2 rw ->
    System.read s r.pc |> Option.map (fun a ->
      { cpu with
        r = { r with pc = r.pc + 1 };
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
    System.read s r.pc |> Option.map (fun lo ->
      { cpu with r = {r with pc = r.pc + 1}; uop = U_CALL_M3(cc, lo) }
    )
  | U_CALL_M3(cc, lo) ->
    System.read s r.pc |> Option.map (fun hi ->
      let taken = match cc with
        | CC_Z -> f.z
        | CC_NZ -> not f.z
        | CC_C -> f.c
        | CC_NC -> not f.c
        | CC_A -> true
      in
      { cpu with
        r = { r with pc = r.pc + 1};
        uop = if taken then U_CALL_M4(lo, hi) else U_FETCH
      }
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
      | R16P_AF -> failwith "not implemented"
    in
    System.write s r.sp v |> Option.map (fun s ->
      { cpu with s; uop = U_FETCH }
    )
  | U_POP_M2 dd ->
    System.read s r.sp |> Option.map (fun lo ->
      let r = match dd with
        | R16P_BC -> { r with c = lo }
        | R16P_DE -> { r with e = lo }
        | R16P_HL -> { r with l = lo }
        | R16P_AF -> failwith "not implemented"
      in
      { cpu with
        r = { r with sp = (r.sp + 1) land 0xFFFF };
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
  | U_RET_M3 ei ->
    System.read s r.sp |> Option.map (fun lo ->
      { cpu with
        r = { r with sp = (r.sp + 1) land 0xFFFF };
        ei = if ei then true else cpu.ei;
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
  | U_ALU16_M2(op, src, dst) ->
    let v =
      match src with
      | R16S_BC -> (r.b lsl 8) lor r.c
      | R16S_DE -> (r.d lsl 8) lor r.e
      | R16S_HL -> (r.h lsl 8) lor r.l
      | R16S_SP -> r.sp
    in
    let v = match op with
      | Inc -> (v + 1) land 0xFFFF
      | Dec -> (v - 1) land 0xFFFF
    in
    let hi = (v land 0xFF00) lsr 8 in
    let lo = (v land 0x00FF) lsr 0 in
    let r = match dst with
      | R16S_BC -> { r with b = hi; c = lo }
      | R16S_DE -> { r with d = hi; e = lo }
      | R16S_HL -> { r with h = hi; l = lo }
      | R16S_SP -> { r with sp = v }
    in
    Some { cpu with r; uop = U_FETCH }
  | U_ALU8_M2(op, dst) ->
    let v0 = get_reg_8 r dst in
    System.read s r.pc |> Option.map (fun v1 ->
      let v, f = execute_alu8 op cpu.f v0 v1 in
      let pc = (cpu.r.pc + 1) land 0xFFFF in
      let r = { (set_reg_8 r dst v) with pc } in
      { cpu with r; f; uop = U_FETCH }
    )
  | U_ALU8_HL_M2(op, dst) ->
    let v0 = get_reg_8 r dst in
    let hl = (r.h lsl 8) lor r.l in
    System.read s hl |> Option.map (fun v1 ->
      let v, f = execute_alu8 op cpu.f v0 v1 in
      let r = set_reg_8 r dst v in
      { cpu with r; f; uop = U_FETCH }
    )
  | U_MOV_D16_R8_M2(src, rw) ->
    System.read s r.pc |> Option.map (fun lo ->
      let pc = (cpu.r.pc + 1) land 0xFFFF in
      { cpu with r = { r with pc }; uop = U_MOV_D16_R8_M3(src, lo, rw) }
    )
  | U_MOV_D16_R8_M3(src, lo, rw) ->
    System.read s r.pc |> Option.map (fun hi ->
      let pc = (cpu.r.pc + 1) land 0xFFFF in
      { cpu with
        r = { r with pc };
        uop = match rw with
          | R -> U_MOV_D16_R8_M4_R(src, lo, hi)
          | W -> U_MOV_D16_R8_M4_W(src, lo, hi)
      }
    )
  | U_MOV_D16_R8_M4_R(src, lo, hi) ->
    failwith "U_MOV_D16_R8_M4_R"
  | U_MOV_D16_R8_M4_W(src, lo, hi) ->
    let addr = (hi lsl 8) lor lo in
    let v = get_reg_8 r src in
    System.write s addr v |> Option.map (fun s ->
      { cpu with s; uop = U_FETCH }
    )

let tick cpu =
  match step cpu with
  | None -> None
  | Some cpu ->
    match System.tick cpu.s with
    | None -> None
    | Some (i, s) ->
      Some { cpu with s }
