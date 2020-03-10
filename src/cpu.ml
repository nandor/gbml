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

(* ALU instructions *)
module U_ALU = struct
  type arg = B | C | D | E | H | L | A

  type op =
    | Add
    | Adc
    | Sub
    | Sbc
    | And
    | Xor
    | Or
    | Cp
    | Bit of int
    | Mov
    | Inc
    | Dec
    | Rl

  type t

  let init op d s r m f =
    let { z; n; h; c } = f in
    let v =
      match s with
      | B -> r.b
      | C -> r.c
      | D -> r.d
      | E -> r.e
      | H -> r.h
      | L -> r.l
      | A -> r.a
    in
    let (v', f) =
      match op with
      | Add -> failwith "not implemented"
      | Adc -> failwith "not implemented"
      | Sub -> failwith "not implemented"
      | Sbc -> failwith "not implemented"
      | And -> failwith "not implemented"
      | Xor ->
        let a = r.a lxor v in
        a, { z = a = 0; n = false; h = false; c = false }
      | Or -> failwith "not implemented"
      | Cp -> failwith "not implemented"
      | Bit n ->
        v, { z = v land (1 lsl n) = 0; n = false; h = true; c }
      | Mov ->
        v, f
      | Rl ->
        let l = if c then 1 else 0 in
        let c = (v land 0x80) <> 0 in
        let v = ((v lsl 1) lor l) land 0xFF in
        v, { z = v = 0; n = false; h = false; c }
      | Inc ->
        let v = (v + 1) land 0xFF in
        v, { z = v = 0; n = false; h = (v land 0x0F) = 0; c }
      | Dec ->
        let v = (v - 1) land 0xFF in
        v, { z = v = 0; n = true; h = (v land 0x0F) = 0; c }
    in
    let r =
      match d with
      | B -> { r with b = v' }
      | C -> { r with c = v' }
      | D -> { r with d = v' }
      | E -> { r with e = v' }
      | H -> { r with h = v' }
      | L -> { r with l = v' }
      | A -> { r with a = v' }
    in
    Some({ r with pc = r.pc + 1 }, m, f, None)

  let step r m f _ =
    None
end

(* IDC/DEC r16 *)
module U_ID_R16 = struct
  type op = Inc | Dec

  type dd = BC | DE | HL | SP

  type t =
    | M2 of dd * op

  let init dd op r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some(M2(dd, op)))

  let step r m f t =
    match t with
    | M2(dd, op) ->
      let v =
        match dd with
        | BC -> (r.b lsl 8) lor r.c
        | DE -> (r.d lsl 8) lor r.e
        | HL -> (r.h lsl 8) lor r.l
        | SP -> r.sp
      in
      let v = match op with
        | Inc -> (v + 1) land 0xFFFF
        | Dec -> (v - 1) land 0xFFFF
      in
      let hi = (v land 0xFF00) lsr 8 in
      let lo = (v land 0x00FF) lsr 0 in
      let r = match dd with
        | BC -> { r with b = hi; c = lo }
        | DE -> { r with d = hi; e = lo }
        | HL -> { r with h = hi; l = lo }
        | SP -> { r with sp = v }
      in
      Some(r, m, f, None)
end

(* LD r16, d16 *)
module U_LD_R16_D16 = struct
  type dd = BC | DE | HL | SP

  type t =
    | M2 of dd
    | M3 of dd

  let init dd r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some(M2 dd))

  let step r m f t =
    match t with
    | M2 dd ->
      Memory.read m r.pc |> Option.map (fun v ->
        let pc = r.pc + 1 in
        let r = match dd with
          | BC -> { r with pc; c = v }
          | DE -> { r with pc; e = v }
          | HL -> { r with pc; l = v }
          | SP -> { r with pc; sp = (r.sp land 0xFF00) lor v }
        in
        r, m, f, Some(M3 dd)
      )
    | M3 dd ->
      Memory.read m r.pc |> Option.map (fun v ->
        let pc = r.pc + 1 in
        let r = match dd with
          | BC -> { r with pc; b = v }
          | DE -> { r with pc; d = v }
          | HL -> { r with pc; h = v }
          | SP -> { r with pc; sp = (r.sp land 0x00FF) lor (v lsl 8) }
        in
        r, m, f, None
      )
end

(* LD r8, d8 *)
module U_LD_R8_D8 = struct
  type d = B | C | D | E | H | L | A

  type t =
    M2 of d

  let init d r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some(M2 d))

  let step r m f t =
    match t with
    | M2 d ->
      Memory.read m r.pc |> Option.map (fun v ->
        let pc = r.pc + 1 in
        let r =
          match d with
          | B -> { r with pc; b = v }
          | C -> { r with pc; c = v }
          | D -> { r with pc; d = v }
          | E -> { r with pc; e = v }
          | H -> { r with pc; h = v }
          | L -> { r with pc; l = v }
          | A -> { r with pc; a = v }
        in
        r, m, f, None
      )
end

(* LD r8, (r16) *)
module U_LD_R8_R16 = struct
  type d = B | C | D | E | H | L | A

  type dd = BC | DE | HL

  type t =
    | M2 of d * dd

  let init d dd r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some(M2(d, dd)))

  let step r m f t =
    match t with
    | M2(d, dd) ->
      let addr =
        match dd with
        | BC -> (r.b lsl 8) lor r.c
        | DE -> (r.d lsl 8) lor r.e
        | HL -> (r.h lsl 8) lor r.l
      in
      Memory.read m addr |> Option.map (fun v ->
        let r =
          match d with
          | B -> { r with b = v }
          | C -> { r with c = v }
          | D -> { r with d = v }
          | E -> { r with e = v }
          | H -> { r with h = v }
          | L -> { r with l = v }
          | A -> { r with a = v }
        in r, m, f, None
      )
end

(* LD (HL±), r8) *)
module U_ST_HL = struct
  type op = Inc | Dec | Nop

  type d = A | B | C | D | E | H | L

  type t =
    | M2 of op * d

  let init op d r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some(M2(op, d)))

  let step r m f t =
    match t with
    | M2(op, d) ->
      let v =
        match d with
        | A -> r.a
        | B -> r.b
        | C -> r.c
        | D -> r.d
        | E -> r.e
        | H -> r.h
        | L -> r.l
      in
      let addr = (r.h lsl 8) lor r.l in
      Memory.write m addr v |> Option.map (fun m ->
        let hl = (((r.h lsl 8) lor r.l) - 1) land 0xFFFF in
        let h = (hl land 0xFF00) lsr 8 in
        let l = (hl land 0x00FF) lsr 0 in
        { r with h; l }, m, f, None
      )
end

(* LD (C), A *)
module U_ST_C = struct
  type t =
    | M2

  let init r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some M2)

  let step (r:r) m f t =
    match t with
    | M2 ->
      Memory.write m (0xFF00 + r.c) r.a |> Option.map (fun m ->
        r, m, f, None
      )
end

(* LD (a8), A *)
module U_STH = struct
  type t =
    | M2
    | M3 of u8

  let init r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some M2)

  let step r m f t =
    match t with
    | M2 ->
      Memory.read m r.pc |> Option.map (fun a ->
        {r with pc = r.pc + 1}, m, f, Some (M3 a)
      )
    | M3 a ->
      Memory.write m (0xFF00 + a) r.a |> Option.map (fun m ->
        r, m, f, None
      )
end

(* JR *)
module U_JR = struct
  type cc = Z | NZ | C | NC | A

  type t =
    | M2 of cc
    | M3 of u8

  let init cc r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some(M2 cc))

  let step r m f t =
    match t with
    | M2 cc ->
      Memory.read m r.pc |> Option.map (fun v ->
        let taken = match cc with
          | Z -> f.z
          | NZ -> not f.z
          | C -> f.c
          | NC -> not f.c
          | A -> true
        in
        { r with pc = r.pc + 1}, m, f, if taken then Some(M3 v) else None
      )
    | M3 v ->
      Some({ r with pc = (r.pc + i8_of_u8 v) land 0xFF}, m, f, None)
end

(* CALL *)
module U_CALL = struct
  type cc = Z | NZ | C | NC | A

  type t =
    | M2 of cc
    | M3 of cc * u8
    | M4 of u8 * u8
    | M5 of u8 * u8
    | M6 of u8

  let init cc r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some(M2 cc))

  let step r m f t =
    match t with
    | M2 cc ->
      Memory.read m r.pc |> Option.map (fun lo ->
        {r with pc = r.pc + 1}, m, f, Some (M3(cc, lo))
      )
    | M3(cc, lo) ->
      Memory.read m r.pc |> Option.map (fun hi ->
        let taken = match cc with
          | Z -> f.z
          | NZ -> not f.z
          | C -> f.c
          | NC -> not f.c
          | A -> true
        in
        { r with pc = r.pc + 1}, m, f, if taken then Some (M4(lo, hi)) else None
      )
    | M4(lo, hi) ->
      let sp = (r.sp - 1) land 0xFFFF in
      Some ({ r with sp }, m, f, Some(M5(lo, hi)))
    | M5(lo, hi) ->
      let pc_hi = (r.pc lsr 8) land 0xFF in
      let pc_lo = (r.pc lsr 0) land 0xFF in
      Memory.write m r.sp pc_hi |> Option.map (fun m ->
        let sp = (r.sp - 1) land 0xFFFF in
        let pc = (hi lsl 8) lor pc_lo in
        { r with pc; sp}, m, f, Some (M6 lo)
      )
    | M6 lo ->
      let pc_hi = (r.pc lsr 8) land 0xFF in
      let pc_lo = (r.pc lsr 0) land 0xFF in
      Memory.write m r.sp pc_lo |> Option.map (fun m ->
        let pc = (pc_hi lsl 8) lor lo in
        { r with pc }, m, f, None
      )
end

(* PUSH r16 *)
module U_PUSH = struct
  type dd = BC | DE | HL | AF

  type t =
    | M2 of dd
    | M3 of dd
    | M4 of dd

  let init dd r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some(M2 dd))

  let step r m f t =
    match t with
    | M2 dd ->
      let sp = (r.sp - 1) land 0xFFFF in
      Some ({ r with sp }, m, f, Some(M3 dd))
    | M3 dd ->
      let v = match dd with
        | BC -> r.b
        | DE -> r.d
        | HL -> r.h
        | AF -> r.a
      in
      Memory.write m r.sp v |> Option.map (fun m ->
        { r with sp = (r.sp - 1) land 0xFFFF}, m, f, Some (M4 dd)
      )
    | M4 dd ->
      let v = match dd with
        | BC -> r.c
        | DE -> r.e
        | HL -> r.l
        | AF -> failwith "not implemented"
      in
      Memory.write m r.sp v |> Option.map (fun m ->
        r, m, f, None
      )
end

(* POP r16 *)
module U_POP = struct
  type dd = BC | DE | HL | AF

  type t =
    | M2 of dd
    | M3 of dd

  let init dd r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some(M2 dd))

  let step r m f t =
    match t with
    | M2 dd ->
      Memory.read m r.sp |> Option.map (fun lo ->
        let r = match dd with
          | BC -> { r with c = lo }
          | DE -> { r with e = lo }
          | HL -> { r with l = lo }
          | AF -> failwith "not implemented"
        in
        { r with sp = (r.sp + 1) land 0xFFFF }, m, f, Some (M3 dd)
      )
    | M3 dd ->
      Memory.read m r.sp |> Option.map (fun hi ->
        let r = match dd with
          | BC -> { r with b = hi }
          | DE -> { r with d = hi }
          | HL -> { r with h = hi }
          | AF -> { r with a = hi }
        in
        { r with sp = (r.sp + 1) land 0xFFFF }, m, f, None
      )
end

(* RET *)
module U_RET = struct
  type cc = A | I | Z | NZ | C | NC

  type ccp = CC_Z | CC_NZ | CC_C | CC_NC

  type t =
    | M2 of ccp
    | M3 of bool
    | M4 of u8
    | M5 of u8 * u8

  let init cc r m f =
    Some({ r with pc = r.pc + 1}, m, f, Some
      (match cc with
      | A  -> M3 false
      | I  -> M3 true
      | Z  -> M2 CC_Z
      | NZ -> M2 CC_NZ
      | C  -> M2 CC_C
      | NC -> M2 CC_NC
      )
    )

  let step r m f t =
    match t with
    | M2 cc ->
      let taken =
        match cc with
        | CC_Z  -> f.z
        | CC_NZ -> not f.z
        | CC_C  -> f.c
        | CC_NC -> not f.c
      in
      Some (r, m, f, if taken then Some (M3 false) else None)
    | M3 ei ->
      Memory.read m r.sp |> Option.map (fun lo ->
        { r with sp = (r.sp + 1) land 0xFFFF }, m, f, Some (M4 lo)
      )
    | M4 lo ->
      Memory.read m r.sp |> Option.map (fun hi ->
        { r with sp = (r.sp + 1) land 0xFFFF }, m, f, Some (M5(lo, hi))
      )
    | M5(lo, hi) ->
      Some ({ r with pc = (hi lsl 8) lor lo }, m, f, None)


end

type uop =
  | O_FETCH
  | O_CB
  | O_LD_R8_D8 of U_LD_R8_D8.t
  | O_LD_R8_R16 of U_LD_R8_R16.t
  | O_LD_R16_D16 of U_LD_R16_D16.t
  | O_ST_HL of U_ST_HL.t
  | O_ST_C of U_ST_C.t
  | O_STH of U_STH.t
  | O_ALU of U_ALU.t
  | O_ID_R16 of U_ID_R16.t
  | O_JR of U_JR.t
  | O_CALL of U_CALL.t
  | O_PUSH of U_PUSH.t
  | O_POP of U_POP.t
  | O_RET of U_RET.t

type t =
  { r: r
  ; f: f
  ; m: Memory.t
  ; uop: uop
  }

let decode cpu fn cons =
  fn cpu.r cpu.m cpu.f |> Option.map (fun (r, m, f, next) ->
    { r
    ; m
    ; f
    ; uop =
        match next with
        | None -> O_FETCH
        | Some uop -> cons uop
    }
  )

let decode_ld_r8_d8 cpu dd =
  decode cpu (U_LD_R8_D8.init dd) (fun s -> O_LD_R8_D8 s)

let decode_ld_r8_r16 cpu d dd =
  decode cpu (U_LD_R8_R16.init d dd) (fun s -> O_LD_R8_R16 s)

let decode_ld_r16_d16 cpu dd =
  decode cpu (U_LD_R16_D16.init dd) (fun s -> O_LD_R16_D16 s)

let decode_id_r16 cpu dd op =
  decode cpu (U_ID_R16.init dd op) (fun s -> O_ID_R16 s)

let decode_st_hl cpu op d =
  decode cpu (U_ST_HL.init op d) (fun s -> O_ST_HL s)

let decode_st_c cpu =
  decode cpu (U_ST_C.init) (fun s -> O_ST_C s)

let decode_sth cpu =
  decode cpu (U_STH.init) (fun s -> O_STH s)

let decode_alu cpu op d s =
  decode cpu (U_ALU.init op d s) (fun s -> O_ALU s)

let decode_jr cpu cc =
  decode cpu (U_JR.init cc) (fun s -> O_JR s)

let decode_call cpu cc =
  decode cpu (U_CALL.init cc) (fun s -> O_CALL s)

let decode_push cpu dd =
  decode cpu (U_PUSH.init dd) (fun s -> O_PUSH s)

let decode_pop cpu dd =
  decode cpu (U_POP.init dd) (fun s -> O_POP s)

let decode_ret cpu cc =
  decode cpu (U_RET.init cc) (fun s -> O_RET s)

let step cons step r m f s =
  Option.map
    (fun (r, m, f, t) ->
      match t with
      | Some t' -> { r; m; f; uop = cons t' }
      | None -> { r; m; f; uop = O_FETCH }
    )
    (step r m f s)

let create m =
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
  ; m
  ; uop = O_FETCH
  }

let step cpu =
  let { r; m; f; uop } = cpu in

  (* Execute a t-state *)
  match uop with
  | O_FETCH ->
    (* Fetch an opcode from memory and run the first 4 cycles *)
    Memory.read m r.pc |> Option.map (fun op ->
      Printf.eprintf "%04x %04x %x\n" r.pc r.sp op;
      (match op with
      | 0x01 -> U_LD_R16_D16.(decode_ld_r16_d16 cpu BC)
      | 0x03 -> U_ID_R16.(decode_id_r16 cpu BC Inc)
      | 0x04 -> U_ALU.(decode_alu cpu Inc B B)
      | 0x05 -> U_ALU.(decode_alu cpu Dec B B)
      | 0x06 -> U_LD_R8_D8.(decode_ld_r8_d8 cpu B)
      | 0x0A -> U_LD_R8_R16.(decode_ld_r8_r16 cpu A BC)
      | 0x0B -> U_ID_R16.(decode_id_r16 cpu BC Dec)
      | 0x0C -> U_ALU.(decode_alu cpu Inc C C)
      | 0x0D -> U_ALU.(decode_alu cpu Dec C C)
      | 0x0E -> U_LD_R8_D8.(decode_ld_r8_d8 cpu C)

      | 0x11 -> U_LD_R16_D16.(decode_ld_r16_d16 cpu DE)
      | 0x13 -> U_ID_R16.(decode_id_r16 cpu DE Inc)
      | 0x14 -> U_ALU.(decode_alu cpu Inc D D)
      | 0x15 -> U_ALU.(decode_alu cpu Dec D D)
      | 0x16 -> U_LD_R8_D8.(decode_ld_r8_d8 cpu D)
      | 0x17 -> U_ALU.(decode_alu cpu Rl A A)
      | 0x18 -> U_JR.(decode_jr cpu A)
      | 0x1A -> U_LD_R8_R16.(decode_ld_r8_r16 cpu A DE)
      | 0x1B -> U_ID_R16.(decode_id_r16 cpu DE Dec)
      | 0x1C -> U_ALU.(decode_alu cpu Inc E E)
      | 0x1D -> U_ALU.(decode_alu cpu Dec E E)
      | 0x1E -> U_LD_R8_D8.(decode_ld_r8_d8 cpu E)

      | 0x20 -> U_JR.(decode_jr cpu NZ)
      | 0x21 -> U_LD_R16_D16.(decode_ld_r16_d16 cpu HL)
      | 0x23 -> U_ID_R16.(decode_id_r16 cpu HL Inc)
      | 0x24 -> U_ALU.(decode_alu cpu Inc H H)
      | 0x25 -> U_ALU.(decode_alu cpu Dec H H)
      | 0x22 -> U_ST_HL.(decode_st_hl cpu Inc A)
      | 0x26 -> U_LD_R8_D8.(decode_ld_r8_d8 cpu H)
      | 0x28 -> U_JR.(decode_jr cpu Z)
      | 0x2B -> U_ID_R16.(decode_id_r16 cpu HL Dec)
      | 0x2C -> U_ALU.(decode_alu cpu Inc L L)
      | 0x2D -> U_ALU.(decode_alu cpu Dec L L)
      | 0x2E -> U_LD_R8_D8.(decode_ld_r8_d8 cpu L)

      | 0x30 -> U_JR.(decode_jr cpu NC)
      | 0x31 -> U_LD_R16_D16.(decode_ld_r16_d16 cpu SP)
      | 0x32 -> U_ST_HL.(decode_st_hl cpu Dec A)
      | 0x33 -> U_ID_R16.(decode_id_r16 cpu SP Inc)
      | 0x38 -> U_JR.(decode_jr cpu C)
      | 0x3B -> U_ID_R16.(decode_id_r16 cpu SP Dec)
      | 0x3C -> U_ALU.(decode_alu cpu Inc A A)
      | 0x3D -> U_ALU.(decode_alu cpu Dec A A)
      | 0x3E -> U_LD_R8_D8.(decode_ld_r8_d8 cpu A)

      | 0x40 -> U_ALU.(decode_alu cpu Mov B B)
      | 0x41 -> U_ALU.(decode_alu cpu Mov B C)
      | 0x42 -> U_ALU.(decode_alu cpu Mov B D)
      | 0x43 -> U_ALU.(decode_alu cpu Mov B E)
      | 0x44 -> U_ALU.(decode_alu cpu Mov B H)
      | 0x45 -> U_ALU.(decode_alu cpu Mov B L)
      | 0x46 -> failwith "not implemented: 0x46"
      | 0x47 -> U_ALU.(decode_alu cpu Mov B A)
      | 0x48 -> U_ALU.(decode_alu cpu Mov C B)
      | 0x49 -> U_ALU.(decode_alu cpu Mov C C)
      | 0x4A -> U_ALU.(decode_alu cpu Mov C D)
      | 0x4B -> U_ALU.(decode_alu cpu Mov C E)
      | 0x4C -> U_ALU.(decode_alu cpu Mov C H)
      | 0x4D -> U_ALU.(decode_alu cpu Mov C L)
      | 0x4E -> failwith "not implemented: 0x4E"
      | 0x4F -> U_ALU.(decode_alu cpu Mov C A)
      | 0x50 -> U_ALU.(decode_alu cpu Mov D B)
      | 0x51 -> U_ALU.(decode_alu cpu Mov D C)
      | 0x52 -> U_ALU.(decode_alu cpu Mov D D)
      | 0x53 -> U_ALU.(decode_alu cpu Mov D E)
      | 0x54 -> U_ALU.(decode_alu cpu Mov D H)
      | 0x55 -> U_ALU.(decode_alu cpu Mov D L)
      | 0x56 -> failwith "not implemented: 0x56"
      | 0x57 -> U_ALU.(decode_alu cpu Mov D A)
      | 0x58 -> U_ALU.(decode_alu cpu Mov E B)
      | 0x59 -> U_ALU.(decode_alu cpu Mov E C)
      | 0x5A -> U_ALU.(decode_alu cpu Mov E D)
      | 0x5B -> U_ALU.(decode_alu cpu Mov E E)
      | 0x5C -> U_ALU.(decode_alu cpu Mov E H)
      | 0x5D -> U_ALU.(decode_alu cpu Mov E L)
      | 0x5E -> failwith "not implemented: 0x5E"
      | 0x5F -> U_ALU.(decode_alu cpu Mov E A)
      | 0x60 -> U_ALU.(decode_alu cpu Mov H B)
      | 0x61 -> U_ALU.(decode_alu cpu Mov H C)
      | 0x62 -> U_ALU.(decode_alu cpu Mov H D)
      | 0x63 -> U_ALU.(decode_alu cpu Mov H E)
      | 0x64 -> U_ALU.(decode_alu cpu Mov H H)
      | 0x65 -> U_ALU.(decode_alu cpu Mov H L)
      | 0x66 -> failwith "not implemented: 0x66"
      | 0x67 -> U_ALU.(decode_alu cpu Mov H A)
      | 0x68 -> U_ALU.(decode_alu cpu Mov L B)
      | 0x69 -> U_ALU.(decode_alu cpu Mov L C)
      | 0x6A -> U_ALU.(decode_alu cpu Mov L D)
      | 0x6B -> U_ALU.(decode_alu cpu Mov L E)
      | 0x6C -> U_ALU.(decode_alu cpu Mov L H)
      | 0x6D -> U_ALU.(decode_alu cpu Mov L L)
      | 0x6E -> failwith "not implemented: 0x6E"
      | 0x6F -> U_ALU.(decode_alu cpu Mov L A)
      | 0x70 -> U_ST_HL.(decode_st_hl cpu Nop B)
      | 0x71 -> U_ST_HL.(decode_st_hl cpu Nop C)
      | 0x72 -> U_ST_HL.(decode_st_hl cpu Nop D)
      | 0x73 -> U_ST_HL.(decode_st_hl cpu Nop E)
      | 0x74 -> U_ST_HL.(decode_st_hl cpu Nop H)
      | 0x75 -> U_ST_HL.(decode_st_hl cpu Nop L)
      | 0x76 -> failwith "not implemented: HALT"
      | 0x77 -> U_ST_HL.(decode_st_hl cpu Nop A)
      | 0x78 -> U_ALU.(decode_alu cpu Mov A B)
      | 0x79 -> U_ALU.(decode_alu cpu Mov A C)
      | 0x7A -> U_ALU.(decode_alu cpu Mov A D)
      | 0x7B -> U_ALU.(decode_alu cpu Mov A E)
      | 0x7C -> U_ALU.(decode_alu cpu Mov A H)
      | 0x7D -> U_ALU.(decode_alu cpu Mov A L)
      | 0x7E -> failwith "not implemented: 0x7E"
      | 0x7F -> U_ALU.(decode_alu cpu Mov A A)
      | 0x80 -> U_ALU.(decode_alu cpu Add A B)
      | 0x81 -> U_ALU.(decode_alu cpu Add A C)
      | 0x82 -> U_ALU.(decode_alu cpu Add A D)
      | 0x83 -> U_ALU.(decode_alu cpu Add A E)
      | 0x84 -> U_ALU.(decode_alu cpu Add A H)
      | 0x85 -> U_ALU.(decode_alu cpu Add A L)
      | 0x86 -> failwith "not implemented: 0x86"
      | 0x87 -> U_ALU.(decode_alu cpu Add A A)
      | 0x88 -> U_ALU.(decode_alu cpu Adc A B)
      | 0x89 -> U_ALU.(decode_alu cpu Adc A C)
      | 0x8A -> U_ALU.(decode_alu cpu Adc A D)
      | 0x8B -> U_ALU.(decode_alu cpu Adc A E)
      | 0x8C -> U_ALU.(decode_alu cpu Adc A H)
      | 0x8D -> U_ALU.(decode_alu cpu Adc A L)
      | 0x8E -> failwith "not implemented: 0x8E"
      | 0x8F -> U_ALU.(decode_alu cpu Adc A A)
      | 0x90 -> U_ALU.(decode_alu cpu Sub A B)
      | 0x91 -> U_ALU.(decode_alu cpu Sub A C)
      | 0x92 -> U_ALU.(decode_alu cpu Sub A D)
      | 0x93 -> U_ALU.(decode_alu cpu Sub A E)
      | 0x94 -> U_ALU.(decode_alu cpu Sub A H)
      | 0x95 -> U_ALU.(decode_alu cpu Sub A L)
      | 0x96 -> failwith "not implemented: 0x96"
      | 0x97 -> U_ALU.(decode_alu cpu Sub A A)
      | 0x98 -> U_ALU.(decode_alu cpu Sbc A B)
      | 0x99 -> U_ALU.(decode_alu cpu Sbc A C)
      | 0x9A -> U_ALU.(decode_alu cpu Sbc A D)
      | 0x9B -> U_ALU.(decode_alu cpu Sbc A E)
      | 0x9C -> U_ALU.(decode_alu cpu Sbc A H)
      | 0x9D -> U_ALU.(decode_alu cpu Sbc A L)
      | 0x9E -> failwith "not implemented: 0x9E"
      | 0x9F -> U_ALU.(decode_alu cpu Sbc A A)
      | 0xA0 -> U_ALU.(decode_alu cpu And A B)
      | 0xA1 -> U_ALU.(decode_alu cpu And A C)
      | 0xA2 -> U_ALU.(decode_alu cpu And A D)
      | 0xA3 -> U_ALU.(decode_alu cpu And A E)
      | 0xA4 -> U_ALU.(decode_alu cpu And A H)
      | 0xA5 -> U_ALU.(decode_alu cpu And A L)
      | 0xA6 -> failwith "not implemented: 0xA6"
      | 0xA7 -> U_ALU.(decode_alu cpu And A A)
      | 0xA8 -> U_ALU.(decode_alu cpu Xor A B)
      | 0xA9 -> U_ALU.(decode_alu cpu Xor A C)
      | 0xAA -> U_ALU.(decode_alu cpu Xor A D)
      | 0xAB -> U_ALU.(decode_alu cpu Xor A E)
      | 0xAC -> U_ALU.(decode_alu cpu Xor A H)
      | 0xAD -> U_ALU.(decode_alu cpu Xor A L)
      | 0xAE -> failwith "not implemented: 0xAE"
      | 0xAF -> U_ALU.(decode_alu cpu Xor A A)
      | 0xB0 -> U_ALU.(decode_alu cpu Or  A B)
      | 0xB1 -> U_ALU.(decode_alu cpu Or  A C)
      | 0xB2 -> U_ALU.(decode_alu cpu Or  A D)
      | 0xB3 -> U_ALU.(decode_alu cpu Or  A E)
      | 0xB4 -> U_ALU.(decode_alu cpu Or  A H)
      | 0xB5 -> U_ALU.(decode_alu cpu Or  A L)
      | 0xB6 -> failwith "not implemented: 0xB6"
      | 0xB7 -> U_ALU.(decode_alu cpu Or  A A)
      | 0xB8 -> U_ALU.(decode_alu cpu Cp  A B)
      | 0xB9 -> U_ALU.(decode_alu cpu Cp  A C)
      | 0xBA -> U_ALU.(decode_alu cpu Cp  A D)
      | 0xBB -> U_ALU.(decode_alu cpu Cp  A E)
      | 0xBC -> U_ALU.(decode_alu cpu Cp  A H)
      | 0xBD -> U_ALU.(decode_alu cpu Cp  A L)
      | 0xBE -> failwith "not implemented: 0xBE"
      | 0xBF -> U_ALU.(decode_alu cpu Cp  A A)

      | 0xC0 -> U_RET.(decode_ret cpu NZ)
      | 0xC1 -> U_POP.(decode_pop cpu DE)
      | 0xC4 -> U_CALL.(decode_call cpu NZ)
      | 0xC5 -> U_PUSH.(decode_push cpu BC)
      | 0xC8 -> U_RET.(decode_ret cpu Z)
      | 0xC9 -> U_RET.(decode_ret cpu A)
      | 0xCB -> Some { cpu with r = { r with pc = r.pc + 1 }; uop = O_CB }
      | 0xCC -> U_CALL.(decode_call cpu Z)
      | 0xCD -> U_CALL.(decode_call cpu A)

      | 0xD0 -> U_RET.(decode_ret cpu NC)
      | 0xD1 -> U_POP.(decode_pop cpu DE)
      | 0xD4 -> U_CALL.(decode_call cpu NC)
      | 0xD5 -> U_PUSH.(decode_push cpu DE)
      | 0xD8 -> U_RET.(decode_ret cpu C)
      | 0xD9 -> U_RET.(decode_ret cpu I)
      | 0xDC -> U_CALL.(decode_call cpu C)

      | 0xE0 -> U_STH.(decode_sth cpu)
      | 0xE1 -> U_POP.(decode_pop cpu HL)
      | 0xE2 -> U_ST_C.(decode_st_c cpu)
      | 0xE5 -> U_PUSH.(decode_push cpu HL)

      | 0xF1 -> U_POP.(decode_pop cpu AF)
      | 0xF5 -> U_PUSH.(decode_push cpu AF)

      | op ->
        Printf.eprintf "invalid opcode: %x\n" op;
        exit (-1)
      )
    ) |> Option.join
  | O_CB ->
    (* Decode a CB-prefixed opcode *)
    (match Memory.read m r.pc with
    | None -> None
    | Some op ->
      (match op with
      | 0x10 -> U_ALU.(decode_alu cpu Rl B B)
      | 0x11 -> U_ALU.(decode_alu cpu Rl C C)
      | 0x12 -> U_ALU.(decode_alu cpu Rl D D)
      | 0x13 -> U_ALU.(decode_alu cpu Rl E E)
      | 0x14 -> U_ALU.(decode_alu cpu Rl H H)
      | 0x15 -> U_ALU.(decode_alu cpu Rl L L)
      | 0x16 -> failwith "not implemented: 0x16"
      | 0x17 -> U_ALU.(decode_alu cpu Rl A A)

      | 0x40 | 0x48 | 0x50 | 0x58 | 0x60 | 0x68 | 0x70 | 0x78 ->
        U_ALU.(decode_alu cpu (Bit ((op lsr 3) land 7)) A B)

      | 0x44 | 0x4C | 0x54 | 0x5C | 0x64 | 0x6C | 0x74 | 0x7C ->
        U_ALU.(decode_alu cpu (Bit ((op lsr 3) land 7)) H H)
      | _ ->
        Printf.eprintf "invalid CBopcode: %x\n" op;
        exit (-1)
      )
    )
  | O_LD_R8_D8 s ->
    step (fun s -> O_LD_R8_D8 s) U_LD_R8_D8.step r m f s
  | O_LD_R8_R16 s ->
    step (fun s -> O_LD_R8_R16 s) U_LD_R8_R16.step r m f s
  | O_LD_R16_D16 s ->
    step (fun s -> O_LD_R16_D16 s) U_LD_R16_D16.step r m f s
  | O_ST_HL s ->
    step (fun s -> O_ST_HL s) U_ST_HL.step r m f s
  | O_ST_C s ->
    step (fun s -> O_ST_C s) U_ST_C.step r m f s
  | O_STH s ->
    step (fun s -> O_STH s) U_STH.step r m f s
  | O_ALU s ->
    step (fun s -> O_ALU s) U_ALU.step r m f s
  | O_JR s ->
    step (fun s -> O_JR s) U_JR.step r m f s
  | O_CALL s ->
    step (fun s -> O_CALL s) U_CALL.step r m f s
  | O_PUSH s ->
    step (fun s -> O_PUSH s) U_PUSH.step r m f s
  | O_POP s ->
    step (fun s -> O_POP s) U_POP.step r m f s
  | O_ID_R16 s ->
    step (fun s -> O_ID_R16 s) U_ID_R16.step r m f s
  | O_RET s ->
    step (fun s -> O_RET s) U_RET.step r m f s
