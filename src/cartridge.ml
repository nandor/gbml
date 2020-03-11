(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

module MBC1 = struct
  type t =
    { rom_bank: int
    ; banks: bytes array
    }

  let load rom_banks bank0 ch =
    { rom_bank = 1
    ; banks = Array.init rom_banks (fun i ->
        if i == 0 then
          bank0
        else begin
          let bank = Buffer.create 0x4000 in
          Buffer.add_channel bank ch 0x4000;
          Buffer.to_bytes bank
        end
      )
    }

  let read cart addr =
    let chr =
      if addr < 0x4000 then
        Bytes.get cart.banks.(0) addr
      else
        Bytes.get cart.banks.(cart.rom_bank) (addr - 0x4000)
    in
    Some (Char.code chr)

  let write _cart _addr = failwith "not implemented: MBC1.write"
end

module MBC2 = struct
  type t

  let load _rom_banks _bank0 _chan = failwith "not implemented: MBC2.load"

  let read _cart _addr = failwith "not implementedL MBC2.read"

  let write _cart _addr = failwith "not implemented: MBC2.write"
end

type t =
  | Cart_MBC1 of MBC1.t
  | Cart_MBC2 of MBC2.t

let load path =
  let ch = open_in path in

  (* Read the .gb file header and the first bank *)
  let bank0 = Buffer.create 0x4000 in
  Buffer.add_channel bank0 ch 0x4000;
  let bank0 = Buffer.to_bytes bank0 in

  (* Find the ROM size *)
  let rom_banks =
    match Char.code (Bytes.get bank0 0x148) with
    | 0x00 -> 2
    | 0x01 -> 4
    | 0x02 -> 8
    | 0x03 -> 16
    | 0x04 -> 32
    | 0x05 -> 64
    | 0x06 -> 128
    | 0x07 -> 256
    | 0x08 -> 512
    | 0x52 -> 72
    | 0x53 -> 80
    | 0x54 -> 96
    | size ->
      Printf.eprintf "Invalid no. of ROM banks: %x" size;
      exit (-1)
  in

  (* Decide what kind of cartridge this is *)
  let cart =
    match Char.code (Bytes.get bank0 0x147) with
    | 0x01 -> Cart_MBC1 (MBC1.load rom_banks bank0 ch)
    | 0x05 -> Cart_MBC2 (MBC2.load rom_banks bank0 ch)
    | code ->
      Printf.eprintf "Invalid cartridge: %x" code;
      exit (-1)
  in
  close_in ch;
  cart

let read cart =
  match cart with
  | Cart_MBC1 c -> MBC1.read c
  | Cart_MBC2 c -> MBC2.read c

let write cart =
  match cart with
  | Cart_MBC1 c -> MBC1.write c
  | Cart_MBC2 c -> MBC2.write c
