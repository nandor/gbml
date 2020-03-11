(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

module MBC1 = struct
  type mode = ROM | RAM

  type t =
    { rom_bank: int
    ; ram_bank: int
    ; has_ram: bool
    ; mode: mode
    ; rom_banks: bytes array
    ; ram_banks: (bytes array) option
    }

  let load rom_banks bank0 ch ~has_ram ~has_battery =
    { rom_bank = 1
    ; ram_bank = 0
    ; has_ram
    ; mode = ROM
    ; rom_banks = Array.init rom_banks (fun i ->
        if i == 0 then
          bank0
        else begin
          let bank = Buffer.create 0x4000 in
          Buffer.add_channel bank ch 0x4000;
          Buffer.to_bytes bank
        end
      )
    ; ram_banks = None
    }

  let read cart addr =
    match addr land 0xF000 with
    | 0x0000 | 0x1000 | 0x2000 | 0x3000 ->
      Some (Char.code (Bytes.get cart.rom_banks.(0) addr))
    | 0x4000 | 0x5000 | 0x6000 | 0x7000 ->
      Some (Char.code (Bytes.get cart.rom_banks.(cart.rom_bank) (addr - 0x4000)))
    | 0xA000 | 0xB000 ->
      cart.ram_banks |> Option.map (fun ram_banks ->
        Char.code (Bytes.get ram_banks.(cart.ram_bank) (addr - 0xA000))
      )
    | _ ->
      failwith "MBC1.read"

  let write cart addr v =
    match addr land 0xF000 with
    | 0x0000 | 0x1000 ->
      if v = 0xA then
        if not cart.has_ram then
          None
        else
          Some { cart with
            ram_banks = Some (Array.init 4 (fun _ ->
              Bytes.make 0x2000 (Char.chr 0)
            ))
          }
      else
        Some { cart with ram_banks = None }
    | 0x2000 | 0x3000 ->
      Some { cart with
        rom_bank =
          (cart.rom_bank land 0xE0)
          lor
          (if v land 0x1F <> 0 then v land 0x1F else 1)
      }
    | 0xA000 | 0xB000 ->
      cart.ram_banks |> Option.map (fun ram_banks ->
        Bytes.set ram_banks.(cart.ram_bank) (addr - 0xA000) (Char.chr v);
        cart
      )
    | _ ->
      failwith (Printf.sprintf "MBC1.write %04x %02x" addr v)
end

module MBC2 = struct
  type t

  let load _rom_banks _bank0 _chan = failwith "not implemented: MBC2.load"

  let read _cart _addr = failwith "not implementedL MBC2.read"

  let write _cart _addr _v = failwith "not implemented: MBC2.write"
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
    | 0x01 -> Cart_MBC1 (MBC1.load rom_banks
        bank0
        ch
        ~has_ram:false
        ~has_battery:false
      )
    | 0x02 -> Cart_MBC1 (MBC1.load rom_banks
        bank0
        ch
        ~has_ram:true
        ~has_battery:false
      )
    | 0x03 -> Cart_MBC1 (MBC1.load rom_banks
        bank0
        ch
        ~has_ram:true
        ~has_battery:true
      )
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

let write cart addr v =
  match cart with
  | Cart_MBC1 c -> MBC1.write c addr v |> Option.map (fun c -> Cart_MBC1 c)
  | Cart_MBC2 c -> MBC2.write c addr v |> Option.map (fun c -> Cart_MBC2 c)
