(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

type t =
  { cart: Cartridge.t
  ; gpu: Gpu.t
  ; sound: Sound.t
  ; high_ram: bytes
  ; boot_rom_enabled: bool
  }

let boot_rom =
   [| 0x31; 0xfe; 0xff; 0xaf; 0x21; 0xff; 0x9f; 0x32
    ; 0xcb; 0x7c; 0x20; 0xfb; 0x21; 0x26; 0xff; 0x0e
    ; 0x11; 0x3e; 0x80; 0x32; 0xe2; 0x0c; 0x3e; 0xf3
    ; 0xe2; 0x32; 0x3e; 0x77; 0x77; 0x3e; 0xfc; 0xe0
    ; 0x47; 0x11; 0x04; 0x01; 0x21; 0x10; 0x80; 0x1a
    ; 0xcd; 0x95; 0x00; 0xcd; 0x96; 0x00; 0x13; 0x7b
    ; 0xfe; 0x34; 0x20; 0xf3; 0x11; 0xd8; 0x00; 0x06
    ; 0x08; 0x1a; 0x13; 0x22; 0x23; 0x05; 0x20; 0xf9
    ; 0x3e; 0x19; 0xea; 0x10; 0x99; 0x21; 0x2f; 0x99
    ; 0x0e; 0x0c; 0x3d; 0x28; 0x08; 0x32; 0x0d; 0x20
    ; 0xf9; 0x2e; 0x0f; 0x18; 0xf3; 0x67; 0x3e; 0x64
    ; 0x57; 0xe0; 0x42; 0x3e; 0x91; 0xe0; 0x40; 0x04
    ; 0x1e; 0x02; 0x0e; 0x0c; 0xf0; 0x44; 0xfe; 0x90
    ; 0x20; 0xfa; 0x0d; 0x20; 0xf7; 0x1d; 0x20; 0xf2
    ; 0x0e; 0x13; 0x24; 0x7c; 0x1e; 0x83; 0xfe; 0x62
    ; 0x28; 0x06; 0x1e; 0xc1; 0xfe; 0x64; 0x20; 0x06
    ; 0x7b; 0xe2; 0x0c; 0x3e; 0x87; 0xe2; 0xf0; 0x42
    ; 0x90; 0xe0; 0x42; 0x15; 0x20; 0xd2; 0x05; 0x20
    ; 0x4f; 0x16; 0x20; 0x18; 0xcb; 0x4f; 0x06; 0x04
    ; 0xc5; 0xcb; 0x11; 0x17; 0xc1; 0xcb; 0x11; 0x17
    ; 0x05; 0x20; 0xf5; 0x22; 0x23; 0x22; 0x23; 0xc9
    ; 0xce; 0xed; 0x66; 0x66; 0xcc; 0x0d; 0x00; 0x0b
    ; 0x03; 0x73; 0x00; 0x83; 0x00; 0x0c; 0x00; 0x0d
    ; 0x00; 0x08; 0x11; 0x1f; 0x88; 0x89; 0x00; 0x0e
    ; 0xdc; 0xcc; 0x6e; 0xe6; 0xdd; 0xdd; 0xd9; 0x99
    ; 0xbb; 0xbb; 0x67; 0x63; 0x6e; 0x0e; 0xec; 0xcc
    ; 0xdd; 0xdc; 0x99; 0x9f; 0xbb; 0xb9; 0x33; 0x3e
    ; 0x3c; 0x42; 0xb9; 0xa5; 0xb9; 0xa5; 0x42; 0x3c
    ; 0x21; 0x04; 0x01; 0x11; 0xa8; 0x00; 0x1a; 0x13
    ; 0xbe; 0x20; 0xfe; 0x23; 0x7d; 0xfe; 0x34; 0x20
    ; 0xf5; 0x06; 0x19; 0x78; 0x86; 0x23; 0x05; 0x20
    ; 0xfb; 0x86; 0x20; 0xfe; 0x3e; 0x01; 0xe0; 0x50
    |]

let create cart gpu sound =
  { cart
  ; gpu
  ; sound
  ; high_ram = Bytes.create 0x80
  ; boot_rom_enabled = true
  }

let read mem addr =
  let { gpu; sound; high_ram; cart; boot_rom_enabled } = mem in
  match addr land 0xF000 with
  | 0x0000 ->
    if addr < 0x100 && boot_rom_enabled then
      Some boot_rom.(addr)
    else
      Cartridge.read cart addr
  | 0x8000 | 0x9000 ->
    Gpu.vram_read gpu addr
  | op when 0xFF80 <= addr && addr <= 0xFFFE ->
    Some (Char.code (Bytes.get high_ram (addr - 0xFF80)))
  | op -> match addr with
  | 0xFF26 ->
    failwith "sound"
  | op ->
    Printf.eprintf "cannot read from %x\n" addr;
    exit (-1)

let write mem addr v =
  let { gpu; sound; high_ram; cart } = mem in
  match addr land 0xF000 with
  | 0x8000 | 0x9000 ->
    Gpu.vram_write gpu addr v |> Option.map (fun gpu -> { mem with gpu })
  | op when 0xFF80 <= addr && addr <= 0xFFFE ->
    Bytes.set high_ram (addr - 0xFF80) (Char.chr v);
    Some mem
  | op -> match addr with
  | 0xFF11 ->
    (* TODO: Sound 1 Register, Sound Length/Wave pattern duty *)
    Some mem
  | 0xFF12 ->
    (* TODO: Sound 1 Register, Envelope *)
    Some mem
  | 0xFF24 ->
    (* TODO: Channel control on/off *)
    Some mem
  | 0xFF25 ->
    (* TODO: Selection of Sound Output Terminal *)
    Some mem
  | 0xFF26 ->
    (* Writing to the 7th bit enables/disables sound *)
    let sound = Sound.(if v land 7 = 1 then enable else disable) sound in
    Some { mem with sound }
  | 0xFF47 ->
    Gpu.set_bgp gpu v |> Option.map (fun gpu -> { mem with gpu })
  | op ->
    Printf.eprintf "cannot write to %x\n" addr;
    exit (-1)

