(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

type ie =
  { iePins: bool
  ; ieSerial: bool
  ; ieTimer: bool
  ; ieStat: bool
  ; ieVBlank: bool
  }

type is =
  { isPins: bool
  ; isSerial: bool
  ; isTimer: bool
  ; isStat: bool
  ; isVBlank: bool
  }

type t =
  { cart: Cartridge.t
  ; gpu: Gpu.t
  ; sound: Sound.t
  ; timer: Timer.t
  ; input: Input.t
  ; high_ram: bytes
  ; boot_rom_enabled: bool
  ; ram: bytes
  ; ie: ie
  ; is: is
  }

type interrupt =
  | Int_VBlank
  | Int_Stat
  | Int_Timer
  | Int_Serial
  | Int_Pins

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

let create cart gpu sound timer input =
  { cart
  ; gpu
  ; sound
  ; timer
  ; input
  ; high_ram = Bytes.create 0x80
  ; boot_rom_enabled = true
  ; ram = Bytes.create 0x2000
  ; ie =
    { iePins = false
    ; ieSerial = false
    ; ieTimer = false
    ; ieStat = false
    ; ieVBlank = false
    }
  ; is =
    { isPins = false
    ; isSerial = false
    ; isTimer = false
    ; isStat = false
    ; isVBlank = false
    }
  }

let read s addr =
  let { gpu; high_ram; cart; ram; timer; boot_rom_enabled; ie; is; _ } = s in
  match addr land 0xF000 with
  | 0x0000 | 0x1000 | 0x2000 | 0x3000
  | 0x4000 | 0x5000 | 0x6000 | 0x7000 ->
    if addr < 0x100 && boot_rom_enabled then
      Some boot_rom.(addr)
    else
      Cartridge.read cart addr
  | 0x8000 | 0x9000 ->
    Gpu.vram_read gpu (addr - 0x8000)
  | 0xA000 | 0xB000 ->
    Cartridge.read cart addr
  | 0xC000 | 0xD000 ->
    Some (Char.code (Bytes.get ram (addr - 0xC000)))
  | _ when 0xE000 <= addr && addr < 0xFE00 ->
    Some (Char.code (Bytes.get ram (addr - 0xE000)))
  | _ when 0xFE00 <= addr && addr < 0xFEA0 ->
    Gpu.oam_read gpu (addr - 0xFE00)
  | _ when 0xFF80 <= addr && addr < 0xFFFF ->
    Some (Char.code (Bytes.get high_ram (addr - 0xFF80)))
  | _ -> match addr with
  | 0xFF00 ->
    (* TODO: Joy pad register *)
    Some 0x00
  | 0xFF01 ->
    (* TODO: Serial transfer data *)
    Some 0x00

  (* DIV *)
  | 0xFF04 -> Timer.get_div timer
  (* TIMA: Timer Counter *)
  | 0xFF05 -> Timer.get_counter timer

  (* IF: Interrupt Flag *)
  | 0xFF0F ->
    let v =
      (if is.isPins   then 0x10 else 0x00)
      lor
      (if is.isSerial then 0x08 else 0x00)
      lor
      (if is.isTimer  then 0x04 else 0x00)
      lor
      (if is.isStat   then 0x02 else 0x00)
      lor
      (if is.isVBlank then 0x01 else 0x00)
    in
    Some v

  | 0xFF26 ->
    Some 0x00

  | 0xFF40 ->
    let v =
      (if Gpu.get_lcd_enable gpu              then 0x80 else 0x00) lor
      (if Gpu.get_lcd_window_tile_map gpu     then 0x40 else 0x00) lor
      (if Gpu.get_lcd_window_display gpu      then 0x20 else 0x00) lor
      (if Gpu.get_lcd_bg_window_tile_data gpu then 0x10 else 0x00) lor
      (if Gpu.get_lcd_bg_window_tile_map gpu  then 0x08 else 0x00) lor
      (if Gpu.get_lcd_bg_window_display gpu   then 0x01 else 0x00) lor
      (if Gpu.get_lcd_sprite_size gpu         then 0x04 else 0x00) lor
      (if Gpu.get_lcd_sprite_display gpu      then 0x02 else 0x00)
    in
    Some v

  | 0xFF41 ->
    let v =
      0x80 lor
      (if Gpu.get_lcd_stat_lyc    gpu then 0x40 else 0x00) lor
      (if Gpu.get_lcd_stat_oam    gpu then 0x20 else 0x00) lor
      (if Gpu.get_lcd_stat_vblank gpu then 0x10 else 0x00) lor
      (if Gpu.get_lcd_stat_hblank gpu then 0x08 else 0x00) lor
      (if Gpu.get_lcd_stat_equ    gpu then 0x04 else 0x00) lor
      (match Gpu.get_lcd_stat_mode gpu with
      | HBlank -> 0
      | VBlank -> 1
      | OAM    -> 2
      | LCD    -> 3
      )
    in
    Some v

  | 0xFF42 -> Gpu.get_scroll_y gpu
  | 0xFF43 -> Gpu.get_scroll_x gpu
  | 0xFF44 -> Gpu.get_ly gpu

  (* Speed *)
  | 0xFF4D ->
    Some 0xFF

  (* Interrupt enable *)
  | 0xFFFF ->
    Some (
      (if ie.iePins   then 0x10 else 0x00) lor
      (if ie.ieSerial then 0x08 else 0x00) lor
      (if ie.ieTimer  then 0x04 else 0x00) lor
      (if ie.ieStat   then 0x02 else 0x00) lor
      (if ie.ieVBlank then 0x01 else 0x00)
    )
  | _ ->
    Printf.eprintf "cannot read from %x\n" addr;
    exit (-1)

let write s addr v =
  let { gpu; sound; high_ram; ram; cart; timer; input; _ } = s in
  match addr land 0xF000 with
  | 0x0000 | 0x1000 | 0x2000 | 0x3000
  | 0x4000 | 0x5000 | 0x6000 | 0x7000 ->
    Cartridge.write cart addr v |> Option.map (fun cart -> { s with cart })
  | 0x8000 | 0x9000 ->
    Gpu.vram_write gpu (addr - 0x8000) v |> Option.map (fun gpu ->
      { s with gpu }
    )
  | 0xA000 | 0xB000 ->
    Cartridge.write cart addr v |> Option.map (fun cart -> { s with cart })
  | 0xC000 | 0xD000 ->
    Bytes.set ram (addr - 0xC000) (Char.chr v);
    Some s
  | _ when 0xE000 <= addr && addr < 0xFE00 ->
    Bytes.set ram (addr - 0xE000) (Char.chr v);
    Some s
  | _ when 0xFE00 <= addr && addr < 0xFEA0 ->
    Gpu.vram_write gpu (addr - 0xFE00) v |> Option.map (fun gpu -> { s with gpu })
  | _ when 0xFF80 <= addr && addr <= 0xFFFE ->
    Bytes.set high_ram (addr - 0xFF80) (Char.chr v);
    Some s
  | _ -> match addr with
  (* P1 *)
  | 0xFF00 ->
    Input.write input v |> Option.map (fun input -> { s with input })

  | 0xFF01 ->
    (* TODO: SB Serial Data Transfer *)
    print_char (Char.chr v); flush_all ();
    Some s
  | 0xFF02 ->
    (* TODO: SC Serial IO Control *)
    Some s

  | 0xFF04 ->
    Timer.clear_div timer |> Option.map (fun timer -> { s with timer })
  | 0xFF05 ->
    Timer.set_counter timer v |> Option.map (fun timer -> { s with timer })
  | 0xFF06 ->
    Timer.set_modulo timer v |> Option.map (fun timer -> { s with timer })
  | 0xFF07 ->
    let timer_enable = (v land 0x4) <> 0 in
    let timer_freq = match (v land 0x3) with
      | 0 -> Timer.T_4KHz
      | 1 -> Timer.T_262KHz
      | 2 -> Timer.T_65KHz
      | 3 -> Timer.T_16KHz
      | _ -> failwith "unreachable"
    in
    Timer.set_control timer timer_enable timer_freq |> Option.map (fun timer ->
      { s with timer }
    )

  | 0xFF0F ->
    let isPins   = (v land 0x10) <> 0 in
    let isSerial = (v land 0x08) <> 0 in
    let isTimer  = (v land 0x04) <> 0 in
    let isStat   = (v land 0x02) <> 0 in
    let isVBlank = (v land 0x01) <> 0 in
    Some { s with is = { isPins; isSerial; isTimer; isStat; isVBlank } }

  | 0xFF11 ->
    (* TODO: Sound 1 Register, Sound Length/Wave pattern duty *)
    Some s
  | 0xFF12 ->
    (* TODO: Sound 1 Register, Envelope *)
    Some s
  | 0xFF13 ->
    (* TODO: Sound 1 Register, Frequency Low Bits *)
    Some s
  | 0xFF14 ->
    (* TODO: Sound 1 Register, Frequency High Bits *)
    Some s

  | 0xFF16 ->
    (* TODO: Sound 2 Register, Sound Length/Wave pattern duty *)
    Some s
  | 0xFF17 ->
    (* TODO: Sound 2 Register, Envelope *)
    Some s
  | 0xFF18 ->
    (* TODO: Sound 2 Register, Frequency Low Bits *)
    Some s
  | 0xFF19 ->
    (* TODO: Sound 2 Register, Frequency High Bits *)
    Some s

  | 0xFF24 ->
    (* TODO: Channel control on/off *)
    Some s

  | 0xFF25 ->
    (* TODO: Selection of Sound Output Terminal *)
    Some s

  | 0xFF26 ->
    (* Writing to the 7th bit enables/disables sound *)
    Sound.(if v land 7 = 1 then enable else disable) sound |> Option.map
      (fun sound -> { s with sound })

  | 0xFF40 ->
    Gpu.set_lcdc
      gpu
      (v land 0x80 <> 0)
      (v land 0x40 <> 0)
      (v land 0x20 <> 0)
      (v land 0x10 <> 0)
      (v land 0x08 <> 0)
      (v land 0x01 <> 0)
      (v land 0x04 <> 0)
      (v land 0x02 <> 0)
      |> Option.map (fun gpu -> { s with gpu })
  | 0xFF41 ->
    Gpu.set_stat
      gpu
      (v land 0x40 <> 0x00)
      (v land 0x20 <> 0x00)
      (v land 0x10 <> 0x00)
      (v land 0x08 <> 0x00)
      |> Option.map (fun gpu -> { s with gpu })

  | 0xFF42 -> Gpu.set_scroll_y gpu v |> Option.map (fun gpu -> { s with gpu })
  | 0xFF43 -> Gpu.set_scroll_x gpu v |> Option.map (fun gpu -> { s with gpu })
  | 0xFF44 -> failwith "0xFF44"
  | 0xFF45 ->
      Gpu.set_lyc gpu v |> Option.map (fun gpu -> { s with gpu })
  | 0xFF47 -> Gpu.set_bgp gpu v |> Option.map (fun gpu -> { s with gpu })
  | 0xFF48 -> Gpu.set_obp0 gpu v |> Option.map (fun gpu -> { s with gpu })
  | 0xFF49 -> Gpu.set_obp1 gpu v |> Option.map (fun gpu -> { s with gpu })
  | 0xFF4A -> Gpu.set_window_y gpu v |> Option.map (fun gpu -> { s with gpu })
  | 0xFF4B -> Gpu.set_window_x gpu v |> Option.map (fun gpu -> { s with gpu })

  | 0xFF50 ->
    Some { s with boot_rom_enabled = false; }

  | 0xFFFF ->
    let iePins   = (v land 0x10) <> 0 in
    let ieSerial = (v land 0x08) <> 0 in
    let ieTimer  = (v land 0x04) <> 0 in
    let ieStat   = (v land 0x02) <> 0 in
    let ieVBlank = (v land 0x01) <> 0 in
    Some { s with ie = { iePins; ieSerial; ieTimer; ieStat; ieVBlank } }

  | _ ->
    Printf.eprintf "cannot write to %x\n" addr;
    exit (-1)

let is_interrupt_enabled t i =
  match i with
  | Int_VBlank -> t.ie.ieVBlank
  | Int_Stat   -> t.ie.ieStat
  | Int_Timer  -> t.ie.ieTimer
  | Int_Serial -> t.ie.ieSerial
  | Int_Pins   -> t.ie.iePins

let is_interrupt_pending t i =
  match i with
  | Int_VBlank -> t.is.isVBlank
  | Int_Stat   -> t.is.isStat
  | Int_Timer  -> t.is.isTimer
  | Int_Serial -> t.is.isSerial
  | Int_Pins   -> t.is.isPins

let clear_interrupt t i =
  match i with
  | Int_VBlank -> Some { t with is = { t.is with isVBlank = false }}
  | Int_Stat   -> Some { t with is = { t.is with isStat   = false }}
  | Int_Timer  -> Some { t with is = { t.is with isTimer  = false }}
  | Int_Serial -> Some { t with is = { t.is with isSerial = false }}
  | Int_Pins   -> Some { t with is = { t.is with isPins   = false }}

let tick s =
  match Input.tick s.input with
  | None -> None
  | Some (inPins, input) ->
    match Timer.tick s.timer with
    | None -> None
    | Some (inTimer, timer) ->
      match Gpu.tick s.gpu with
      | None -> None
      | Some ((inStat, inVBlank), gpu) ->
        if inStat then Printf.eprintf "INTERRUPT\n";
        let is =
          { isPins   = s.is.isPins || inPins
          ; isSerial = s.is.isSerial
          ; isTimer  = s.is.isTimer || inTimer
          ; isStat   = s.is.isStat || inStat
          ; isVBlank = s.is.isVBlank || inVBlank
          }
        in
        Some { s with gpu; timer; input; is }
