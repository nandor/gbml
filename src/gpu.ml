(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

type gpu_state =
  | HBlank of int
  | VBlank of int
  | OAMRead of int
  | VRAMRead of int

type t =
  { scroll_y: u8
  ; lcd_on: bool
  ; lcd_window_tile_map: u16
  ; lcd_window_display: bool
  ; lcd_bg_window_tile_data: u16
  ; lcd_bg_window_tile_map: u16
  ; lcd_bg_window_display: bool
  ; lcd_sprite_size: u8
  ; lcd_sprite_display: bool
  ; lcd_ly: int
  ; state: gpu_state
  }

type i =
  { igStat: bool
  ; igVBlank: bool
  }

let create () =
  { scroll_y = 0
  ; lcd_on = false
  ; lcd_window_tile_map = 0x9800
  ; lcd_window_display = false
  ; lcd_bg_window_tile_data = 0x8800
  ; lcd_bg_window_tile_map = 0x9800
  ; lcd_bg_window_display = false
  ; lcd_sprite_size = 8
  ; lcd_sprite_display = false
  ; lcd_ly = 0
  ; state = HBlank 0
  }

let vram_read _gpu _addr =
  failwith "not implemented"

let vram_write gpu _addr _v =
  Some gpu

let set_bgp gpu _v =
  Some gpu

let set_scroll_y gpu scroll_y =
  Some { gpu with scroll_y }

let get_scroll_y gpu =
  Some gpu.scroll_y

let set_lcdc
  gpu
  lcd_on
  lcd_window_tile_map
  lcd_window_display
  lcd_bg_window_tile_data
  lcd_bg_window_tile_map
  lcd_bg_window_display
  lcd_sprite_size
  lcd_sprite_display =
  Some { gpu with
    lcd_on;
    lcd_window_tile_map;
    lcd_window_display;
    lcd_bg_window_tile_data;
    lcd_bg_window_tile_map;
    lcd_bg_window_display;
    lcd_sprite_size;
    lcd_sprite_display;
  }


let get_ly gpu = Some gpu.lcd_ly

let no_i = { igStat = false; igVBlank = false }

let tick gpu =
  match gpu.state with
  | HBlank n when n < 51 ->
    Some (no_i, { gpu with state = HBlank (n + 1) })
  | HBlank _ ->
    let lcd_ly = gpu.lcd_ly + 1 in
    if lcd_ly > 143 then
      (* VBlank here *)
      Some (no_i, { gpu with lcd_ly; state = VBlank 0 })
    else
      (* Scanline here *)
      Some (no_i, { gpu with lcd_ly; state = OAMRead 0 })

  | VBlank n when n < 114 ->
    Some (no_i, { gpu with state = VBlank (n + 1) })
  | VBlank _ ->
    if gpu.lcd_ly >= 153 then
      Some (no_i, { gpu with state = OAMRead 0; lcd_ly = 0 })
    else
      Some (no_i, { gpu with state = OAMRead 0; lcd_ly = gpu.lcd_ly + 1 })
  | OAMRead n when n < 20 ->
    Some (no_i, { gpu with state = OAMRead (n + 1) })
  | OAMRead _ ->
    (* HBlank here *)
    Some (no_i, { gpu with state = VRAMRead 0 })

  | VRAMRead n when n < 43 ->
    Some (no_i, { gpu with state = VRAMRead (n + 1) })
  | VRAMRead _ ->
    Some (no_i, { gpu with state = HBlank 0 })
