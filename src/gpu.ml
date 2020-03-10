(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

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


let get_ly gpu = Some 144

let tick gpu = Some gpu
