(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

type stat = HBlank | VBlank | OAM | LCD

type t

val create : unit -> t

val vram_read : t -> u16 -> u8 option

val vram_write : t -> u16 -> u8 -> t option

val set_bgp : t -> u8 -> t option
val set_obp0 : t -> u8 -> t option
val set_obp1 : t -> u8 -> t option

val set_scroll_y : t -> u8 -> t option
val get_scroll_y : t -> u8 option
val set_scroll_x : t -> u8 -> t option
val get_scroll_x : t -> u8 option

val set_window_x : t -> u8 -> t option
val get_window_x : t -> u8 option
val set_window_y : t -> u8 -> t option
val get_window_y : t -> u8 option

val set_lcdc
   : t
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> t option

val get_lcd_enable : t -> bool
val get_lcd_window_tile_map : t -> bool
val get_lcd_window_display : t -> bool
val get_lcd_bg_window_tile_data : t -> bool
val get_lcd_bg_window_tile_map : t -> bool
val get_lcd_bg_window_display : t -> bool
val get_lcd_sprite_size : t -> bool
val get_lcd_sprite_display  : t -> bool

val set_stat
   : t
  -> bool
  -> bool
  -> bool
  -> bool
  -> bool
  -> stat
  -> t option

val get_ly : t -> u8 option

val tick : t -> ((bool * bool) * t) option
