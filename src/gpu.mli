(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

type t

type i =
  { igStat: bool
  ; igVBlank: bool
  }

val create : unit -> t

val vram_read : t -> u16 -> u8 option

val vram_write : t -> u16 -> u8 -> t option

val set_bgp : t -> u8 -> t option

val set_scroll_y : t -> u8 -> t option
val get_scroll_y : t -> u8 option
val set_scroll_x : t -> u8 -> t option
val get_scroll_x : t -> u8 option

val set_lcdc
   : t
  -> bool
  -> u16
  -> bool
  -> u16
  -> u16
  -> bool
  -> u8
  -> bool
  -> t option

val get_ly : t -> u8 option

val tick : t -> (i * t) option
