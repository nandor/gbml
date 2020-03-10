(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

type t

val create : unit -> t

val vram_read : t -> Types.u16 -> Types.u8 option

val vram_write : t -> Types.u16 -> Types.u8 -> t option

val set_bgp : t -> Types.u8 -> t option
