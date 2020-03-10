(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

type t

type i =
  { isPins: bool
  ; isSerial: bool
  ; isTimer: bool
  ; isStat: bool
  ; isVBlank: bool
  }

val create : Cartridge.t -> Gpu.t -> Sound.t -> t

val read : t -> Types.u16 -> Types.u8 option

val write : t -> Types.u16 -> Types.u8 -> t option

val tick : t -> (i * t) option
