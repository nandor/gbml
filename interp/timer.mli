(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

type t


val create : unit -> t

val clear_div : t -> t option
val get_div : t -> u8 option

val set_counter : t -> u8 -> t option
val get_counter : t -> u8 option

val set_modulo : t -> u8 -> t option
val get_modulo : t -> u8 option

type freq =
  | T_4KHz
  | T_262KHz
  | T_65KHz
  | T_16KHz

val set_control : t -> bool -> freq -> t option

val tick : t -> (bool * t) option
