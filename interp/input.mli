(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

type t


val create : unit -> t

val read : t -> u8 option
val write : t -> u8 -> t option

val tick : t -> (bool * t) option
