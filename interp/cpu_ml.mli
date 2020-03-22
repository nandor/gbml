(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

type t

val create : unit -> t

val dump : t -> unit

val tick : t -> System.t -> (t * System.t) option
