(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

type t

(* Initialises the CPU *)
val create : Memory.t -> t

(* Executes a step of an instruction *)
val step : t -> t option
