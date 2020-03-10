(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)


type t

type sound = Sound0 | Sound1 | Sound2 | Sound3

val create : unit -> t

(* Enable/Disable whole sound system *)
val enable : t -> t
val disable : t -> t
val is_enabled : t -> bool

val is_sound_enabled : t -> sound -> bool
