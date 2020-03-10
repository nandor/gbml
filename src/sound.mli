(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

type t

type sound = Sound0 | Sound1 | Sound2 | Sound3

val create : unit -> t

(* Enable/Disable whole sound system *)
val enable : t -> t option
val disable : t -> t option
val is_enabled : t -> bool

(* Sound mode setup *)
val set_sound_freq_lo : t -> sound -> u8 -> t option
val is_sound_enabled : t -> sound -> bool
