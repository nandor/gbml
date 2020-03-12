(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

type t =
  { is_enabled: bool
  }

type sound = Sound0 | Sound1 | Sound2 | Sound3

let create () =
  { is_enabled = true
  }

let enable _s = Some { is_enabled = true }

let disable _s = Some { is_enabled = false }

let is_enabled s =
  s.is_enabled

let set_sound_freq_lo s _snd _f =
  Some s

let is_sound_enabled s _snd =
  s.is_enabled
