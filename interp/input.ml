(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

type t =
  { key_select: bool
  ; key_start: bool
  ; key_a: bool
  ; key_b: bool
  ; key_up: bool
  ; key_down: bool
  ; key_left: bool
  ; key_right: bool
  ; interrupt: bool
  ; mask: u8
  }

let create () =
  { key_select = false
  ; key_start = false
  ; key_a = false
  ; key_b = false
  ; key_up = false
  ; key_down = false
  ; key_left = false
  ; key_right = false
  ; interrupt = false
  ; mask = 0x00
  }

let read i = Some i.mask

let write i v =
  if (v land 0x20) = 0 then
    Some { i with mask =
      0x2F
      land
      (if i.key_start  then 0x07 else 0xFF)
      land
      (if i.key_select then 0x0B else 0xFF)
      land
      (if i.key_b      then 0x0D else 0xFF)
      land
      (if i.key_a      then 0x0E else 0xFF)
    }
  else if (v land 0x10) = 0 then
    Some { i with mask =
      0x2F
      land
      (if i.key_up     then 0x07 else 0xFF)
      land
      (if i.key_down   then 0x0B else 0xFF)
      land
      (if i.key_left   then 0x0D else 0xFF)
      land
      (if i.key_right  then 0x0E else 0xFF)
    }
  else
    Some i

let tick i =
  Some (i.interrupt, { i with interrupt = false })
