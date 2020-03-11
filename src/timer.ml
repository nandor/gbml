(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

open Types

type freq =
  | T_4KHz
  | T_262KHz
  | T_65KHz
  | T_16KHz

type t =
  { timer_div: u8
  ; timer_counter: u8
  ; timer_modulo: u8
  ; timer_enable: bool
  ; timer_freq: freq
  ; clock_4: int
  ; clock_4_16: int
  ; clock_main: int
  }


let create () =
  { timer_div = 0
  ; timer_counter = 0
  ; timer_modulo = 0
  ; timer_enable = false
  ; timer_freq = T_4KHz
  ; clock_4 = 0
  ; clock_4_16 = 0
  ; clock_main = 0
  }

let clear_div t = Some { t with timer_div = 0 }
let get_div t  = Some t.timer_div

let set_counter t timer_counter = Some { t with timer_counter }
let get_counter t = Some t.timer_counter

let set_modulo t timer_modulo = Some { t with timer_modulo }
let get_modulo t = Some t.timer_modulo

let set_control t timer_enable timer_freq =
  Some { t with timer_enable; timer_freq }


let tick t =
  (* First, update the timer_div *)
  let t =
    if t.clock_4 < 3 then
      { t with clock_4 = t.clock_4 + 1 }
    else if t.clock_4_16 < 15 then
      let clock_main = t.clock_main + 1 in
      { t with clock_4 = 0; clock_4_16 = t.clock_4_16 + 1; clock_main }
    else
      let clock_main = t.clock_main + 1 in
      let timer_div = (t.timer_div + 1) land 0xFF in
      { t with clock_4 = 0; clock_4_16 = 0; clock_main; timer_div; }
  in

  (* If timer is not enabled, move on. *)
  if t.timer_enable then begin
    flush_all ();
    let threshold =
      match t.timer_freq with
      | T_4KHz   -> 64
      | T_262KHz -> 1
      | T_65KHz  -> 4
      | T_16KHz  -> 16
    in
    if t.clock_main < threshold then
      Some (false, t)
    else
      let timer_counter = t.timer_counter + 1 in
      if timer_counter <= 255 then
        Some (false, { t with clock_main = 0; timer_counter })
      else
        Some (true, { t with clock_main = 0; timer_counter = t.timer_modulo })
  end else
    Some (false, t)
