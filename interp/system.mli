(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

type t

type interrupt =
  | Int_VBlank
  | Int_Stat
  | Int_Timer
  | Int_Serial
  | Int_Pins

val create : Cartridge.t -> Gpu.t -> Sound.t -> Timer.t -> Input.t -> t

val read : t -> Types.u16 -> Types.u8 option
val write : t -> Types.u16 -> Types.u8 -> t option

val is_interrupt_enabled : t -> interrupt -> bool
val is_interrupt_pending : t -> interrupt -> bool
val clear_interrupt : t -> interrupt -> t option

val tick : t -> t option
