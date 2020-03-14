(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

let to_u8 v =
  Cpu_v.Pair((v land 0x0F) lsr 0, (v land 0xF0) lsr 4)

let from_u8 v =
  match v with
  | Cpu_v.Pair(lo, hi) -> (hi lsl 4) lor lo

let from_u16 v =
  match v with
  | Cpu_v.Pair(lo, hi) -> (from_u8 hi lsl 8) lor from_u8 lo

let rec to_sys_v sys =
  Cpu_v.S
  ( System.is_interrupt_pending
  , System.is_interrupt_enabled
  , (fun s i ->
      match System.clear_interrupt s i with
      | None -> None
      | Some sys' -> Some (to_sys_v sys')
    )
  , (fun s addr ->
      match System.read s (from_u16 addr) with
      | None -> None
      | Some v -> Some (Cpu_v.Pair(to_u8 v, to_sys_v s))
    )
  , (fun s addr v ->
      match System.write s (from_u16 addr) (from_u8 v) with
      | None -> None
      | Some sys' -> Some (to_sys_v sys')
    )
  , sys
  )

let loop_v sys =
  let rec loop_v cpu sys =
    match System.tick sys with
    | None -> ()
    | Some sys' ->
      match Cpu_v.tick cpu (to_sys_v sys') with
      | None ->  ()
      | Some Cpu_v.Pair(cpu', Cpu_v.S(_, _, _, _, _, sys'')) ->
        loop_v cpu' sys''
  in loop_v (Cpu_v.create ()) sys

let loop_ml sys =
  let rec loop_ml cpu sys =
    match System.tick sys with
    | None -> ()
    | Some sys' ->
      match Cpu_ml.tick cpu sys' with
      | None ->  ()
      | Some (cpu', sys'') -> loop_ml cpu' sys''
  in loop_ml (Cpu_ml.create ()) sys

let () =
  (* Set up components *)
  let cart = Cartridge.load (Sys.argv.(1)) in
  let timer = Timer.create () in
  let gpu = Gpu.create () in
  let sound = Sound.create () in
  let input = Input.create () in
  let sys = System.create cart gpu sound timer input in

  match Sys.argv.(2) with
  | "v" -> loop_v sys
  | "ml" -> loop_ml sys
  | _ ->
    ()
