(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

let () =
  (* Set up components *)
  let cart = Cartridge.load (Sys.argv.(1)) in
  let timer = Timer.create () in
  let gpu = Gpu.create () in
  let sound = Sound.create () in
  let input = Input.create () in
  let sys = System.create cart gpu sound timer input in

  match Sys.argv.(2) with
  | "v" ->
    let rec loop_v cpu =
      match Cpu_v.tick cpu with
      | None ->  ()
      | Some cpu' -> loop_v cpu'
    in loop_v (Cpu_v.create sys)
  | "ml" ->
    let rec loop_ml cpu =
      match Cpu_ml.tick cpu with
      | None ->  ()
      | Some cpu' -> loop_ml cpu'
    in loop_ml (Cpu_ml.create sys)
  | _ ->
    ()
