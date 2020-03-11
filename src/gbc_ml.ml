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
  let mem = System.create cart gpu sound timer input in
  let cpu = Cpu.create mem in

  (* Main loop *)
  let rec loop cpu =
    match Cpu.tick cpu with
    | None ->  ()
    | Some cpu' -> loop cpu'
  in loop cpu
