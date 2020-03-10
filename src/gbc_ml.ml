(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

let () =
  (* Set up components *)
  let cart = Cartridge.load (Sys.argv.(1)) in
  let gpu = Gpu.create () in
  let sound = Sound.create () in
  let mem = Memory.create cart gpu sound in
  let cpu = Cpu.create mem in

  (* Main loop *)
  let rec loop cpu =
    match Cpu.step cpu with
    | None ->  ()
    | Some cpu' -> loop cpu'
  in loop cpu
