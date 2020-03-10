(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

type t =
  { a: unit
  }

let create () = { a = () }

let vram_read _gpu _addr =
  failwith "not implemented"

let vram_write gpu _addr _val =
  Some gpu

let set_bgp gpu _val =
  Some gpu
