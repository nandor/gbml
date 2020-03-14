(* This file is part of the GBC-ML project. *)
(* Licensing information is available in the LICENSE file. *)
(* (C) 2020 Nandor Licker. All rights reserved. *)

type u8 = int
type i8 = int
type u16 = int


let i8_of_u8 n =
  if (n land 0x80) = 0x80 then
    (n land 0xFF) lor (lnot 0xFF)
  else
    n

let crash () = failwith "WTF"
