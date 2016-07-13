(* ========================================================================= *)
(* ORDERED TYPES                                                             *)
(* Copyright (c) 2004 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Ordered =
sig

type t

val compare : t * t -> order

(*
  PROVIDES

  !x : t. compare (x,x) = EQUAL

  !x y : t. compare (x,y) = LESS <=> compare (y,x) = GREATER

  !x y : t. compare (x,y) = EQUAL ==> compare (y,x) = EQUAL

  !x y z : t. compare (x,y) = EQUAL ==> compare (x,z) = compare (y,z)

  !x y z : t.
    compare (x,y) = LESS andalso compare (y,z) = LESS ==>
    compare (x,z) = LESS

  !x y z : t.
    compare (x,y) = GREATER andalso compare (y,z) = GREATER ==>
    compare (x,z) = GREATER
*)

end
