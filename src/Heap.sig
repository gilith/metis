(* ========================================================================= *)
(* A HEAP DATATYPE FOR ML                                                    *)
(* Copyright (c) 2001 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Heap =
sig

type 'a heap

val new : ('a * 'a -> order) -> 'a heap

val add : 'a heap -> 'a -> 'a heap

val null : 'a heap -> bool

val top : 'a heap -> 'a  (* raises Empty *)

val remove : 'a heap -> 'a * 'a heap  (* raises Empty *)

val size : 'a heap -> int

val app : ('a -> unit) -> 'a heap -> unit

val toList : 'a heap -> 'a list

val toStream : 'a heap -> 'a Stream.stream

val toString : 'a heap -> string

end
