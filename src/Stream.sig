(* ========================================================================= *)
(* A POSSIBLY-INFINITE STREAM DATATYPE FOR ML                                *)
(* Copyright (c) 2001 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

signature Stream =
sig

(* ------------------------------------------------------------------------- *)
(* The stream type.                                                          *)
(* ------------------------------------------------------------------------- *)

datatype 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

(* If you're wondering how to create an infinite stream: *)
(* val stream4 = let fun s4 () = Stream.Cons (4,s4) in s4 () end; *)

(* ------------------------------------------------------------------------- *)
(* Stream constructors.                                                      *)
(* ------------------------------------------------------------------------- *)

val repeat : 'a -> 'a stream

val count : int -> int stream

val funpows : ('a -> 'a) -> 'a -> 'a stream

(* ------------------------------------------------------------------------- *)
(* Stream versions of standard list operations: these should all terminate.  *)
(* ------------------------------------------------------------------------- *)

val cons : 'a -> (unit -> 'a stream) -> 'a stream

val null : 'a stream -> bool

val hd : 'a stream -> 'a  (* raises Empty *)

val tl : 'a stream -> 'a stream  (* raises Empty *)

val hdTl : 'a stream -> 'a * 'a stream  (* raises Empty *)

val singleton : 'a -> 'a stream

val append : 'a stream -> (unit -> 'a stream) -> 'a stream

val map : ('a -> 'b) -> 'a stream -> 'b stream

val maps :
    ('a -> 's -> 'b * 's) -> ('s -> 'b stream) -> 's -> 'a stream -> 'b stream

val zipwith : ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream

val zip : 'a stream -> 'b stream -> ('a * 'b) stream

val take : int -> 'a stream -> 'a stream  (* raises Subscript *)

val drop : int -> 'a stream -> 'a stream  (* raises Subscript *)

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a stream

(* ------------------------------------------------------------------------- *)
(* Stream versions of standard list operations: these might not terminate.   *)
(* ------------------------------------------------------------------------- *)

val length : 'a stream -> int

val exists : ('a -> bool) -> 'a stream -> bool

val all : ('a -> bool) -> 'a stream -> bool

val filter : ('a -> bool) -> 'a stream -> 'a stream

val foldl : ('a * 's -> 's) -> 's -> 'a stream -> 's

val concat : 'a stream stream -> 'a stream

val mapPartial : ('a -> 'b option) -> 'a stream -> 'b stream

val mapsPartial :
    ('a -> 's -> 'b option * 's) -> ('s -> 'b stream) -> 's ->
    'a stream -> 'b stream

val mapConcat : ('a -> 'b stream) -> 'a stream -> 'b stream

val mapsConcat :
    ('a -> 's -> 'b stream * 's) -> ('s -> 'b stream) -> 's ->
    'a stream -> 'b stream

(* ------------------------------------------------------------------------- *)
(* Stream operations.                                                        *)
(* ------------------------------------------------------------------------- *)

val primes : int stream

val memoize : 'a stream -> 'a stream

val listConcat : 'a list stream -> 'a stream

val concatList : 'a stream list -> 'a stream

val toList : 'a stream -> 'a list

val fromList : 'a list -> 'a stream

val toString : char stream -> string

val fromString : string -> char stream

val toTextFile : {filename : string} -> string stream -> unit

val fromTextFile : {filename : string} -> string stream  (* line by line *)

end
