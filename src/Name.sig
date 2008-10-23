(* ========================================================================= *)
(* NAMES                                                                     *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Name =
sig

(* ------------------------------------------------------------------------- *)
(* A type of names.                                                          *)
(* ------------------------------------------------------------------------- *)

type name

(***
val toString : name -> string

val fromString : string -> name
***)

val mkVarName : string -> name
val destVarName : name -> string

val mkFnName : string -> name
val destFnName : name -> string

val mkRelName : string -> name
val destRelName : name -> string

(* ------------------------------------------------------------------------- *)
(* A total ordering.                                                         *)
(* ------------------------------------------------------------------------- *)

val compare : name * name -> order

val equal : name -> name -> bool

(* ------------------------------------------------------------------------- *)
(* Fresh names.                                                              *)
(* ------------------------------------------------------------------------- *)

val variantPrime : (name -> bool) -> name -> name

val variantNum : (name -> bool) -> name -> name

(* ------------------------------------------------------------------------- *)
(* Parsing and pretty printing.                                              *)
(* ------------------------------------------------------------------------- *)

val pp : name Print.pp

end
