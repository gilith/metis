(* ========================================================================= *)
(* NAME/ARITY PAIRS                                                          *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature NameArity =
sig

type nameArity = Name.name * int

val name : nameArity -> Name.name

val arity : nameArity -> int

val nary : int -> nameArity -> bool

val nullary : nameArity -> bool
val unary : nameArity -> bool
val binary : nameArity -> bool
val ternary : nameArity -> bool

val compare : nameArity * nameArity -> order

val pp : nameArity Print.pp

end
