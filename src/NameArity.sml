(* ========================================================================= *)
(* NAME/ARITY PAIRS                                                          *)
(* Copyright (c) 2004-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure NameArity :> NameArity =
struct

type nameArity = Name.name * int;

fun name ((n,_) : nameArity) = n;

fun arity ((_,i) : nameArity) = i;

fun nary i n_i = arity n_i = i;

val nullary = nary 0
and unary = nary 1
and binary = nary 2
and ternary = nary 3;

fun compare ((n1,i1),(n2,i2)) =
    case Name.compare (n1,n2) of
      LESS => LESS
    | EQUAL => Int.compare (i1,i2)
    | GREATER => GREATER;

val pp = Print.ppMap (fn (n,i) => n ^ "/" ^ Int.toString i) Print.ppString;

end

structure NameArityOrdered =
struct type t = NameArity.nameArity val compare = NameArity.compare end

structure NameAritySet =
struct

  local
    structure S = ElementSet (NameArityOrdered);
  in
    open S;
  end;

  val allNullary = all NameArity.nullary;

  val pp =
      Print.ppMap
        toList
        (Print.ppBracket "{" "}" (Print.ppOpList "," NameArity.pp));

end

structure NameArityMap = KeyMap (NameArityOrdered);
