(* ========================================================================= *)
(* SUPPORT FOR LAZY EVALUATION                                               *)
(* Copyright (c) 2007 Joe Leslie-Hurd, distributed under the MIT license     *)
(* ========================================================================= *)

structure Lazy :> Lazy =
struct

datatype 'a thunk =
    Value of 'a
  | Thunk of unit -> 'a;

datatype 'a lazy = Lazy of 'a thunk ref;

fun quickly v = Lazy (ref (Value v));

fun delay f = Lazy (ref (Thunk f));

fun force (Lazy s) =
    case !s of
      Value v => v
    | Thunk f =>
      let
        val v = f ()

        val () = s := Value v
      in
        v
      end;

fun memoize f =
    let
      val t = delay f
    in
      fn () => force t
    end;

end
