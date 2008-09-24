(* ========================================================================= *)
(* THE RESOLUTION PROOF PROCEDURE                                            *)
(* Copyright (c) 2001-2007 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Resolution :> Resolution =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of resolution proof procedures.                                    *)
(* ------------------------------------------------------------------------- *)

type parameters =
     {active : Active.parameters,
      waiting : Waiting.parameters};

datatype resolution =
    Resolution of
      {parameters : parameters,
       active : Active.active,
       waiting : Waiting.waiting};

(* ------------------------------------------------------------------------- *)
(* Basic operations.                                                         *)
(* ------------------------------------------------------------------------- *)

val default : parameters =
    {active = Active.default,
     waiting = Waiting.default};

fun new parameters ths =
    let
      val {active = activeParm, waiting = waitingParm} = parameters
      val (active,cls) = Active.new activeParm ths  (* cls = factored ths *)
      val waiting = Waiting.new waitingParm cls
    in
      Resolution {parameters = parameters, active = active, waiting = waiting}
    end;

fun active (Resolution {active = a, ...}) = a;

fun waiting (Resolution {waiting = w, ...}) = w;

val pp =
    Print.ppMap
      (fn Resolution {active,waiting,...} =>
          "Resolution(" ^ Int.toString (Active.size active) ^
          "<-" ^ Int.toString (Waiting.size waiting) ^ ")")
      Print.ppString;

(* ------------------------------------------------------------------------- *)
(* The main proof loop.                                                      *)
(* ------------------------------------------------------------------------- *)

datatype decision =
    Contradiction of Thm.thm
  | Satisfiable of Thm.thm list;

datatype state =
    Decided of decision
  | Undecided of resolution;

fun iterate resolution =
    let
      val Resolution {parameters,active,waiting} = resolution
(*MetisTrace2
      val () = Print.trace Active.pp "Resolution.iterate: active" active
      val () = Print.trace Waiting.pp "Resolution.iterate: waiting" waiting
*)
    in
      case Waiting.remove waiting of
        NONE =>
        Decided (Satisfiable (map Clause.thm (Active.saturation active)))
      | SOME ((d,cl),waiting) =>
        if Clause.isContradiction cl then
          Decided (Contradiction (Clause.thm cl))
        else
          let
(*MetisTrace1
            val () = Print.trace Clause.pp "Resolution.iterate: cl" cl
*)
            val (active,cls) = Active.add active cl
            val waiting = Waiting.add waiting (d,cls)
          in
            Undecided
              (Resolution
                 {parameters = parameters, active = active, waiting = waiting})
          end
    end;

fun loop resolution =
    case iterate resolution of
      Decided decision => decision
    | Undecided resolution => loop resolution;

end
