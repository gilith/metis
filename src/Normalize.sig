(* ========================================================================= *)
(* NORMALIZING FORMULAS                                                      *)
(* Copyright (c) 2001-2007 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Normalize =
sig

(* ------------------------------------------------------------------------- *)
(* Negation normal form.                                                     *)
(* ------------------------------------------------------------------------- *)

val nnf : Formula.formula -> Formula.formula

(* ------------------------------------------------------------------------- *)
(* Normalization proofs.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype inference =
    Axiom of string * Formula.formula
  | Conjecture of string * Formula.formula
  | Definition of string * Formula.formula
  | Negation
  | Simplification
  | Conjunct
  | Specialization
  | Skolemization

type thm

val destThm : thm -> Formula.formula * inference * thm list

val axiomThm : string -> Formula.formula -> thm

val conjectureThm : string -> Formula.formula -> thm

val negationThm : thm -> thm

val thmProof :
    thm list -> (Formula.formula * inference * Formula.formula list) list

(* ------------------------------------------------------------------------- *)
(* Conjunctive normal form.                                                  *)
(* ------------------------------------------------------------------------- *)

type cnf

val initialCnf : cnf

val addCnf : thm -> cnf -> (Thm.clause * thm) list * cnf

val thmCnf : thm list -> (Thm.clause * thm) list

val cnf : Formula.formula -> Thm.clause list

end
