(* ========================================================================= *)
(* NORMALIZING FORMULAS                                                      *)
(* Copyright (c) 2001-2009 Joe Hurd, distributed under the GNU GPL version 2 *)
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
    Axiom of string
  | Conjecture of string
  | Definition of string * Formula.formula
  | Negation
  | Simplification
  | Conjunct
  | Specialization
  | Skolemization
  | Clausification

type thm

datatype proof = Proof of inference * thm list

val axiomProof : string -> proof

val conjectureProof : string -> proof

val negationProof : thm -> proof

val mkThm : Formula.formula * proof -> thm

val destThm : thm -> Formula.formula * proof

val formulaThm : thm -> Formula.formula

val proofThm : thm -> proof

val axiomThm : Formula.formula -> string -> thm

val conjectureThm : Formula.formula -> string -> thm

val negationThm : thm -> thm

val proveThms :
    thm list -> (Formula.formula * inference * Formula.formula list) list

(* ------------------------------------------------------------------------- *)
(* Conjunctive normal form.                                                  *)
(* ------------------------------------------------------------------------- *)

type cnf

val initialCnf : cnf

val addCnf : thm -> cnf -> (Thm.clause * proof) list * cnf

val thmCnf : thm list -> (Thm.clause * proof) list

val cnf : Formula.formula -> Thm.clause list

end
