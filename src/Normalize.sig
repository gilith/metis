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

type proof = StringSet.set

val noProof : proof

val singletonProof : string -> proof

val combineProofs : proof -> proof -> proof

(* ------------------------------------------------------------------------- *)
(* Conjunctive normal form.                                                  *)
(* ------------------------------------------------------------------------- *)

type cnfState

val cnfStateInitial : cnfState

val cnfStateAdd :
    Formula.formula * proof -> cnfState ->
    (Thm.clause * proof) list * cnfState

val cnfProof : (Formula.formula * proof) list -> (Thm.clause * proof) list

val cnf : Formula.formula -> Thm.clause list

end
