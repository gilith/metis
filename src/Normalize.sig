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
(* Normalization derivations.                                                *)
(* ------------------------------------------------------------------------- *)

datatype derivationStep =
    Axiom of string
  | Conjecture of string
  | Definition of string * Formula.formula
  | Generalization
  | Negation
  | Simplification
  | Conjunct
  | Specialization
  | Skolemization
  | Clausification

type derivedFormula

datatype derivation = Derivation of derivationStep * derivedFormula list

val axiomDerivation : string -> derivation

val conjectureDerivation : string -> derivation

val generalizationDerivation : derivedFormula -> derivation

val negationDerivation : derivedFormula -> derivation

val mkDerivedFormula : Formula.formula * derivation -> derivedFormula

val destDerivedFormula : derivedFormula -> Formula.formula * derivation

val deriveFormulas :
    derivedFormula list ->
    (Formula.formula * derivationStep * Formula.formula list) list

(* ------------------------------------------------------------------------- *)
(* Normalization formula derivation rules.                                   *)
(* ------------------------------------------------------------------------- *)

val deriveAxiom : Formula.formula -> string -> derivedFormula

val deriveConjecture : Formula.formula -> string -> derivedFormula

val deriveGeneralization : derivedFormula -> derivedFormula

val deriveNegation : derivedFormula -> derivedFormula

(* ------------------------------------------------------------------------- *)
(* Conjunctive normal form.                                                  *)
(* ------------------------------------------------------------------------- *)

type cnf

val initialCnf : cnf

val addCnf : derivedFormula -> cnf -> (Thm.clause * derivation) list * cnf

val derivedCnf : derivedFormula list -> (Thm.clause * derivation) list

val cnf : Formula.formula -> Thm.clause list

end
