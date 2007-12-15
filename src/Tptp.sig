(* ========================================================================= *)
(* THE TPTP PROBLEM FILE FORMAT (TPTP v2)                                    *)
(* Copyright (c) 2001-2007 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Tptp =
sig

(* ------------------------------------------------------------------------- *)
(* Mapping TPTP functions and relations to different names.                  *)
(* ------------------------------------------------------------------------- *)

val functionMapping : {name : string, arity : int, tptp : string} list ref

val relationMapping : {name : string, arity : int, tptp : string} list ref

(* ------------------------------------------------------------------------- *)
(* TPTP literals.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype literal =
    Boolean of bool
  | Literal of Literal.literal

val negate : literal -> literal

val literalFunctions : literal -> NameAritySet.set

val literalRelation : literal -> Atom.relation option

val literalFreeVars : literal -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* TPTP formulas.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype formula =
    CnfFormula of {name : string, role : string, clause : literal list}
  | FofFormula of {name : string, role : string, formula : Formula.formula}

val formulaFunctions : formula -> NameAritySet.set

val formulaRelations : formula -> NameAritySet.set

val formulaFreeVars : formula -> NameSet.set

val formulaIsConjecture : formula -> bool

(* ------------------------------------------------------------------------- *)
(* TPTP problems.                                                            *)
(* ------------------------------------------------------------------------- *)

type axiomNames = string LiteralSetMap.map

type problem = {comments : string list, formulas : formula list}

val isCnfProblem : problem -> bool

val isFofProblem : problem -> bool

val hasConjecture : problem -> bool

val normalizeFofToCnf : problem -> problem list

val mkCnfProblem : Problem.problem * axiomNames -> problem

val destCnfProblem : problem -> Problem.problem * axiomNames

val read : {filename : string} -> problem

val write : {filename : string} -> problem -> unit

val prove : {filename : string} -> bool

(* ------------------------------------------------------------------------- *)
(* TSTP proofs.                                                              *)
(* ------------------------------------------------------------------------- *)

val writeProof : {filename : string} -> axiomNames -> Proof.proof -> unit

end
