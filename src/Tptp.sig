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
(* Clause information.                                                       *)
(* ------------------------------------------------------------------------- *)

type 'a clauseInfo = 'a LiteralSetMap.map

type clauseNames = string clauseInfo

type clauseRoles = string clauseInfo

type clauseProofs = Normalize.proof clauseInfo

val noClauseNames : clauseNames

val allClauseNames : clauseNames -> StringSet.set

val noClauseRoles : clauseRoles

val noClauseProofs : clauseProofs

(* ------------------------------------------------------------------------- *)
(* TPTP problems.                                                            *)
(* ------------------------------------------------------------------------- *)

type comments = string list

type problem = {comments : comments, formulas : formula list}

val isCnfProblem : problem -> bool

val isFofProblem : problem -> bool

val hasConjecture : problem -> bool

val mkCnfProblem : {comments : comments,
                    names : clauseNames,
                    roles : clauseRoles,
                    problem : Problem.problem} -> problem

val destCnfProblem : problem -> {comments : comments,
                                 names : clauseNames,
                                 roles : clauseRoles,
                                 problem : Problem.problem}

val normalizeFof : problem -> {definitions : (string * Formula.formula) list,
                               roles : clauseRoles,
                               problem : Problem.problem,
                               proofs : clauseProofs} list

val normalizeFofToCnf : problem -> problem list

val goal : problem -> Formula.formula

val read : {filename : string} -> problem

val write : {filename : string} -> problem -> unit

val prove : {filename : string} -> bool

(* ------------------------------------------------------------------------- *)
(* TSTP proofs.                                                              *)
(* ------------------------------------------------------------------------- *)

val writeProof : {filename : string,
                  prefix : string,
                  names : clauseNames,
                  proofs : clauseProofs} -> Proof.proof -> unit

end
