(* ========================================================================= *)
(* THE TPTP PROBLEM FILE FORMAT (TPTP v2)                                    *)
(* Copyright (c) 2001-2007 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

signature Tptp =
sig

(* ------------------------------------------------------------------------- *)
(* Mapping to and from TPTP variable, function and relation names.           *)
(* ------------------------------------------------------------------------- *)

type tptpMapping

val defaultTptpMapping : tptpMapping

val mkTptpMapping :
    {functionMapping : {name : Name.name, arity : int, tptp : string} list,
     relationMapping : {name : Name.name, arity : int, tptp : string} list} ->
    tptpMapping

val addVarSetTptpMapping : tptpMapping -> NameSet.set -> tptpMapping

(* ------------------------------------------------------------------------- *)
(* TPTP roles.                                                               *)
(* ------------------------------------------------------------------------- *)

type role = string

val ROLE_AXIOM : role
val ROLE_CONJECTURE : role
val ROLE_DEFINITION : role
val ROLE_NEGATED_CONJECTURE : role
val ROLE_PLAIN : role
val ROLE_THEOREM : role

val roleIsCnfConjecture : role -> bool

val roleIsFofConjecture : role -> bool

(* ------------------------------------------------------------------------- *)
(* SZS Statuses.                                                             *)
(* ------------------------------------------------------------------------- *)

type status = string

val STATUS_COUNTER_SATISFIABLE : status
val STATUS_THEOREM : status
val STATUS_SATISFIABLE : status
val STATUS_UNKNOWN : status
val STATUS_UNSATISFIABLE : status

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
    CnfFormula of {name : string, role : role, clause : literal list}
  | FofFormula of {name : string, role : role, formula : Formula.formula}

val formulaName : formula -> string

val formulaFunctions : formula -> NameAritySet.set

val formulaRelations : formula -> NameAritySet.set

val formulaFreeVars : formula -> NameSet.set

val formulaListFreeVars : formula list -> NameSet.set

val formulaIsCnfConjecture : formula -> bool
val formulaIsFofConjecture : formula -> bool
val formulaIsConjecture : formula -> bool

(* ------------------------------------------------------------------------- *)
(* Clause information.                                                       *)
(* ------------------------------------------------------------------------- *)

type 'a clauseInfo = 'a LiteralSetMap.map

type clauseNames = string clauseInfo

type clauseRoles = role clauseInfo

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

val hasCnfConjecture : problem -> bool
val hasFofConjecture : problem -> bool
val hasConjecture : problem -> bool

val freeVars : problem -> NameSet.set

val mkProblem :
    {comments : comments,
     names : clauseNames,
     roles : clauseRoles,
     problem : Problem.problem} -> problem

val normalize :
    problem ->
    {definitions : (string * Formula.formula) list,
     roles : clauseRoles,
     problem : Problem.problem,
     proofs : clauseProofs} list

val goal : problem -> Formula.formula

val read : {mapping : tptpMapping, filename : string} -> problem

val write :
    {problem : problem,
     mapping : tptpMapping,
     filename : string} -> unit

(***
val prove : {filename : string, mapping : tptpMapping} -> bool
***)

(* ------------------------------------------------------------------------- *)
(* TSTP proofs.                                                              *)
(* ------------------------------------------------------------------------- *)

val writeProof :
    {proof : Proof.proof,
     mapping : tptpMapping,
     filename : string,
     avoid : StringSet.set,
     prefix : string,
     names : clauseNames,
     roles : clauseRoles,
     proofs : clauseProofs} -> unit

end
