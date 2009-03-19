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

datatype role =
    AxiomRole
  | ConjectureRole
  | DefinitionRole
  | NegatedConjectureRole
  | PlainRole
  | TheoremRole
  | OtherRole of string;

val isCnfConjectureRole : role -> bool

val isFofConjectureRole : role -> bool

val toStringRole : role -> string

val fromStringRole : string -> role

val ppRole : role Print.pp

(* ------------------------------------------------------------------------- *)
(* SZS statuses.                                                             *)
(* ------------------------------------------------------------------------- *)

datatype status =
    CounterSatisfiableStatus
  | TheoremStatus
  | SatisfiableStatus
  | UnknownStatus
  | UnsatisfiableStatus

val toStringStatus : status -> string

val ppStatus : status Print.pp

(* ------------------------------------------------------------------------- *)
(* TPTP literals.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype literal =
    Boolean of bool
  | Literal of Literal.literal

val negateLiteral : literal -> literal

val functionsLiteral : literal -> NameAritySet.set

val relationLiteral : literal -> Atom.relation option

val freeVarsLiteral : literal -> NameSet.set

(* ------------------------------------------------------------------------- *)
(* TPTP formula bodies.                                                      *)
(* ------------------------------------------------------------------------- *)

datatype formulaBody =
    CnfFormulaBody of literal list
  | FofFormulaBody of Formula.formula

(* ------------------------------------------------------------------------- *)
(* TPTP formula sources.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype formulaSource =
    NoFormulaSource
  | NormalizeFormulaSource of
      {step : Normalize.derivationStep,
       parents : string list}
  | ProofFormulaSource of
      {inference : Proof.inference,
       parents : string list}

(* ------------------------------------------------------------------------- *)
(* TPTP formulas.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype formula =
    Formula of
      {name : string,
       role : role,
       body : formulaBody,
       source : formulaSource}

val nameFormula : formula -> string

val roleFormula : formula -> role

val bodyFormula : formula -> formulaBody

val sourceFormula : formula -> formulaSource

val functionsFormula : formula -> NameAritySet.set

val relationsFormula : formula -> NameAritySet.set

val freeVarsFormula : formula -> NameSet.set

val freeVarsListFormula : formula list -> NameSet.set

val isCnfConjectureFormula : formula -> bool
val isFofConjectureFormula : formula -> bool
val isConjectureFormula : formula -> bool

(* ------------------------------------------------------------------------- *)
(* Clause information.                                                       *)
(* ------------------------------------------------------------------------- *)

type 'a clauseInfo = 'a LiteralSetMap.map

type clauseNames = string clauseInfo

type clauseRoles = role clauseInfo

type clauseDerivations = Normalize.derivation clauseInfo

val noClauseNames : clauseNames

val allClauseNames : clauseNames -> StringSet.set

val noClauseRoles : clauseRoles

val noClauseDerivations : clauseDerivations

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
    {problem : Problem.problem,
     derivations : clauseDerivations} list

val goal : problem -> Formula.formula

val read : {mapping : tptpMapping, filename : string} -> problem

val write :
    {problem : problem,
     mapping : tptpMapping,
     filename : string} -> unit

val prove : {filename : string, mapping : tptpMapping} -> bool

(* ------------------------------------------------------------------------- *)
(* TSTP proofs.                                                              *)
(* ------------------------------------------------------------------------- *)

val fromProof :
    {problem : problem,
     proofs : (clauseDerivations * Proof.proof) list} -> formula list

end
