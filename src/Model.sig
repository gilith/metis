(* ========================================================================= *)
(* RANDOM FINITE MODELS                                                      *)
(* Copyright (c) 2003 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

signature Model =
sig

(* ------------------------------------------------------------------------- *)
(* A model of size N has integer elements 0...N-1.                           *)
(* ------------------------------------------------------------------------- *)

type element = int

val incrementElement : {size : int} -> element -> element option

(* ------------------------------------------------------------------------- *)
(* The parts of the model that are fixed.                                    *)
(* ------------------------------------------------------------------------- *)

type fixed =
     {size : int} ->
     {functions : Term.functionName * element list -> element option,
      relations : Atom.relationName * element list -> bool option}

val fixedMerge : fixed -> fixed -> fixed  (* Prefers the second fixed *)

val fixedMergeList : fixed list -> fixed

val fixedPure : fixed  (* = *)

val fixedBasic : fixed  (* : id fst snd #1 #2 #3 <> *)

val fixedModulo : fixed  (* <numerals> suc pre ~ + - * exp div mod *)
                         (* is_0 divides even odd *)

val fixedOverflowNum : fixed  (* <numerals> suc pre + - * exp div mod *)
                              (* is_0 <= < >= > divides even odd *)

val fixedOverflowInt : fixed  (* <numerals> suc pre + - * exp div mod *)
                              (* is_0 <= < >= > divides even odd *)

val fixedSet : fixed  (* empty univ union intersect compl card in subset *)

val fixedList : fixed  (* nil :: @ *)

(* ------------------------------------------------------------------------- *)
(* A type of random finite models.                                           *)
(* ------------------------------------------------------------------------- *)

type parameters = {size : int, fixed : fixed}

type model

val new : parameters -> model

val size : model -> int

(* ------------------------------------------------------------------------- *)
(* Valuations.                                                               *)
(* ------------------------------------------------------------------------- *)

type valuation

val emptyValuation : valuation

val zeroValuation : NameSet.set -> valuation

val constantValuation : element -> NameSet.set -> valuation

val peekValuation : valuation -> Name.name -> element option

val getValuation : valuation -> Name.name -> element

val insertValuation : valuation -> Name.name * element -> valuation

val randomValuation : {size : int} -> NameSet.set -> valuation

val incrementValuation :
    {size : int} -> NameSet.set -> valuation -> valuation option

val foldValuation :
    {size : int} -> NameSet.set -> (valuation * 'a -> 'a) -> 'a -> 'a

(* ------------------------------------------------------------------------- *)
(* Interpreting terms and formulas in the model.                             *)
(* ------------------------------------------------------------------------- *)

val interpretTerm : model -> valuation -> Term.term -> element

val interpretAtom : model -> valuation -> Atom.atom -> bool

val interpretFormula : model -> valuation -> Formula.formula -> bool

val interpretLiteral : model -> valuation -> Literal.literal -> bool

val interpretClause : model -> valuation -> Thm.clause -> bool

(* ------------------------------------------------------------------------- *)
(* Check whether random groundings of a formula are true in the model.       *)
(* Note: if it's cheaper, a systematic check will be performed instead.      *)
(* ------------------------------------------------------------------------- *)

val check :
    (model -> valuation -> 'a -> bool) -> {maxChecks : int option} -> model ->
    NameSet.set -> 'a -> {T : int, F : int}

val checkAtom :
    {maxChecks : int option} -> model -> Atom.atom -> {T : int, F : int}

val checkFormula :
    {maxChecks : int option} -> model -> Formula.formula -> {T : int, F : int}

val checkLiteral :
    {maxChecks : int option} -> model -> Literal.literal -> {T : int, F : int}

val checkClause :
    {maxChecks : int option} -> model -> Thm.clause -> {T : int, F : int}

(* ------------------------------------------------------------------------- *)
(* Perturbing the model.                                                     *)
(* ------------------------------------------------------------------------- *)

val perturbFunction :
    model -> (Term.functionName * element list) * element -> unit

val perturbRelation :
    model -> (Atom.relationName * element list) * bool -> unit

(* Choosing a random perturbation to make a formula true *)

val perturbTerm : model -> valuation -> Term.term * element list -> unit

val perturbAtom : model -> valuation -> Atom.atom * bool -> unit

val perturbLiteral : model -> valuation -> Literal.literal -> unit

val perturbClause : model -> valuation -> Thm.clause -> unit

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val pp : model Print.pp

end
