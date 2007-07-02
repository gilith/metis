(* ========================================================================= *)
(* THE WAITING SET OF CLAUSES                                                *)
(* Copyright (c) 2002-2007 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Waiting :> Waiting =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* A type of waiting sets of clauses.                                        *)
(* ------------------------------------------------------------------------- *)

(* The parameter type controls the heuristics for clause selection.          *)
(* Increasing any of the *Weight parameters will favour clauses with low     *)
(* values of that field.                                                     *)

(* Note that there is an extra parameter of inference distance from the      *)
(* starting axioms (a.k.a. time) which has a fixed weight of 1, so all       *)
(* the other parameters should be set relative to this baseline.             *)

(* The first two parameters, symbolsWeight and literalsWeight, control the   *)
(* time:weight ratio, i.e., whether to favour clauses derived in a few       *)
(* steps from the axioms (time) or whether to favour small clauses (weight). *)
(* Small can be a combination of low number of symbols (the symbolWeight     *)
(* parameter) or literals (the literalsWeight parameter).                    *)

(* modelsWeight controls the semantic guidance. Increasing this parameter    *)
(* favours clauses that return false more often when interpreted             *)
(* modelChecks times over the given list of models.                          *)

type parameters =
     {symbolsWeight : real,
      literalsWeight : real,
      modelsWeight : real,
      modelChecks : int,
      models : Model.parameters list};

type distance = real;

type weight = real;

datatype waiting =
    Waiting of
      {parameters : parameters,
       clauses : (weight * (distance * Clause.clause)) Heap.heap,
       models : Model.model list};

(* ------------------------------------------------------------------------- *)
(* Basic operations.                                                         *)
(* ------------------------------------------------------------------------- *)

val default : parameters =
     {symbolsWeight = 1.0,
      literalsWeight = 1.0,
      modelsWeight = 1.0,
      modelChecks = 20,
      models =
        [{size = 8,
          fixed =
            Model.fixedMergeList
              [Model.fixedPure,
               Model.fixedBasic,
               Model.fixedModulo,
               Model.fixedSet]},
         {size = 5,
          fixed =
            Model.fixedMergeList
              [Model.fixedPure,
               Model.fixedBasic,
               Model.fixedOverflowNum]}]};

fun size (Waiting {clauses,...}) = Heap.size clauses;

val pp =
    Parser.ppMap
      (fn w => "Waiting{" ^ Int.toString (size w) ^ "}")
      Parser.ppString;

(*DEBUG
val pp =
    Parser.ppMap
      (fn Waiting {clauses,...} =>
          map (fn (w,(_,cl)) => (w, Clause.id cl, cl)) (Heap.toList clauses))
      (Parser.ppList (Parser.ppTriple Parser.ppReal Parser.ppInt Clause.pp));
*)

(* ------------------------------------------------------------------------- *)
(* Clause weights.                                                           *)
(* ------------------------------------------------------------------------- *)

local
  fun clauseSymbols cl = Real.fromInt (LiteralSet.typedSymbols cl);

  fun clauseLiterals cl = Real.fromInt (LiteralSet.size cl);

  fun clauseSat modelChecks models cl =
      let
        fun g {T,F} = (Real.fromInt T / Real.fromInt (T + F)) + 1.0
        fun f (m,z) = g (Model.checkClause {maxChecks = modelChecks} m cl) * z
      in
        foldl f 1.0 models
      end;

  fun priority cl = 1e~12 * Real.fromInt (Clause.id cl);
in
  fun clauseWeight (parm : parameters) models dist cl =
      let
(*TRACE3
        val () = Parser.ppTrace Clause.pp "Waiting.clauseWeight: cl" cl
*)
        val {symbolsWeight,literalsWeight,modelsWeight,modelChecks,...} = parm
        val lits = Clause.literals cl
        val symbolsW = Math.pow (clauseSymbols lits, symbolsWeight)
        val literalsW = Math.pow (clauseLiterals lits, literalsWeight)
        val modelsW = Math.pow (clauseSat modelChecks models lits, modelsWeight)
(*TRACE4
        val () = trace ("Waiting.clauseWeight: dist = " ^
                        Real.toString dist ^ "\n")
        val () = trace ("Waiting.clauseWeight: symbolsW = " ^
                        Real.toString symbolsW ^ "\n")
        val () = trace ("Waiting.clauseWeight: literalsW = " ^
                        Real.toString literalsW ^ "\n")
        val () = trace ("Waiting.clauseWeight: modelsW = " ^
                        Real.toString modelsW ^ "\n")
*)
        val weight = dist * symbolsW * literalsW * modelsW + priority cl
(*TRACE3
        val () = trace ("Waiting.clauseWeight: weight = " ^
                        Real.toString weight ^ "\n")
*)
      in
        weight
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Adding new clauses.                                                       *)
(* ------------------------------------------------------------------------- *)

fun add waiting (_,[]) = waiting
  | add waiting (dist,cls) =
    let
(*TRACE3
      val () = Parser.ppTrace pp "Waiting.add: waiting" waiting
      val () = Parser.ppTrace (Parser.ppList Clause.pp) "Waiting.add: cls" cls
*)

      val Waiting {parameters,clauses,models} = waiting

      val dist = dist + Math.ln (Real.fromInt (length cls))

      val weight = clauseWeight parameters models dist

      fun f (cl,acc) = Heap.add acc (weight cl, (dist,cl))

      val clauses = foldl f clauses cls

      val waiting =
          Waiting {parameters = parameters, clauses = clauses, models = models}

(*TRACE3
      val () = Parser.ppTrace pp "Waiting.add: waiting" waiting
*)
    in
      waiting
    end;

local
  fun cmp ((w1,_),(w2,_)) = Real.compare (w1,w2);

  fun empty parameters =
      let
        val clauses = Heap.new cmp
        and models = map Model.new (#models parameters)
      in
        Waiting {parameters = parameters, clauses = clauses, models = models}
      end;
in
  fun new parameters cls = add (empty parameters) (0.0,cls);
end;

(* ------------------------------------------------------------------------- *)
(* Removing the lightest clause.                                             *)
(* ------------------------------------------------------------------------- *)

fun remove (Waiting {parameters,clauses,models}) =
    if Heap.null clauses then NONE
    else
      let
        val ((_,dcl),clauses) = Heap.remove clauses
        val waiting =
            Waiting
              {parameters = parameters, clauses = clauses, models = models}
      in
        SOME (dcl,waiting)
      end;

end
