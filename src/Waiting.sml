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

type weight = real;

type modelParameters =
     {model : Model.parameters,
      initialPerturbations : int,
      checks : int,
      perturbations : int,
      weight : weight}

type parameters =
     {symbolsWeight : weight,
      literalsWeight : weight,
      models : modelParameters list};

type distance = real;

datatype waiting =
    Waiting of
      {parameters : parameters,
       clauses : (weight * (distance * Clause.clause)) Heap.heap,
       models : Model.model list};

(* ------------------------------------------------------------------------- *)
(* Basic operations.                                                         *)
(* ------------------------------------------------------------------------- *)

val defaultModels : modelParameters list =
    [{model =
        {size = 4,
         fixed =
           Model.fixedMergeList
             [Model.fixedPure,
              Model.fixedBasic,
              Model.fixedModulo,
              Model.fixedSet]},
      initialPerturbations = 100,
      checks = 20,
      perturbations = 0,
      weight = 1.0}];
                     
val default : parameters =
     {symbolsWeight = 1.0,
      literalsWeight = 1.0,
      models = defaultModels};

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
(* Perturbing the models.                                                    *)
(* ------------------------------------------------------------------------- *)

type modelClause = NameSet.set * Thm.clause;

fun mkModelClause cl =
    let
      val lits = Clause.literals cl
      val fvs = LiteralSet.freeVars lits
    in
      (fvs,lits)
    end;

val mkModelClauses = map mkModelClause;

fun perturbModel _ [] = K ()
  | perturbModel model cls =
    let
      val modelSize = {size = Model.size model}

      fun perturbClause (fv,cl) =
          let
            val v = Model.valuationRandom modelSize fv
          in
            if Model.interpretClause model v cl then ()
            else Model.perturbClause model v cl
          end

        fun perturbClauses 0 = ()
          | perturbClauses n =
            let
              val () = app perturbClause cls
            in
              perturbClauses (n - 1)
            end
    in
      perturbClauses
    end;

fun initialModel axioms parm =
    let
      val {model,initialPerturbations,...} : modelParameters = parm
      val m = Model.new model
      val () = perturbModel m axioms initialPerturbations
    in
      m
    end;

fun checkModels parms models (fv,cl) =
    let
      fun check ((parm,model),z) =
          let
            val {checks,weight,...} : modelParameters = parm
            val n = {maxChecks = checks}
            val {T,F} = Model.check Model.interpretClause n model fv cl
          in
            Math.pow (1.0 + Real.fromInt T / Real.fromInt (T + F), weight) * z
          end
    in
      List.foldl check 1.0 (zip parms models)
    end;

fun perturbModels parms models cls =
    let
      fun perturb (parm,model) =
          let
            val {perturbations,...} : modelParameters = parm
          in
            perturbModel model cls perturbations
          end
    in
      app perturb (zip parms models)
    end;

(* ------------------------------------------------------------------------- *)
(* Clause weights.                                                           *)
(* ------------------------------------------------------------------------- *)

local
  fun clauseSymbols cl = Real.fromInt (LiteralSet.typedSymbols cl);

  fun clauseLiterals cl = Real.fromInt (LiteralSet.size cl);

  fun priority cl = 1e~12 * Real.fromInt (Clause.id cl);
in
  fun clauseWeight (parm : parameters) mods dist mcl cl =
      let
(*TRACE3
        val () = Parser.ppTrace Clause.pp "Waiting.clauseWeight: cl" cl
*)
        val {symbolsWeight,literalsWeight,models,...} = parm
        val lits = Clause.literals cl
        val symbolsW = Math.pow (clauseSymbols lits, symbolsWeight)
        val literalsW = Math.pow (clauseLiterals lits, literalsWeight)
        val modelsW = checkModels models mods mcl
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

fun add' waiting dist mcls cls =
    let
      val Waiting {parameters,clauses,models} = waiting
      val {models = modelParameters, ...} = parameters

      val dist = dist + Math.ln (Real.fromInt (length cls))

      fun addCl ((mcl,cl),acc) =
          let
            val weight = clauseWeight parameters models dist mcl cl
          in
            Heap.add acc (weight,(dist,cl))
          end

      val clauses = List.foldl addCl clauses (zip mcls cls)

      val () = perturbModels modelParameters models mcls
    in
      Waiting {parameters = parameters, clauses = clauses, models = models}
    end;

fun add waiting (_,[]) = waiting
  | add waiting (dist,cls) =
    let
(*TRACE3
      val () = Parser.ppTrace pp "Waiting.add: waiting" waiting
      val () = Parser.ppTrace (Parser.ppList Clause.pp) "Waiting.add: cls" cls
*)

      val waiting = add' waiting dist (mkModelClauses cls) cls 

(*TRACE3
      val () = Parser.ppTrace pp "Waiting.add: waiting" waiting
*)
    in
      waiting
    end;

local
  fun cmp ((w1,_),(w2,_)) = Real.compare (w1,w2);

  fun empty parameters axioms =
      let
        val {models = modelParameters, ...} = parameters
        val clauses = Heap.new cmp
        and models = map (initialModel axioms) modelParameters
      in
        Waiting {parameters = parameters, clauses = clauses, models = models}
      end;
in
  fun new parameters {axioms,conjecture} =
      let
        val mAxioms = mkModelClauses axioms
        and mConjecture = mkModelClauses conjecture

        val waiting = empty parameters mAxioms
      in
        add' waiting 0.0 (mAxioms @ mConjecture) (axioms @ conjecture)
      end;
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
