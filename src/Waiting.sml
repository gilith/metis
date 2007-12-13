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

type modelParameters =
     {model : Model.parameters,
      checks : int,
      weight : real};

type parameters =
     {symbolsWeight : real,
      literalsWeight : real,
      models : modelParameters list};

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
      models =
        [{model = {size = 4,
                   fixed =
                     Model.fixedMergeList
                       [Model.fixedPure,
                        Model.fixedBasic,
                        Model.fixedModulo,
                        Model.fixedSet]},
          checks = 20,
          weight = 1.0}]};

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

  fun checkPerturb model cls =
      let
        val modelSize = {size = Model.size model}

        fun check (fv,cl) n =
            let
              val v = Model.valuationRandom modelSize fv
            in
              if Model.interpretClause model v cl then n + 1
              else
                let
                  val () = Model.perturbClause model v cl
                in
                  n
                end
            end

        val fv_cls =
            let
              fun mk cl =
                  let
                    val lits = Clause.literals cl
                    val fvs = LiteralSet.freeVars lits
                  in
                    (fvs,lits)
                  end
            in
              map mk cls
            end
      in
        zipWith check fv_cls
      end;

  fun clauseModelWeights (parm,model) cls =
      let
        val {checks,weight,...} : modelParameters = parm

        val countToWeight =
            let
              val realChecks = Real.fromInt checks
            in
              fn n => Math.pow (1.0 + Real.fromInt n / realChecks, weight)
            end

        val counts = map (K 0) cls
        val counts = funpow checks (checkPerturb model cls) counts
      in
        map countToWeight counts
      end;

  fun priority cl = 1e~12 * Real.fromInt (Clause.id cl);

  fun clauseWeight (parm : parameters) dist modelsW cl =
      let
(*TRACE3
        val () = Parser.ppTrace Clause.pp "Waiting.clauseWeight: cl" cl
*)
        val {symbolsWeight,literalsWeight,...} = parm
        val lits = Clause.literals cl
        val symbolsW = Math.pow (clauseSymbols lits, symbolsWeight)
        val literalsW = Math.pow (clauseLiterals lits, literalsWeight)
(*TRACE4
        val () = trace ("Waiting.clauseWeight: dist = " ^
                        Real.toString dist ^ "\n")
        val () = trace ("Waiting.clauseWeight: symbolsW = " ^
                        Real.toString symbolsW ^ "\n")
        val () = trace ("Waiting.clauseWeight: literalsW = " ^
                        Real.toString literalsW ^ "\n")
        val () = trace ("Waiting.clauseWeight: modelW = " ^
                        Real.toString modelW ^ "\n")
*)
        val weight = dist * symbolsW * literalsW * modelsW + priority cl
(*TRACE3
        val () = trace ("Waiting.clauseWeight: weight = " ^
                        Real.toString weight ^ "\n")
*)
      in
        (cl,weight)
      end;
in
  fun clauseWeights parm models dist cls =
      let
        val {models = modelsParm, ...} = parm
      in
        case zip modelsParm models of
          [] => map (clauseWeight parm dist 1.0) cls
        | m :: ms =>
          let
            fun inc (m,w) = zipWith (curry op* ) w (clauseModelWeights m cls)
            val modelsW = clauseModelWeights m cls
            val modelsW = foldl inc modelsW ms
          in
            zipWith (clauseWeight parm dist) modelsW cls
          end
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

      val cl_weights = clauseWeights parameters models dist cls

      fun f ((cl,weight),acc) = Heap.add acc (weight,(dist,cl))

      val clauses = foldl f clauses cl_weights

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
        and models = map (Model.new o #model) (#models parameters)
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
