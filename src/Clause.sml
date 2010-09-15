(* ========================================================================= *)
(* CLAUSE = ID + THEOREM                                                     *)
(* Copyright (c) 2002 Joe Hurd, distributed under the MIT license            *)
(* ========================================================================= *)

structure Clause :> Clause =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

val newId =
    let
      val r = ref 0

      fun new () =
          let
            val ref n = r

            val () = r := n + 1
          in
            n
          end
    in
      fn () => Portable.critical new ()
    end;

(* ------------------------------------------------------------------------- *)
(* A type of clause.                                                         *)
(* ------------------------------------------------------------------------- *)

datatype literalOrder =
    NoLiteralOrder
  | UnsignedLiteralOrder
  | PositiveLiteralOrder;

type parameters =
     {ordering : KnuthBendixOrder.kbo,
      orderLiterals : literalOrder,
      orderTerms : bool};

type clauseId = int;

type clauseInfo = {parameters : parameters, id : clauseId, thm : Thm.thm};

datatype clause = Clause of clauseInfo;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

val showId = ref false;

local
  val ppIdThm = Print.ppPair Print.ppInt Thm.pp;
in
  fun pp (Clause {id,thm,...}) =
      if !showId then ppIdThm (id,thm) else Thm.pp thm;
end;

fun toString cl = Print.toString pp cl;

(* ------------------------------------------------------------------------- *)
(* Basic operations.                                                         *)
(* ------------------------------------------------------------------------- *)

val default : parameters =
    {ordering = KnuthBendixOrder.default,
     orderLiterals = UnsignedLiteralOrder,
     orderTerms = true};

fun mk info = Clause info

fun dest (Clause info) = info;

fun id (Clause {id = i, ...}) = i;

fun thm (Clause {thm = th, ...}) = th;

fun equalThms cl cl' = Thm.equal (thm cl) (thm cl');

fun new parameters thm =
    Clause {parameters = parameters, id = newId (), thm = thm};

fun literals cl = Thm.clause (thm cl);

fun isTautology (Clause {thm,...}) = Thm.isTautology thm;

fun isContradiction (Clause {thm,...}) = Thm.isContradiction thm;

(* ------------------------------------------------------------------------- *)
(* The term ordering is used to cut down inferences.                         *)
(* ------------------------------------------------------------------------- *)

fun strictlyLess ordering x_y =
    case KnuthBendixOrder.compare ordering x_y of
      SOME LESS => true
    | _ => false;

fun isLargerTerm ({ordering,orderTerms,...} : parameters) l_r =
    not orderTerms orelse not (strictlyLess ordering l_r);

local
  fun atomToTerms atm =
      case total Atom.destEq atm of
        NONE => [Term.Fn atm]
      | SOME (l,r) => [l,r];

  fun notStrictlyLess ordering (xs,ys) =
      let
        fun less x = List.exists (fn y => strictlyLess ordering (x,y)) ys
      in
        not (List.all less xs)
      end;
in
  fun isLargerLiteral ({ordering,orderLiterals,...} : parameters) lits =
      case orderLiterals of
        NoLiteralOrder => K true
      | UnsignedLiteralOrder =>
        let
          fun addLit ((_,atm),acc) = atomToTerms atm @ acc

          val tms = LiteralSet.foldl addLit [] lits
        in
          fn (_,atm') => notStrictlyLess ordering (atomToTerms atm', tms)
        end
      | PositiveLiteralOrder =>
        case LiteralSet.findl (K true) lits of
          NONE => K true
        | SOME (pol,_) =>
          let
            fun addLit ((p,atm),acc) =
                if p = pol then atomToTerms atm @ acc else acc

            val tms = LiteralSet.foldl addLit [] lits
          in
            fn (pol',atm') =>
               if pol <> pol' then pol
               else notStrictlyLess ordering (atomToTerms atm', tms)
          end;
end;

fun largestLiterals (Clause {parameters,thm,...}) =
    let
      val litSet = Thm.clause thm
      val isLarger = isLargerLiteral parameters litSet
      fun addLit (lit,s) = if isLarger lit then LiteralSet.add s lit else s
    in
      LiteralSet.foldr addLit LiteralSet.empty litSet
    end;

(*MetisTrace6
val largestLiterals = fn cl =>
    let
      val ppResult = LiteralSet.pp
      val () = Print.trace pp "Clause.largestLiterals: cl" cl
      val result = largestLiterals cl
      val () = Print.trace ppResult "Clause.largestLiterals: result" result
    in
      result
    end;
*)

fun largestEquations (cl as Clause {parameters,...}) =
    let
      fun addEq lit ort (l_r as (l,_)) acc =
          if isLargerTerm parameters l_r then (lit,ort,l) :: acc else acc

      fun addLit (lit,acc) =
          case total Literal.destEq lit of
            NONE => acc
          | SOME (l,r) =>
            let
              val acc = addEq lit Rewrite.RightToLeft (r,l) acc
              val acc = addEq lit Rewrite.LeftToRight (l,r) acc
            in
              acc
            end
    in
      LiteralSet.foldr addLit [] (largestLiterals cl)
    end;

local
  fun addLit (lit,acc) =
      let
        fun addTm ((path,tm),acc) = (lit,path,tm) :: acc
      in
        List.foldl addTm acc (Literal.nonVarTypedSubterms lit)
      end;
in
  fun largestSubterms cl = LiteralSet.foldl addLit [] (largestLiterals cl);

  fun allSubterms cl = LiteralSet.foldl addLit [] (literals cl);
end;

(* ------------------------------------------------------------------------- *)
(* Subsumption.                                                              *)
(* ------------------------------------------------------------------------- *)

fun subsumes (subs : clause Subsume.subsume) cl =
    Subsume.isStrictlySubsumed subs (literals cl);

(* ------------------------------------------------------------------------- *)
(* Simplifying rules: these preserve the clause id.                          *)
(* ------------------------------------------------------------------------- *)

fun freshVars (Clause {parameters,id,thm}) =
    Clause {parameters = parameters, id = id, thm = Rule.freshVars thm};

fun simplify (Clause {parameters,id,thm}) =
    case Rule.simplify thm of
      NONE => NONE
    | SOME thm => SOME (Clause {parameters = parameters, id = id, thm = thm});

fun reduce units (Clause {parameters,id,thm}) =
    Clause {parameters = parameters, id = id, thm = Units.reduce units thm};

fun rewrite rewr (cl as Clause {parameters,id,thm}) =
    let
      fun simp th =
          let
            val {ordering,...} = parameters
            val cmp = KnuthBendixOrder.compare ordering
          in
            Rewrite.rewriteIdRule rewr cmp id th
          end

(*MetisTrace4
      val () = Print.trace Rewrite.pp "Clause.rewrite: rewr" rewr
      val () = Print.trace Print.ppInt "Clause.rewrite: id" id
      val () = Print.trace pp "Clause.rewrite: cl" cl
*)

      val thm =
          case Rewrite.peek rewr id of
            NONE => simp thm
          | SOME ((_,thm),_) => if Rewrite.isReduced rewr then thm else simp thm

      val result = Clause {parameters = parameters, id = id, thm = thm}

(*MetisTrace4
      val () = Print.trace pp "Clause.rewrite: result" result
*)
    in
      result
    end
(*MetisDebug
    handle Error err => raise Error ("Clause.rewrite:\n" ^ err);
*)

(* ------------------------------------------------------------------------- *)
(* Inference rules: these generate new clause ids.                           *)
(* ------------------------------------------------------------------------- *)

fun factor (cl as Clause {parameters,thm,...}) =
    let
      val lits = largestLiterals cl

      fun apply sub = new parameters (Thm.subst sub thm)
    in
      map apply (Rule.factor' lits)
    end;

(*MetisTrace5
val factor = fn cl =>
    let
      val () = Print.trace pp "Clause.factor: cl" cl
      val result = factor cl
      val () = Print.trace (Print.ppList pp) "Clause.factor: result" result
    in
      result
    end;
*)

fun resolve (cl1,lit1) (cl2,lit2) =
    let
(*MetisTrace5
      val () = Print.trace pp "Clause.resolve: cl1" cl1
      val () = Print.trace Literal.pp "Clause.resolve: lit1" lit1
      val () = Print.trace pp "Clause.resolve: cl2" cl2
      val () = Print.trace Literal.pp "Clause.resolve: lit2" lit2
*)
      val Clause {parameters, thm = th1, ...} = cl1
      and Clause {thm = th2, ...} = cl2
      val sub = Literal.unify Subst.empty lit1 (Literal.negate lit2)
(*MetisTrace5
      val () = Print.trace Subst.pp "Clause.resolve: sub" sub
*)
      val lit1 = Literal.subst sub lit1
      val lit2 = Literal.negate lit1
      val th1 = Thm.subst sub th1
      and th2 = Thm.subst sub th2
      val _ = isLargerLiteral parameters (Thm.clause th1) lit1 orelse
(*MetisTrace5
              (trace "Clause.resolve: th1 violates ordering\n"; false) orelse
*)
              raise Error "resolve: clause1: ordering constraints"
      val _ = isLargerLiteral parameters (Thm.clause th2) lit2 orelse
(*MetisTrace5
              (trace "Clause.resolve: th2 violates ordering\n"; false) orelse
*)
              raise Error "resolve: clause2: ordering constraints"
      val th = Thm.resolve lit1 th1 th2
(*MetisTrace5
      val () = Print.trace Thm.pp "Clause.resolve: th" th
*)
      val cl = Clause {parameters = parameters, id = newId (), thm = th}
(*MetisTrace5
      val () = Print.trace pp "Clause.resolve: cl" cl
*)
    in
      cl
    end;

fun paramodulate (cl1,lit1,ort1,tm1) (cl2,lit2,path2,tm2) =
    let
(*MetisTrace5
      val () = Print.trace pp "Clause.paramodulate: cl1" cl1
      val () = Print.trace Literal.pp "Clause.paramodulate: lit1" lit1
      val () = Print.trace Rewrite.ppOrient "Clause.paramodulate: ort1" ort1
      val () = Print.trace Term.pp "Clause.paramodulate: tm1" tm1
      val () = Print.trace pp "Clause.paramodulate: cl2" cl2
      val () = Print.trace Literal.pp "Clause.paramodulate: lit2" lit2
      val () = Print.trace Term.ppPath "Clause.paramodulate: path2" path2
      val () = Print.trace Term.pp "Clause.paramodulate: tm2" tm2
*)
      val Clause {parameters, thm = th1, ...} = cl1
      and Clause {thm = th2, ...} = cl2
      val sub = Subst.unify Subst.empty tm1 tm2
      val lit1 = Literal.subst sub lit1
      and lit2 = Literal.subst sub lit2
      and th1 = Thm.subst sub th1
      and th2 = Thm.subst sub th2

      val _ = isLargerLiteral parameters (Thm.clause th1) lit1 orelse
              raise Error "Clause.paramodulate: with clause: ordering"
      val _ = isLargerLiteral parameters (Thm.clause th2) lit2 orelse
              raise Error "Clause.paramodulate: into clause: ordering"

      val eqn = (Literal.destEq lit1, th1)
      val eqn as (l_r,_) =
          case ort1 of
            Rewrite.LeftToRight => eqn
          | Rewrite.RightToLeft => Rule.symEqn eqn
(*MetisTrace6
      val () = Print.trace Rule.ppEquation "Clause.paramodulate: eqn" eqn
*)
      val _ = isLargerTerm parameters l_r orelse
              raise Error "Clause.paramodulate: equation: ordering constraints"
      val th = Rule.rewrRule eqn lit2 path2 th2
(*MetisTrace5
      val () = Print.trace Thm.pp "Clause.paramodulate: th" th
*)
    in
      Clause {parameters = parameters, id = newId (), thm = th}
    end
(*MetisTrace5
    handle Error err =>
      let
        val () = trace ("Clause.paramodulate: failed: " ^ err ^ "\n")
      in
        raise Error err
      end;
*)

end
