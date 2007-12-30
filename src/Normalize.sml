(* ========================================================================= *)
(* NORMALIZING FORMULAS                                                      *)
(* Copyright (c) 2001-2007 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Normalize :> Normalize =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val prefix = "FOFtoCNF";

val skolemPrefix = "skolem" ^ prefix;

val definitionPrefix = "definition" ^ prefix;

(* ------------------------------------------------------------------------- *)
(* Counting the clauses that would be generated by conjunctive normal form.  *)
(* ------------------------------------------------------------------------- *)

datatype count = Count of {positive : real, negative : real};

fun positive (Count {positive = p, ...}) = p;

fun negative (Count {negative = n, ...}) = n;

fun countNegate (Count {positive = p, negative = n}) =
    Count {positive = n, negative = p};

fun countEqualish count1 count2 =
    let
      val Count {positive = p1, negative = n1} = count1
      and Count {positive = p2, negative = n2} = count2
    in
      Real.abs (p1 - p2) < 0.5 andalso Real.abs (n1 - n2) < 0.5
    end;

val countTrue = Count {positive = 0.0, negative = 1.0};

val countFalse = Count {positive = 1.0, negative = 0.0};

val countLiteral = Count {positive = 1.0, negative = 1.0};

fun countAnd2 (count1,count2) =
    let
      val Count {positive = p1, negative = n1} = count1
      and Count {positive = p2, negative = n2} = count2
      val p = p1 + p2
      and n = n1 * n2
    in
      Count {positive = p, negative = n}
    end;

fun countOr2 (count1,count2) =
    let
      val Count {positive = p1, negative = n1} = count1
      and Count {positive = p2, negative = n2} = count2
      val p = p1 * p2
      and n = n1 + n2
    in
      Count {positive = p, negative = n}
    end;

(*** Is this associative? ***)
fun countXor2 (count1,count2) =
    let
      val Count {positive = p1, negative = n1} = count1
      and Count {positive = p2, negative = n2} = count2
      val p = p1 * p2 + n1 * n2
      and n = p1 * n2 + n1 * p2
    in
      Count {positive = p, negative = n}
    end;

fun countDefinition body_count = countXor2 (countLiteral,body_count);

(* ------------------------------------------------------------------------- *)
(* A type of normalized formula.                                             *)
(* ------------------------------------------------------------------------- *)

datatype formula =
    True
  | False
  | Literal of NameSet.set * Literal.literal
  | And of NameSet.set * count * formula Set.set
  | Or of NameSet.set * count * formula Set.set
  | Xor of NameSet.set * count * bool * formula Set.set
  | Exists of NameSet.set * count * NameSet.set * formula
  | Forall of NameSet.set * count * NameSet.set * formula;

fun compare f1_f2 =
    case f1_f2 of
      (True,True) => EQUAL
    | (True,_) => LESS
    | (_,True) => GREATER
    | (False,False) => EQUAL
    | (False,_) => LESS
    | (_,False) => GREATER
    | (Literal (_,l1), Literal (_,l2)) => Literal.compare (l1,l2)
    | (Literal _, _) => LESS
    | (_, Literal _) => GREATER
    | (And (_,_,s1), And (_,_,s2)) => Set.compare (s1,s2)
    | (And _, _) => LESS
    | (_, And _) => GREATER
    | (Or (_,_,s1), Or (_,_,s2)) => Set.compare (s1,s2)
    | (Or _, _) => LESS
    | (_, Or _) => GREATER
    | (Xor (_,_,p1,s1), Xor (_,_,p2,s2)) =>
      (case boolCompare (p1,p2) of
         LESS => LESS
       | EQUAL => Set.compare (s1,s2)
       | GREATER => GREATER)
    | (Xor _, _) => LESS
    | (_, Xor _) => GREATER
    | (Exists (_,_,n1,f1), Exists (_,_,n2,f2)) =>
      (case NameSet.compare (n1,n2) of
         LESS => LESS
       | EQUAL => compare (f1,f2)
       | GREATER => GREATER)
    | (Exists _, _) => LESS
    | (_, Exists _) => GREATER
    | (Forall (_,_,n1,f1), Forall (_,_,n2,f2)) =>
      (case NameSet.compare (n1,n2) of
         LESS => LESS
       | EQUAL => compare (f1,f2)
       | GREATER => GREATER);

val empty = Set.empty compare;

val singleton = Set.singleton compare;

local
  fun neg True = False
    | neg False = True
    | neg (Literal (fv,lit)) = Literal (fv, Literal.negate lit)
    | neg (And (fv,c,s)) = Or (fv, countNegate c, neg_set s)
    | neg (Or (fv,c,s)) = And (fv, countNegate c, neg_set s)
    | neg (Xor (fv,c,p,s)) = Xor (fv, c, not p, s)
    | neg (Exists (fv,c,n,f)) = Forall (fv, countNegate c, n, neg f)
    | neg (Forall (fv,c,n,f)) = Exists (fv, countNegate c, n, neg f)

  and neg_set s = Set.foldl neg_elt empty s

  and neg_elt (f,s) = Set.add s (neg f);
in
  val negate = neg;

  val negateSet = neg_set;
end;

fun negateMember x s = Set.member (negate x) s;

local
  fun member s x = negateMember x s;
in
  fun negateDisjoint s1 s2 =
      if Set.size s1 < Set.size s2 then not (Set.exists (member s2) s1)
      else not (Set.exists (member s1) s2);
end;

fun polarity True = true
  | polarity False = false
  | polarity (Literal (_,(pol,_))) = not pol
  | polarity (And _) = true
  | polarity (Or _) = false
  | polarity (Xor (_,_,pol,_)) = pol
  | polarity (Exists _) = true
  | polarity (Forall _) = false;

(*DEBUG
val polarity = fn f =>
    let
      val res1 = compare (f, negate f) = LESS
      val res2 = polarity f
      val _ = res1 = res2 orelse raise Bug "polarity"
    in
      res2
    end;
*)

fun applyPolarity true fm = fm
  | applyPolarity false fm = negate fm;

fun freeVars True = NameSet.empty
  | freeVars False = NameSet.empty
  | freeVars (Literal (fv,_)) = fv
  | freeVars (And (fv,_,_)) = fv
  | freeVars (Or (fv,_,_)) = fv
  | freeVars (Xor (fv,_,_,_)) = fv
  | freeVars (Exists (fv,_,_,_)) = fv
  | freeVars (Forall (fv,_,_,_)) = fv;

fun freeIn v fm = NameSet.member v (freeVars fm);

val freeVarsSet =
    let
      fun free (fm,acc) = NameSet.union (freeVars fm) acc
    in
      Set.foldl free NameSet.empty
    end;

fun count True = countTrue
  | count False = countFalse
  | count (Literal _) = countLiteral
  | count (And (_,c,_)) = c
  | count (Or (_,c,_)) = c
  | count (Xor (_,c,p,_)) = if p then c else countNegate c
  | count (Exists (_,c,_,_)) = c
  | count (Forall (_,c,_,_)) = c;

val countAndSet =
    let
      fun countAnd (fm,c) = countAnd2 (count fm, c)
    in
      Set.foldl countAnd countTrue
    end;

val countOrSet =
    let
      fun countOr (fm,c) = countOr2 (count fm, c)
    in
      Set.foldl countOr countFalse
    end;

val countXorSet =
    let
      fun countXor (fm,c) = countXor2 (count fm, c)
    in
      Set.foldl countXor countFalse
    end;

fun And2 (False,_) = False
  | And2 (_,False) = False
  | And2 (True,f2) = f2
  | And2 (f1,True) = f1
  | And2 (f1,f2) =
    let
      val (fv1,c1,s1) =
          case f1 of
            And fv_c_s => fv_c_s
          | _ => (freeVars f1, count f1, singleton f1)

      and (fv2,c2,s2) =
          case f2 of
            And fv_c_s => fv_c_s
          | _ => (freeVars f2, count f2, singleton f2)
    in
      if not (negateDisjoint s1 s2) then False
      else
        let
          val s = Set.union s1 s2
        in
          case Set.size s of
            0 => True
          | 1 => Set.pick s
          | n =>
            if n = Set.size s1 + Set.size s2 then
              And (NameSet.union fv1 fv2, countAnd2 (c1,c2), s)
            else
              And (freeVarsSet s, countAndSet s, s)
        end
    end;

val AndList = foldl And2 True;

val AndSet = Set.foldl And2 True;

fun Or2 (True,_) = True
  | Or2 (_,True) = True
  | Or2 (False,f2) = f2
  | Or2 (f1,False) = f1
  | Or2 (f1,f2) =
    let
      val (fv1,c1,s1) =
          case f1 of
            Or fv_c_s => fv_c_s
          | _ => (freeVars f1, count f1, singleton f1)

      and (fv2,c2,s2) =
          case f2 of
            Or fv_c_s => fv_c_s
          | _ => (freeVars f2, count f2, singleton f2)
    in
      if not (negateDisjoint s1 s2) then True
      else
        let
          val s = Set.union s1 s2
        in
          case Set.size s of
            0 => False
          | 1 => Set.pick s
          | n =>
            if n = Set.size s1 + Set.size s2 then
              Or (NameSet.union fv1 fv2, countOr2 (c1,c2), s)
            else
              Or (freeVarsSet s, countOrSet s, s)
        end
    end;

val OrList = foldl Or2 False;

val OrSet = Set.foldl Or2 False;

fun pushOr2 (f1,f2) =
    let
      val s1 = case f1 of And (_,_,s) => s | _ => singleton f1
      and s2 = case f2 of And (_,_,s) => s | _ => singleton f2

      fun g x1 (x2,acc) = And2 (Or2 (x1,x2), acc)
      fun f (x1,acc) = Set.foldl (g x1) acc s2
    in
      Set.foldl f True s1
    end;

val pushOrList = foldl pushOr2 False;

local
  fun normalize fm =
      let
        val p = polarity fm
        val fm = applyPolarity p fm
      in
        (freeVars fm, count fm, p, singleton fm)
      end;
in
  fun Xor2 (False,f2) = f2
    | Xor2 (f1,False) = f1
    | Xor2 (True,f2) = negate f2
    | Xor2 (f1,True) = negate f1
    | Xor2 (f1,f2) =
      let
        val (fv1,c1,p1,s1) = case f1 of Xor x => x | _ => normalize f1
        and (fv2,c2,p2,s2) = case f2 of Xor x => x | _ => normalize f2

        val s = Set.symmetricDifference s1 s2

        val fm =
            case Set.size s of
              0 => False
            | 1 => Set.pick s
            | n =>
              if n = Set.size s1 + Set.size s2 then
                Xor (NameSet.union fv1 fv2, countXor2 (c1,c2), true, s)
              else
                Xor (freeVarsSet s, countXorSet s, true, s)

        val p = p1 = p2
      in
        applyPolarity p fm
      end;
end;

val XorList = foldl Xor2 False;

val XorSet = Set.foldl Xor2 False;

fun XorPolarityList (p,l) = applyPolarity p (XorList l);

fun XorPolaritySet (p,s) = applyPolarity p (XorSet s);

fun destXor (Xor (_,_,p,s)) =
    let
      val (fm1,s) = Set.deletePick s
      val fm2 =
          if Set.size s = 1 then applyPolarity p (Set.pick s)
          else Xor (freeVarsSet s, countXorSet s, p, s)
    in
      (fm1,fm2)
    end
  | destXor _ = raise Error "destXor";

fun pushXor fm =
    let
      val (f1,f2) = destXor fm
      val f1' = negate f1
      and f2' = negate f2
    in
      And2 (Or2 (f1,f2), Or2 (f1',f2'))
    end;

fun Exists1 (v,init_fm) =
    let
      fun exists_gen fm =
          let
            val fv = NameSet.delete (freeVars fm) v
            val c = count fm
            val n = NameSet.singleton v
          in
            Exists (fv,c,n,fm)
          end

      fun exists fm = if freeIn v fm then exists_free fm else fm

      and exists_free (Or (_,_,s)) = OrList (Set.transform exists s)
        | exists_free (fm as And (_,_,s)) =
          let
            val sv = Set.filter (freeIn v) s
          in
            if Set.size sv <> 1 then exists_gen fm
            else
              let
                val fm = Set.pick sv
                val s = Set.delete s fm
              in
                And2 (exists_free fm, AndSet s)
              end
          end
        | exists_free (Exists (fv,c,n,f)) =
          Exists (NameSet.delete fv v, c, NameSet.add n v, f)
        | exists_free fm = exists_gen fm
    in
      exists init_fm
    end;

fun ExistsList (vs,f) = foldl Exists1 f vs;

fun ExistsSet (n,f) = NameSet.foldl Exists1 f n;

fun Forall1 (v,init_fm) =
    let
      fun forall_gen fm =
          let
            val fv = NameSet.delete (freeVars fm) v
            val c = count fm
            val n = NameSet.singleton v
          in
            Forall (fv,c,n,fm)
          end

      fun forall fm = if freeIn v fm then forall_free fm else fm

      and forall_free (And (_,_,s)) = AndList (Set.transform forall s)
        | forall_free (fm as Or (_,_,s)) =
          let
            val sv = Set.filter (freeIn v) s
          in
            if Set.size sv <> 1 then forall_gen fm
            else
              let
                val fm = Set.pick sv
                val s = Set.delete s fm
              in
                Or2 (forall_free fm, OrSet s)
              end
          end
        | forall_free (Forall (fv,c,n,f)) =
          Forall (NameSet.delete fv v, c, NameSet.add n v, f)
        | forall_free fm = forall_gen fm
    in
      forall init_fm
    end;

fun ForallList (vs,f) = foldl Forall1 f vs;

fun ForallSet (n,f) = NameSet.foldl Forall1 f n;

local
  fun subst_fv fvSub =
      let
        fun add_fv (v,s) = NameSet.union (NameMap.get fvSub v) s
      in
        NameSet.foldl add_fv NameSet.empty
      end;

  fun subst_rename (v,(avoid,bv,sub,domain,fvSub)) =
      let
        val v' = Term.variantPrime avoid v
        val avoid = NameSet.add avoid v'
        val bv = NameSet.add bv v'
        val sub = Subst.insert sub (v, Term.Var v')
        val domain = NameSet.add domain v
        val fvSub = NameMap.insert fvSub (v, NameSet.singleton v')
      in
        (avoid,bv,sub,domain,fvSub)
      end;

  fun subst_check sub domain fvSub fm =
      let
        val domain = NameSet.intersect domain (freeVars fm)
      in
        if NameSet.null domain then fm
        else subst_domain sub domain fvSub fm
      end

  and subst_domain sub domain fvSub fm =
      case fm of
        Literal (fv,lit) =>
        let
          val fv = NameSet.difference fv domain
          val fv = NameSet.union fv (subst_fv fvSub domain)
          val lit = Literal.subst sub lit
        in
          Literal (fv,lit)
        end
      | And (_,_,s) =>
        AndList (Set.transform (subst_check sub domain fvSub) s)
      | Or (_,_,s) =>
        OrList (Set.transform (subst_check sub domain fvSub) s)
      | Xor (_,_,p,s) =>
        XorPolarityList (p, Set.transform (subst_check sub domain fvSub) s)
      | Exists fv_c_n_f => subst_quant Exists sub domain fvSub fv_c_n_f
      | Forall fv_c_n_f => subst_quant Forall sub domain fvSub fv_c_n_f
      | _ => raise Bug "subst_domain"

  and subst_quant quant sub domain fvSub (fv,c,bv,fm) =
      let
        val sub_fv = subst_fv fvSub domain
        val fv = NameSet.union sub_fv (NameSet.difference fv domain)
        val captured = NameSet.intersect bv sub_fv
        val bv = NameSet.difference bv captured
        val avoid = NameSet.union fv bv
        val (_,bv,sub,domain,fvSub) =
            NameSet.foldl subst_rename (avoid,bv,sub,domain,fvSub) captured
        val fm = subst_domain sub domain fvSub fm
      in
        quant (fv,c,bv,fm)
      end;
in
  fun subst sub =
      let
        fun mk_dom (v,tm,(d,fv)) =
            (NameSet.add d v, NameMap.insert fv (v, Term.freeVars tm))

        val domain_fvSub = (NameSet.empty, NameMap.new ())
        val (domain,fvSub) = Subst.foldl mk_dom domain_fvSub sub
      in
        subst_check sub domain fvSub
      end;
end;

fun fromFormula fm =
    case fm of
      Formula.True => True
    | Formula.False => False
    | Formula.Atom atm => Literal (Atom.freeVars atm, (true,atm))
    | Formula.Not p => negateFromFormula p
    | Formula.And (p,q) => And2 (fromFormula p, fromFormula q)
    | Formula.Or (p,q) => Or2 (fromFormula p, fromFormula q)
    | Formula.Imp (p,q) => Or2 (negateFromFormula p, fromFormula q)
    | Formula.Iff (p,q) => Xor2 (negateFromFormula p, fromFormula q)
    | Formula.Forall (v,p) => Forall1 (v, fromFormula p)
    | Formula.Exists (v,p) => Exists1 (v, fromFormula p)

and negateFromFormula fm =
    case fm of
      Formula.True => False
    | Formula.False => True
    | Formula.Atom atm => Literal (Atom.freeVars atm, (false,atm))
    | Formula.Not p => fromFormula p
    | Formula.And (p,q) => Or2 (negateFromFormula p, negateFromFormula q)
    | Formula.Or (p,q) => And2 (negateFromFormula p, negateFromFormula q)
    | Formula.Imp (p,q) => And2 (fromFormula p, negateFromFormula q)
    | Formula.Iff (p,q) => Xor2 (fromFormula p, fromFormula q)
    | Formula.Forall (v,p) => Exists1 (v, negateFromFormula p)
    | Formula.Exists (v,p) => Forall1 (v, negateFromFormula p);

local
  fun lastElt (s : formula Set.set) =
      case Set.findr (K true) s of
        NONE => raise Bug "lastElt: empty set"
      | SOME fm => fm;

  fun negateLastElt s =
      let
        val fm = lastElt s
      in
        Set.add (Set.delete s fm) (negate fm)
      end;

  fun form fm =
      case fm of
        True => Formula.True
      | False => Formula.False
      | Literal (_,lit) => Literal.toFormula lit
      | And (_,_,s) => Formula.listMkConj (Set.transform form s)
      | Or (_,_,s) => Formula.listMkDisj (Set.transform form s)
      | Xor (_,_,p,s) =>
        let
          val s = if p then negateLastElt s else s
        in
          Formula.listMkEquiv (Set.transform form s)
        end
      | Exists (_,_,n,f) => Formula.listMkExists (NameSet.toList n, form f)
      | Forall (_,_,n,f) => Formula.listMkForall (NameSet.toList n, form f);
in
  val toFormula = form;
end;

fun toLiteral (Literal (_,lit)) = lit
  | toLiteral _ = raise Error "Normalize.toLiteral";

local
  fun addLiteral (l,s) = LiteralSet.add s (toLiteral l);
in
  fun toClause False = LiteralSet.empty
    | toClause (Or (_,_,s)) = Set.foldl addLiteral LiteralSet.empty s
    | toClause l = LiteralSet.singleton (toLiteral l);
end;

val pp = Parser.ppMap toFormula Formula.pp;

val toString = Parser.toString pp;

(* ------------------------------------------------------------------------- *)
(* Negation normal form.                                                     *)
(* ------------------------------------------------------------------------- *)

fun nnf fm = toFormula (fromFormula fm);

(* ------------------------------------------------------------------------- *)
(* Basic conjunctive normal form.                                            *)
(* ------------------------------------------------------------------------- *)

val newSkolemFunction =
    let
      val counter : int NameMap.map ref = ref (NameMap.new ())
    in
      fn n =>
      let
        val ref m = counter
        val i = Option.getOpt (NameMap.peek m n, 0)
        val () = counter := NameMap.insert m (n, i + 1)
      in
        skolemPrefix ^ "_" ^ n ^ (if i = 0 then "" else "_" ^ Int.toString i)
      end
    end;

fun skolemize fv bv fm =
    let
      val fv = NameSet.transform Term.Var fv
               
      fun mk (v,s) = Subst.insert s (v, Term.Fn (newSkolemFunction v, fv))
    in
      subst (NameSet.foldl mk Subst.empty bv) fm
    end;

local
  fun rename avoid fv bv fm =
      let
        val captured = NameSet.intersect avoid bv
      in
        if NameSet.null captured then fm
        else
          let
            fun ren (v,(a,s)) =
                let
                  val v' = Term.variantPrime a v
                in
                  (NameSet.add a v', Subst.insert s (v, Term.Var v'))
                end
              
            val avoid = NameSet.union (NameSet.union avoid fv) bv

            val (_,sub) = NameSet.foldl ren (avoid,Subst.empty) captured
          in
            subst sub fm
          end
      end;

  fun cnfFm avoid fm =
(*TRACE5
      let
        val fm' = cnfFm' avoid fm
        val () = Parser.ppTrace pp "Normalize.cnfFm: fm" fm
        val () = Parser.ppTrace pp "Normalize.cnfFm: fm'" fm'
      in
        fm'
      end
  and cnfFm' avoid fm =
*)
      case fm of
        True => True
      | False => False
      | Literal _ => fm
      | And (_,_,s) => AndList (Set.transform (cnfFm avoid) s)
      | Or (_,_,s) => pushOrList (snd (Set.foldl cnfOr (avoid,[]) s))
      | Xor _ => cnfFm avoid (pushXor fm)
      | Exists (fv,_,n,f) => cnfFm avoid (skolemize fv n f)
      | Forall (fv,_,n,f) => cnfFm avoid (rename avoid fv n f)

  and cnfOr (fm,(avoid,acc)) =
      let
        val fm = cnfFm avoid fm
      in
        (NameSet.union (freeVars fm) avoid, fm :: acc)
      end;
in
  val basicCnf = cnfFm NameSet.empty;
end;

(* ------------------------------------------------------------------------- *)
(* Finding the formula definition that minimizes the number of clauses.      *)
(* ------------------------------------------------------------------------- *)

local
  type best = real * formula option;

  fun minBreak countClauses fm best =
      case fm of
        True => best
      | False => best
      | Literal _ => best
      | And (_,_,s) =>
        minBreakSet countClauses countAnd2 countTrue AndSet s best
      | Or (_,_,s) =>
        minBreakSet countClauses countOr2 countFalse OrSet s best
      | Xor (_,_,_,s) =>
        minBreakSet countClauses countXor2 countFalse XorSet s best
      | Exists (_,_,_,f) => minBreak countClauses f best
      | Forall (_,_,_,f) => minBreak countClauses f best
                            
  and minBreakSet countClauses count2 count0 mkSet fmSet best =
      let
        fun cumulatives fms =
            let
              fun fwd (fm,(c1,s1,l)) =
                  let
                    val c1' = count2 (count fm, c1)
                    and s1' = Set.add s1 fm
                  in
                    (c1', s1', (c1,s1,fm) :: l)
                  end

              fun bwd ((c1,s1,fm),(c2,s2,l)) =
                  let
                    val c2' = count2 (count fm, c2)
                    and s2' = Set.add s2 fm
                  in
                    (c2', s2', (c1,s1,fm,c2,s2) :: l)
                  end

              val (c1,_,fms) = foldl fwd (count0,empty,[]) fms
              val (c2,_,fms) = foldl bwd (count0,empty,[]) fms

              val _ = countEqualish c1 c2 orelse raise Bug "cumulativeCounts"
            in
              fms
            end

        fun breakSing ((c1,_,fm,c2,_),best) =
            let
              val cFms = count2 (c1,c2)
              fun countCls cFm = countClauses (count2 (cFms,cFm))
            in
              minBreak countCls fm best
            end

        val breakSet1 =
            let
              fun break c1 s1 fm c2 (best as (bcl,_)) =
                  if Set.null s1 then best
                  else
                    let
                      val cDef = countDefinition (count2 (c1, count fm))
                      val cFm = count2 (countLiteral,c2)
                      val cl = positive cDef + countClauses cFm
                      val better = cl < bcl - 0.5
                    in
                      if better then (cl, SOME (mkSet (Set.add s1 fm)))
                      else best
                    end
            in
              fn ((c1,s1,fm,c2,s2),best) =>
                 break c1 s1 fm c2 (break c2 s2 fm c1 best)
            end

        val fms = Set.toList fmSet

        fun breakSet measure best =
            let
              val fms = sortMap (measure o count) Real.compare fms
            in
              foldl breakSet1 best (cumulatives fms)
            end

        val best = foldl breakSing best (cumulatives fms)
        val best = breakSet positive best
        val best = breakSet negative best
        val best = breakSet countClauses best
      in
        best
      end
in
  fun minimumDefinition fm =
      let
        val countClauses = positive
        val cl = countClauses (count fm)
      in
        if cl < 1.5 then NONE
        else
          let
            val (cl',def) = minBreak countClauses fm (cl,NONE)
(*TRACE1
            val () =
                case def of
                  NONE => ()
                | SOME d =>
                  Parser.ppTrace pp ("defCNF: before = " ^ Real.toString cl ^
                                     ", after = " ^ Real.toString cl' ^
                                     ", definition") d
*)
          in
            def
          end
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Normalization proofs.                                                     *)
(* ------------------------------------------------------------------------- *)

type proof = StringSet.set;

val noProof = StringSet.empty;

val singletonProof = StringSet.singleton;

val combineProofs = StringSet.union;

(* ------------------------------------------------------------------------- *)
(* Simplifying with definitions.                                             *)
(* ------------------------------------------------------------------------- *)

datatype simplify =
    Simplify of
      {formula : (formula, formula * proof) Map.map,
       andSet : (formula Set.set * formula * proof) list,
       orSet : (formula Set.set * formula * proof) list,
       xorSet : (formula Set.set * formula * proof) list};

val simplifyEmpty =
    Simplify
      {formula = Map.new compare,
       andSet = [],
       orSet = [],
       xorSet = []};

local
  fun simpler fm s =
      Set.size s <> 1 orelse
      case Set.pick s of
        True => false
      | False => false
      | Literal _ => false
      | _ => true;

  fun addSet set_defs body_def =
      let
        fun def_body_size (body,_,_) = Set.size body

        val body_size = def_body_size body_def

        val (body,_,_) = body_def

        fun add acc [] = List.revAppend (acc,[body_def])
          | add acc (l as (bd as (b,_,_)) :: bds) =
            case Int.compare (def_body_size bd, body_size) of
              LESS => List.revAppend (acc, body_def :: l)
            | EQUAL =>
              if Set.equal b body then List.revAppend (acc,l)
              else add (bd :: acc) bds
            | GREATER => add (bd :: acc) bds
      in
        add [] set_defs
      end;

  fun add simp (body,False,prf) = add simp (negate body, True, prf)
    | add simp (True,_,_) = simp
    | add (Simplify {formula,andSet,orSet,xorSet}) (And (_,_,s), def, prf) =
      let
        val andSet = addSet andSet (s,def,prf)
        and orSet = addSet orSet (negateSet s, negate def, prf)
      in
        Simplify
          {formula = formula,
           andSet = andSet,
           orSet = orSet,
           xorSet = xorSet}
      end
    | add (Simplify {formula,andSet,orSet,xorSet}) (Or (_,_,s), def, prf) =
      let
        val orSet = addSet orSet (s,def,prf)
        and andSet = addSet andSet (negateSet s, negate def, prf)
      in
        Simplify
          {formula = formula,
           andSet = andSet,
           orSet = orSet,
           xorSet = xorSet}
      end
    | add simp (Xor (_,_,p,s), def, prf) =
      let
        val simp = addXorSet simp (s, applyPolarity p def, prf)
      in
        case def of
          True =>
          let
            fun addXorLiteral (fm as Literal _, simp) =
                let
                  val s = Set.delete s fm
                in
                  if not (simpler fm s) then simp
                  else addXorSet simp (s, applyPolarity (not p) fm, prf)
                end
              | addXorLiteral (_,simp) = simp
          in
            Set.foldl addXorLiteral simp s
          end
        | _ => simp
      end
    | add (simp as Simplify {formula,andSet,orSet,xorSet}) (body,def,prf) =
      if Map.inDomain body formula then simp
      else
        let
          val formula = Map.insert formula (body,(def,prf))
          val formula = Map.insert formula (negate body, (negate def, prf))
        in
          Simplify
            {formula = formula,
             andSet = andSet,
             orSet = orSet,
             xorSet = xorSet}
        end

  and addXorSet (simp as Simplify {formula,andSet,orSet,xorSet}) (s,def,prf) =
      if Set.size s = 1 then add simp (Set.pick s, def, prf)
      else
        let
          val xorSet = addSet xorSet (s,def,prf)
        in
          Simplify
            {formula = formula,
             andSet = andSet,
             orSet = orSet,
             xorSet = xorSet}
        end;
in
  fun simplifyAdd simp (fm,prf) = add simp (fm,True,prf);
end;

local
  fun simplifySet set_defs set prf =
      let
        fun pred (s,_,_) = Set.subset s set
      in
        case List.find pred set_defs of
          NONE => NONE
        | SOME (s,f,p) =>
          let
            val set = Set.add (Set.difference set s) f
            val prf = combineProofs prf p
          in
            SOME (set,prf)
          end
      end;
in
  fun simplify (Simplify {formula,andSet,orSet,xorSet}) =
      let
        fun simp fm prf =
            case simp_sub fm prf of
              NONE => simp_top fm prf
            | SOME (fm,prf) => try_simp_top fm prf

        and try_simp_top fm prf =
            case simp_top fm prf of
              NONE => SOME (fm,prf)
            | x => x

        and simp_top fm prf =
            case fm of
              And (_,_,s) =>
              (case simplifySet andSet s prf of
                 NONE => NONE
               | SOME (s,prf) => try_simp_top (AndSet s) prf)
            | Or (_,_,s) =>
              (case simplifySet orSet s prf of
                 NONE => NONE
               | SOME (s,prf) => try_simp_top (OrSet s) prf)
            | Xor (_,_,p,s) =>
              (case simplifySet xorSet s prf of
                 NONE => NONE
               | SOME (s,prf) => try_simp_top (XorPolaritySet (p,s)) prf)
            | _ =>
              (case Map.peek formula fm of
                 NONE => NONE
               | SOME (fm,p) => try_simp_top fm (combineProofs prf p))
              
        and simp_sub fm prf =
            case fm of
              And (_,_,s) =>
              (case simp_set s prf of
                 NONE => NONE
               | SOME (l,prf) => SOME (AndList l, prf))
            | Or (_,_,s) =>
              (case simp_set s prf of
                 NONE => NONE
               | SOME (l,prf) => SOME (OrList l, prf))
            | Xor (_,_,p,s) =>
              (case simp_set s prf of
                 NONE => NONE
               | SOME (l,prf) => SOME (XorPolarityList (p,l), prf))
            | Exists (_,_,n,f) =>
              (case simp f prf of
                 NONE => NONE
               | SOME (f,prf) => SOME (ExistsSet (n,f), prf))
            | Forall (_,_,n,f) =>
              (case simp f prf of
                 NONE => NONE
               | SOME (f,prf) => SOME (ForallSet (n,f), prf))
            | _ => NONE

        and simp_set s prf =
            let
              val (changed,l,prf) = Set.foldr simp_set_elt (false,[],prf) s
            in
              if changed then SOME (l,prf) else NONE
            end

        and simp_set_elt (fm,(changed,l,prf)) =
            case simp fm prf of
              NONE => (changed, fm :: l, prf)
            | SOME (fm,prf) => (true, fm :: l, prf)
      in
        fn fm_prf as (fm,prf) => Option.getOpt (simp fm prf, fm_prf)
      end;
end;

(*TRACE2
val simplify = fn simp => fn fm =>
    let
      val fm' = simplify simp fm
      val () = if compare (fm,fm') = EQUAL then ()
               else (Parser.ppTrace pp "Normalize.simplify: fm" fm;
                     Parser.ppTrace pp "Normalize.simplify: fm'" fm')
    in
      fm'
    end;
*)

(* ------------------------------------------------------------------------- *)
(* Definitions.                                                              *)
(* ------------------------------------------------------------------------- *)

val newDefinitionRelation =
    let
      val counter : int ref = ref 0
    in
      fn () =>
      let
        val ref i = counter
        val () = counter := i + 1
      in
        definitionPrefix ^ "_" ^ Int.toString i
      end
    end;

fun newDefinition def =
    let
      val fv = freeVars def
      val rel = newDefinitionRelation ()
      val atm = (rel, NameSet.transform Term.Var fv)
      val lit = Literal (fv,(false,atm))
      val prf = singletonProof rel
    in
      ((rel, Formula.Iff (Formula.Atom atm, toFormula def)),
       (Xor2 (lit,def), prf))
    end;

(* ------------------------------------------------------------------------- *)
(* Definitional conjunctive normal form.                                     *)
(* ------------------------------------------------------------------------- *)

type cnfResult =
     {definitions : (string * Formula.formula) list,
      clauses : (Thm.clause * proof) list};

datatype cnfState =
    CnfState of simplify
  | CnfInconsistent;

val cnfStateInitial = CnfState simplifyEmpty;

local
  fun def_cnf defs cls simp [] =
      ({definitions = defs, clauses = cls}, CnfState simp)
    | def_cnf defs cls simp (fm :: fms) =
      def_cnf_formula defs cls simp (simplify simp fm) fms

  and def_cnf_formula defs cls simp (fm_prf as (fm,prf)) fms =
      case fm of
        True => def_cnf defs cls simp fms
      | False => def_cnf_inconsistent defs prf
      | And (_,_,s) =>
        let
          fun add (f,z) = (f,prf) :: z
        in
          def_cnf defs cls simp (Set.foldr add fms s)
        end
      | Exists (fv,_,n,f) =>
        def_cnf_formula defs cls simp (skolemize fv n f, prf) fms
      | Forall (_,_,_,f) => def_cnf_formula defs cls simp (f,prf) fms
      | _ =>
        case minimumDefinition fm of
          SOME def =>
          let
            val (def,fm) = newDefinition def
            and fms = fm_prf :: fms
          in
            def_cnf_formula (def :: defs) cls simp fm fms
          end
        | NONE =>
          let
            val simp = simplifyAdd simp fm_prf
            fun add (f,l) =
                (toClause f, prf) :: l
(*DEBUG
                handle Error err =>
                  (Parser.ppTrace pp "Normalize.def_cnf_formula: f" f;
                   raise Bug ("Normalize.cnfStateAdd.def_cnf_formula: "^err))
*)
          in
            case basicCnf fm of
              True => def_cnf defs cls simp fms
            | False => def_cnf_inconsistent defs prf
            | And (_,_,s) => def_cnf defs (Set.foldl add cls s) simp fms
            | fm => def_cnf defs (add (fm,cls)) simp fms
          end

  and def_cnf_inconsistent defs prf =
      let
        val cls = [(LiteralSet.empty,prf)]
      in
        ({definitions = defs, clauses = cls}, CnfInconsistent)
      end;
in
  fun cnfStateAdd (fm,prf) (CnfState simp) =
      def_cnf [] [] simp [(fromFormula fm, prf)]
    | cnfStateAdd _ CnfInconsistent =
      ({definitions = [], clauses = []}, CnfInconsistent);
end;

local
  fun add (fm,(defs,cls,state)) =
      let
        val ({definitions,clauses},state) = cnfStateAdd fm state
      in
        (definitions @ defs, clauses @ cls, state)
      end;
in
  fun cnfProof fms : cnfResult =
      let
        val (defs,cls,_) = List.foldl add ([],[],cnfStateInitial) fms
      in
        {definitions = rev defs, clauses = rev cls}
      end;
end;

fun cnf fm =
    let
      val {clauses,...} = cnfProof [(fm,noProof)]
    in
      map fst clauses
    end;

end
