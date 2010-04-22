(* ========================================================================= *)
(* RANDOM FINITE MODELS                                                      *)
(* Copyright (c) 2003 Joe Hurd, distributed under the GNU GPL version 2      *)
(* ========================================================================= *)

structure Model :> Model =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Constants.                                                                *)
(* ------------------------------------------------------------------------- *)

val maxSpace = 1000;

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

val multInt =
    case Int.maxInt of
      NONE => (fn x => fn y => SOME (x * y))
    | SOME m =>
      let
        val m = Real.floor (Math.sqrt (Real.fromInt m))
      in
        fn x => fn y => if x <= m andalso y <= m then SOME (x * y) else NONE
      end;

local
  fun iexp x y acc =
      if y mod 2 = 0 then iexp' x y acc
      else
        case multInt acc x of
          SOME acc => iexp' x y acc
        | NONE => NONE

  and iexp' x y acc =
      if y = 1 then SOME acc
      else
        let
          val y = y div 2
        in
          case multInt x x of
            SOME x => iexp x y acc
          | NONE => NONE
        end;
in
  fun expInt x y =
      if y <= 1 then
        if y = 0 then SOME 1
        else if y = 1 then SOME x
        else raise Bug "expInt: negative exponent"
      else if x <= 1 then
        if 0 <= x then SOME x
        else raise Bug "expInt: negative exponand"
      else iexp x y 1;
end;

fun boolToInt true = 1
  | boolToInt false = 0;

fun intToBool 1 = true
  | intToBool 0 = false
  | intToBool _ = raise Bug "Model.intToBool";

fun minMaxInterval i j = interval i (1 + j - i);

(* ------------------------------------------------------------------------- *)
(* Model size.                                                               *)
(* ------------------------------------------------------------------------- *)

type size = {size : int};

(* ------------------------------------------------------------------------- *)
(* A model of size N has integer elements 0...N-1.                           *)
(* ------------------------------------------------------------------------- *)

type element = int;

val zeroElement = 0;

fun incrementElement {size = N} i =
    let
      val i = i + 1
    in
      if i = N then NONE else SOME i
    end;

fun elementListSpace {size = N} arity =
    case expInt N arity of
      NONE => NONE
    | s as SOME m => if m <= maxSpace then s else NONE;

fun elementListIndex {size = N} =
    let
      fun f acc elts =
          case elts of
            [] => acc
          | elt :: elts => f (N * acc + elt) elts
    in
      f 0
    end;

(* ------------------------------------------------------------------------- *)
(* The parts of the model that are fixed.                                    *)
(* ------------------------------------------------------------------------- *)

type fixedFunction = size -> element list -> element option;

type fixedRelation = size -> element list -> bool option;

datatype fixed =
    Fixed of
      {functions : fixedFunction NameArityMap.map,
       relations : fixedRelation NameArityMap.map};

val emptyFunctions : fixedFunction NameArityMap.map = NameArityMap.new ();

val emptyRelations : fixedRelation NameArityMap.map = NameArityMap.new ();

fun fixed0 f sz elts =
    case elts of
      [] => f sz
    | _ => raise Bug "Model.fixed0: wrong arity";

fun fixed1 f sz elts =
    case elts of
      [x] => f sz x
    | _ => raise Bug "Model.fixed1: wrong arity";

fun fixed2 f sz elts =
    case elts of
      [x,y] => f sz x y
    | _ => raise Bug "Model.fixed2: wrong arity";

val emptyFixed =
    let
      val fns = emptyFunctions
      and rels = emptyRelations
    in
      Fixed
        {functions = fns,
         relations = rels}
    end;

local
  fun hasTypeFn _ elts =
      case elts of
        [x,_] => SOME x
      | _ => raise Bug "Model.hasTypeFn: wrong arity";

  fun eqRel _ elts =
      case elts of
        [x,y] => SOME (x = y)
      | _ => raise Bug "Model.eqRel: wrong arity";
in
  val basicFixed =
      let
        val fns = NameArityMap.singleton (Term.hasTypeFunction,hasTypeFn)

        val rels = NameArityMap.singleton (Atom.eqRelation,eqRel)
      in
        Fixed
          {functions = fns,
           relations = rels}
      end;
end;

local
  fun union _ = raise Bug "Model.unionFixed: nameArity clash";
in
  fun unionFixed fix1 fix2 =
      let
        val Fixed {functions = fns1, relations = rels1} = fix1
        and Fixed {functions = fns2, relations = rels2} = fix2

        val fns = NameArityMap.union union fns1 fns2

        val rels = NameArityMap.union union rels1 rels2
      in
        Fixed
          {functions = fns,
           relations = rels}
      end;
end;

val unionListFixed =
    let
      fun union (fix,acc) = unionFixed acc fix
    in
      List.foldl union emptyFixed
    end;

(* ------------------------------------------------------------------------- *)
(* Renaming fixed model parts.                                               *)
(* ------------------------------------------------------------------------- *)

type fixedMap =
     {functionMap : Name.name NameArityMap.map,
      relationMap : Name.name NameArityMap.map};

fun mapFixed fixMap fix =
    let
      val {functionMap = fnMap, relationMap = relMap} = fixMap
      and Fixed {functions = fns, relations = rels} = fix

      val fns = NameArityMap.compose fnMap fns

      val rels = NameArityMap.compose relMap rels
    in
      Fixed
        {functions = fns,
         relations = rels}
    end;

(* ------------------------------------------------------------------------- *)
(* Standard fixed model parts.                                               *)
(* ------------------------------------------------------------------------- *)

(* Projections *)

val projectionMin = 1
and projectionMax = 9;

val projectionList = minMaxInterval projectionMin projectionMax;

fun projectionName i =
    let
      val _ = projectionMin <= i orelse
              raise Bug "Model.projectionName: less than projectionMin"

      val _ = i <= projectionMax orelse
              raise Bug "Model.projectionName: greater than projectionMax"
    in
      Name.fromString ("#" ^ Int.toString i)
    end;

fun projectionFn i _ elts = SOME (List.nth (elts,i));

fun arityProjectionFixed arity =
    let
      fun mkProj i = ((projectionName i, arity), projectionFn i)

      fun addProj i acc =
          if i > arity then acc
          else addProj (i + 1) (NameArityMap.insert acc (mkProj i))

      val fns = addProj projectionMin emptyFunctions

      val rels = emptyRelations
    in
      Fixed
        {functions = fns,
         relations = rels}
    end;

val projectionFixed =
    unionListFixed (map arityProjectionFixed projectionList);

(* Arithmetic *)

val numeralMin = ~100
and numeralMax = 100;

val numeralList = minMaxInterval numeralMin numeralMax;

fun numeralName i =
    let
      val _ = numeralMin <= i orelse
              raise Bug "Model.numeralName: less than numeralMin"

      val _ = i <= numeralMax orelse
              raise Bug "Model.numeralName: greater than numeralMax"
    in
      Name.fromString (Int.toString i)
    end;

val addName = Name.fromString "+"
and divName = Name.fromString "div"
and dividesName = Name.fromString "divides"
and evenName = Name.fromString "even"
and expName = Name.fromString "exp"
and geName = Name.fromString ">="
and gtName = Name.fromString ">"
and isZeroName = Name.fromString "isZero"
and leName = Name.fromString "<="
and ltName = Name.fromString "<"
and modName = Name.fromString "mod"
and multName = Name.fromString "*"
and negName = Name.fromString "~"
and oddName = Name.fromString "odd"
and preName = Name.fromString "pre"
and subName = Name.fromString "-"
and sucName = Name.fromString "suc";

local
  (* Support *)

  fun modN {size = N} x = x mod N;

  fun oneN sz = modN sz 1;

  fun multN sz (x,y) = modN sz (x * y);

  (* Functions *)

  fun numeralFn i sz = SOME (modN sz i);

  fun addFn sz x y = SOME (modN sz (x + y));

  fun expFn sz x y = SOME (exp (multN sz) x y (oneN sz));

  fun multFn sz x y = SOME (multN sz (x,y));

  fun negFn {size = N} x = SOME (if x = 0 then 0 else N - x);

  fun preFn {size = N} x = SOME (if x = 0 then N - 1 else x - 1);

  fun subFn {size = N} x y = SOME (if x < y then N + x - y else x - y);

  fun sucFn {size = N} x = SOME (if x = N - 1 then 0 else x + 1);

  (* Relations *)

  fun dividesRel {size = N} x y =
      let
        val g = gcd N x
      in
        if divides g y then NONE else SOME false
      end;

  fun evenRel {size = N} x = if N mod 2 = 0 then SOME (x mod 2 = 0) else NONE;

  fun isZeroRel _ x = if x <> 0 then SOME false else NONE;

  fun oddRel {size = N} x = if N mod 2 = 0 then SOME (x mod 2 = 1) else NONE;
in
  val modularFixed =
      let
        val fns =
            NameArityMap.fromList
              (map (fn i => ((numeralName i,0), fixed0 (numeralFn i)))
                 numeralList @
               [((addName,2), fixed2 addFn),
                ((expName,2), fixed2 expFn),
                ((multName,2), fixed2 multFn),
                ((negName,1), fixed1 negFn),
                ((preName,1), fixed1 preFn),
                ((subName,2), fixed2 subFn),
                ((sucName,1), fixed1 sucFn)])

        val rels =
            NameArityMap.fromList
              [((dividesName,2), fixed2 dividesRel),
               ((evenName,1), fixed1 evenRel),
               ((isZeroName,1), fixed1 isZeroRel),
               ((oddName,1), fixed1 oddRel)]
      in
        Fixed
          {functions = fns,
           relations = rels}
      end;
end;

local
  (* Support *)

  fun cutN {size = N} x = if x >= N then N - 1 else x;

  fun oneN sz = cutN sz 1;

  fun multN sz (x,y) = cutN sz (x * y);

  (* Functions *)

  fun numeralFn i sz = if i < 0 then NONE else SOME (cutN sz i);

  fun addFn sz x y = SOME (cutN sz (x + y));

  fun divFn _ x y = if y = 0 then NONE else SOME (x div y);

  fun expFn sz x y = SOME (exp (multN sz) x y (oneN sz));

  fun modFn {size = N} x y =
      if y = 0 orelse x = N - 1 then NONE else SOME (x mod y);

  fun multFn sz x y = SOME (multN sz (x,y));

  fun negFn _ x = if x = 0 then SOME 0 else NONE;

  fun preFn _ x = if x = 0 then NONE else SOME (x - 1);

  fun subFn {size = N} x y =
      if y = 0 then SOME x
      else if x = N - 1 orelse x < y then NONE
      else SOME (x - y);

  fun sucFn sz x = SOME (cutN sz (x + 1));

  (* Relations *)

  fun dividesRel {size = N} x y =
      if x = 1 orelse y = 0 then SOME true
      else if x = 0 then SOME false
      else if y = N - 1 then NONE
      else SOME (divides x y);

  fun evenRel {size = N} x =
      if x = N - 1 then NONE else SOME (x mod 2 = 0);

  fun geRel {size = N} y x =
      if x = N - 1 then if y = N - 1 then NONE else SOME false
      else if y = N - 1 then SOME true else SOME (x <= y);

  fun gtRel {size = N} y x =
      if x = N - 1 then if y = N - 1 then NONE else SOME false
      else if y = N - 1 then SOME true else SOME (x < y);

  fun isZeroRel _ x = SOME (x = 0);

  fun leRel {size = N} x y =
      if x = N - 1 then if y = N - 1 then NONE else SOME false
      else if y = N - 1 then SOME true else SOME (x <= y);

  fun ltRel {size = N} x y =
      if x = N - 1 then if y = N - 1 then NONE else SOME false
      else if y = N - 1 then SOME true else SOME (x < y);

  fun oddRel {size = N} x =
      if x = N - 1 then NONE else SOME (x mod 2 = 1);
in
  val overflowFixed =
      let
        val fns =
            NameArityMap.fromList
              (map (fn i => ((numeralName i,0), fixed0 (numeralFn i)))
                 numeralList @
               [((addName,2), fixed2 addFn),
                ((divName,2), fixed2 divFn),
                ((expName,2), fixed2 expFn),
                ((modName,2), fixed2 modFn),
                ((multName,2), fixed2 multFn),
                ((negName,1), fixed1 negFn),
                ((preName,1), fixed1 preFn),
                ((subName,2), fixed2 subFn),
                ((sucName,1), fixed1 sucFn)])

        val rels =
            NameArityMap.fromList
              [((dividesName,2), fixed2 dividesRel),
               ((evenName,1), fixed1 evenRel),
               ((geName,2), fixed2 geRel),
               ((gtName,2), fixed2 gtRel),
               ((isZeroName,1), fixed1 isZeroRel),
               ((leName,2), fixed2 leRel),
               ((ltName,2), fixed2 ltRel),
               ((oddName,1), fixed1 oddRel)]
      in
        Fixed
          {functions = fns,
           relations = rels}
      end;
end;

(* Sets *)

val cardName = Name.fromString "card"
and complementName = Name.fromString "complement"
and emptyName = Name.fromString "empty"
and memberName = Name.fromString "member"
and insertName = Name.fromString "insert"
and intersectName = Name.fromString "intersect"
and subsetName = Name.fromString "subset"
and unionName = Name.fromString "union"
and universeName = Name.fromString "universe";

local
  (* Support *)

  fun eltN {size = N} =
      let
        fun f 0 acc = acc
          | f x acc = f (x div 2) (acc + 1)
      in
        f N ~1
      end;

  fun posN i = Word.<< (0w1, Word.fromInt i);

  fun univN sz = Word.- (posN (eltN sz), 0w1);

  fun setN sz x = Word.andb (Word.fromInt x, univN sz);

  (* Functions *)

  fun cardFn sz x =
      let
        fun f 0w0 acc = acc
          | f s acc =
            let
              val acc = if Word.andb (s,0w1) = 0w0 then acc else acc + 1
            in
              f (Word.>> (s,0w1)) acc
            end
      in
        SOME (f (setN sz x) 0)
      end;

  fun complementFn sz x = SOME (Word.toInt (Word.xorb (univN sz, setN sz x)));

  fun emptyFn _ = SOME 0;

  fun insertFn sz x y =
      let
        val x = x mod eltN sz
        and y = setN sz y
      in
        SOME (Word.toInt (Word.orb (posN x, y)))
      end;

  fun intersectFn sz x y =
      SOME (Word.toInt (Word.andb (setN sz x, setN sz y)));

  fun unionFn sz x y =
      SOME (Word.toInt (Word.orb (setN sz x, setN sz y)));

  fun universeFn sz = SOME (Word.toInt (univN sz));

  (* Relations *)

  fun memberRel sz x y =
      let
        val x = x mod eltN sz
        and y = setN sz y
      in
        SOME (Word.andb (posN x, y) <> 0w0)
      end;

  fun subsetRel sz x y =
      let
        val x = setN sz x
        and y = setN sz y
      in
        SOME (Word.andb (Word.xorb (univN sz, x), y) = 0w0)
      end;
in
  val setFixed =
      let
        val fns =
            NameArityMap.fromList
              [((cardName,1), fixed1 cardFn),
               ((complementName,1), fixed1 complementFn),
               ((emptyName,0), fixed0 emptyFn),
               ((insertName,2), fixed2 insertFn),
               ((intersectName,2), fixed2 intersectFn),
               ((unionName,2), fixed2 unionFn),
               ((universeName,0), fixed0 universeFn)]

        val rels =
            NameArityMap.fromList
              [((memberName,2), fixed2 memberRel),
               ((subsetName,2), fixed2 subsetRel)]
      in
        Fixed
          {functions = fns,
           relations = rels}
      end;
end;

(* Lists *)

val appendName = Name.fromString "@"
and consName = Name.fromString "::"
and nilName = Name.fromString "nil"
and nullName = Name.fromString "null"
and tailName = Name.fromString "tail";

local
  val fixMap =
     {functionMap = NameArityMap.fromList
                      [((appendName,2),addName),
                       ((consName,1),sucName),
                       ((nilName,0), numeralName 0),
                       ((tailName,1),preName)],
      relationMap = NameArityMap.fromList
                      [((nullName,1),isZeroName)]};
in
  val listFixed = mapFixed fixMap overflowFixed;
end;

(* ------------------------------------------------------------------------- *)
(* Valuations.                                                               *)
(* ------------------------------------------------------------------------- *)

datatype valuation = Valuation of element NameMap.map;

val emptyValuation = Valuation (NameMap.new ());

fun insertValuation (Valuation m) v_i = Valuation (NameMap.insert m v_i);

fun peekValuation (Valuation m) v = NameMap.peek m v;

fun constantValuation i =
    let
      fun add (v,V) = insertValuation V (v,i)
    in
      NameSet.foldl add emptyValuation
    end;

val zeroValuation = constantValuation zeroElement;

fun getValuation V v =
    case peekValuation V v of
      SOME i => i
    | NONE => raise Error "Model.getValuation: incomplete valuation";

fun randomValuation {size = N} vs =
    let
      fun f (v,V) = insertValuation V (v, Portable.randomInt N)
    in
      NameSet.foldl f emptyValuation vs
    end;

fun incrementValuation N vars =
    let
      fun inc vs V =
          case vs of
            [] => NONE
          | v :: vs =>
            let
              val (carry,i) =
                  case incrementElement N (getValuation V v) of
                    SOME i => (false,i)
                  | NONE => (true,zeroElement)

              val V = insertValuation V (v,i)
            in
              if carry then inc vs V else SOME V
            end
    in
      inc (NameSet.toList vars)
    end;

fun foldValuation N vars f =
    let
      val inc = incrementValuation N vars

      fun fold V acc =
          let
            val acc = f (V,acc)
          in
            case inc V of
              NONE => acc
            | SOME V => fold V acc
          end

      val zero = zeroValuation vars
    in
      fold zero
    end;


(***

(* ------------------------------------------------------------------------- *)
(* Interpreted names.                                                        *)
(* ------------------------------------------------------------------------- *)

(* Projections *)

fun ithFnName i = Name.fromString ("#" ^ Int.toString i);

val firstFnName = ithFnName 1
and secondFnName = ithFnName 2
and thirdFnName = ithFnName 3;

(* Pure *)

val eqRelName = Atom.eqRelationName;

(* Basic *)

val hasTypeFnName = Term.hasTypeName
and idFnName = Name.fromString "id"
and fstFnName = Name.fromString "fst"
and sndFnName = Name.fromString "snd";

val notEqualRelName = Name.fromString "<>";

(* Number *)

val addFnName = Name.fromString "+"
and divFnName = Name.fromString "div"
and expFnName = Name.fromString "exp"
and modFnName = Name.fromString "mod"
and multFnName = Name.fromString "*"
and negFnName = Name.fromString "~"
and preFnName = Name.fromString "pre"
and subFnName = Name.fromString "-"
and sucFnName = Name.fromString "suc"
and zeroFnName = Name.fromString "0";

val dividesRelName = Name.fromString "divides"
and evenRelName = Name.fromString "even"
and geRelName = Name.fromString ">="
and gtRelName = Name.fromString ">"
and isZeroRelName = Name.fromString "is_0"
and leRelName = Name.fromString "<="
and ltRelName = Name.fromString "<"
and oddRelName = Name.fromString "odd";

(* Sets *)

val emptyFnName = Name.fromString "empty"
and univFnName = Name.fromString "univ"
and unionFnName = Name.fromString "union"
and intersectFnName = Name.fromString "intersect"
and complFnName = Name.fromString "compl"
and cardFnName = Name.fromString "card";

val inRelName = Name.fromString "in"
and subsetRelName = Name.fromString "subset";

(* Lists *)

val appendFnName = Name.fromString "@"
and consFnName = Name.fromString "::"
and nilFnName = Name.fromString "nil";

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

local
  val M = Option.getOpt (Int.maxInt,~1);

  val Msqrt = if M < 0 then M else Real.floor (Math.sqrt (Real.fromInt M));

  fun iexp _ 0 z = SOME z
    | iexp x 1 z =
      if x <= Msqrt then SOME (x * z)
      else if z > Msqrt orelse z > M div x then NONE
      else SOME (x * z)
    | iexp x y z =
      if x > Msqrt then NONE
      else iexp (x * x) (y div 2) (if y mod 2 = 0 then z else z * x);
in
  (* intExp x y = SOME (exp op* x y 1) handle Overflow => NONE *)
  fun intExp _ 0 = SOME 1
    | intExp x 1 = SOME x
    | intExp 1 _ = SOME 1
    | intExp ~1 y = SOME (if y mod 2 = 0 then 1 else ~1)
    | intExp x y =
      if y < 0 then raise Bug "intExp: negative exponent"
      else if x = 0 then SOME 0
      else if x < 0 then
        case intExp (~x) y of
          SOME r => SOME (if y mod 2 = 0 then r else ~r)
        | NONE => NONE
      else if M < 0 then SOME (exp op* x y 1)
      else iexp x y 1;
end;

fun boolToInt true = 1
  | boolToInt false = 0;

fun intToBool 1 = true
  | intToBool 0 = false
  | intToBool _ = raise Bug "Model.intToBool";

fun intListToInt base =
    let
      fun f [] = 0
        | f (x :: xs) = x + base * f xs
    in
      f
    end;

fun natFromString "" = NONE
  | natFromString "0" = SOME 0
  | natFromString s =
    if not (Option.isSome (intExp 10 (size s))) then NONE
    else
      case charToInt (String.sub (s,0)) of
        NONE => NONE
      | SOME 0 => NONE
      | SOME d =>
        let
          fun parse 0 _ acc = SOME acc
            | parse n i acc =
              case charToInt (String.sub (s,i)) of
                NONE => NONE
              | SOME d => parse (n - 1) (i + 1) (10 * acc + d)
        in
          parse (size s - 1) 1 d
        end;

fun projection func =
    case func of
      (_,[]) => NONE
    | (f,[x]) => if Name.equal f firstFnName then SOME x else NONE
    | (f,[x,y]) =>
      if Name.equal f firstFnName then SOME x
      else if Name.equal f secondFnName then SOME y
      else NONE
    | (f,[x,y,z]) =>
      if Name.equal f firstFnName then SOME x
      else if Name.equal f secondFnName then SOME y
      else if Name.equal f thirdFnName then SOME z
      else NONE
    | (f,args) =>
      let
        val f = Name.toString f
        val fSz = size f
        and n = length args

        val p =
            if fSz < 2 orelse String.sub (f,0) <> #"#" then NONE
            else if fSz = 2 then charToInt (String.sub (f,1))
            else
              case intExp 10 (fSz - 1) of
                NONE => NONE
              | SOME n' =>
                if 10 * n < n' then NONE
                else natFromString (String.extract (f,1,NONE))
      in
        case p of
          NONE => NONE
        | SOME k =>
          if k <= 0 orelse k > n then NONE
          else SOME (List.nth (args, k - 1))
      end;

(* ------------------------------------------------------------------------- *)
(* The parts of the model that are fixed.                                    *)
(* ------------------------------------------------------------------------- *)

type fixedModel =
     {functions : Term.functionName * element list -> element option,
      relations : Atom.relationName * element list -> bool option};

type fixed = {size : int} -> fixedModel

fun fixedMerge fixed1 fixed2 parm =
    let
      val {functions = f1, relations = r1} = fixed1 parm
      and {functions = f2, relations = r2} = fixed2 parm

      fun functions x = case f2 x of NONE => f1 x | s => s

      fun relations x = case r2 x of NONE => r1 x | s => s
    in
      {functions = functions, relations = relations}
    end;

fun fixedMergeList [] = raise Bug "fixedMergeList: empty"
  | fixedMergeList (f :: l) = foldl (uncurry fixedMerge) f l;

fun fixedPure {size = _} =
    let
      fun functions _ = NONE

      fun relations (rel,[x,y]) =
          if Name.equal rel eqRelName then SOME (x = y) else NONE
        | relations _ = NONE
    in
      {functions = functions, relations = relations}
    end;

fun fixedBasic {size = _} =
    let
      fun functions func =
          let
            val result =
                case func of
                  (f,[x]) =>
                  if Name.equal f idFnName then SOME x else NONE
                | (f,[x,y]) =>
                  if Name.equal f hasTypeFnName then SOME x
                  else if Name.equal f fstFnName then SOME x
                  else if Name.equal f sndFnName then SOME y
                  else NONE
                | _ => NONE
          in
            case result of
              SOME _ => result
            | NONE => projection func
          end

      fun relations rel =
          case rel of
            (r,[x,y]) =>
            if Name.equal r notEqualRelName then SOME (x <> y)
            else NONE
          | _ => NONE
    in
      {functions = functions, relations = relations}
    end;

fun fixedModulo {size = N} =
    let
      fun mod_N k = k mod N

      val one = mod_N 1

      fun mult (x,y) = mod_N (x * y)

      fun divides_N 0 = false
        | divides_N x = N mod x = 0

      val even_N = divides_N 2

      fun functions func =
          case func of
            (f,[]) => Option.map mod_N (natFromString (Name.toString f))
          | (f,[x]) =>
            if Name.equal f sucFnName then
              SOME (if x = N - 1 then 0 else x + 1)
            else if Name.equal f preFnName then
              SOME (if x = 0 then N - 1 else x - 1)
            else if Name.equal f negFnName then
              SOME (if x = 0 then 0 else N - x)
            else
              NONE
          | (f,[x,y]) =>
            if Name.equal f addFnName then
              SOME (mod_N (x + y))
            else if Name.equal f subFnName then
              SOME (if x < y then N + x - y else x - y)
            else if Name.equal f multFnName then
              SOME (mult (x,y))
            else if Name.equal f divFnName then
              if divides_N y then SOME (x div y) else NONE
            else if Name.equal f modFnName then
              if divides_N y then SOME (x mod y) else NONE
            else if Name.equal f expFnName then
              SOME (exp mult x y one)
            else
              NONE
          | _ => NONE

      fun relations rel =
          case rel of
            (r,[x]) =>
            if Name.equal r isZeroRelName then
              SOME (x = 0)
            else if Name.equal r evenRelName then
              if even_N then SOME (x mod 2 = 0) else NONE
            else if Name.equal r oddRelName then
              if even_N then SOME (x mod 2 = 1) else NONE
            else
              NONE
          | (r,[x,y]) =>
            if Name.equal r dividesRelName then
              if x = 0 then SOME (y = 0)
              else if divides_N x then SOME (y mod x = 0) else NONE
            else
              NONE
          | _ => NONE
    in
      {functions = functions, relations = relations}
    end;

local
  datatype onum = ONeg | ONum of int | OInf;

  val zero = ONum 0
  and one = ONum 1
  and two = ONum 2;

  fun suc (ONum x) = ONum (x + 1)
    | suc v = v;

  fun pre (ONum 0) = ONeg
    | pre (ONum x) = ONum (x - 1)
    | pre v = v;

  fun neg ONeg = NONE
    | neg (n as ONum 0) = SOME n
    | neg _ = SOME ONeg;

  fun add ONeg ONeg = SOME ONeg
    | add ONeg (ONum y) = if y = 0 then SOME ONeg else NONE
    | add ONeg OInf = NONE
    | add (ONum x) ONeg = if x = 0 then SOME ONeg else NONE
    | add (ONum x) (ONum y) = SOME (ONum (x + y))
    | add (ONum _) OInf = SOME OInf
    | add OInf ONeg = NONE
    | add OInf (ONum _) = SOME OInf
    | add OInf OInf = SOME OInf;

  fun sub ONeg ONeg = NONE
    | sub ONeg (ONum _) = SOME ONeg
    | sub ONeg OInf = SOME ONeg
    | sub (ONum _) ONeg = NONE
    | sub (ONum x) (ONum y) = SOME (if x < y then ONeg else ONum (x - y))
    | sub (ONum _) OInf = SOME ONeg
    | sub OInf ONeg = SOME OInf
    | sub OInf (ONum y) = if y = 0 then SOME OInf else NONE
    | sub OInf OInf = NONE;

  fun mult ONeg ONeg = NONE
    | mult ONeg (ONum y) = SOME (if y = 0 then zero else ONeg)
    | mult ONeg OInf = SOME ONeg
    | mult (ONum x) ONeg = SOME (if x = 0 then zero else ONeg)
    | mult (ONum x) (ONum y) = SOME (ONum (x * y))
    | mult (ONum x) OInf = SOME (if x = 0 then zero else OInf)
    | mult OInf ONeg = SOME ONeg
    | mult OInf (ONum y) = SOME (if y = 0 then zero else OInf)
    | mult OInf OInf = SOME OInf;

  fun exp ONeg ONeg = NONE
    | exp ONeg (ONum y) =
      if y = 0 then SOME one else if y mod 2 = 0 then NONE else SOME ONeg
    | exp ONeg OInf = NONE
    | exp (ONum x) ONeg = NONE
    | exp (ONum x) (ONum y) =
      SOME (case intExp x y of SOME n => ONum n | NONE => OInf)
    | exp (ONum x) OInf =
      SOME (if x = 0 then zero else if x = 1 then one else OInf)
    | exp OInf ONeg = NONE
    | exp OInf (ONum y) = SOME (if y = 0 then one else OInf)
    | exp OInf OInf = SOME OInf;

  fun odiv ONeg ONeg = NONE
    | odiv ONeg (ONum y) = if y = 1 then SOME ONeg else NONE
    | odiv ONeg OInf = NONE
    | odiv (ONum _) ONeg = NONE
    | odiv (ONum x) (ONum y) = if y = 0 then NONE else SOME (ONum (x div y))
    | odiv (ONum _) OInf = SOME zero
    | odiv OInf ONeg = NONE
    | odiv OInf (ONum y) = if y = 1 then SOME OInf else NONE
    | odiv OInf OInf = NONE;

  fun omod ONeg ONeg = NONE
    | omod ONeg (ONum y) = if y = 1 then SOME zero else NONE
    | omod ONeg OInf = NONE
    | omod (ONum _) ONeg = NONE
    | omod (ONum x) (ONum y) = if y = 0 then NONE else SOME (ONum (x mod y))
    | omod (x as ONum _) OInf = SOME x
    | omod OInf ONeg = NONE
    | omod OInf (ONum y) = if y = 1 then SOME OInf else NONE
    | omod OInf OInf = NONE;

  fun le ONeg ONeg = NONE
    | le ONeg (ONum y) = SOME true
    | le ONeg OInf = SOME true
    | le (ONum _) ONeg = SOME false
    | le (ONum x) (ONum y) = SOME (x <= y)
    | le (ONum _) OInf = SOME true
    | le OInf ONeg = SOME false
    | le OInf (ONum _) = SOME false
    | le OInf OInf = NONE;

  fun lt x y = Option.map not (le y x);

  fun ge x y = le y x;

  fun gt x y = lt y x;

  fun divides ONeg ONeg = NONE
    | divides ONeg (ONum y) = if y = 0 then SOME true else NONE
    | divides ONeg OInf = NONE
    | divides (ONum x) ONeg =
      if x = 0 then SOME false else if x = 1 then SOME true else NONE
    | divides (ONum x) (ONum y) = SOME (Useful.divides x y)
    | divides (ONum x) OInf =
      if x = 0 then SOME false else if x = 1 then SOME true else NONE
    | divides OInf ONeg = NONE
    | divides OInf (ONum y) = SOME (y = 0)
    | divides OInf OInf = NONE;

  fun even n = divides two n;

  fun odd n = Option.map not (even n);

  fun fixedOverflow mk_onum dest_onum =
      let
        fun partial_dest_onum NONE = NONE
          | partial_dest_onum (SOME n) = dest_onum n

        fun functions func =
            case func of
              (f,[]) =>
              (case natFromString (Name.toString f) of
                 SOME n => dest_onum (ONum n)
               | NONE => NONE)
            | (f,[x]) =>
              if Name.equal f sucFnName then
                dest_onum (suc (mk_onum x))
              else if Name.equal f preFnName then
                dest_onum (pre (mk_onum x))
              else if Name.equal f negFnName then
                partial_dest_onum (neg (mk_onum x))
              else
                NONE
            | (f,[x,y]) =>
              if Name.equal f addFnName then
                partial_dest_onum (add (mk_onum x) (mk_onum y))
              else if Name.equal f subFnName then
                partial_dest_onum (sub (mk_onum x) (mk_onum y))
              else if Name.equal f multFnName then
                partial_dest_onum (mult (mk_onum x) (mk_onum y))
              else if Name.equal f expFnName then
                partial_dest_onum (exp (mk_onum x) (mk_onum y))
              else if Name.equal f divFnName then
                partial_dest_onum (odiv (mk_onum x) (mk_onum y))
              else if Name.equal f modFnName then
                partial_dest_onum (omod (mk_onum x) (mk_onum y))
              else
                NONE
            | _ => NONE

        fun relations rel =
            case rel of
              (r,[x]) =>
              if Name.equal r isZeroRelName then
                SOME (mk_onum x = zero)
              else if Name.equal r evenRelName then
                even (mk_onum x)
              else if Name.equal r oddRelName then
                odd (mk_onum x)
              else
                NONE
            | (r,[x,y]) =>
              if Name.equal r leRelName then
                le (mk_onum x) (mk_onum y)
              else if Name.equal r ltRelName then
                lt (mk_onum x) (mk_onum y)
              else if Name.equal r geRelName then
                ge (mk_onum x) (mk_onum y)
              else if Name.equal r gtRelName then
                gt (mk_onum x) (mk_onum y)
              else if Name.equal r dividesRelName then
                divides (mk_onum x) (mk_onum y)
              else
                NONE
            | _ => NONE
      in
        {functions = functions, relations = relations}
      end;
in
  fun fixedOverflowNum {size = N} =
      let
        val oinf = N - 1

        fun mk_onum x = if x = oinf then OInf else ONum x

        fun dest_onum ONeg = NONE
          | dest_onum (ONum x) = SOME (if x < oinf then x else oinf)
          | dest_onum OInf = SOME oinf
      in
        fixedOverflow mk_onum dest_onum
      end;

  fun fixedOverflowInt {size = N} =
      let
        val oinf = N - 2
        val oneg = N - 1

        fun mk_onum x =
            if x = oneg then ONeg else if x = oinf then OInf else ONum x

        fun dest_onum ONeg = SOME oneg
          | dest_onum (ONum x) = SOME (if x < oinf then x else oinf)
          | dest_onum OInf = SOME oinf
      in
        fixedOverflow mk_onum dest_onum
      end;
end;

fun fixedSet {size = N} =
    let
      val M =
          let
            fun f 0 acc = acc
              | f x acc = f (x div 2) (acc + 1)
          in
            f N 0
          end

      val univ = IntSet.fromList (interval 0 M)

      val mk_set =
          let
            fun f _ s 0 = s
              | f k s x =
                let
                  val s = if x mod 2 = 0 then s else IntSet.add s k
                in
                  f (k + 1) s (x div 2)
                end
          in
            f 0 IntSet.empty
          end

      fun dest_set s =
          let
            fun f 0 x = x
              | f k x =
                let
                  val k = k - 1
                in
                  f k (if IntSet.member k s then 2 * x + 1 else 2 * x)
                end

            val x = case IntSet.findr (K true) s of NONE => 0 | SOME k => f k 1
          in
            if x < N then SOME x else NONE
          end

      fun functions func =
          case func of
            (f,[]) =>
            if Name.equal f emptyFnName then
              dest_set IntSet.empty
            else if Name.equal f univFnName then
              dest_set univ
            else
              NONE
          | (f,[x]) =>
            if Name.equal f complFnName then
              dest_set (IntSet.difference univ (mk_set x))
            else if Name.equal f cardFnName then
              SOME (IntSet.size (mk_set x))
            else
              NONE
          | (f,[x,y]) =>
            if Name.equal f unionFnName then
              dest_set (IntSet.union (mk_set x) (mk_set y))
            else if Name.equal f intersectFnName then
              dest_set (IntSet.intersect (mk_set x) (mk_set y))
            else
              NONE
          | _ => NONE

      fun relations rel =
          case rel of
            (r,[x,y]) =>
            if Name.equal r inRelName then
              SOME (IntSet.member (x mod M) (mk_set y))
            else if Name.equal r subsetRelName then
              SOME (IntSet.subset (mk_set x) (mk_set y))
            else
              NONE
          | _ => NONE
    in
      {functions = functions, relations = relations}
    end;

fun fixedList {size = N} =
    let
      val {functions = funcs, relations = rels} = fixedOverflowNum {size = N}

      fun functions func =
          case func of
            (f,[]) =>
            if Name.equal f nilFnName then
              funcs (zeroFnName,[])
            else
              NONE
          | (f,[x,y]) =>
            if Name.equal f consFnName then
              funcs (sucFnName,[y])
            else if Name.equal f appendFnName then
              funcs (addFnName,[x,y])
            else
              NONE
          | _ => NONE

      fun relations _ = NONE
    in
      {functions = functions,
       relations = relations}
    end;

(* ------------------------------------------------------------------------- *)
(* A type of random finite mapping Z^n -> Z.                                 *)
(* ------------------------------------------------------------------------- *)

val SMALL_SPACE = 100;

val SPARSENESS_THRESHOLD = 10;

val UNKNOWN = 0;

datatype table =
    MapTable of int * (int list, int) Map.map
  | ArrayTable of int Array.array;

fun emptyTable N arity =
    let
      val space = Option.getOpt (intExp N arity, ~1)
      val small =
          space > 0 andalso
          (N = 1 orelse arity <= 1 orelse space < SMALL_SPACE)
    in
      if small then ArrayTable (Array.array (space,UNKNOWN))
      else MapTable (space, Map.new (lexCompare Int.compare))
    end;

fun lookupFixed R fixed elts =
    case fixed elts of
      SOME r => ~(r + 1)
    | NONE => Portable.randomInt R + 1;

fun lookupTable N R fixed table elts =
    case table of
      ref (ArrayTable a) =>
      let
        val i = intListToInt N elts
        val r = Array.sub (a,i)
      in
        if r <> UNKNOWN then r
        else
          let
            val r = lookupFixed R fixed elts
            val () = Array.update (a,i,r)
          in
            r
          end
      end
    | ref (MapTable (space,m)) =>
      case Map.peek m elts of
        SOME r => r
      | NONE =>
        let
          val r = lookupFixed R fixed elts
          val m = Map.insert m (elts,r)
          val sparse =
              space <= 0 orelse SPARSENESS_THRESHOLD * Map.size m < space
          val t =
              if sparse then MapTable (space,m)
              else
                let
                  val a = Array.array (space,UNKNOWN)

                  fun pop (elts,r) = Array.update (a, intListToInt N elts, r)

                  val () = Map.app pop m
                in
                  ArrayTable a
                end

          val () = table := t
        in
          r
        end;

fun updateTable N table (elts,r) =
    case table of
      ref (ArrayTable a) => Array.update (a, intListToInt N elts, r)
    | ref (MapTable (space,m)) =>
      let
        val m = Map.insert m (elts,r)
      in
        table := MapTable (space,m)
      end;

(* ------------------------------------------------------------------------- *)
(* A type of random finite mappings name * arity -> Z^arity -> Z.            *)
(* ------------------------------------------------------------------------- *)

datatype tables =
    Tables of
      {fixed : Name.name -> int list -> int option,
       tables : table ref NameArityMap.map ref};

fun emptyTables fixed =
    Tables
      {fixed = fixed,
       tables = ref (NameArityMap.new ())};

fun getTables tables N name_arity =
    let
      val Tables {tables as ref m, ...} = tables
      val (_,arity) = name_arity
    in
      case NameArityMap.peek m name_arity of
        SOME t => t
      | NONE =>
        let
          val t = ref (emptyTable N arity)
          val () = tables := NameArityMap.insert m (name_arity,t)
        in
          t
        end
    end;

local
  fun lookup tables N R (name,elts) =
      let
        val Tables {fixed, ...} = tables
        val table = getTables tables N (name, length elts)
      in
        lookupTable N R (fixed name) table elts
      end;
in
  fun lookupTables tables N R name_elts =
      Int.abs (lookup tables N R name_elts) - 1;

  fun isFixedTables tables N R name_elts = lookup tables N R name_elts < 0;
end;

fun updateTables tables N ((name,elts),elt) =
    let
      val table = getTables tables N (name, length elts)
    in
      updateTable N table (elts, elt + 1)
    end;

(* ------------------------------------------------------------------------- *)
(* A type of random finite models.                                           *)
(* ------------------------------------------------------------------------- *)

type parameters = {size : int, fixed : fixed};

datatype model =
    Model of
      {size : int,
       functions : tables,
       relations : tables};

fun new {size = N, fixed} =
    let
      val {functions = funcs, relations = rels} = fixed {size = N}
      fun funcs' func elts = funcs (func,elts)
      fun rels' rel elts = Option.map boolToInt (rels (rel,elts))
      val functions = emptyTables funcs'
      val relations = emptyTables rels'
    in
      Model
        {size = N,
         functions = functions,
         relations = relations}
    end;

fun size (Model {size = N, ...}) = N;

fun lookupFunction M func_elts =
    let
      val Model {size = N, functions, ...} = M
    in
      lookupTables functions N N func_elts
    end;

fun isFixedFunction M func_elts =
    let
      val Model {size = N, functions, ...} = M
    in
      isFixedTables functions N N func_elts
    end;

fun perturbFunction M func_elts_elt =
    let
      val Model {size = N, functions, ...} = M
    in
      updateTables functions N func_elts_elt
    end;

fun lookupRelation M rel_elts =
    let
      val Model {size = N, relations, ...} = M
    in
      intToBool (lookupTables relations N 2 rel_elts)
    end;

fun isFixedRelation M rel_elts =
    let
      val Model {size = N, relations, ...} = M
    in
      isFixedTables relations N 2 rel_elts
    end;

fun perturbRelation M (rel_elts,pol) =
    let
      val Model {size = N, relations, ...} = M
    in
      updateTables relations N (rel_elts, boolToInt pol)
    end;

(* ------------------------------------------------------------------------- *)
(* A type of terms with interpretations embedded in the subterms.            *)
(* ------------------------------------------------------------------------- *)

datatype modelTerm =
    ModelVar
  | ModelFn of Term.functionName * modelTerm list * int list;

fun destTerm tm =
    case tm of
      Term.Var _ => tm
    | Term.Fn f_tms =>
      case Term.stripApp tm of
        (_,[]) => tm
      | (v as Term.Var _, tms) => Term.Fn (Term.appName, v :: tms)
      | (Term.Fn (f,tms), tms') => Term.Fn (f, tms @ tms');

fun modelTerm M V =
    let
      fun modelTm tm =
          case destTerm tm of
            Term.Var v => (ModelVar, getValuation V v)
          | Term.Fn (f,tms) =>
            let
              val (tms,xs) = unzip (map modelTm tms)
            in
              (ModelFn (f,tms,xs), lookupFunction M (f,xs))
            end
    in
      modelTm
    end;

(* ------------------------------------------------------------------------- *)
(* Interpreting terms and formulas in the model.                             *)
(* ------------------------------------------------------------------------- *)

fun interpretTerm M V =
    let
      fun interpret tm =
          case destTerm tm of
            Term.Var v => getValuation V v
          | Term.Fn (f,tms) => lookupFunction M (f, map interpret tms)
    in
      interpret
    end;

fun interpretAtom M V (r,tms) =
    lookupRelation M (r, map (interpretTerm M V) tms);

fun interpretFormula M =
    let
      val N = size M

      fun interpret V fm =
          case fm of
            Formula.True => true
          | Formula.False => false
          | Formula.Atom atm => interpretAtom M V atm
          | Formula.Not p => not (interpret V p)
          | Formula.Or (p,q) => interpret V p orelse interpret V q
          | Formula.And (p,q) => interpret V p andalso interpret V q
          | Formula.Imp (p,q) => interpret V (Formula.Or (Formula.Not p, q))
          | Formula.Iff (p,q) => interpret V p = interpret V q
          | Formula.Forall (v,p) => interpret' V p v N
          | Formula.Exists (v,p) =>
            interpret V (Formula.Not (Formula.Forall (v, Formula.Not p)))

      and interpret' V fm v i =
          i = 0 orelse
          let
            val i = i - 1
            val V' = insertValuation V (v,i)
          in
            interpret V' fm andalso interpret' V fm v i
          end
    in
      interpret
    end;

fun interpretLiteral M V (pol,atm) =
    let
      val b = interpretAtom M V atm
    in
      if pol then b else not b
    end;

fun interpretClause M V cl = LiteralSet.exists (interpretLiteral M V) cl;

(* ------------------------------------------------------------------------- *)
(* Check whether random groundings of a formula are true in the model.       *)
(* Note: if it's cheaper, a systematic check will be performed instead.      *)
(* ------------------------------------------------------------------------- *)

fun check interpret {maxChecks} M fv x =
    let
      val N = size M

      fun score (V,{T,F}) =
          if interpret M V x then {T = T + 1, F = F} else {T = T, F = F + 1}

      fun randomCheck acc = score (randomValuation {size = N} fv, acc)

      val maxChecks =
          case maxChecks of
            NONE => maxChecks
          | SOME m =>
            case intExp N (NameSet.size fv) of
              SOME n => if n <= m then NONE else maxChecks
            | NONE => maxChecks
    in
      case maxChecks of
        SOME m => funpow m randomCheck {T = 0, F = 0}
      | NONE => foldValuation {size = N} fv score {T = 0, F = 0}
    end;

fun checkAtom maxChecks M atm =
    check interpretAtom maxChecks M (Atom.freeVars atm) atm;

fun checkFormula maxChecks M fm =
    check interpretFormula maxChecks M (Formula.freeVars fm) fm;

fun checkLiteral maxChecks M lit =
    check interpretLiteral maxChecks M (Literal.freeVars lit) lit;

fun checkClause maxChecks M cl =
    check interpretClause maxChecks M (LiteralSet.freeVars cl) cl;

(* ------------------------------------------------------------------------- *)
(* Perturbing the model.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype perturbation =
    FunctionPerturbation of (Term.functionName * element list) * element
  | RelationPerturbation of (Atom.relationName * element list) * bool;

fun perturb M pert =
    case pert of
      FunctionPerturbation func_elts_elt => perturbFunction M func_elts_elt
    | RelationPerturbation rel_elts_pol => perturbRelation M rel_elts_pol;

local
  fun pertTerm _ [] _ acc = acc
    | pertTerm M target tm acc =
      case tm of
        ModelVar => acc
      | ModelFn (func,tms,xs) =>
        let
          fun onTarget ys = mem (lookupFunction M (func,ys)) target

          val func_xs = (func,xs)

          val acc =
              if isFixedFunction M func_xs then acc
              else
                let
                  fun add (y,acc) = FunctionPerturbation (func_xs,y) :: acc
                in
                  foldl add acc target
                end
        in
          pertTerms M onTarget tms xs acc
        end

  and pertTerms M onTarget =
      let
        val N = size M

        fun filterElements pred =
            let
              fun filt 0 acc = acc
                | filt i acc =
                  let
                    val i = i - 1
                    val acc = if pred i then i :: acc else acc
                  in
                    filt i acc
                  end
            in
              filt N []
            end

        fun pert _ [] [] acc = acc
          | pert ys (tm :: tms) (x :: xs) acc =
            let
              fun pred y =
                  y <> x andalso onTarget (List.revAppend (ys, y :: xs))

              val target = filterElements pred

              val acc = pertTerm M target tm acc
            in
              pert (x :: ys) tms xs acc
            end
          | pert _ _ _ _ = raise Bug "Model.pertTerms.pert"
      in
        pert []
      end;

  fun pertAtom M V target (rel,tms) acc =
      let
        fun onTarget ys = lookupRelation M (rel,ys) = target

        val (tms,xs) = unzip (map (modelTerm M V) tms)

        val rel_xs = (rel,xs)

        val acc =
            if isFixedRelation M rel_xs then acc
            else RelationPerturbation (rel_xs,target) :: acc
      in
        pertTerms M onTarget tms xs acc
      end;

  fun pertLiteral M V ((pol,atm),acc) = pertAtom M V pol atm acc;

  fun pertClause M V cl acc = LiteralSet.foldl (pertLiteral M V) acc cl;

  fun pickPerturb M perts =
      if null perts then ()
      else perturb M (List.nth (perts, Portable.randomInt (length perts)));
in
  fun perturbTerm M V (tm,target) =
      pickPerturb M (pertTerm M target (fst (modelTerm M V tm)) []);

  fun perturbAtom M V (atm,target) =
      pickPerturb M (pertAtom M V target atm []);

  fun perturbLiteral M V lit = pickPerturb M (pertLiteral M V (lit,[]));

  fun perturbClause M V cl = pickPerturb M (pertClause M V cl []);
end;

(* ------------------------------------------------------------------------- *)
(* Pretty printing.                                                          *)
(* ------------------------------------------------------------------------- *)

fun pp M =
    Print.program
      [Print.addString "Model{",
       Print.ppInt (size M),
       Print.addString "}"];
***)

end
