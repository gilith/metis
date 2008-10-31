(* ========================================================================= *)
(* THE TPTP PROBLEM FILE FORMAT (TPTP v2)                                    *)
(* Copyright (c) 2001-2007 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Tptp :> Tptp =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Default TPTP function and relation name mapping.                          *)
(* ------------------------------------------------------------------------- *)

val defaultFunctionMapping =
    [(* Mapping TPTP functions to infix symbols *)
     {name = "~", arity = 1, tptp = "negate"},
     {name = "*", arity = 2, tptp = "multiply"},
     {name = "/", arity = 2, tptp = "divide"},
     {name = "+", arity = 2, tptp = "add"},
     {name = "-", arity = 2, tptp = "subtract"},
     {name = "::", arity = 2, tptp = "cons"},
     {name = "@", arity = 2, tptp = "append"},
     {name = ",", arity = 2, tptp = "pair"},
     (* Expanding HOL symbols to TPTP alphanumerics *)
     {name = ":", arity = 2, tptp = "has_type"},
     {name = ".", arity = 2, tptp = "apply"},
     {name = "<=", arity = 0, tptp = "less_equal"}];

val defaultRelationMapping =
    [(* Mapping TPTP relations to infix symbols *)
     {name = "=", arity = 2, tptp = "="},  (* this preserves the = symbol *)
     {name = "==", arity = 2, tptp = "equalish"},
     {name = "<=", arity = 2, tptp = "less_equal"},
     {name = "<", arity = 2, tptp = "less_than"},
     (* Expanding HOL symbols to TPTP alphanumerics *)
     {name = "{}", arity = 1, tptp = "bool"}];

(* ------------------------------------------------------------------------- *)
(* Helper functions.                                                         *)
(* ------------------------------------------------------------------------- *)

fun isHdTlString hp tp s =
    let
      fun ct 0 = true
        | ct i = tp (String.sub (s,i)) andalso ct (i - 1)

      val n = size s
    in
      n > 0 andalso hp (String.sub (s,0)) andalso ct (n - 1)
    end;

fun stripSuffix pred s =
    let
      fun f 0 = ""
        | f n =
          let
            val n' = n - 1
          in
            if pred (String.sub (s,n')) then f n'
            else String.substring (s,0,n)
          end
    in
      f (size s)
    end;

fun variant avoid s =
    if not (StringSet.member s avoid) then s
    else
      let
        val s = stripSuffix Char.isDigit s

        fun var i =
            let
              val s_i = s ^ Int.toString i
            in
              if not (StringSet.member s_i avoid) then s_i else var (i + 1)
            end
      in
        var 0
      end;

(* ------------------------------------------------------------------------- *)
(* Mapping to legal TPTP names.                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun isTptpChar #"_" = true
    | isTptpChar c = Char.isAlphaNum c;

  val isTptpInitialChar = Char.isAlpha;

  fun explodeTptp s =
      let
        val l = explode s
        val l = List.filter isTptpChar l
        val l = dropWhile (not o isTptpInitialChar) l
      in
        l
      end;

  fun mkTptpName isLower emp s =
      let
        val s =
            case explodeTptp s of
              [] => emp
            | c :: cs =>
              let
                val first = if isLower then Char.toLower else Char.toUpper

                val c = first c
              in
                implode (c :: cs)
              end
      in
        s
      end;
in
  val mkTptpVarName = mkTptpName false "X"
  and mkTptpConstName = mkTptpName true "c"
  and mkTptpFnName = mkTptpName true "f"
  and mkTptpPropName = mkTptpName true "p"
  and mkTptpRelName = mkTptpName true "r";
end;

(* ------------------------------------------------------------------------- *)
(* Mapping to legal TPTP variable names.                                     *)
(* ------------------------------------------------------------------------- *)

datatype varToTptp =
    VarToTptp of StringSet.set * string NameMap.map;

val emptyVarToTptp = VarToTptp (StringSet.empty, NameMap.new ());

fun varToTptp v vm =
    let
      val VarToTptp (avoid,mapping) = vm
    in
      case NameMap.peek mapping v of
        SOME s => (s,vm)
      | NONE =>
        let
          val s = variant avoid (mkTptpVarName (Name.toString v))
          val avoid = StringSet.add avoid s
          and mapping = NameMap.insert mapping (v,s)
          val vm = VarToTptp (avoid,mapping)
        in
          (s,vm)
        end
    end;

(* ------------------------------------------------------------------------- *)
(* Mapping from TPTP variable names.                                         *)
(* ------------------------------------------------------------------------- *)

fun varFromTptp s = Name.fromString s;

(* ------------------------------------------------------------------------- *)
(* Mapping to TPTP function and relation names.                              *)
(* ------------------------------------------------------------------------- *)

datatype nameToTptp =
    NameToTptp of
      {funcs : string NameArityMap.map,
       rels : string NameArityMap.map};

local
  val emptyNames : string NameArityMap.map = NameArityMap.new ();

  fun addNames ({name,arity,tptp},mapping) =
      NameArityMap.insert mapping ((name,arity),tptp);

  val fromListNames = List.foldl addNames emptyNames;
in
  fun mkNameToTptp mapping =
      let
        val {functionMapping = funcMap, relationMapping = relMap} = mapping
        val funcs = fromListNames funcMap
        and rels = fromListNames relMap
      in
        NameToTptp {funcs = funcs, rels = rels}
      end;
end;

local
  fun singleQuote s = "'" ^ s ^ "'";

  fun nameToTptp zeroToTptp plusToTptp mapping na =
      case NameArityMap.peek mapping na of
        SOME s => s
      | NONE =>
        let
          val (n,a) = na
          val s = Name.toString n
          val toTptp = if a = 0 then zeroToTptp else plusToTptp
          val s = if toTptp s = s then s else singleQuote s
        in
          s
        end;
in
  fun fnNameToTptp names fa =
      let
        val NameToTptp {funcs, rels = _} = names
      in
        nameToTptp mkTptpConstName mkTptpFnName funcs fa
      end;

  fun relNameToTptp names ra =
      let
        val NameToTptp {funcs = _, rels} = names
      in
        nameToTptp mkTptpPropName mkTptpRelName rels ra
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Mapping from TPTP function and relation names.                            *)
(* ------------------------------------------------------------------------- *)

datatype nameFromTptp =
    NameFromTptp of
      {funcs : (string * int, Name.name) Map.map,
       rels : (string * int, Name.name) Map.map};

local
  val stringArityCompare = prodCompare String.compare Int.compare;

  val emptyStringArityMap = Map.new stringArityCompare;

  fun addStringArityMap ({name,arity,tptp},mapping) =
      Map.insert mapping ((tptp,arity),name);

  val fromListStringArityMap =
      List.foldl addStringArityMap emptyStringArityMap;
in
  fun mkNameFromTptp mapping =
      let
        val {functionMapping = funcMap, relationMapping = relMap} = mapping
        val funcs = fromListStringArityMap funcMap
        and rels = fromListStringArityMap relMap
      in
        NameFromTptp {funcs = funcs, rels = rels}
      end;
end;

local
  fun nameFromTptp mapping sa =
      case Map.peek mapping sa of
        SOME n => n
      | NONE =>
        let
          val (s,_) = sa
        in
          Name.fromString s
        end;
in
  fun fnNameFromTptp names fa =
      let
        val NameFromTptp {funcs, rels = _} = names
      in
        nameFromTptp funcs fa
      end;

  fun relNameFromTptp names ra =
      let
        val NameFromTptp {funcs = _, rels} = names
      in
        nameFromTptp rels ra
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Mapping to and from TPTP function and relation names.                     *)
(* ------------------------------------------------------------------------- *)

datatype tptpMapping =
    TptpMapping of
      {toTptp : nameToTptp,
       fromTptp : nameFromTptp};

fun mkTptpMapping mapping =
    let
      val toTptp = mkNameToTptp mapping
      val fromTptp = mkNameFromTptp mapping
    in
      TptpMapping
        {toTptp = toTptp,
         fromTptp = fromTptp}
    end;

val defaultTptpMapping =
    let
      fun lift {name,arity,tptp} =
          {name = Name.fromString name, arity = arity, tptp = tptp}

      val functionMapping = map lift defaultFunctionMapping
      and relationMapping = map lift defaultRelationMapping

      val mapping =
          {functionMapping = functionMapping,
           relationMapping = relationMapping}
    in
      mkTptpMapping mapping
    end;

(* ------------------------------------------------------------------------- *)
(* Comments.                                                                 *)
(* ------------------------------------------------------------------------- *)

fun mkComment "" = "%"
  | mkComment line = "% " ^ line;

fun destComment "" = ""
  | destComment l =
    let
      val _ = String.sub (l,0) = #"%" orelse raise Error "destComment"
      val n = if size l >= 2 andalso String.sub (l,1) = #" " then 2 else 1
    in
      String.extract (l,n,NONE)
    end;

val isComment = can destComment;

(* ------------------------------------------------------------------------- *)
(* TPTP roles.                                                               *)
(* ------------------------------------------------------------------------- *)

type role = string;

val ROLE_AXIOM = "axiom"
and ROLE_CONJECTURE = "conjecture"
and ROLE_DEFINITION = "definition"
and ROLE_NEGATED_CONJECTURE = "negated_conjecture"
and ROLE_PLAIN = "plain"
and ROLE_THEOREM = "theorem";

fun roleIsCnfConjecture role = role = ROLE_NEGATED_CONJECTURE;

fun roleIsFofConjecture role = role = ROLE_CONJECTURE;

(* ------------------------------------------------------------------------- *)
(* SZS Statuses.                                                             *)
(* ------------------------------------------------------------------------- *)

type status = string;

val STATUS_COUNTER_SATISFIABLE = "CounterSatisfiable"
and STATUS_THEOREM = "Theorem"
and STATUS_SATISFIABLE = "Satisfiable"
and STATUS_UNKNOWN = "Unknown"
and STATUS_UNSATISFIABLE = "Unsatisfiable";

(* ------------------------------------------------------------------------- *)
(* TPTP literals.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype literal =
    Boolean of bool
  | Literal of Literal.literal;

fun negate (Boolean b) = (Boolean (not b))
  | negate (Literal l) = (Literal (Literal.negate l));

fun literalFunctions (Boolean _) = NameAritySet.empty
  | literalFunctions (Literal lit) = Literal.functions lit;

fun literalRelation (Boolean _) = NONE
  | literalRelation (Literal lit) = SOME (Literal.relation lit);

fun literalToFormula (Boolean true) = Formula.True
  | literalToFormula (Boolean false) = Formula.False
  | literalToFormula (Literal lit) = Literal.toFormula lit;

fun literalFromFormula Formula.True = Boolean true
  | literalFromFormula Formula.False = Boolean false
  | literalFromFormula fm = Literal (Literal.fromFormula fm);

fun literalFreeVars (Boolean _) = NameSet.empty
  | literalFreeVars (Literal lit) = Literal.freeVars lit;

fun literalSubst sub lit =
    case lit of
      Boolean _ => lit
    | Literal l => Literal (Literal.subst sub l);

fun destLiteral (Literal l) = l
  | destLiteral _ = raise Error "destLiteral";

(* ------------------------------------------------------------------------- *)
(* Printing formulas using TPTP syntax.                                      *)
(* ------------------------------------------------------------------------- *)

val ppVar = Name.pp;

val ppConst = Name.pp;

local
  fun term (Term.Var v) = ppVar v
    | term (Term.Fn (c,[])) = ppConst c
    | term (Term.Fn (f,tms)) =
      Print.blockProgram Print.Inconsistent 2
        [Name.pp f,
         Print.addString "(",
         Print.ppOpList "," term tms,
         Print.addString ")"];
in
  fun ppTerm tm = Print.block Print.Inconsistent 0 (term tm);
end;

fun ppAtom atm = ppTerm (Term.Fn atm);

local
  val neg = Print.sequence (Print.addString "~") (Print.addBreak 1);

  fun fof fm =
      case fm of
        Formula.And _ => assoc_binary ("&", Formula.stripConj fm)
      | Formula.Or _ => assoc_binary ("|", Formula.stripDisj fm)
      | Formula.Imp a_b => nonassoc_binary ("=>",a_b)
      | Formula.Iff a_b => nonassoc_binary ("<=>",a_b)
      | _ => unitary fm

  and nonassoc_binary (s,a_b) = Print.ppOp2 (" " ^ s) unitary unitary a_b

  and assoc_binary (s,l) = Print.ppOpList (" " ^ s) unitary l

  and unitary fm =
      case fm of
        Formula.True => Print.addString "$true"
      | Formula.False => Print.addString "$false"
      | Formula.Forall _ => quantified ("!", Formula.stripForall fm)
      | Formula.Exists _ => quantified ("?", Formula.stripExists fm)
      | Formula.Not _ =>
        (case total Formula.destNeq fm of
           SOME a_b => Print.ppOp2 " !=" ppTerm ppTerm a_b
         | NONE =>
           let
             val (n,fm) = Formula.stripNeg fm
           in
             Print.blockProgram Print.Inconsistent 2
               [Print.duplicate n neg,
                unitary fm]
           end)
      | Formula.Atom atm =>
        (case total Formula.destEq fm of
           SOME a_b => Print.ppOp2 " =" ppTerm ppTerm a_b
         | NONE => ppAtom atm)
      | _ =>
        Print.blockProgram Print.Inconsistent 1
          [Print.addString "(",
           fof fm,
           Print.addString ")"]

  and quantified (q,(vs,fm)) =
      Print.blockProgram Print.Inconsistent 2
        [Print.addString (q ^ " "),
         Print.blockProgram Print.Inconsistent (String.size q)
           [Print.addString "[",
            Print.ppOpList "," ppVar vs,
            Print.addString "] :"],
         Print.addBreak 1,
         unitary fm]
in
  fun ppFof fm = Print.block Print.Inconsistent 0 (fof fm);
end;

(* ------------------------------------------------------------------------- *)
(* TPTP clauses.                                                             *)
(* ------------------------------------------------------------------------- *)

type clause = literal list;

val clauseFunctions =
    let
      fun funcs (lit,acc) = NameAritySet.union (literalFunctions lit) acc
    in
      foldl funcs NameAritySet.empty
    end;

val clauseRelations =
    let
      fun rels (lit,acc) =
          case literalRelation lit of
            NONE => acc
          | SOME r => NameAritySet.add acc r
    in
      foldl rels NameAritySet.empty
    end;

val clauseFreeVars =
    let
      fun fvs (lit,acc) = NameSet.union (literalFreeVars lit) acc
    in
      foldl fvs NameSet.empty
    end;

fun clauseSubst sub lits = map (literalSubst sub) lits;

fun clauseToFormula lits = Formula.listMkDisj (map literalToFormula lits);

fun clauseFromFormula fm = map literalFromFormula (Formula.stripDisj fm);

fun clauseFromLiteralSet cl =
    clauseFromFormula
      (Formula.listMkDisj (LiteralSet.transform Literal.toFormula cl));

fun clauseFromThm th = clauseFromLiteralSet (Thm.clause th);

val ppClause = Print.ppMap clauseToFormula ppFof;

(* ------------------------------------------------------------------------- *)
(* TPTP formulas.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype formula =
    CnfFormula of {name : string, role : string, clause : clause}
  | FofFormula of {name : string, role : string, formula : Formula.formula};

fun destCnfFormula (CnfFormula x) = x
  | destCnfFormula _ = raise Error "destCnfFormula";

val isCnfFormula = can destCnfFormula;

fun destFofFormula (FofFormula x) = x
  | destFofFormula _ = raise Error "destFofFormula";

val isFofFormula = can destFofFormula;

fun formulaFunctions (CnfFormula {clause,...}) = clauseFunctions clause
  | formulaFunctions (FofFormula {formula,...}) = Formula.functions formula;

fun formulaRelations (CnfFormula {clause,...}) = clauseRelations clause
  | formulaRelations (FofFormula {formula,...}) = Formula.relations formula;

fun formulaFreeVars (CnfFormula {clause,...}) = clauseFreeVars clause
  | formulaFreeVars (FofFormula {formula,...}) = Formula.freeVars formula;

val formulasFunctions =
    let
      fun funcs (fm,acc) = NameAritySet.union (formulaFunctions fm) acc
    in
      foldl funcs NameAritySet.empty
    end;

val formulasRelations =
    let
      fun rels (fm,acc) = NameAritySet.union (formulaRelations fm) acc
    in
      foldl rels NameAritySet.empty
    end;

fun formulaIsConjecture (CnfFormula {role,...}) = roleIsCnfConjecture role
  | formulaIsConjecture (FofFormula {role,...}) = roleIsFofConjecture role;

(* Parsing and pretty-printing *)

local
  fun ppGen ppX (gen,name,role,x) =
      Print.blockProgram Print.Inconsistent (size gen + 1)
        [Print.addString (gen ^ "(" ^ name ^ ","),
         Print.addBreak 1,
         Print.addString (role ^ ","),
         Print.addBreak 1,
         Print.blockProgram Print.Consistent 1
           [Print.addString "(",
            ppX x,
            Print.addString ")"],
         Print.addString ")."];
in
  fun ppFormula (CnfFormula {name,role,clause}) =
      ppGen ppClause ("cnf",name,role,clause)
    | ppFormula (FofFormula {name,role,formula}) =
      ppGen ppFof ("fof",name,role,formula);
end;

val formulaToString = Print.toString ppFormula;

local
  open Parse;

  infixr 9 >>++
  infixr 8 ++
  infixr 7 >>
  infixr 6 ||

  datatype token =
      AlphaNum of string
    | Punct of char
    | Quote of string;

  fun isAlphaNum #"_" = true
    | isAlphaNum c = Char.isAlphaNum c;

  local
    val alphaNumToken = atLeastOne (some isAlphaNum) >> (AlphaNum o implode);

    val punctToken =
        let
          val punctChars = "<>=-*+/\\?@|!$%&#^:;~()[]{}.,"
        in
          some (Char.contains punctChars) >> Punct
        end;

    val quoteToken =
        let
          val escapeParser =
              exact #"'" >> singleton ||
              exact #"\\" >> singleton

          fun stopOn #"'" = true
            | stopOn #"\n" = true
            | stopOn _ = false

          val quotedParser =
              exact #"\\" ++ escapeParser >> op:: ||
              some (not o stopOn) >> singleton
        in
          exact #"'" ++ many quotedParser ++ exact #"'" >>
          (fn (_,(l,_)) => Quote (implode (List.concat l)))
        end;

    val lexToken = alphaNumToken || punctToken || quoteToken;

    val space = many (some Char.isSpace) >> K ();
  in
    val lexer = (space ++ lexToken ++ space) >> (fn ((),(tok,())) => tok);
  end;

  local
    fun someAlphaNum p =
        maybe (fn AlphaNum s => if p s then SOME s else NONE | _ => NONE);

    fun alphaNumParser s = someAlphaNum (equal s) >> K ();

    val lowerParser = someAlphaNum (fn s => Char.isLower (String.sub (s,0)));

    val upperParser = someAlphaNum (fn s => Char.isUpper (String.sub (s,0)));

    val stringParser = lowerParser || upperParser;

    val numberParser = someAlphaNum (List.all Char.isDigit o explode);

    fun somePunct p =
        maybe (fn Punct c => if p c then SOME c else NONE | _ => NONE);

    fun punctParser c = somePunct (equal c) >> K ();

    val quoteParser =
        let
          val p = isHdTlString Char.isLower isAlphaNum

          fun q s = if p s then s else "'" ^ s ^ "'"
        in
          maybe (fn Quote s => SOME (q s) | _ => NONE)
        end;

    local
      fun f [] = raise Bug "symbolParser"
        | f [x] = x
        | f (h :: t) = (h ++ f t) >> K ();
    in
      fun symbolParser s = f (map punctParser (explode s));
    end;

    val definedParser =
        punctParser #"$" ++ someAlphaNum (K true) >> (fn ((),s) => "$" ^ s);

    val systemParser =
        punctParser #"$" ++ punctParser #"$" ++ someAlphaNum (K true) >>
        (fn ((),((),s)) => "$$" ^ s);

    val nameParser = stringParser || numberParser || quoteParser;

    val roleParser = lowerParser;

    local
      fun isProposition s = isHdTlString Char.isLower isAlphaNum s;
    in
      val propositionParser =
          someAlphaNum isProposition ||
          definedParser ||
          systemParser ||
          quoteParser;
    end;

    local
      fun isFunction s = isHdTlString Char.isLower isAlphaNum s;
    in
      val functionParser =
          someAlphaNum isFunction ||
          definedParser ||
          systemParser ||
          quoteParser;
    end;

    local
      fun isConstant s =
          isHdTlString Char.isLower isAlphaNum s orelse
          isHdTlString Char.isDigit Char.isDigit s;
    in
      val constantParser =
          someAlphaNum isConstant ||
          definedParser ||
          systemParser ||
          quoteParser;
    end;

  fun formulaParser mapping =
let
in
  fun parseFormula mapping =
    let

in
  val varParser = upperParser;

  val varListParser =
      (punctParser #"[" ++ varParser ++
       many ((punctParser #"," ++ varParser) >> snd) ++
       punctParser #"]") >>
      (fn ((),(h,(t,()))) => h :: t);

  fun termParser input =
      ((functionArgumentsParser >>
       (fn (f,tms) => Term.Fn (Name.fromString f, tms))) ||
       nonFunctionArgumentsTermParser) input

  and functionArgumentsParser input =
      ((functionParser ++ punctParser #"(" ++ termParser ++
        many ((punctParser #"," ++ termParser) >> snd) ++
        punctParser #")") >>
       (fn (f,((),(t,(ts,())))) => (f, t :: ts))) input

  and nonFunctionArgumentsTermParser input =
      ((varParser >> (Term.Var o Name.fromString)) ||
       (constantParser >> (fn n => Term.Fn (Name.fromString n, [])))) input

  fun binaryAtomParser tm =
      ((punctParser #"=" ++ termParser) >>
       (fn ((),r) => (true,("$equal",[tm,r])))) ||
      ((symbolParser "!=" ++ termParser) >>
       (fn ((),r) => (false,("$equal",[tm,r]))));

  fun maybeBinaryAtomParser (s,tms) =
      let
        val tm = Term.Fn (Name.fromString s, tms)
      in
        optional (binaryAtomParser tm) >>
        (fn SOME lit => lit
          | NONE => (true,(s,tms)))
      end;

  val literalAtomParser =
      (functionArgumentsParser >>++ maybeBinaryAtomParser) ||
      (nonFunctionArgumentsTermParser >>++ binaryAtomParser) ||
      (propositionParser >> (fn s => (true,(s,[]))));

  val atomParser =
      literalAtomParser >>
      (fn (pol,rel) =>
          case rel of
            ("$true",[]) => Boolean pol
          | ("$false",[]) => Boolean (not pol)
          | ("$equal",[l,r]) => Literal (pol, Atom.mkEq (l,r))
          | (r,tms) => Literal (pol, (Name.fromString r, tms)));

  val literalParser =
      ((punctParser #"~" ++ atomParser) >> (negate o snd)) ||
      atomParser;

  val disjunctionParser =
      (literalParser ++ many ((punctParser #"|" ++ literalParser) >> snd)) >>
      (fn (h,t) => h :: t);

  val clauseParser =
      ((punctParser #"(" ++ disjunctionParser ++ punctParser #")") >>
       (fn ((),(c,())) => c)) ||
      disjunctionParser;

(*
  An exact transcription of the fof_formula syntax from

    TPTP-v3.2.0/Documents/SyntaxBNF,

  fun fofFormulaParser input =
      (binaryFormulaParser || unitaryFormulaParser) input

  and binaryFormulaParser input =
      (nonAssocBinaryFormulaParser || assocBinaryFormulaParser) input

  and nonAssocBinaryFormulaParser input =
      ((unitaryFormulaParser ++ binaryConnectiveParser ++
        unitaryFormulaParser) >>
       (fn (f,(c,g)) => c (f,g))) input

  and binaryConnectiveParser input =
      ((symbolParser "<=>" >> K Formula.Iff) ||
       (symbolParser "=>" >> K Formula.Imp) ||
       (symbolParser "<=" >> K (fn (f,g) => Formula.Imp (g,f))) ||
       (symbolParser "<~>" >> K (Formula.Not o Formula.Iff)) ||
       (symbolParser "~|" >> K (Formula.Not o Formula.Or)) ||
       (symbolParser "~&" >> K (Formula.Not o Formula.And))) input

  and assocBinaryFormulaParser input =
      (orFormulaParser || andFormulaParser) input

  and orFormulaParser input =
      ((unitaryFormulaParser ++
        atLeastOne ((punctParser #"|" ++ unitaryFormulaParser) >> snd)) >>
       (fn (f,fs) => Formula.listMkDisj (f :: fs))) input

  and andFormulaParser input =
      ((unitaryFormulaParser ++
        atLeastOne ((punctParser #"&" ++ unitaryFormulaParser) >> snd)) >>
       (fn (f,fs) => Formula.listMkConj (f :: fs))) input

  and unitaryFormulaParser input =
      (quantifiedFormulaParser ||
       unaryFormulaParser ||
       ((punctParser #"(" ++ fofFormulaParser ++ punctParser #")") >>
        (fn ((),(f,())) => f)) ||
       (atomParser >>
        (fn Boolean b => Formula.mkBoolean b
          | Literal l => Literal.toFormula l))) input

  and quantifiedFormulaParser input =
      ((quantifierParser ++ varListParser ++ punctParser #":" ++
        unitaryFormulaParser) >>
       (fn (q,(v,((),f))) => q (v,f))) input

  and quantifierParser input =
      ((punctParser #"!" >> K Formula.listMkForall) ||
       (punctParser #"?" >> K Formula.listMkExists)) input

  and unaryFormulaParser input =
      ((unaryConnectiveParser ++ unitaryFormulaParser) >>
       (fn (c,f) => c f)) input

  and unaryConnectiveParser input =
      (punctParser #"~" >> K Formula.Not) input;
*)

(*
  This version is supposed to be equivalent to the spec version above,
  but uses closures to avoid reparsing prefixes.
*)

  fun fofFormulaParser input =
      ((unitaryFormulaParser ++ optional binaryFormulaParser) >>
       (fn (f,NONE) => f | (f, SOME t) => t f)) input

  and binaryFormulaParser input =
      (nonAssocBinaryFormulaParser || assocBinaryFormulaParser) input

  and nonAssocBinaryFormulaParser input =
      ((binaryConnectiveParser ++ unitaryFormulaParser) >>
       (fn (c,g) => fn f => c (f,g))) input

  and binaryConnectiveParser input =
      ((symbolParser "<=>" >> K Formula.Iff) ||
       (symbolParser "=>" >> K Formula.Imp) ||
       (symbolParser "<=" >> K (fn (f,g) => Formula.Imp (g,f))) ||
       (symbolParser "<~>" >> K (Formula.Not o Formula.Iff)) ||
       (symbolParser "~|" >> K (Formula.Not o Formula.Or)) ||
       (symbolParser "~&" >> K (Formula.Not o Formula.And))) input

  and assocBinaryFormulaParser input =
      (orFormulaParser || andFormulaParser) input

  and orFormulaParser input =
      (atLeastOne ((punctParser #"|" ++ unitaryFormulaParser) >> snd) >>
       (fn fs => fn f => Formula.listMkDisj (f :: fs))) input

  and andFormulaParser input =
      (atLeastOne ((punctParser #"&" ++ unitaryFormulaParser) >> snd) >>
       (fn fs => fn f => Formula.listMkConj (f :: fs))) input

  and unitaryFormulaParser input =
      (quantifiedFormulaParser ||
       unaryFormulaParser ||
       ((punctParser #"(" ++ fofFormulaParser ++ punctParser #")") >>
        (fn ((),(f,())) => f)) ||
       (atomParser >>
        (fn Boolean b => Formula.mkBoolean b
          | Literal l => Literal.toFormula l))) input

  and quantifiedFormulaParser input =
      ((quantifierParser ++ varListParser ++ punctParser #":" ++
        unitaryFormulaParser) >>
       (fn (q,(vs,((),f))) => q (map Name.fromString vs, f))) input

  and quantifierParser input =
      ((punctParser #"!" >> K Formula.listMkForall) ||
       (punctParser #"?" >> K Formula.listMkExists)) input

  and unaryFormulaParser input =
      ((unaryConnectiveParser ++ unitaryFormulaParser) >>
       (fn (c,f) => c f)) input

  and unaryConnectiveParser input =
      (punctParser #"~" >> K Formula.Not) input;

  val cnfParser =
      (alphaNumParser "cnf" ++ punctParser #"(" ++
       nameParser ++ punctParser #"," ++
       roleParser ++ punctParser #"," ++
       clauseParser ++ punctParser #")" ++
       punctParser #".") >>
      (fn ((),((),(n,((),(r,((),(c,((),())))))))) =>
          CnfFormula {name = n, role = r, clause = c});

  val fofParser =
      (alphaNumParser "fof" ++ punctParser #"(" ++
       nameParser ++ punctParser #"," ++
       roleParser ++ punctParser #"," ++
       fofFormulaParser ++ punctParser #")" ++
       punctParser #".") >>
      (fn ((),((),(n,((),(r,((),(f,((),())))))))) =>
          FofFormula {name = n, role = r, formula = f});

  val formulaParser = cnfParser || fofParser;

  fun parseChars parser chars =
      let
        val tokens = Parse.everything (lexer >> singleton) chars
      in
        Parse.everything (parser >> singleton) tokens
      end;
in
  val parseFormula = parseChars formulaParser;
end;

(* ------------------------------------------------------------------------- *)
(* Clause information.                                                       *)
(* ------------------------------------------------------------------------- *)

type 'a clauseInfo = 'a LiteralSetMap.map;

type clauseNames = string clauseInfo;

type clauseRoles = string clauseInfo;

type clauseProofs = Normalize.proof clauseInfo;

val noClauseNames : clauseNames = LiteralSetMap.new ();

val allClauseNames : clauseNames -> StringSet.set =
    let
      fun add (_,n,s) = StringSet.add s n
    in
      LiteralSetMap.foldl add StringSet.empty
    end;

val noClauseRoles : clauseRoles = LiteralSetMap.new ();

val noClauseProofs : clauseProofs = LiteralSetMap.new ();

(* ------------------------------------------------------------------------- *)
(* TPTP problems.                                                            *)
(* ------------------------------------------------------------------------- *)

type comments = string list;

type problem = {comments : comments, formulas : formula list};

fun isCnfProblem ({formulas,...} : problem) =
    let
      val cnf = List.exists isCnfFormula formulas
      and fof = List.exists isFofFormula formulas
    in
      case (cnf,fof) of
        (false,false) => raise Error "TPTP problem has no formulas"
      | (true,true) => raise Error "TPTP problem has both cnf and fof formulas"
      | (cnf,_) => cnf
    end;

fun isFofProblem prob = not (isCnfProblem prob);

fun hasConjecture ({formulas,...} : problem) =
    List.exists formulaIsConjecture formulas;

local
  fun bump n avoid =
      let
        val s = Int.toString n
      in
        if StringSet.member s avoid then bump (n + 1) avoid
        else (s, n, StringSet.add avoid s)
      end;

  fun fromClause defaultRole names roles cl (n,avoid) =
      let
        val (name,n,avoid) =
            case LiteralSetMap.peek names cl of
              SOME name => (name,n,avoid)
            | NONE => bump n avoid

        val role = Option.getOpt (LiteralSetMap.peek roles cl, defaultRole)

        val clause = clauseFromLiteralSet cl
      in
        (CnfFormula {name = name, role = role, clause = clause}, (n,avoid))
      end;
in
  fun mkCnfProblem {comments,names,roles,problem} =
      let
        fun fromCl defaultRole = fromClause defaultRole names roles

        val {axioms,conjecture} = problem

        val n_avoid = (0, allClauseNames names)

        val (axiomFormulas,n_avoid) = maps (fromCl ROLE_AXIOM) axioms n_avoid

        val (conjectureFormulas,_) =
            maps (fromCl ROLE_NEGATED_CONJECTURE) conjecture n_avoid

        val formulas = axiomFormulas @ conjectureFormulas
      in
        {comments = comments, formulas = formulas}
      end;
end;

local
  fun addCnfFormula (CnfFormula {name,role,clause}, acc) =
      if List.exists (fn Boolean true => true | _ => false) clause then acc
      else
        let
          val litl = List.mapPartial (total destLiteral) clause
          val lits = LiteralSet.fromList litl
          val (names,roles,axioms,conjecture) = acc
        in
          if LiteralSetMap.inDomain lits names then acc
          else
            let
              val names = LiteralSetMap.insert names (lits,name)
              val roles = LiteralSetMap.insert roles (lits,role)
              val (axioms,conjecture) =
                  if roleIsCnfConjecture role then (axioms, lits :: conjecture)
                  else (lits :: axioms, conjecture)
            in
              (names,roles,axioms,conjecture)
            end
        end
    | addCnfFormula (FofFormula _, _) = raise Bug "destCnfProblem";
in
  fun destCnfProblem ({comments,formulas} : problem) =
      let
        val names = LiteralSetMap.new ()
        val roles = LiteralSetMap.new ()
        val (names,roles,axioms,conjecture) =
            foldl addCnfFormula (names,roles,[],[]) formulas
        val problem = {axioms = rev axioms, conjecture = rev conjecture}
      in
        {comments = comments,
         names = names,
         roles = roles,
         problem = problem}
      end;
end;

type normalizedFof =
     {definitions : (string * Formula.formula) list,
      roles : clauseRoles,
      problem : Problem.problem,
      proofs : clauseProofs};

val initialNormalizedFof : normalizedFof =
    {definitions = [],
     roles = LiteralSetMap.new (),
     problem = {axioms = [], conjecture = []},
     proofs = LiteralSetMap.new ()};

local
  fun partitionFofFormula (fm,(axioms,goals)) =
      case fm of
        FofFormula {name,role,formula} =>
        if roleIsFofConjecture role then (axioms, (name,formula) :: goals)
        else ((name,formula) :: axioms, goals)
      | _ => raise Bug "Tptp.partitionFofFormula";

  fun partitionFofFormulas fms =
      let
        val (axioms,goals) = List.foldl partitionFofFormula ([],[]) fms
      in
        {axioms = rev axioms, goals = rev goals}
      end;

  fun addClauses role new acc : normalizedFof =
      let
        fun addClause ((cl,prf),(roles,proofs)) =
            (LiteralSetMap.insert roles (cl,role),
             LiteralSetMap.insert proofs (cl,prf))

        val {definitions = defs, clauses} = new
        and {definitions,roles,problem,proofs} : normalizedFof = acc
        val {axioms,conjecture} = problem

        val cls = map fst clauses
        val (axioms,conjecture) =
            if roleIsCnfConjecture role then (axioms, cls @ conjecture)
            else (cls @ axioms, conjecture)

        val definitions = defs @ definitions
        and problem = {axioms = axioms, conjecture = conjecture}
        and (roles,proofs) = List.foldl addClause (roles,proofs) clauses
      in
        {definitions = definitions,
         roles = roles,
         problem = problem,
         proofs = proofs}
      end

  fun addFof role ((name,fm),(acc,cnf)) =
      let
        val prf = Normalize.singletonProof name
        val (new,cnf) = Normalize.cnfStateAdd (fm,prf) cnf
        val acc = addClauses role new acc
      in
        (acc,cnf)
      end;

  fun mkProblem (acc,_) : normalizedFof =
      let
        val {definitions,roles,problem,proofs} = acc
        val {axioms,conjecture} = problem
      in
        {definitions = rev definitions,
         roles = roles,
         problem = {axioms = rev axioms, conjecture = rev conjecture},
         proofs = proofs}
      end;
in
  fun goalFofProblem ({formulas,...} : problem) =
      let
        val {axioms,goals} = partitionFofFormulas formulas
        val hyp = Formula.listMkConj (map (Formula.generalize o snd) axioms)
        and concl = Formula.listMkConj (map (Formula.generalize o snd) goals)
      in
        case (null axioms, null goals) of
          (true,true) => raise Bug "Tptp.goalFofProblem"
        | (false,true) => Formula.Imp (hyp,Formula.False)
        | (true,false) => concl
        | (false,false) => Formula.Imp (hyp,concl)
      end;

  fun normalizeFof ({formulas,...} : problem) : normalizedFof list =
      let
        val {axioms,goals} = partitionFofFormulas formulas
        val acc = (initialNormalizedFof, Normalize.cnfStateInitial)
        val acc = List.foldl (addFof ROLE_AXIOM) acc axioms
      in
        if null goals then [mkProblem acc]
        else
          let
            fun mk name goal =
                let
                  val goal = Formula.Not (Formula.generalize goal)
                  val acc = addFof ROLE_NEGATED_CONJECTURE ((name,goal),acc)
                in
                  mkProblem acc
                end

            fun split (name,goal) =
                let
                  val goals = Formula.splitGoal goal
                  val goals = if null goals then [Formula.True] else goals
                in
                  map (mk name) goals
                end
          in
            List.concat (map split goals)
          end
      end;
end;

fun normalizeFofToCnf (problem as {comments,...}) =
    let
      val comments = comments @ (if null comments then [] else [""])

      val names = LiteralSetMap.new ()

      val problems = normalizeFof problem

      val n = Int.toString (length problems)

      fun mk {definitions = _, roles, problem = p, proofs = _} i =
          let
            val i = i + 1
            val comments =
                comments @ ["FOFtoCNF subgoal: " ^ Int.toString i ^ "/" ^ n]
            val prob = mkCnfProblem {comments = comments,
                                     names = names,
                                     roles = roles,
                                     problem = p}
          in
            (prob,i)
          end

      val (probs,_) = maps mk problems 0
    in
      probs
    end;

fun goal problem =
    if isCnfProblem problem then
      let
        val {problem,...} = destCnfProblem problem
      in
        Problem.toGoal problem
      end
    else
      goalFofProblem problem;

local
  fun stripComments acc strm =
      case strm of
        Stream.Nil => (rev acc, Stream.Nil)
      | Stream.Cons (line,rest) =>
        case total destComment line of
          SOME s => stripComments (s :: acc) (rest ())
        | NONE => (rev acc, Stream.filter (not o isComment) strm);
in
  fun read {filename} =
      let
        val lines = Stream.fromTextFile {filename = filename}

        val lines = Stream.map chomp lines

        val (comments,lines) = stripComments [] lines

        val chars = Stream.concat (Stream.map Stream.fromString lines)

        val formulas = Stream.toList (parseFormula chars)

        val formulas = formulasFromTptp formulas
      in
        {comments = comments, formulas = formulas}
      end;
end;

local
  fun mkCommentLine comment = mkComment comment ^ "\n";

  fun formulaStream _ [] () = Stream.Nil
    | formulaStream start (h :: t) () =
      let
        val s = formulaToString h ^ "\n"
      in
        Stream.Cons (if start then s else "\n" ^ s, formulaStream false t)
      end;
in
  fun write {filename} {comments,formulas} =
      Stream.toTextFile
        {filename = filename}
        (Stream.append
           (Stream.map mkCommentLine (Stream.fromList comments))
           (formulaStream (null comments) (formulasToTptp formulas)));
end;

local
  fun refute problem =
      let
        val {problem = {axioms,conjecture}, ...} = destCnfProblem problem
        val axioms = map Thm.axiom axioms
        and conjecture = map Thm.axiom conjecture
        val prob = {axioms = axioms, conjecture = conjecture}
        val res = Resolution.new Resolution.default prob
      in
        case Resolution.loop res of
          Resolution.Contradiction _ => true
        | Resolution.Satisfiable _ => false
      end;
in
  fun prove filename =
      let
        val problem = read filename
        val problems =
            if isCnfProblem problem then [problem]
            else normalizeFofToCnf problem
      in
        List.all refute problems
      end;
end;

(* ------------------------------------------------------------------------- *)
(* TSTP proofs.                                                              *)
(* ------------------------------------------------------------------------- *)

type axiomProofs = Normalize.proof LiteralSetMap.map;

local
  fun newThmName avoid prefix =
      let
        fun bump i =
            let
              val name = prefix ^ Int.toString i
              val i = i + 1
            in
              if StringSet.member name avoid then bump i else (name,i)
            end
      in
        bump
      end;

  fun mapSubstTerm fr sub tm =
      let
        val {functionMap, relationMap = _} = fr
      in
        Subst.subst sub (mapTerm functionMap tm)
      end;

  fun mapSubstAtom fr sub atm = Atom.subst sub (mapAtom fr atm);

  fun mapSubstSubst fr sub =
      let
        fun inc (v,tm,s) =
            let
              val v =
                  case Subst.peek sub v of
                    NONE => v
                  | SOME vt => Term.destVar vt

              val tm = mapSubstTerm fr sub tm
            in
              Subst.insert s (v,tm)
            end
      in
        Subst.foldl inc Subst.empty
      end;

  fun mapSubstLiteral fr sub lit = Literal.subst sub (mapLit fr lit);

  fun mapSubstClause fr sub cl = clauseSubst sub (mapClause fr cl);

  val ppTermTstp = ppTerm;

  fun ppAtomTstp atm =
      case total Atom.destEq atm of
        SOME (a,b) => ppAtom (Name.fromString "$equal", [a,b])
      | NONE => ppAtom atm;

  fun ppLiteralTstp (pol,atm) =
      Print.sequence
        (if pol then Print.skip else Print.addString "~ ")
        (ppAtomTstp atm);

  val ppTermInfo = Print.ppBracket "$fot(" ")" ppTermTstp;

  val ppAtomInfo = Print.ppBracket "$cnf(" ")" ppAtomTstp;

  val ppLiteralInfo = Print.ppBracket "$cnf(" ")" ppLiteralTstp;

  val ppAssumeInfo = ppAtomInfo;

  val ppSubstInfo =
      Print.ppMap
        Subst.toList
        (Print.ppList
           (Print.ppBracket "bind(" ")"
              (Print.ppOp2 "," ppVar ppTermInfo)));

  val ppResolveInfo = ppAtomInfo;

  val ppReflInfo = ppTermInfo;

  fun ppEqualityInfo (lit,path,res) =
      Print.program
        [ppLiteralInfo lit,
         Print.addString ",",
         Print.addBreak 1,
         Term.ppPath path,
         Print.addString ",",
         Print.addBreak 1,
         ppTermInfo res];

  fun ppInfInfo fr sub inf =
      case inf of
        Proof.Axiom _ => raise Bug "ppInfInfo"
      | Proof.Assume a => ppAssumeInfo (mapSubstAtom fr sub a)
      | Proof.Subst _ => Print.skip
      | Proof.Resolve (r,_,_) => ppResolveInfo (mapSubstAtom fr sub r)
      | Proof.Refl t => ppReflInfo (mapSubstTerm fr sub t)
      | Proof.Equality (l,p,t) =>
        let
          val l = mapSubstLiteral fr sub l
          and t = mapSubstTerm fr sub t
        in
          ppEqualityInfo (l,p,t)
        end;

  fun ppAxiomProof prf =
      Print.program
        [Print.addString "fof_to_cnf,",
         Print.addBreak 1,
         Print.addString "[],",
         Print.addBreak 1,
         Print.ppMap StringSet.toList (Print.ppList Print.ppString) prf];

  fun ppThm prevNames th =
      case LiteralSetMap.peek prevNames (Thm.clause th) of
        SOME name => Print.addString name
      | NONE => raise Error "previous theorem not found";

  fun ppThmSub prevNames fr sub (th,s) =
      if Subst.null s then ppThm prevNames th
      else
        let
          val s = mapSubstSubst fr sub s
        in
          Print.ppOp2 " :" (ppThm prevNames) ppSubstInfo (th,s)
        end;
in
  fun ppProof avoid prefix names roles proofs prf =
      let
        val fr =
            {functionMap = mappingToTptp (!functionMapping),
             relationMap = mappingToTptp (!relationMapping)}

        val (_,sub) = mkTptpVars (Proof.freeVars prf)

        fun ppInf prevNames inf =
            let
              val name = Thm.inferenceTypeToString (Proof.inferenceType inf)
              val name = String.map Char.toLower name
              val parentSubs =
                  case inf of
                    Proof.Subst (s,th) => [(th,s)]
                  | _ => map (fn th => (th,Subst.empty)) (Proof.parents inf)
            in
              Print.program
                ([Print.addString (name ^ ","),
                  Print.addBreak 1,
                  Print.ppBracket "[" "]" (ppInfInfo fr sub) inf] @
                 (if null parentSubs then []
                  else
                    [Print.addString ",",
                     Print.addBreak 1,
                     Print.ppList (ppThmSub prevNames fr sub) parentSubs]))
            end

        fun ppTaut inf =
            Print.program
              [Print.addString "tautology,",
               Print.addBreak 1,
               Print.ppBracket "[" "]" (ppInf noClauseNames) inf]

        fun ppStepInfo prevNames (name,cl,inf) =
            let
              val is_axiom = case inf of Proof.Axiom _ => true | _ => false
              val role =
                  case LiteralSetMap.peek roles cl of
                    SOME role => role
                  | NONE => if is_axiom then ROLE_AXIOM else ROLE_PLAIN
              val cl' = mapSubstClause fr sub (clauseFromLiteralSet cl)
            in
              Print.program
                ([Print.addString (name ^ ","),
                  Print.addBreak 1,
                  Print.addString (role ^ ","),
                  Print.addBreak 1,
                  Print.ppBracket "(" ")" ppClause cl'] @
                 (if is_axiom then
                    case LiteralSetMap.peek proofs cl of
                      NONE => []
                    | SOME axiomPrf =>
                      [Print.addString ",",
                       Print.addBreak 1,
                       Print.ppBracket "inference(" ")" ppAxiomProof axiomPrf]
                  else
                    let
                      val is_tautology = null (Proof.parents inf)
                    in
                      [Print.addString ",",
                       Print.addBreak 1,
                       (if is_tautology then
                          Print.ppBracket "introduced(" ")" ppTaut inf
                        else
                          Print.ppBracket "inference(" ")"
                            (ppInf prevNames) inf)]
                    end))
            end

        fun ppStep (th,inf) (start,prevNames,i) =
            let
              val cl = Thm.clause th
              val (name,i) =
                  case LiteralSetMap.peek names cl of
                    SOME name => (name,i)
                  | NONE => newThmName avoid prefix i
            in
              (Print.program
                 [(if start then Print.skip else Print.addNewline),
                  Print.ppBracket "cnf(" ")"
                    (ppStepInfo prevNames) (name,cl,inf),
                  Print.addString ".",
                  Print.addNewline],
                (false, LiteralSetMap.insert prevNames (cl,name), i))
            end
      in
        Print.block Print.Consistent 0
          (Print.stream
             (Stream.maps ppStep (K Stream.Nil) (true,noClauseNames,0)
                (Stream.fromList prf)))
      end
(*MetisDebug
      handle Error err => raise Bug ("Tptp.ppProof: shouldn't fail:\n" ^ err);
*)
end;

fun writeProof {filename,avoid,prefix,names,roles,proofs} =
    Stream.toTextFile {filename = filename} o
    Print.toStream (ppProof avoid prefix names roles proofs);

end
