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

datatype varToTptp = VarToTptp of StringSet.set * string NameMap.map;

val emptyVarToTptp = VarToTptp (StringSet.empty, NameMap.new ());

fun addVarToTptp vm v =
    let
      val VarToTptp (avoid,mapping) = vm
    in
      if NameMap.inDomain v mapping then vm
      else
        let
          val s = variant avoid (mkTptpVarName (Name.toString v))

          val avoid = StringSet.add avoid s
          and mapping = NameMap.insert mapping (v,s)
        in
          VarToTptp (avoid,mapping)
        end
    end;

local
  fun add (v,vm) = addVarToTptp vm v;
in
  val addListVarToTptp = List.foldl add;

  val addSetVarToTptp = NameSet.foldl add;
end;

val fromListVarToTptp = addListVarToTptp emptyVarToTptp;

val fromSetVarToTptp = addSetVarToTptp emptyVarToTptp;

fun getVarToTptp vm v =
    let
      val VarToTptp (_,mapping) = vm
    in
      case NameMap.peek mapping v of
        SOME s => s
      | NONE => raise Bug "Tptp.getVarToTptp: unknown var"
    end;

(* ------------------------------------------------------------------------- *)
(* Mapping from TPTP variable names.                                         *)
(* ------------------------------------------------------------------------- *)

fun getVarFromTptp s = Name.fromString s;

(* ------------------------------------------------------------------------- *)
(* Mapping to TPTP function and relation names.                              *)
(* ------------------------------------------------------------------------- *)

datatype nameToTptp = NameToTptp of string NameArityMap.map;

local
  val emptyNames : string NameArityMap.map = NameArityMap.new ();

  fun addNames ({name,arity,tptp},mapping) =
      NameArityMap.insert mapping ((name,arity),tptp);

  val fromListNames = List.foldl addNames emptyNames;
in
  fun mkNameToTptp mapping = NameToTptp (fromListNames mapping);
end;

local
  fun singleQuote s = "'" ^ s ^ "'";
in
  fun getNameToTptp zeroToTptp plusToTptp (NameToTptp mapping) na =
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
end;

(* ------------------------------------------------------------------------- *)
(* Mapping from TPTP function and relation names.                            *)
(* ------------------------------------------------------------------------- *)

datatype nameFromTptp = NameFromTptp of (string * int, Name.name) Map.map;

local
  val stringArityCompare = prodCompare String.compare Int.compare;

  val emptyStringArityMap = Map.new stringArityCompare;

  fun addStringArityMap ({name,arity,tptp},mapping) =
      Map.insert mapping ((tptp,arity),name);

  val fromListStringArityMap =
      List.foldl addStringArityMap emptyStringArityMap;
in
  fun mkNameFromTptp mapping = NameFromTptp (fromListStringArityMap mapping);
end;

fun getNameFromTptp (NameFromTptp mapping) sa =
    case Map.peek mapping sa of
      SOME n => n
    | NONE =>
      let
        val (s,_) = sa
      in
        Name.fromString s
      end;

(* ------------------------------------------------------------------------- *)
(* Mapping to and from TPTP variable, function and relation names.           *)
(* ------------------------------------------------------------------------- *)

datatype tptpMapping =
    TptpMapping of
      {varTo : varToTptp,
       fnTo : nameToTptp,
       relTo : nameToTptp,
       fnFrom : nameFromTptp,
       relFrom : nameFromTptp};

fun mkTptpMapping mapping =
    let
      val {functionMapping,relationMapping} = mapping

      val varTo = emptyVarToTptp
      val fnTo = mkNameToTptp functionMapping
      val relTo = mkNameToTptp relationMapping

      val fnFrom = mkNameFromTptp functionMapping
      val relFrom = mkNameFromTptp relationMapping
    in
      TptpMapping
        {varTo = varTo,
         fnTo = fnTo,
         relTo = relTo,
         fnFrom = fnFrom,
         relFrom = relFrom}
    end;

fun addVarListTptpMapping mapping vs =
    let
      val TptpMapping
            {varTo,
             fnTo,
             relTo,
             fnFrom,
             relFrom} = mapping

      val varTo = addListVarToTptp varTo vs
    in
      TptpMapping
        {varTo = varTo,
         fnTo = fnTo,
         relTo = relTo,
         fnFrom = fnFrom,
         relFrom = relFrom}
    end;

fun addVarSetTptpMapping mapping vs =
    let
      val TptpMapping
            {varTo,
             fnTo,
             relTo,
             fnFrom,
             relFrom} = mapping

      val varTo = addSetVarToTptp varTo vs
    in
      TptpMapping
        {varTo = varTo,
         fnTo = fnTo,
         relTo = relTo,
         fnFrom = fnFrom,
         relFrom = relFrom}
    end;

fun varToTptp mapping v =
    let
      val TptpMapping {varTo,...} = mapping
    in
      getVarToTptp varTo v
    end;

fun fnToTptp mapping fa =
    let
      val TptpMapping {fnTo,...} = mapping
    in
      getNameToTptp mkTptpConstName mkTptpFnName fnTo fa
    end;

fun relToTptp mapping ra =
    let
      val TptpMapping {relTo,...} = mapping
    in
      getNameToTptp mkTptpPropName mkTptpRelName relTo ra
    end;

fun varFromTptp (_ : tptpMapping) v = getVarFromTptp v;

fun fnFromTptp mapping fa =
    let
      val TptpMapping {fnFrom,...} = mapping
    in
      getNameFromTptp fnFrom fa
    end;

fun relFromTptp mapping ra =
    let
      val TptpMapping {relFrom,...} = mapping
    in
      getNameFromTptp relFrom ra
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

fun destComment cs =
    case cs of
      [] => ""
    | #"%" :: #" " :: rest => implode rest
    | #"%" :: rest => implode rest
    | _ => raise Error "Tptp.destComment";

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

fun literalListToFormula lits =
    Formula.listMkDisj (map literalToFormula lits);

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

fun literalIsBooleanTrue (Boolean true) = true
  | literalIsBooleanTrue _ = false;

(* ------------------------------------------------------------------------- *)
(* Printing formulas using TPTP syntax.                                      *)
(* ------------------------------------------------------------------------- *)

fun ppVar mapping v =
    let
      val s = varToTptp mapping v
    in
      Print.addString s
    end;

fun ppFnName mapping fa = Print.addString (fnToTptp mapping fa);

fun ppConst mapping c = ppFnName mapping (c,0);

fun ppTerm mapping =
    let
      fun term tm =
          case tm of
            Term.Var v => ppVar mapping v
          | Term.Fn (f,tms) =>
            case length tms of
              0 => ppConst mapping f
            | a =>
              Print.blockProgram Print.Inconsistent 2
                [ppFnName mapping (f,a),
                 Print.addString "(",
                 Print.ppOpList "," term tms,
                 Print.addString ")"]
    in
      Print.block Print.Inconsistent 0 o term
    end;

fun ppRelName mapping ra = Print.addString (relToTptp mapping ra);

fun ppProp mapping p = ppRelName mapping (p,0);

fun ppAtom mapping (r,tms) =
    case length tms of
      0 => ppProp mapping r
    | a =>
      Print.blockProgram Print.Inconsistent 2
        [ppRelName mapping (r,a),
         Print.addString "(",
         Print.ppOpList "," (ppTerm mapping) tms,
         Print.addString ")"];

local
  val neg = Print.sequence (Print.addString "~") (Print.addBreak 1);

  fun fof mapping fm =
      case fm of
        Formula.And _ => assoc_binary mapping ("&", Formula.stripConj fm)
      | Formula.Or _ => assoc_binary mapping ("|", Formula.stripDisj fm)
      | Formula.Imp a_b => nonassoc_binary mapping ("=>",a_b)
      | Formula.Iff a_b => nonassoc_binary mapping ("<=>",a_b)
      | _ => unitary mapping fm

  and nonassoc_binary mapping (s,a_b) =
      Print.ppOp2 (" " ^ s) (unitary mapping) (unitary mapping) a_b

  and assoc_binary mapping (s,l) = Print.ppOpList (" " ^ s) (unitary mapping) l

  and unitary mapping fm =
      case fm of
        Formula.True => Print.addString "$true"
      | Formula.False => Print.addString "$false"
      | Formula.Forall _ => quantified mapping ("!", Formula.stripForall fm)
      | Formula.Exists _ => quantified mapping ("?", Formula.stripExists fm)
      | Formula.Not _ =>
        (case total Formula.destNeq fm of
           SOME a_b => Print.ppOp2 " !=" (ppTerm mapping) (ppTerm mapping) a_b
         | NONE =>
           let
             val (n,fm) = Formula.stripNeg fm
           in
             Print.blockProgram Print.Inconsistent 2
               [Print.duplicate n neg,
                unitary mapping fm]
           end)
      | Formula.Atom atm =>
        (case total Formula.destEq fm of
           SOME a_b => Print.ppOp2 " =" (ppTerm mapping) (ppTerm mapping) a_b
         | NONE => ppAtom mapping atm)
      | _ =>
        Print.blockProgram Print.Inconsistent 1
          [Print.addString "(",
           fof mapping fm,
           Print.addString ")"]

  and quantified mapping (q,(vs,fm)) =
      let
        val mapping = addVarListTptpMapping mapping vs
      in
        Print.blockProgram Print.Inconsistent 2
          [Print.addString (q ^ " "),
           Print.blockProgram Print.Inconsistent (String.size q)
             [Print.addString "[",
              Print.ppOpList "," (ppVar mapping) vs,
              Print.addString "] :"],
           Print.addBreak 1,
           unitary mapping fm]
      end;
in
  fun ppFof mapping fm = Print.block Print.Inconsistent 0 (fof mapping fm);
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

fun ppClause mapping = Print.ppMap clauseToFormula (ppFof mapping);

(* ------------------------------------------------------------------------- *)
(* TPTP formulas.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype formula =
    CnfFormula of {name : string, role : string, clause : clause}
  | FofFormula of {name : string, role : string, formula : Formula.formula};

fun formulaName formula =
    case formula of
      CnfFormula {name,...} => name
    | FofFormula {name,...} => name;

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

val formulaListFreeVars =
    let
      fun add (fm,vs) = NameSet.union vs (formulaFreeVars fm)
    in
      List.foldl add NameSet.empty
    end;

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

fun formulaIsCnfConjecture fm =
    case fm of
      CnfFormula {role,...} => roleIsCnfConjecture role
    | FofFormula _ => false;

fun formulaIsFofConjecture fm =
    case fm of
      FofFormula {role,...} => roleIsFofConjecture role
    | CnfFormula _ => false;

fun formulaIsConjecture fm =
    formulaIsCnfConjecture fm orelse formulaIsFofConjecture fm;

(* Parsing and pretty-printing *)

local
  fun ppGen mapping ppX (gen,name,role,x) =
      Print.blockProgram Print.Inconsistent (size gen + 1)
        [Print.addString (gen ^ "(" ^ name ^ ","),
         Print.addBreak 1,
         Print.addString (role ^ ","),
         Print.addBreak 1,
         Print.blockProgram Print.Consistent 1
           [Print.addString "(",
            ppX mapping x,
            Print.addString ")"],
         Print.addString ")."];
in
  fun ppFormula mapping fm =
      case fm of
        CnfFormula {name,role,clause} =>
        ppGen mapping ppClause ("cnf",name,role,clause)
      | FofFormula {name,role,formula} =>
        ppGen mapping ppFof ("fof",name,role,formula);
end;

fun formulaToString mapping = Print.toString (ppFormula mapping);

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

    val varParser = upperParser;

    val varListParser =
        (punctParser #"[" ++ varParser ++
         many ((punctParser #"," ++ varParser) >> snd) ++
         punctParser #"]") >>
        (fn ((),(h,(t,()))) => h :: t);

    fun mkVarName mapping v = varFromTptp mapping v;

    fun mkVar mapping v =
        let
          val v = mkVarName mapping v
        in
          Term.Var v
        end

    fun mkFn mapping (f,tms) =
        let
          val f = fnFromTptp mapping (f, length tms)
        in
          Term.Fn (f,tms)
        end;

    fun mkConst mapping c = mkFn mapping (c,[]);

    fun mkAtom mapping (r,tms) =
        let
          val r = relFromTptp mapping (r, length tms)
        in
          (r,tms)
        end;

    fun termParser mapping input =
        let
          val fnP = functionArgumentsParser mapping >> mkFn mapping
          val nonFnP = nonFunctionArgumentsTermParser mapping
        in
          fnP || nonFnP
        end input

    and functionArgumentsParser mapping input =
        let
          val commaTmP = (punctParser #"," ++ termParser mapping) >> snd
        in
          (functionParser ++ punctParser #"(" ++ termParser mapping ++
           many commaTmP ++ punctParser #")") >>
          (fn (f,((),(t,(ts,())))) => (f, t :: ts))
        end input

    and nonFunctionArgumentsTermParser mapping input =
        let
          val varP = varParser >> mkVar mapping
          val constP = constantParser >> mkConst mapping
        in
          varP || constP
        end input;

    fun binaryAtomParser mapping tm input =
        let
          val eqP =
              (punctParser #"=" ++ termParser mapping) >>
              (fn ((),r) => (true,("$equal",[tm,r])))

          val neqP =
              (symbolParser "!=" ++ termParser mapping) >>
              (fn ((),r) => (false,("$equal",[tm,r])))
        in
          eqP || neqP
        end input;

    fun maybeBinaryAtomParser mapping (s,tms) input =
        let
          val tm = mkFn mapping (s,tms)
        in
          optional (binaryAtomParser mapping tm) >>
          (fn SOME lit => lit
            | NONE => (true,(s,tms)))
        end input;

    fun literalAtomParser mapping input =
        let
          val fnP =
              functionArgumentsParser mapping >>++
              maybeBinaryAtomParser mapping

          val nonFnP =
              nonFunctionArgumentsTermParser mapping >>++
              binaryAtomParser mapping

          val propP = propositionParser >> (fn s => (true,(s,[])))
        in
          fnP || nonFnP || propP
        end input;

    fun atomParser mapping input =
        let
          fun mk (pol,rel) =
            case rel of
              ("$true",[]) => Boolean pol
            | ("$false",[]) => Boolean (not pol)
            | ("$equal",[l,r]) => Literal (pol, Atom.mkEq (l,r))
            | (r,tms) => Literal (pol, mkAtom mapping (r,tms))
        in
          literalAtomParser mapping >> mk
        end input;

    fun literalParser mapping input =
        let
          val negP = (punctParser #"~" ++ atomParser mapping) >> (negate o snd)

          val posP = atomParser mapping
        in
          negP || posP
        end input;

    fun disjunctionParser mapping input =
        let
          val orLitP = (punctParser #"|" ++ literalParser mapping) >> snd
        in
          (literalParser mapping ++ many orLitP) >> (fn (h,t) => h :: t)
        end input;

    fun clauseParser mapping input =
        let
          val disjP = disjunctionParser mapping

          val bracketDisjP =
              (punctParser #"(" ++ disjP ++ punctParser #")") >>
              (fn ((),(c,())) => c)
        in
          bracketDisjP || disjP
        end input;

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
    but passes around a name mapping and uses closures to avoid reparsing
    prefixes.
*)

    val binaryConnectiveParser =
        (symbolParser "<=>" >> K Formula.Iff) ||
        (symbolParser "=>" >> K Formula.Imp) ||
        (symbolParser "<=" >> K (fn (f,g) => Formula.Imp (g,f))) ||
        (symbolParser "<~>" >> K (Formula.Not o Formula.Iff)) ||
        (symbolParser "~|" >> K (Formula.Not o Formula.Or)) ||
        (symbolParser "~&" >> K (Formula.Not o Formula.And));

    val quantifierParser =
        (punctParser #"!" >> K Formula.listMkForall) ||
        (punctParser #"?" >> K Formula.listMkExists);

    fun fofFormulaParser mapping input =
        let
          fun mk (f,NONE) = f
            | mk (f, SOME t) = t f
        in
          (unitaryFormulaParser mapping ++
           optional (binaryFormulaParser mapping)) >> mk
        end input

    and binaryFormulaParser mapping input =
        let
          val nonAssocP = nonAssocBinaryFormulaParser mapping

          val assocP = assocBinaryFormulaParser mapping
        in
          nonAssocP || assocP
        end input

    and nonAssocBinaryFormulaParser mapping input =
        let
          fun mk (c,g) f = c (f,g)
        in
          (binaryConnectiveParser ++ unitaryFormulaParser mapping) >> mk
        end input

    and assocBinaryFormulaParser mapping input =
        let
          val orP = orFormulaParser mapping

          val andP = andFormulaParser mapping
        in
          orP || andP
        end input

    and orFormulaParser mapping input =
        let
          val orFmP = (punctParser #"|" ++ unitaryFormulaParser mapping) >> snd
        in
          atLeastOne orFmP >>
          (fn fs => fn f => Formula.listMkDisj (f :: fs))
        end input

    and andFormulaParser mapping input =
        let
          val andFmP = (punctParser #"&" ++ unitaryFormulaParser mapping) >> snd
        in
          atLeastOne andFmP >>
          (fn fs => fn f => Formula.listMkConj (f :: fs))
        end input

    and unitaryFormulaParser mapping input =
        let
          val quantP = quantifiedFormulaParser mapping

          val unaryP = unaryFormulaParser mapping

          val brackP =
              (punctParser #"(" ++ fofFormulaParser mapping ++
               punctParser #")") >>
              (fn ((),(f,())) => f)

          val atomP =
              atomParser mapping >>
              (fn Boolean b => Formula.mkBoolean b
                | Literal l => Literal.toFormula l)
        in
          quantP ||
          unaryP ||
          brackP ||
          atomP
        end input

    and quantifiedFormulaParser mapping input =
        let
          fun mk (q,(vs,((),f))) = q (map (mkVarName mapping) vs, f)
        in
          (quantifierParser ++ varListParser ++ punctParser #":" ++
           unitaryFormulaParser mapping) >> mk
        end input

    and unaryFormulaParser mapping input =
        let
          fun mk (c,f) = c f
        in
          (unaryConnectiveParser ++ unitaryFormulaParser mapping) >> mk
        end input

    and unaryConnectiveParser input =
        (punctParser #"~" >> K Formula.Not) input;

    fun cnfParser mapping input =
        let
          fun mk ((),((),(n,((),(r,((),(c,((),())))))))) =
              CnfFormula {name = n, role = r, clause = c}
        in
          (alphaNumParser "cnf" ++ punctParser #"(" ++
           nameParser ++ punctParser #"," ++
           roleParser ++ punctParser #"," ++
           clauseParser mapping ++ punctParser #")" ++
           punctParser #".") >> mk
        end input;

    fun fofParser mapping input =
        let
          fun mk ((),((),(n,((),(r,((),(f,((),())))))))) =
              FofFormula {name = n, role = r, formula = f}
        in
          (alphaNumParser "fof" ++ punctParser #"(" ++
           nameParser ++ punctParser #"," ++
           roleParser ++ punctParser #"," ++
           fofFormulaParser mapping ++ punctParser #")" ++
           punctParser #".") >> mk
        end input;
  in
    fun formulaParser mapping input =
        let
          val cnfP = cnfParser mapping

          val fofP = fofParser mapping
        in
          cnfP || fofP
        end input;
  end;

  fun parseChars parser chars =
      let
        val tokens = Parse.everything (lexer >> singleton) chars
      in
        Parse.everything (parser >> singleton) tokens
      end;
in
  fun parseFormula mapping = parseChars (formulaParser mapping);
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

(***
fun isFofProblem ({formulas,...} : problem) =


prob = not (isCnfProblem prob);

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
***)

fun hasCnfConjecture ({formulas,...} : problem) =
    List.exists formulaIsCnfConjecture formulas;

fun hasFofConjecture ({formulas,...} : problem) =
    List.exists formulaIsFofConjecture formulas;

fun hasConjecture ({formulas,...} : problem) =
    List.exists formulaIsConjecture formulas;

fun freeVars ({formulas,...} : problem) = formulaListFreeVars formulas;

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

(***
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
***)

type normalization =
     {definitions : (string * Formula.formula) list,
      roles : clauseRoles,
      problem : Problem.problem,
      proofs : clauseProofs};

val initialNormalization : normalization =
    {definitions = [],
     roles = LiteralSetMap.new (),
     problem = {axioms = [], conjecture = []},
     proofs = LiteralSetMap.new ()};

datatype problemGoal =
    NoGoal
  | CnfGoal of (string * clause) list
  | FofGoal of (string * Formula.formula) list;

local
  fun partitionFormula (fm,(cnfAxioms,fofAxioms,cnfGoals,fofGoals)) =
      case fm of
        CnfFormula {name,role,clause} =>
        if roleIsCnfConjecture role then
          let
            val cnfGoals = (name,clause) :: cnfGoals
          in
            (cnfAxioms,fofAxioms,cnfGoals,fofGoals)
          end
        else
          let
            val cnfAxioms = (name,clause) :: cnfAxioms
          in
            (cnfAxioms,fofAxioms,cnfGoals,fofGoals)
          end
      | FofFormula {name,role,formula} =>
        if roleIsFofConjecture role then
          let
            val fofGoals = (name,formula) :: fofGoals
          in
            (cnfAxioms,fofAxioms,cnfGoals,fofGoals)
          end
        else
          let
            val fofAxioms = (name,formula) :: fofAxioms
          in
            (cnfAxioms,fofAxioms,cnfGoals,fofGoals)
          end;

  fun partitionFormulas fms =
      let
        val (cnfAxioms,fofAxioms,cnfGoals,fofGoals) =
            List.foldl partitionFormula ([],[],[],[]) fms

        val goal =
            case (rev cnfGoals, rev fofGoals) of
              ([],[]) => NoGoal
            | (cnfGoals,[]) => CnfGoal cnfGoals
            | ([],fofGoals) => FofGoal fofGoals
            | (_ :: _, _ :: _) =>
              raise Error "TPTP problem has both cnf and fof conjecture formulas"
      in
        {cnfAxioms = rev cnfAxioms,
         fofAxioms = rev fofAxioms,
         goal = goal}
      end;

  fun addClauses role new acc : normalization =
      let
        fun addClause ((cl,prf),(roles,proofs)) =
            (LiteralSetMap.insert roles (cl,role),
             LiteralSetMap.insert proofs (cl,prf))

        val {definitions = defs, clauses} = new
        and {definitions,roles,problem,proofs} : normalization = acc
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
      end;

  fun addCnf role ((name,clause),(norm,cnf)) =
      if List.exists literalIsBooleanTrue clause then (norm,cnf)
      else
        let
          val cl = List.mapPartial (total destLiteral) clause
          val cl = LiteralSet.fromList cl

          val prf = Normalize.singletonProof name
          val new = {definitions = [], clauses = [(cl,prf)]}

          val norm = addClauses role new norm
        in
          (norm,cnf)
        end;

  fun addFof role ((name,fm),(norm,cnf)) =
      let
        val prf = Normalize.singletonProof name
        val (new,cnf) = Normalize.cnfStateAdd (fm,prf) cnf
        val norm = addClauses role new norm
      in
        (norm,cnf)
      end;

  fun mkProblem (norm,_) : normalization =
      let
        val {definitions,roles,problem,proofs} = norm
        val {axioms,conjecture} = problem
      in
        {definitions = rev definitions,
         roles = roles,
         problem = {axioms = rev axioms, conjecture = rev conjecture},
         proofs = proofs}
      end;

  fun splitProblem acc =
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
              val subgoals = Formula.splitGoal goal
              val subgoals =
                  if null subgoals then [Formula.True] else subgoals
            in
              map (mk name) subgoals
            end
      in
        fn goals => List.concat (map split goals)
      end;

  fun clausesToGoal cls =
      let
        val cls = map (Formula.generalize o literalListToFormula o snd) cls
      in
        Formula.listMkConj cls
      end;

  fun formulasToGoal fms =
      let
        val fms = map (Formula.generalize o snd) fms
      in
        Formula.listMkConj fms
      end;
in
  fun goal ({formulas,...} : problem) =
      let
        val {cnfAxioms,fofAxioms,goal} = partitionFormulas formulas

        val fm =
            case goal of
              NoGoal => Formula.False
            | CnfGoal cls => Formula.Imp (clausesToGoal cls, Formula.False)
            | FofGoal goals => formulasToGoal goals

        val fm =
            if null fofAxioms then fm
            else Formula.Imp (formulasToGoal fofAxioms, fm)

        val fm =
            if null cnfAxioms then fm
            else Formula.Imp (clausesToGoal cnfAxioms, fm)
      in
        fm
      end;

  fun normalize ({formulas,...} : problem) : normalization list =
      let
        val {cnfAxioms,fofAxioms,goal} = partitionFormulas formulas

        val acc = (initialNormalization, Normalize.cnfStateInitial)
        val acc = List.foldl (addCnf ROLE_AXIOM) acc cnfAxioms
        val acc = List.foldl (addFof ROLE_AXIOM) acc fofAxioms
      in
        case goal of
          NoGoal => [mkProblem acc]
        | CnfGoal cls =>
          let
            val acc = List.foldl (addCnf ROLE_NEGATED_CONJECTURE) acc cls
          in
            [mkProblem acc]
          end
        | FofGoal goals => splitProblem acc goals
      end;
end;

(***
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
***)

local
  fun stripComments acc strm =
      case strm of
        Stream.Nil => (rev acc, Stream.Nil)
      | Stream.Cons (line,rest) =>
        case total destComment line of
          SOME s => stripComments (s :: acc) (rest ())
        | NONE => (rev acc, Stream.filter (not o isComment) strm);
in
  fun read {mapping,filename} =
      let
        (* Estimating parse error line numbers *)

        val lines = Stream.fromTextFile {filename = filename}

        val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
      in
        (let
           (* The character stream *)

           val (comments,chars) = stripComments [] chars

           val chars = Parse.everything Parse.any chars

           (* The formula stream *)

           val formulas = Stream.toList (parseFormula mapping chars)
         in
           {comments = comments, formulas = formulas}
         end
         handle Parse.NoParse => raise Error "parse error")
        handle Error err =>
          raise Error ("error in TPTP file \"" ^ filename ^ "\" " ^
                       parseErrorLocation () ^ "\n" ^ err)
      end;
end;

local
  fun mkCommentLine comment = mkComment comment ^ "\n";

  fun formulaStream _ _ [] () = Stream.Nil
    | formulaStream mapping start (h :: t) () =
      let
        val s = formulaToString mapping h ^ "\n"
        val s = if start then s else "\n" ^ s
      in
        Stream.Cons (s, formulaStream mapping false t)
      end;
in
  fun write {problem,mapping,filename} =
      let
        val {comments,formulas} = problem
      in
        Stream.toTextFile
          {filename = filename}
          (Stream.append
             (Stream.map mkCommentLine (Stream.fromList comments))
             (formulaStream mapping (null comments) formulas))
      end;
end;

(***
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
***)

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

  fun ppTermTstp mapping = ppTerm mapping;

  fun ppAtomTstp mapping atm =
      case total Atom.destEq atm of
        SOME (a,b) => ppAtom mapping (Name.fromString "$equal", [a,b])
      | NONE => ppAtom mapping atm;

  fun ppLiteralTstp mapping (pol,atm) =
      Print.sequence
        (if pol then Print.skip else Print.addString "~ ")
        (ppAtomTstp mapping atm);

  fun ppTermInfo mapping = Print.ppBracket "$fot(" ")" (ppTermTstp mapping);

  fun ppAtomInfo mapping = Print.ppBracket "$cnf(" ")" (ppAtomTstp mapping);

  fun ppLiteralInfo mapping =
      Print.ppBracket "$cnf(" ")" (ppLiteralTstp mapping);

  fun ppAssumeInfo mapping = ppAtomInfo mapping;

  fun ppSubstInfo mapping =
      Print.ppMap
        Subst.toList
        (Print.ppList
           (Print.ppBracket "bind(" ")"
              (Print.ppOp2 "," (ppVar mapping) (ppTermInfo mapping))));

  fun ppResolveInfo mapping = ppAtomInfo mapping;

  fun ppReflInfo mapping = ppTermInfo mapping;

  fun ppEqualityInfo mapping (lit,path,res) =
      Print.program
        [ppLiteralInfo mapping lit,
         Print.addString ",",
         Print.addBreak 1,
         Term.ppPath path,
         Print.addString ",",
         Print.addBreak 1,
         ppTermInfo mapping res];

  fun ppInfInfo mapping inf =
      case inf of
        Proof.Axiom _ => raise Bug "ppInfInfo"
      | Proof.Assume a => ppAssumeInfo mapping a
      | Proof.Subst _ => Print.skip
      | Proof.Resolve (r,_,_) => ppResolveInfo mapping r
      | Proof.Refl t => ppReflInfo mapping t
      | Proof.Equality (l,p,t) => ppEqualityInfo mapping (l,p,t);

  fun ppAxiomProof prf =
      Print.program
        [Print.addString "cnf_normalization,",
         Print.addBreak 1,
         Print.addString "[],",
         Print.addBreak 1,
         Print.ppMap StringSet.toList (Print.ppList Print.ppString) prf];

  fun ppThm prevNames th =
      case LiteralSetMap.peek prevNames (Thm.clause th) of
        SOME name => Print.addString name
      | NONE => raise Error "previous theorem not found";

  fun ppThmSub prevNames mapping (th,s) =
      if Subst.null s then ppThm prevNames th
      else Print.ppOp2 " :" (ppThm prevNames) (ppSubstInfo mapping) (th,s);
in
  fun ppProof mapping avoid prefix names roles proofs prf =
      let
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
                  Print.ppBracket "[" "]" (ppInfInfo mapping) inf] @
                 (if null parentSubs then []
                  else
                    [Print.addString ",",
                     Print.addBreak 1,
                     Print.ppList (ppThmSub prevNames mapping) parentSubs]))
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

              val cl' = clauseFromLiteralSet cl
            in
              Print.program
                ([Print.addString (name ^ ","),
                  Print.addBreak 1,
                  Print.addString (role ^ ","),
                  Print.addBreak 1,
                  Print.ppBracket "(" ")" (ppClause mapping) cl'] @
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

fun writeProof {proof,mapping,filename,avoid,prefix,names,roles,proofs} =
    Stream.toTextFile {filename = filename}
      (Print.toStream (ppProof mapping avoid prefix names roles proofs) proof);

end
