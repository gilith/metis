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

fun mkLineComment "" = "%"
  | mkLineComment line = "% " ^ line;

fun destLineComment cs =
    case cs of
      [] => ""
    | #"%" :: #" " :: rest => implode rest
    | #"%" :: rest => implode rest
    | _ => raise Error "Tptp.destLineComment";

val isLineComment = can destLineComment;

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

fun isCnfConjectureRole role =
    case role of
      NegatedConjectureRole => true
    | _ => false;

fun isFofConjectureRole role =
    case role of
      ConjectureRole => true
    | _ => false;

fun toStringRole role =
    case role of
      AxiomRole => "axiom"
    | ConjectureRole => "conjecture"
    | DefinitionRole => "definition"
    | NegatedConjectureRole => "negated_conjecture"
    | PlainRole => "plain"
    | TheoremRole => "theorem"
    | OtherRole s => s;

fun fromStringRole s =
    case s of
      "axiom" => AxiomRole
    | "conjecture" => ConjectureRole
    | "definition" => DefinitionRole
    | "negated_conjecture" => NegatedConjectureRole
    | "plain" => PlainRole
    | "theorem" => TheoremRole
    | _ => OtherRole s;

val ppRole = Print.ppMap toStringRole Print.ppString;

(* ------------------------------------------------------------------------- *)
(* SZS statuses.                                                             *)
(* ------------------------------------------------------------------------- *)

datatype status =
    CounterSatisfiableStatus
  | TheoremStatus
  | SatisfiableStatus
  | UnknownStatus
  | UnsatisfiableStatus;

fun toStringStatus status =
    case status of
      CounterSatisfiableStatus => "CounterSatisfiable"
    | TheoremStatus => "Theorem"
    | SatisfiableStatus => "Satisfiable"
    | UnknownStatus => "Unknown"
    | UnsatisfiableStatus => "Unsatisfiable";

val ppStatus = Print.ppMap toStringStatus Print.ppString;

(* ------------------------------------------------------------------------- *)
(* TPTP literals.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype literal =
    Boolean of bool
  | Literal of Literal.literal;

fun destLiteral (Literal l) = l
  | destLiteral _ = raise Error "destLiteral";

fun literalIsBooleanTrue (Boolean true) = true
  | literalIsBooleanTrue _ = false;

fun negateLiteral (Boolean b) = (Boolean (not b))
  | negateLiteral (Literal l) = (Literal (Literal.negate l));

fun functionsLiteral (Boolean _) = NameAritySet.empty
  | functionsLiteral (Literal lit) = Literal.functions lit;

fun relationLiteral (Boolean _) = NONE
  | relationLiteral (Literal lit) = SOME (Literal.relation lit);

fun literalToFormula (Boolean true) = Formula.True
  | literalToFormula (Boolean false) = Formula.False
  | literalToFormula (Literal lit) = Literal.toFormula lit;

fun literalFromFormula Formula.True = Boolean true
  | literalFromFormula Formula.False = Boolean false
  | literalFromFormula fm = Literal (Literal.fromFormula fm);

fun freeVarsLiteral (Boolean _) = NameSet.empty
  | freeVarsLiteral (Literal lit) = Literal.freeVars lit;

fun literalSubst sub lit =
    case lit of
      Boolean _ => lit
    | Literal l => Literal (Literal.subst sub l);

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
      fun funcs (lit,acc) = NameAritySet.union (functionsLiteral lit) acc
    in
      foldl funcs NameAritySet.empty
    end;

val clauseRelations =
    let
      fun rels (lit,acc) =
          case relationLiteral lit of
            NONE => acc
          | SOME r => NameAritySet.add acc r
    in
      foldl rels NameAritySet.empty
    end;

val clauseFreeVars =
    let
      fun fvs (lit,acc) = NameSet.union (freeVarsLiteral lit) acc
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
(* TPTP formula bodies.                                                      *)
(* ------------------------------------------------------------------------- *)

datatype formulaBody =
    CnfFormulaBody of literal list
  | FofFormulaBody of Formula.formula;

fun destCnfFormulaBody body =
    case body of
      CnfFormulaBody x => x
    | _ => raise Error "destCnfFormulaBody";

val isCnfFormulaBody = can destCnfFormulaBody;

fun destFofFormulaBody body =
    case body of
      FofFormulaBody x => x
    | _ => raise Error "destFofFormulaBody";

val isFofFormulaBody = can destFofFormulaBody;

fun formulaBodyFunctions body =
    case body of
      CnfFormulaBody cl => clauseFunctions cl
    | FofFormulaBody fm => Formula.functions fm;

fun formulaBodyRelations body =
    case body of
      CnfFormulaBody cl => clauseRelations cl
    | FofFormulaBody fm => Formula.relations fm;

fun formulaBodyFreeVars body =
    case body of
      CnfFormulaBody cl => clauseFreeVars cl
    | FofFormulaBody fm => Formula.freeVars fm;

fun ppFormulaBody mapping body =
    case body of
      CnfFormulaBody cl => ppClause mapping cl
    | FofFormulaBody fm => ppFof mapping fm;

(* ------------------------------------------------------------------------- *)
(* TPTP formula sources.                                                     *)
(* ------------------------------------------------------------------------- *)

datatype formulaSource =
    NoFormulaSource
  | StripFormulaSource of
      {inference : string,
       parents : string list}
  | NormalizeFormulaSource of
      {inference : Normalize.inference,
       parents : string list}
  | ProofFormulaSource of
      {inference : Proof.inference,
       parents : string list};

fun isNoFormulaSource source =
    case source of
      NoFormulaSource => true
    | _ => false;

fun functionsFormulaSource source =
    case source of
      NoFormulaSource => NameAritySet.empty
    | NormalizeFormulaSource data =>
      let
        val {inference = inf, parents = _} = data
      in
        case inf of
          Normalize.Axiom fm => Formula.functions fm
        | Normalize.Definition (_,fm) => Formula.functions fm
        | _ => NameAritySet.empty
      end
    | ProofFormulaSource data =>
      let
        val {inference = inf, parents = _} = data
      in
        case inf of
          Proof.Axiom cl => LiteralSet.functions cl
        | Proof.Assume atm => Atom.functions atm
        | Proof.Subst (sub,_) => Subst.functions sub
        | Proof.Resolve (atm,_,_) => Atom.functions atm
        | Proof.Refl tm => Term.functions tm
        | Proof.Equality (lit,_,tm) =>
          NameAritySet.union (Literal.functions lit) (Term.functions tm)
      end;

fun relationsFormulaSource source =
    case source of
      NoFormulaSource => NameAritySet.empty
    | NormalizeFormulaSource data =>
      let
        val {inference = inf, parents = _} = data
      in
        case inf of
          Normalize.Axiom fm => Formula.relations fm
        | Normalize.Definition (_,fm) => Formula.relations fm
        | _ => NameAritySet.empty
      end
    | ProofFormulaSource data =>
      let
        val {inference = inf, parents = _} = data
      in
        case inf of
          Proof.Axiom cl => LiteralSet.relations cl
        | Proof.Assume atm => NameAritySet.singleton (Atom.relation atm)
        | Proof.Subst _ => NameAritySet.empty
        | Proof.Resolve (atm,_,_) => NameAritySet.singleton (Atom.relation atm)
        | Proof.Refl tm => NameAritySet.empty
        | Proof.Equality (lit,_,_) =>
          NameAritySet.singleton (Literal.relation lit)
      end;

fun freeVarsFormulaSource source =
    case source of
      NoFormulaSource => NameSet.empty
    | NormalizeFormulaSource data => NameSet.empty
    | ProofFormulaSource data =>
      let
        val {inference = inf, parents = _} = data
      in
        case inf of
          Proof.Axiom cl => LiteralSet.freeVars cl
        | Proof.Assume atm => Atom.freeVars atm
        | Proof.Subst (sub,_) => Subst.freeVars sub
        | Proof.Resolve (atm,_,_) => Atom.freeVars atm
        | Proof.Refl tm => Term.freeVars tm
        | Proof.Equality (lit,_,tm) =>
          NameSet.union (Literal.freeVars lit) (Term.freeVars tm)
      end;

local
  val GEN_INFERENCE = "inference"
  and GEN_INTRODUCED = "introduced";

  fun ppNormalize mapping inf = Print.skip;

  local
    fun ppTermInf mapping = ppTerm mapping;

    fun ppAtomInf mapping atm =
        case total Atom.destEq atm of
          SOME (a,b) => ppAtom mapping (Name.fromString "$equal", [a,b])
        | NONE => ppAtom mapping atm;

    fun ppLiteralInf mapping (pol,atm) =
        Print.sequence
          (if pol then Print.skip else Print.addString "~ ")
          (ppAtomInf mapping atm);
  in
    fun ppProofTerm mapping =
        Print.ppBracket "$fot(" ")" (ppTermInf mapping);

    fun ppProofAtom mapping =
        Print.ppBracket "$cnf(" ")" (ppAtomInf mapping);

    fun ppProofLiteral mapping =
        Print.ppBracket "$cnf(" ")" (ppLiteralInf mapping);
  end;

  val ppProofVar = ppVar;

  val ppProofPath = Term.ppPath;

  fun ppProof mapping inf =
      Print.blockProgram Print.Inconsistent 1
        [Print.addString "[",
         (case inf of
            Proof.Axiom _ => raise Bug "Tptp.ppProof"
          | Proof.Assume atm => ppProofAtom mapping atm
          | Proof.Subst _ => Print.skip
          | Proof.Resolve (atm,_,_) => ppProofAtom mapping atm
          | Proof.Refl tm => ppProofTerm mapping tm
          | Proof.Equality (lit,path,tm) =>
            Print.program
              [ppProofLiteral mapping lit,
               Print.addString ",",
               Print.addBreak 1,
               ppProofPath path,
               Print.addString ",",
               Print.addBreak 1,
               ppProofTerm mapping tm]),
         Print.addString "]"];

  val ppParent = Print.ppString;

  fun ppProofSubst mapping =
      Print.ppMap Subst.toList
        (Print.ppList
           (Print.ppBracket "bind(" ")"
              (Print.ppOp2 "," (ppProofVar mapping)
                 (ppProofTerm mapping))));

  fun ppProofParent mapping (p,s) =
      if Subst.null s then ppParent p
      else Print.ppOp2 " :" ppParent (ppProofSubst mapping) (p,s);
in
  fun ppFormulaSource mapping source =
      case source of
        NoFormulaSource => Print.skip
      | NormalizeFormulaSource {inference,parents} =>
        let
          val gen = GEN_INFERENCE

          val name = Normalize.toStringInference inference
        in
          Print.blockProgram Print.Inconsistent (size gen + 1)
            [Print.addString gen,
             Print.addString "(",
             Print.addString name,
             Print.addString ",",
             Print.addBreak 1,
             Print.ppBracket "[" "]" (ppNormalize mapping) inference,
             Print.addString ",",
             Print.addBreak 1,
             Print.ppList ppParent parents]
        end
      | ProofFormulaSource {inference,parents} =>
        let
          val isTaut = null parents

          val gen = if isTaut then GEN_INTRODUCED else GEN_INFERENCE

          val name = Thm.inferenceTypeToString (Proof.inferenceType inference)

          val parents =
              let
                val sub =
                    case inference of
                      Proof.Subst (s,_) => s
                    | _ => Subst.empty
              in
                map (fn parent => (parent,sub)) parents
              end
        in
          Print.blockProgram Print.Inconsistent (size gen + 1)
            ([Print.addString gen,
              Print.addString "("] @
             (if isTaut then
                [Print.addString "tautology",
                 Print.addString ",",
                 Print.addBreak 1,
                 Print.blockProgram Print.Inconsistent 1
                   [Print.addString "[",
                    Print.addString name,
                    Print.addString ",",
                    Print.addBreak 1,
                    ppProof mapping inference]]
              else
                [Print.addString name,
                 Print.addString ",",
                 Print.addBreak 1,
                 ppProof mapping inference,
                 Print.addString ",",
                 Print.addBreak 1,
                 Print.ppList (ppProofParent mapping) parents]) @
             [Print.addString ")"])
        end
end;

(* ------------------------------------------------------------------------- *)
(* TPTP formulas.                                                            *)
(* ------------------------------------------------------------------------- *)

datatype formula =
    Formula of
      {name : string,
       role : role,
       body : formulaBody,
       source : formulaSource};

fun nameFormula (Formula {name,...}) = name;

fun roleFormula (Formula {role,...}) = role;

fun bodyFormula (Formula {body,...}) = body;

fun sourceFormula (Formula {source,...}) = source;

fun destCnfFormula fm = destCnfFormulaBody (bodyFormula fm);

val isCnfFormula = can destCnfFormula;

fun destFofFormula fm = destFofFormulaBody (bodyFormula fm);

val isFofFormula = can destFofFormula;

fun functionsFormula fm =
    let
      val bodyFns = formulaBodyFunctions (bodyFormula fm)
      and sourceFns = functionsFormulaSource (sourceFormula fm)
    in
      NameAritySet.union bodyFns sourceFns
    end;

fun relationsFormula fm =
    let
      val bodyRels = formulaBodyRelations (bodyFormula fm)
      and sourceRels = relationsFormulaSource (sourceFormula fm)
    in
      NameAritySet.union bodyRels sourceRels
    end;

fun freeVarsFormula fm =
    let
      val bodyFvs = formulaBodyFreeVars (bodyFormula fm)
      and sourceFvs = freeVarsFormulaSource (sourceFormula fm)
    in
      NameSet.union bodyFvs sourceFvs
    end;

val freeVarsListFormula =
    let
      fun add (fm,vs) = NameSet.union vs (freeVarsFormula fm)
    in
      List.foldl add NameSet.empty
    end;

val formulasFunctions =
    let
      fun funcs (fm,acc) = NameAritySet.union (functionsFormula fm) acc
    in
      foldl funcs NameAritySet.empty
    end;

val formulasRelations =
    let
      fun rels (fm,acc) = NameAritySet.union (relationsFormula fm) acc
    in
      foldl rels NameAritySet.empty
    end;

fun isCnfConjectureFormula fm =
    case fm of
      Formula {role, body = CnfFormulaBody _, ...} => isCnfConjectureRole role
    | _ => false;

fun isFofConjectureFormula fm =
    case fm of
      Formula {role, body = FofFormulaBody _, ...} => isFofConjectureRole role
    | _ => false;

fun isConjectureFormula fm =
    isCnfConjectureFormula fm orelse
    isFofConjectureFormula fm;

(* Parsing and pretty-printing *)

fun ppFormula mapping fm =
    let
      val Formula {name,role,body,source} = fm

      val gen =
          case body of
            CnfFormulaBody _ => "cnf"
          | FofFormulaBody _ => "fof"
    in
      Print.blockProgram Print.Inconsistent (size gen + 1)
        ([Print.addString (gen ^ "(" ^ name ^ ","),
          Print.addBreak 1,
          ppRole role,
          Print.addString ",",
          Print.addBreak 1,
          Print.blockProgram Print.Consistent 1
            [Print.addString "(",
             ppFormulaBody mapping body,
             Print.addString ")"]] @
         (if isNoFormulaSource source then []
          else
            [Print.addString ",",
             Print.addBreak 1,
             ppFormulaSource mapping source]) @
         [Print.addString ")."])
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

    val roleParser = lowerParser >> fromStringRole;

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
          val negP =
              (punctParser #"~" ++ atomParser mapping) >>
              (negateLiteral o snd)

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
          fun mk ((),((),(name,((),(role,((),(cl,((),())))))))) =
              let
                val body = CnfFormulaBody cl
                val source = NoFormulaSource
              in
                Formula
                  {name = name,
                   role = role,
                   body = body,
                   source = source}
              end
        in
          (alphaNumParser "cnf" ++ punctParser #"(" ++
           nameParser ++ punctParser #"," ++
           roleParser ++ punctParser #"," ++
           clauseParser mapping ++ punctParser #")" ++
           punctParser #".") >> mk
        end input;

    fun fofParser mapping input =
        let
          fun mk ((),((),(name,((),(role,((),(fm,((),())))))))) =
              let
                val body = FofFormulaBody fm
                val source = NoFormulaSource
              in
                Formula
                  {name = name,
                   role = role,
                   body = body,
                   source = source}
              end
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

datatype clauseSource =
    CnfClauseSource of string
  | FofClauseSource of Normalize.inference;

type 'a clauseInfo = 'a LiteralSetMap.map;

type clauseNames = string clauseInfo;

type clauseRoles = role clauseInfo;

type clauseSources = clauseSource clauseInfo;

val noClauseNames : clauseNames = LiteralSetMap.new ();

val allClauseNames : clauseNames -> StringSet.set =
    let
      fun add (_,n,s) = StringSet.add s n
    in
      LiteralSetMap.foldl add StringSet.empty
    end;

val noClauseRoles : clauseRoles = LiteralSetMap.new ();

val noClauseSources : clauseSources = LiteralSetMap.new ();

(* ------------------------------------------------------------------------- *)
(* TPTP problems.                                                            *)
(* ------------------------------------------------------------------------- *)

type comments = string list;

type problem = {comments : comments, formulas : formula list};

fun hasCnfConjecture ({formulas,...} : problem) =
    List.exists isCnfConjectureFormula formulas;

fun hasFofConjecture ({formulas,...} : problem) =
    List.exists isFofConjectureFormula formulas;

fun hasConjecture ({formulas,...} : problem) =
    List.exists isConjectureFormula formulas;

fun freeVars ({formulas,...} : problem) = freeVarsListFormula formulas;

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

        val body = CnfFormulaBody (clauseFromLiteralSet cl)

        val source = NoFormulaSource

        val formula =
            Formula
              {name = name,
               role = role,
               body = body,
               source = source}
      in
        (formula,(n,avoid))
      end;
in
  fun mkProblem {comments,names,roles,problem} =
      let
        fun fromCl defaultRole = fromClause defaultRole names roles

        val {axioms,conjecture} = problem

        val n_avoid = (0, allClauseNames names)

        val (axiomFormulas,n_avoid) = maps (fromCl AxiomRole) axioms n_avoid

        val (conjectureFormulas,_) =
            maps (fromCl NegatedConjectureRole) conjecture n_avoid

        val formulas = axiomFormulas @ conjectureFormulas
      in
        {comments = comments, formulas = formulas}
      end;
end;

type normalization =
     {problem : Problem.problem,
      sources : clauseSources};

val initialNormalization : normalization =
    {problem = {axioms = [], conjecture = []},
     sources = noClauseSources};

datatype problemGoal =
    NoGoal
  | CnfGoal of (string * clause) list
  | FofGoal of (string * Formula.formula) list;

local
  fun partitionFormula (formula,(cnfAxioms,fofAxioms,cnfGoals,fofGoals)) =
      let
        val Formula {name,role,body,...} = formula
      in
        case body of
          CnfFormulaBody cl =>
          if isCnfConjectureRole role then
            let
              val cnfGoals = (name,cl) :: cnfGoals
            in
              (cnfAxioms,fofAxioms,cnfGoals,fofGoals)
            end
          else
            let
              val cnfAxioms = (name,cl) :: cnfAxioms
            in
              (cnfAxioms,fofAxioms,cnfGoals,fofGoals)
            end
        | FofFormulaBody fm =>
          if isFofConjectureRole role then
            let
              val fofGoals = (name,fm) :: fofGoals
            in
              (cnfAxioms,fofAxioms,cnfGoals,fofGoals)
            end
          else
            let
              val fofAxioms = (name,fm) :: fofAxioms
            in
              (cnfAxioms,fofAxioms,cnfGoals,fofGoals)
            end
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

  fun addClauses role clauses acc : normalization =
      let
        fun addClause (cl_src,sources) =
            LiteralSetMap.insert sources cl_src

        val {problem,sources} : normalization = acc
        val {axioms,conjecture} = problem

        val cls = map fst clauses
        val (axioms,conjecture) =
            if isCnfConjectureRole role then (axioms, cls @ conjecture)
            else (cls @ axioms, conjecture)

        val problem = {axioms = axioms, conjecture = conjecture}
        and sources = List.foldl addClause sources clauses
      in
        {problem = problem,
         sources = sources}
      end;

  fun addCnf role ((name,clause),(norm,cnf)) =
      if List.exists literalIsBooleanTrue clause then (norm,cnf)
      else
        let
          val cl = List.mapPartial (total destLiteral) clause
          val cl = LiteralSet.fromList cl

          val src = CnfClauseSource name

          val norm = addClauses role [(cl,src)] norm
        in
          (norm,cnf)
        end;

  val addCnfAxiom = addCnf AxiomRole;

  val addCnfGoal = addCnf NegatedConjectureRole;

  fun addFof role (th,(norm,cnf)) =
      let
        fun sourcify (cl,inf) = (cl, FofClauseSource inf)

        val (clauses,cnf) = Normalize.addCnf th cnf
        val clauses = map sourcify clauses
        val norm = addClauses role clauses norm
      in
        (norm,cnf)
      end;

  fun addFofAxiom ((_,fm),acc) =
      addFof AxiomRole (Normalize.mkAxiom fm, acc);

  fun normProblem subgoal (norm,_) =
      let
        val {problem,sources} = norm
        val {axioms,conjecture} = problem
        val problem = {axioms = rev axioms, conjecture = rev conjecture}
      in
        {subgoal = subgoal,
         problem = problem,
         sources = sources}
      end;

  val normProblemFalse = normProblem (Formula.False,[]);

  fun splitProblem acc =
      let
        fun mk parents subgoal =
            let
              val subgoal = Formula.generalize subgoal

              val th = Normalize.mkAxiom (Formula.Not subgoal)

              val acc = addFof NegatedConjectureRole (th,acc)
            in
              normProblem (subgoal,parents) acc
            end

        fun split (name,goal) =
            let
              val subgoals = Formula.splitGoal goal
              val subgoals =
                  if null subgoals then [Formula.True] else subgoals

              val parents = [name]
            in
              map (mk parents) subgoals
            end
      in
        fn goals => List.concat (map split goals)
      end;

  fun clausesToGoal cls =
      let
        val cls = map (Formula.generalize o clauseToFormula o snd) cls
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

  fun normalize ({formulas,...} : problem) =
      let
        val {cnfAxioms,fofAxioms,goal} = partitionFormulas formulas

        val acc = (initialNormalization, Normalize.initialCnf)
        val acc = List.foldl addCnfAxiom acc cnfAxioms
        val acc = List.foldl addFofAxiom acc fofAxioms
      in
        case goal of
          NoGoal => [normProblemFalse acc]
        | CnfGoal cls => [normProblemFalse (List.foldl addCnfGoal acc cls)]
        | FofGoal goals => splitProblem acc goals
      end;
end;

local
  datatype blockComment =
      OutsideBlockComment
    | EnteringBlockComment
    | InsideBlockComment
    | LeavingBlockComment;

  fun stripLineComments acc strm =
      case strm of
        Stream.Nil => (rev acc, Stream.Nil)
      | Stream.Cons (line,rest) =>
        case total destLineComment line of
          SOME s => stripLineComments (s :: acc) (rest ())
        | NONE => (rev acc, Stream.filter (not o isLineComment) strm);

  fun advanceBlockComment c state =
      case state of
        OutsideBlockComment =>
        if c = #"/" then (Stream.Nil, EnteringBlockComment)
        else (Stream.singleton c, OutsideBlockComment)
      | EnteringBlockComment =>
        if c = #"*" then (Stream.Nil, InsideBlockComment)
        else if c = #"/" then (Stream.singleton #"/", EnteringBlockComment)
        else (Stream.fromList [#"/",c], OutsideBlockComment)
      | InsideBlockComment =>
        if c = #"*" then (Stream.Nil, LeavingBlockComment)
        else (Stream.Nil, InsideBlockComment)
      | LeavingBlockComment =>
        if c = #"/" then (Stream.Nil, OutsideBlockComment)
        else if c = #"*" then (Stream.Nil, LeavingBlockComment)
        else (Stream.Nil, InsideBlockComment);

  fun eofBlockComment state =
      case state of
        OutsideBlockComment => Stream.Nil
      | EnteringBlockComment => Stream.singleton #"/"
      | _ => raise Error "EOF inside a block comment";

  val stripBlockComments =
      Stream.mapsConcat advanceBlockComment eofBlockComment
        OutsideBlockComment;
in
  fun read {mapping,filename} =
      let
        (* Estimating parse error line numbers *)

        val lines = Stream.fromTextFile {filename = filename}

        val {chars,parseErrorLocation} = Parse.initialize {lines = lines}
      in
        (let
           (* The character stream *)

           val (comments,chars) = stripLineComments [] chars

           val chars = Parse.everything Parse.any chars

           val chars = stripBlockComments chars

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
  fun mkComment comment = mkLineComment comment ^ "\n";

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
             (Stream.map mkComment (Stream.fromList comments))
             (formulaStream mapping (null comments) formulas))
      end;
end;

local
  fun refute {axioms,conjecture} =
      let
        val axioms = map Thm.axiom axioms
        and conjecture = map Thm.axiom conjecture
        val problem = {axioms = axioms, conjecture = conjecture}
        val resolution = Resolution.new Resolution.default problem
      in
        case Resolution.loop resolution of
          Resolution.Contradiction _ => true
        | Resolution.Satisfiable _ => false
      end;
in
  fun prove filename =
      let
        val problem = read filename
        val problems = map #problem (normalize problem)
      in
        List.all refute problems
      end;
end;

(* ------------------------------------------------------------------------- *)
(* TSTP proofs.                                                              *)
(* ------------------------------------------------------------------------- *)

local
  fun newName avoid prefix =
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

  fun lookupClauseDerivation norm cl =
      case LiteralSetMap.peek norm cl of
        SOME deriv => deriv
      | NONE => raise Bug "Tptp.lookupClauseDerivation";

  fun lookupDefName defNames defName =
      case StringMap.peek defNames defName of
        SOME name => name
      | NONE => raise Bug "Tptp.lookupDefName";

  fun lookupFmName fmNames fm =
      case FormulaMap.peek fmNames fm of
        SOME name => name
      | NONE => raise Bug "Tptp.lookupFmName";

  fun lookupClName clNames cl =
      case LiteralSetMap.peek clNames cl of
        SOME name => name
      | NONE => raise Bug "Tptp.lookupClName";

  fun collectInferenceDeps (inf,(names,defs)) =
      let
        val names =
            case inf of
              Normalize.Axiom n => StringSet.add names n
            | Normalize.Conjecture n => StringSet.add names n
            | _ => names

        val defs =
            case inf of
              Normalize.Definition n_d => StringMap.insert defs n_d
            | _ => defs
      in
        (names,defs)
      end;

  fun collectProofStepDeps norm ((_,inf),names_defs_ths) =
      case inf of
        Proof.Axiom cl =>
        let
          val (names,defs,ths) = names_defs_ths

          val Normalize.Derivation (step,ths') = lookupClauseDerivation norm cl

          val (names,defs) = collectInferenceDeps (step,(names,defs))

          val ths = ths' @ ths
        in
          (names,defs,ths)
        end
      | _ => names_defs_ths;

  fun collectProofDeps ((norm,proof),names_defs_ths) =
      List.foldl (collectProofStepDeps norm) names_defs_ths proof;

  fun collectNormInferenceDeps ((_,inf,_),names_defs) =
      collectInferenceDeps (inf,names_defs);

  fun collectNormDeps (norm,names_defs) =
      List.foldl collectNormInferenceDeps names_defs norm;

  fun addProblemFormula names (formula,(formulas,avoid)) =
      let
        val name = nameFormula formula

        val formulas =
            if not (StringSet.member name names) then formulas
            else formula :: formulas

        val avoid = StringSet.add avoid name
      in
        (formulas,avoid)
      end;

  fun addDefinitionFormula avoid (defName,def,(formulas,i,defNames)) =
      let
        val (name,i) = newName avoid "definition_" i

        val role = DefinitionRole

        val body = FofFormulaBody def

        val source = NoFormulaSource

        val formula =
            Formula
              {name = name,
               role = role,
               body = body,
               source = source}

        val formulas = formula :: formulas

        val defNames = StringMap.insert defNames (defName,name)
      in
        (formulas,i,defNames)
      end;

  fun mkNormalizeFormulaSource defNames fmNames step fms =
      let
        val parents = map (lookupFmName fmNames) fms

        val parents =
            case step of
              Normalize.Axiom name => name :: parents
            | Normalize.Conjecture name => name :: parents
            | Normalize.Definition (defName,_) =>
              let
                val name = lookupDefName defNames defName
              in
                name :: parents
              end
            | _ => parents
      in
        NormalizeFormulaSource
          {step = step,
           parents = parents}
      end;

  fun mkProofFormulaSource norm defNames fmNames clNames inference =
      case inference of
        Proof.Axiom cl =>
        let
          val Normalize.Derivation (step,ths) = lookupClauseDerivation norm cl

          val fms = map (fst o Normalize.destDerivedFormula) ths
        in
          mkNormalizeFormulaSource defNames fmNames step fms
        end
      | _ =>
        let
          val cls = map Thm.clause (Proof.parents inference)

          val parents = map (lookupClName clNames) cls
        in
          ProofFormulaSource
            {inference = inference,
             parents = parents}
        end;

  fun addNormalizationFormula avoid defNames ((fm,inf,fms),acc) =
      let
        val (formulas,i,fmNames) = acc

        val (name,i) = newName avoid "normalization_" i

        val role = PlainRole

        val body = FofFormulaBody fm

        val source = mkNormalizeFormulaSource defNames fmNames inf fms

        val formula =
            Formula
              {name = name,
               role = role,
               body = body,
               source = source}

        val formulas = formula :: formulas

        val fmNames = FormulaMap.insert fmNames (fm,name)
      in
        (formulas,i,fmNames)
      end;

  fun addRefutationFormula avoid norm defNames fmNames prefix ((th,inf),acc) =
      let
        val (formulas,i,clNames) = acc

        val cl = Thm.clause th

        val (name,i) = newName avoid prefix i

        val role = PlainRole

        val body = CnfFormulaBody (clauseFromLiteralSet cl)

        val source = mkProofFormulaSource norm defNames fmNames clNames inf

        val formula =
            Formula
              {name = name,
               role = role,
               body = body,
               source = source}

        val formulas = formula :: formulas

        val clNames = LiteralSetMap.insert clNames (cl,name)
      in
        (formulas,i,clNames)
      end;

  fun addRefutationFormulas avoid defNames fmNames ((norm,proof),acc) =
      let
        val (formulas,i) = acc

        val prefix = "refutation_" ^ Int.toString i ^ "_"

        val clNames : string LiteralSetMap.map = LiteralSetMap.new ()
        val (formulas,_,_) =
            List.foldl (addRefutationFormula avoid norm defNames fmNames prefix)
              (formulas,0,clNames) proof

        val i = i + 1
      in
        (formulas,i)
      end;
in
  fun fromProof {problem,proofs} =
      let
        val names = StringSet.empty
        and defs : Formula.formula StringMap.map = StringMap.new ()
        and ths : Normalize.derivedFormula list = []

        val (names,defs,ths) =
            List.foldl collectProofDeps (names,defs,ths) proofs

        val norm = Normalize.deriveFormulas (rev ths)

        val (names,defs) = collectNormDeps (norm,(names,defs))

        val {comments = _, formulas} = problem

        val (formulas,avoid) =
            List.foldl (addProblemFormula names) ([],StringSet.empty) formulas

        val defNames : string StringMap.map = StringMap.new ()
        val (formulas,_,defNames) =
            StringMap.foldl (addDefinitionFormula avoid)
              (formulas,0,defNames) defs

        val fmNames : string FormulaMap.map = FormulaMap.new ()
        val (formulas,_,fmNames) =
            List.foldl (addNormalizationFormula avoid defNames)
              (formulas,0,fmNames) norm

        val (formulas,_) =
            List.foldl (addRefutationFormulas avoid defNames fmNames)
              (formulas,0) proofs
      in
        rev formulas
      end
(*MetisDebug
      handle Error err => raise Bug ("Tptp.fromProof: shouldn't fail:\n" ^ err);
*)
end;

end
