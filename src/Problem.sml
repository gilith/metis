(* ========================================================================= *)
(* SOME SAMPLE PROBLEMS TO TEST PROOF PROCEDURES                             *)
(* Copyright (c) 2001-2006 Joe Hurd, distributed under the GNU GPL version 2 *)
(* ========================================================================= *)

structure Problem :> Problem =
struct

open Useful;

(* ------------------------------------------------------------------------- *)
(* Problems.                                                                 *)
(* ------------------------------------------------------------------------- *)

type problem = Thm.clause list;

fun size cls =
    {clauses = length cls,
     literals = foldl (fn (cl,n) => n + LiteralSet.size cl) 0 cls,
     symbols = foldl (fn (cl,n) => n + LiteralSet.symbols cl) 0 cls,
     typedSymbols = foldl (fn (cl,n) => n + LiteralSet.typedSymbols cl) 0 cls};

fun toFormula cls =
    let
      fun formulize cl =
          Formula.listMkDisj (LiteralSet.transform Literal.toFormula cl)
    in
      Formula.listMkConj (map formulize cls)
    end;

fun toString cls = Formula.toString (toFormula cls);

(* ------------------------------------------------------------------------- *)
(* Categorizing problems.                                                    *)
(* ------------------------------------------------------------------------- *)

datatype propositional =
    Propositional
  | EffectivelyPropositional
  | NonPropositional;

datatype equality =
    NonEquality
  | Equality
  | PureEquality;

datatype horn =
    Trivial
  | Unit
  | DoubleHorn
  | Horn
  | NegativeHorn
  | NonHorn;

type category =
     {propositional : propositional,
      equality : equality,
      horn : horn};

fun categorize cls =
    let
      val rels =
          let
            fun f (cl,set) = NameAritySet.union set (LiteralSet.relations cl)
          in
            List.foldl f NameAritySet.empty cls
          end

      val funs =
          let
            fun f (cl,set) = NameAritySet.union set (LiteralSet.functions cl)
          in
            List.foldl f NameAritySet.empty cls
          end

      val propositional =
          if NameAritySet.allNullary rels then Propositional
          else if NameAritySet.allNullary funs then EffectivelyPropositional
          else NonPropositional

      val equality =
          if not (NameAritySet.member Atom.eqRelation rels) then NonEquality
          else if NameAritySet.size rels = 1 then PureEquality
          else Equality

      val horn =
          if List.exists LiteralSet.null cls then Trivial
          else if List.all (fn cl => LiteralSet.size cl = 1) cls then Unit
          else 
            let
              fun pos cl = LiteralSet.count Literal.positive cl <= 1
              fun neg cl = LiteralSet.count Literal.negative cl <= 1
            in
              case (List.all pos cls, List.all neg cls) of
                (true,true) => DoubleHorn
              | (true,false) => Horn
              | (false,true) => NegativeHorn
              | (false,false) => NonHorn
            end
    in
      {propositional = propositional,
       equality = equality,
       horn = horn}
    end;

fun categoryToString {propositional,equality,horn} =
    (case propositional of
       Propositional => "propositional"
     | EffectivelyPropositional => "effectively propositional"
     | NonPropositional => "non-propositional") ^
    ", " ^
    (case equality of
       NonEquality => "non-equality"
     | Equality => "equality"
     | PureEquality => "pure equality") ^
    ", " ^
    (case horn of
       Trivial => "trivial"
     | Unit => "unit"
     | DoubleHorn => "horn (and negative horn)"
     | Horn => "horn"
     | NegativeHorn => "negative horn"
     | NonHorn => "non-horn");

end
