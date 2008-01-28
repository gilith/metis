(* ========================================================================= *)
(* METIS FIRST ORDER PROVER                                                  *)
(*                                                                           *)
(* Copyright (c) 2001-2007 Joe Hurd                                          *)
(*                                                                           *)
(* Metis is free software; you can redistribute it and/or modify             *)
(* it under the terms of the GNU General Public License as published by      *)
(* the Free Software Foundation; either version 2 of the License, or         *)
(* (at your option) any later version.                                       *)
(*                                                                           *)
(* Metis is distributed in the hope that it will be useful,                  *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *)
(* GNU General Public License for more details.                              *)
(*                                                                           *)
(* You should have received a copy of the GNU General Public License         *)
(* along with Metis; if not, write to the Free Software                      *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA *)
(* ========================================================================= *)

open Useful;

(* ------------------------------------------------------------------------- *)
(* The program name.                                                         *)
(* ------------------------------------------------------------------------- *)

val PROGRAM = "metis";

(* ------------------------------------------------------------------------- *)
(* Program options.                                                          *)
(* ------------------------------------------------------------------------- *)

val QUIET = ref false;

val TEST = ref false;

val ITEMS = ["name","goal","clauses","size","category","proof","saturated"];

val show_items = map (fn s => (s, ref false)) ITEMS;

fun show_ref s =
    case List.find (equal s o fst) show_items of
      NONE => raise Bug ("item " ^ s ^ " not found")
    | SOME (_,r) => r;

fun showing s = not (!QUIET) andalso (s = "status" orelse !(show_ref s));

fun notshowing s = not (showing s);

fun showing_any () = List.exists showing ITEMS;

fun notshowing_any () = not (showing_any ());

fun show s = case show_ref s of r => r := true;

fun hide s = case show_ref s of r => r := false;

local
  open Useful Options;
in
  val specialOptions =
    [{switches = ["--show"], arguments = ["ITEM"],
      description = "show ITEM (see below for list)",
      processor =
        beginOpt (enumOpt ITEMS endOpt) (fn _ => fn s => show s)},
     {switches = ["--hide"], arguments = ["ITEM"],
      description = "hide ITEM (see below for list)",
      processor =
        beginOpt (enumOpt ITEMS endOpt) (fn _ => fn s => hide s)},
     {switches = ["-q","--quiet"], arguments = [],
      description = "Run quietly; indicate provability with return value",
      processor = beginOpt endOpt (fn _ => QUIET := true)},
     {switches = ["--test"], arguments = [],
      description = "Skip the proof search for the input problems",
      processor = beginOpt endOpt (fn _ => TEST := true)}];
end;

val VERSION = "2.0";

val versionString = "Metis "^VERSION^" (release 20080128)"^"\n";

val programOptions =
    {name = PROGRAM,
     version = versionString,
     header = "usage: "^PROGRAM^" [option ...] problem.tptp ...\n" ^
              "Proves the input TPTP problem files.\n",
     footer = "Possible ITEMs are {" ^ join "," ITEMS ^ "}.\n" ^
              "Problems can be read from standard input using the " ^
              "special - filename.\n",
     options = specialOptions @ Options.basicOptions};

fun exit x : unit = Options.exit programOptions x;
fun succeed () = Options.succeed programOptions;
fun fail mesg = Options.fail programOptions mesg;
fun usage mesg = Options.usage programOptions mesg;

val (opts,work) =
    Options.processOptions programOptions (CommandLine.arguments ());

val () = if null work then usage "no input problem files" else ();

(* ------------------------------------------------------------------------- *)
(* The core application.                                                     *)
(* ------------------------------------------------------------------------- *)

local
  fun display_sep () =
      if notshowing_any () then ()
      else print (nChars #"-" (!Parser.lineLength) ^ "\n");

  fun display_name filename =
      if notshowing "name" then ()
      else print ("Problem: " ^ filename ^ "\n\n");

  fun display_goal tptp =
      if notshowing "goal" then ()
      else
        let
          val goal = Tptp.goal tptp
        in
          print ("Goal:\n" ^ Formula.toString goal ^ "\n\n")
        end;

  fun display_clauses cls =
      if notshowing "clauses" then ()
      else print ("Clauses:\n" ^ Problem.toString cls ^ "\n\n");

  fun display_size cls =
      if notshowing "size" then ()
      else
        let
          fun plural 1 s = "1 " ^ s
            | plural n s = Int.toString n ^ " " ^ s ^ "s"
                           
          val {clauses,literals,symbols,typedSymbols} = Problem.size cls
        in
          print
            ("Size: " ^
             plural clauses "clause" ^ ", " ^
             plural literals "literal" ^ ", " ^
             plural symbols "symbol" ^ ", " ^
             plural typedSymbols "typed symbol" ^ ".\n\n")
        end;
        
  fun display_category cls =
      if notshowing "category" then ()
      else
        let
          val cat = Problem.categorize cls
        in
          print ("Category: " ^ Problem.categoryToString cat ^ ".\n\n")
        end;

  local
    fun display_proof_start filename =
        print ("\nSZS output start CNFRefutation for " ^ filename ^ "\n");

    fun display_proof_body avoid prefix names roles proofs proof =
        Tptp.writeProof
          {filename = "-",
           avoid = avoid,
           prefix = prefix,
           names = names,
           roles = roles,
           proofs = proofs}
          proof;

    fun display_proof_end filename =
        print ("SZS output end CNFRefutation for " ^ filename ^ "\n\n");
  in
    fun display_cnf_proof filename names th = 
        if notshowing "proof" then ()
        else
          let
            val avoid = Tptp.allClauseNames names
            and prefix = ""
            and roles = Tptp.noClauseRoles
            and proofs = Tptp.noClauseProofs
            and proof = Proof.proof th
          in
            display_proof_start filename;
            display_proof_body avoid prefix names roles proofs proof;
            display_proof_end filename
          end;

    fun display_fof_proof filename tptp acc =
        if notshowing "proof" then ()
        else
          let
            fun calc_used ((defns,proofs,th),(avoid,used,defs,acc)) =
                let
                  fun add_line ((t,_),(used,roles)) =
                      let
                        val cl = Thm.clause t
                      in
                        case LiteralSetMap.peek proofs cl of
                          NONE => (used,roles)
                        | SOME set =>
                          (StringSet.union set used,
                           LiteralSetMap.insert roles (cl,Tptp.ROLE_PLAIN))
                      end

                  val proof = Proof.proof th
                  val roles = Tptp.noClauseRoles
                  val (used,roles) = List.foldl add_line (used,roles) proof

                  fun add_def ((name,def),(avoid,defs)) =
                      (StringSet.add avoid name,
                       if not (StringSet.member name used) then defs
                       else StringMap.insert defs (name,def))

                  val (avoid,defs) = List.foldl add_def (avoid,defs) defns
                  val acc = (roles,proofs,proof) :: acc
                in
                  (avoid,used,defs,acc)
                end

            val avoid = StringSet.empty
            and used = StringSet.empty
            and defs = StringMap.new ()
            val (avoid,used,defs,acc) =
                List.foldl calc_used (avoid,used,defs,[]) acc

            fun get_used (formula,(avoid,formulas)) =
                case formula of
                  Tptp.FofFormula {name,...} =>
                  (StringSet.add avoid name,
                   if not (StringSet.member name used) then formulas
                   else formula :: formulas)
                | Tptp.CnfFormula _ => raise Bug "get_used"

            val {comments = _, formulas} = tptp
            val (avoid,formulas) = List.foldl get_used (avoid,[]) formulas

            fun add_def (name,def,formulas) =
                let
                  val role = Tptp.ROLE_DEFINITION
                  val formula =
                      Tptp.FofFormula {name = name, role = role, formula = def}
                in
                  formula :: formulas
                end

            val formulas = StringMap.foldl add_def formulas defs

            val axioms = {comments = [], formulas = rev formulas}

            val names = Tptp.noClauseNames

            fun display n ((roles,proofs,proof),(start,i)) =
                let
                  val prefix = if n = 1 then ""
                               else "subgoal" ^ Int.toString (i + 1) ^ "_"
                in
                  if start then () else print "\n";
                  display_proof_body avoid prefix names roles proofs proof;
                  (false, i + 1)
                end
          in
            display_proof_start filename;
            if null formulas then ()
            else Tptp.write {filename = "-"} axioms;
            List.foldl (display (length acc)) (null formulas, 0) acc;
            display_proof_end filename
          end;
  end;

  fun display_saturated filename ths =
      if notshowing "saturated" then ()
      else
        let
(*DEBUG
          val () = Tptp.write {filename = "saturated.tptp"}
                     (Tptp.mkCnfProblem
                        {comments = ["Saturated clause set for " ^ filename],
                         names = Tptp.noClauseNames,
                         roles = Tptp.noClauseRoles,
                         problem = map Thm.clause ths})
*)
          val () = print ("\nSZS output start Saturated for " ^ filename ^ "\n")
          val () = app (fn th => print (Thm.toString th ^ "\n")) ths
          val () = print ("SZS output end Saturated for " ^ filename ^ "\n\n")
        in
          ()
        end;

  fun display_status filename status =
      if notshowing "status" then ()
      else print ("SZS status " ^ status ^ " for " ^ filename ^ "\n");

  fun display_problem filename cls =
      let
(*DEBUG
          val () = Tptp.write {filename = "cnf.tptp"}
                     (Tptp.mkCnfProblem
                        {comments = ["CNF clauses for " ^ filename],
                         names = Tptp.noClauseNames,
                         roles = Tptp.noClauseRoles,
                         problem = cls})
*)
        val () = display_clauses cls
        val () = display_size cls
        val () = display_category cls
      in
        ()
      end;

  fun refute cls =
      Resolution.loop (Resolution.new Resolution.default (map Thm.axiom cls));
in
  fun prove filename =
      let
        val () = display_sep ()
        val () = display_name filename
        val tptp = Tptp.read {filename = filename}
        val () = display_goal tptp
      in
        if Tptp.isCnfProblem tptp then
          let
            val {names,problem,...} = Tptp.destCnfProblem tptp
            val () = display_problem filename problem
          in
            if !TEST then
              (display_status filename Tptp.STATUS_UNKNOWN;
               true)
            else
              case refute problem of
                Resolution.Contradiction th =>
                (display_status filename Tptp.STATUS_UNSATISFIABLE;
                 display_cnf_proof filename names th;
                 true)
              | Resolution.Satisfiable ths =>
                (display_status filename Tptp.STATUS_SATISFIABLE;
                 display_saturated filename ths;
                 false)
          end
        else
          let
            fun refuteAll acc [] =
                let
                  val status =
                      if !TEST then Tptp.STATUS_UNKNOWN
                      else if Tptp.hasConjecture tptp then Tptp.STATUS_THEOREM
                      else Tptp.STATUS_UNSATISFIABLE

                  val () = display_status filename status

                  val () = if !TEST then ()
                           else display_fof_proof filename tptp acc
                in
                  true
                end
              | refuteAll acc (prob :: problems) =
                let
                  val {definitions, roles = _, problem, proofs} = prob
                  val () = display_problem filename problem
                in
                  if !TEST then refuteAll acc problems
                  else
                    case refute problem of
                      Resolution.Contradiction th =>
                      refuteAll ((definitions,proofs,th) :: acc) problems
                    | Resolution.Satisfiable ths =>
                      let
                        val status =
                            if Tptp.hasConjecture tptp then
                              Tptp.STATUS_COUNTER_SATISFIABLE
                            else
                              Tptp.STATUS_SATISFIABLE

                        val () = display_status filename status
                        val () = display_saturated filename ths
                      in
                        false
                      end
                end

            val problems = Tptp.normalizeFof tptp
          in
            refuteAll [] problems
          end
      end;
end;

(* ------------------------------------------------------------------------- *)
(* Top level.                                                                *)
(* ------------------------------------------------------------------------- *)

val () =
let
(*DEBUG
  val () = print "Running in DEBUG mode.\n"
*)
  val success = List.all prove work
  val return = not (!QUIET) orelse success
in
  exit {message = NONE, usage = false, success = return}
end
handle Error s => die (PROGRAM^" failed:\n" ^ s)
     | Bug s => die ("BUG found in "^PROGRAM^" program:\n" ^ s);
