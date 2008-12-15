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

val ITEMS = ["name","goal","clauses","size","category","proof","saturation"];

val extended_items = "all" :: ITEMS;

val show_items = map (fn s => (s, ref false)) ITEMS;

fun show_ref s =
    case List.find (equal s o fst) show_items of
      NONE => raise Bug ("item " ^ s ^ " not found")
    | SOME (_,r) => r;

fun show_set b = app (fn (_,r) => r := b) show_items;

fun showing s = not (!QUIET) andalso (s = "status" orelse !(show_ref s));

fun notshowing s = not (showing s);

fun showing_any () = List.exists showing ITEMS;

fun notshowing_any () = not (showing_any ());

fun show "all" = show_set true
  | show s = case show_ref s of r => r := true;

fun hide "all" = show_set false
  | hide s = case show_ref s of r => r := false;

local
  open Useful Options;
in
  val specialOptions =
    [{switches = ["--show"], arguments = ["ITEM"],
      description = "show ITEM (see below for list)",
      processor =
        beginOpt (enumOpt extended_items endOpt) (fn _ => fn s => show s)},
     {switches = ["--hide"], arguments = ["ITEM"],
      description = "hide ITEM (see below for list)",
      processor =
        beginOpt (enumOpt extended_items endOpt) (fn _ => fn s => hide s)},
     {switches = ["-q","--quiet"], arguments = [],
      description = "Run quietly; indicate provability with return value",
      processor = beginOpt endOpt (fn _ => QUIET := true)},
     {switches = ["--test"], arguments = [],
      description = "Skip the proof search for the input problems",
      processor = beginOpt endOpt (fn _ => TEST := true)}];
end;

val VERSION = "2.1";

val versionString = "Metis "^VERSION^" (release 20081215)"^"\n";

val programOptions =
    {name = PROGRAM,
     version = versionString,
     header = "usage: "^PROGRAM^" [option ...] problem.tptp ...\n" ^
              "Proves the input TPTP problem files.\n",
     footer = "Possible ITEMs are {" ^ join "," extended_items ^ "}.\n" ^
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
      else print (nChars #"-" (!Print.lineLength) ^ "\n");

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

    fun display_proof_body mapping avoid prefix names roles proofs proof =
        let
          val mapping =
              Tptp.addVarSetTptpMapping mapping (Proof.freeVars proof)

          val filename = "-"
        in
          Tptp.writeProof
            {proof = proof,
             mapping = mapping,
             filename = filename,
             avoid = avoid,
             prefix = prefix,
             names = names,
             roles = roles,
             proofs = proofs}
        end;

    fun display_proof_end filename =
        print ("SZS output end CNFRefutation for " ^ filename ^ "\n\n");
  in
    fun display_cnf_proof filename names th =
        if notshowing "proof" then ()
        else
          let
            val mapping = Tptp.defaultTptpMapping
            and avoid = Tptp.allClauseNames names
            and prefix = ""
            and roles = Tptp.noClauseRoles
            and proofs = Tptp.noClauseProofs
            and proof = Proof.proof th

            val () = display_proof_start filename
            val () =
                display_proof_body mapping avoid prefix names roles proofs proof
            val () = display_proof_end filename
          in
            ()
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
                          let
                            val used = StringSet.union set used
                            val role = Tptp.ROLE_PLAIN
                            val roles = LiteralSetMap.insert roles (cl,role)
                          in
                            (used,roles)
                          end
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

            val mapping = Tptp.defaultTptpMapping
            val mapping =
                Tptp.addVarSetTptpMapping mapping
                  (Tptp.formulaListFreeVars formulas)

            fun display n ((roles,proofs,proof),(start,i)) =
                let
                  val prefix = if n = 1 then ""
                               else "subgoal" ^ Int.toString (i + 1) ^ "_"

                  val () = if start then () else print "\n"

                  val () =
                      display_proof_body mapping avoid
                        prefix names roles proofs proof
                in
                  (false, i + 1)
                end

            val () = display_proof_start filename

            val _ = List.foldl (display (length acc)) (null formulas, 0) acc

            val () = display_proof_end filename
          in
            ()
          end;
  end;

  fun display_saturation filename ths =
      if notshowing "saturation" then ()
      else
        let
(*MetisDebug
          val () = Tptp.write {filename = "saturation.tptp"}
                     (Tptp.mkCnfProblem
                        {comments = ["Saturation clause set for " ^ filename],
                         names = Tptp.noClauseNames,
                         roles = Tptp.noClauseRoles,
                         problem = {axioms = [],
                                    conjecture = map Thm.clause ths}})
*)
          val () = print ("\nSZS output start Saturation for " ^ filename ^ "\n")
          val () = app (fn th => print (Thm.toString th ^ "\n")) ths
          val () = print ("SZS output end Saturation for " ^ filename ^ "\n\n")
        in
          ()
        end;

  fun display_status filename status =
      if notshowing "status" then ()
      else print ("SZS status " ^ status ^ " for " ^ filename ^ "\n");

  fun display_problem filename cls =
      let
(*MetisDebug
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

  fun refute {axioms,conjecture} =
      let
        val axioms = map Thm.axiom axioms
        and conjecture = map Thm.axiom conjecture
        val problem = {axioms = axioms, conjecture = conjecture}
        val resolution = Resolution.new Resolution.default problem
      in
        Resolution.loop resolution
      end
in
  fun prove mapping filename =
      let
        val () = display_sep ()
        val () = display_name filename
        val tptp = Tptp.read {filename = filename, mapping = mapping}
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
                 display_saturation filename ths;
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
                        val () = display_saturation filename ths
                      in
                        false
                      end
                end

            val problems = Tptp.normalizeFof tptp
          in
            refuteAll [] problems
          end
      end;

  fun proveAll mapping filenames =
      List.all
        (if !QUIET then prove mapping
         else fn filename => prove mapping filename orelse true)
        filenames;
end;

(* ------------------------------------------------------------------------- *)
(* Top level.                                                                *)
(* ------------------------------------------------------------------------- *)

val () =
let
  (*BasicDebug val () = print "Running in basic DEBUG mode.\n" *)
  (*MetisDebug val () = print "Running in metis DEBUG mode.\n" *)

  val mapping = Tptp.defaultTptpMapping

  val success = proveAll mapping work
in
  exit {message = NONE, usage = false, success = success}
end
handle Error s => die (PROGRAM^" failed:\n" ^ s)
     | Bug s => die ("BUG found in "^PROGRAM^" program:\n" ^ s);
