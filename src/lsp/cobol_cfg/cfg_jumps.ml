(******************************************************************************)
(*                                                                            *)
(*     Copyright (c) 2021-2024 OCamlPro SAS                                   *)
(*                                                                            *)
(*     All rights reserved.                                                   *)
(*     This file is distributed under the terms of the                        *)
(*     OCAMLPRO-NON-COMMERCIAL license.                                       *)
(*                                                                            *)
(******************************************************************************)

open Cobol_unit
open Cobol_unit.Types
open Cobol_common.Visitor
open Cobol_common.Srcloc.INFIX

type qualname = Cobol_ptree.qualname

type jumps =
  | Go of qualname
  | GoDepending of qualname
  | Perform of qualname
  | Call of Cobol_ptree.call_prefix
  | Entry of Cobol_ptree.entry_stmt

module Jumps = Set.Make(struct
    type t = jumps
    let compare j1 j2 =
      let to_int = function
        | Go _ -> 0
        | GoDepending _ -> 1
        | Perform _ -> 2
        | Call _ -> 3
        | Entry _ -> 4
      in
      match j1, j2 with
      | Go qn1, Go qn2
      | GoDepending qn1, GoDepending qn2
      | Perform qn1, Perform qn2 -> Cobol_ptree.compare_qualname qn1 qn2
      | Call cp1, Call cp2 -> Cobol_ptree.compare_call_prefix cp1 cp2
      | Entry en1 , Entry en2 -> Cobol_ptree.compare_entry_stmt en1 en2
      | _ -> to_int j2 - to_int j1
  end)

let full_qn ~cu qn =
  (Qualmap.find_binding qn cu.unit_procedure.named).full_qn

let full_qn' ~cu qn = full_qn ~cu ~&qn


module JumpsCollector = struct
  type acc = {
    jumps: Jumps.t;
    will_fallthru: bool;
    terminal: bool;
    skip_remaining: bool;
  }

  let init = { jumps = Jumps.empty;
               terminal = false;
               will_fallthru = true;
               skip_remaining = false;
             }

  let folder ~cu = object (v)
    inherit [acc] Visitor.folder

    method! fold_goback' _ acc =
      skip @@ { acc with terminal = true;
                         will_fallthru = false;
                         skip_remaining = true }

    method! fold_stop' _ acc =
      skip @@ { acc with terminal = true;
                         will_fallthru = false;
                         skip_remaining = true }

    method! fold_exit' { payload = exit_stmt; _ } acc =
      skip @@
      match exit_stmt with
      | ExitSimple
      | ExitPerform _ -> acc
      | ExitMethod _
      | ExitProgram _ (* if in main program, does nothing, if in a called program, exit  *)
      | ExitFunction _ -> { acc with terminal = true; skip_remaining = true }
      | ExitParagraph -> { acc with skip_remaining = true } (* TODO add a goto next para ? *)
      | ExitSection -> { acc with skip_remaining = true } (* TODO: go to next section ? *)

    method! fold_evaluate' { payload; _ } acc =
      let acc_list_split l =
        List.fold_left begin fun
          (a_acc, b_acc, c_acc, d_acc)
          { jumps; terminal; will_fallthru; skip_remaining } ->
          (jumps::a_acc,
           terminal::b_acc,
           will_fallthru::c_acc,
           skip_remaining::d_acc)
        end ([], [], [], []) l in
      let { eval_branches; eval_otherwise; _ }: Cobol_ptree.evaluate_stmt =
        payload in
      let jumps, terminals, unreachables, skips = List.map begin fun branch ->
          Cobol_ptree.Visitor.fold_evaluate_branch v branch init
        end eval_branches |> acc_list_split in
      let other =
        Cobol_ptree.Visitor.fold_statements v eval_otherwise init in
      skip {
        jumps = List.fold_left Jumps.union acc.jumps (other.jumps::jumps);
        will_fallthru = List.fold_left (||) other.will_fallthru unreachables;
        terminal = List.fold_left (||) other.terminal terminals;
        skip_remaining = List.fold_left (&&) other.skip_remaining skips;
      }

    method! fold_statement' _ ({ skip_remaining; _ } as acc) =
      if skip_remaining
      then skip acc
      else do_children acc

    method! fold_if' { payload = { then_branch; else_branch; _ }; _ } acc =
      let {  jumps; terminal; will_fallthru; skip_remaining } =
        Cobol_ptree.Visitor.fold_statements v then_branch acc in
      let { jumps = else_jumps;
            terminal = else_terminal;
            will_fallthru = else_fallthru;
            skip_remaining = else_skip } =
        Cobol_ptree.Visitor.fold_statements v else_branch init in
      skip {
        jumps = Jumps.union jumps else_jumps;
        will_fallthru = will_fallthru || else_fallthru;
        terminal = terminal || else_terminal;
        skip_remaining = skip_remaining && else_skip;
      }

    method! fold_goto' { payload; _ } acc =
      skip @@
      match payload with
      | GoToEntry _ -> acc (* TODO couldn't find doc *)
      | GoToSimple { target } ->
        {
          acc with
          jumps = Jumps.add (Go (full_qn' ~cu target)) acc.jumps;
          will_fallthru = false;
          skip_remaining = true;
        }
      | GoToDepending { targets; _ } ->
        Cobol_common.Basics.NEL.(
          targets
          |> map ~f:(full_qn' ~cu)
          |> fold_left ~f:begin fun acc target ->
            Jumps.add (GoDepending target) acc
          end acc.jumps)
        |> begin fun jumps -> { acc with jumps } end

    method! fold_perform_target' { payload; _ } acc =
      let start = full_qn' ~cu payload.perform_target.procedure_start in
      skip { acc with jumps = Jumps.add (Perform start) acc.jumps }

    method! fold_call' { payload = { call_prefix; _ }; _ } acc =
      skip { acc with jumps = Jumps.add (Call call_prefix) acc.jumps }

    method! fold_entry' { payload; _ } acc =
      skip { acc with jumps = Jumps.add (Entry payload) acc.jumps }
  end
end
