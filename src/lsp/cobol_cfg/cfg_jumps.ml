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

module Jumps = Set.Make(struct
    type t = jumps
    let compare j1 j2 =
      let to_int = function
        | Go _ -> 0
        | GoDepending _ -> 1
        | Perform _ -> 2 in
      match j1, j2 with
      | Go qn1, Go qn2
      | GoDepending qn1, GoDepending qn2
      | Perform qn1, Perform qn2 -> Cobol_ptree.compare_qualname qn1 qn2
      | _ -> to_int j2 - to_int j1
  end)

let full_qn ~cu qn =
  (Qualmap.find_binding qn cu.unit_procedure.named).full_qn

let full_qn' ~cu qn = full_qn ~cu ~&qn


module JumpsCollector = struct
  let listsplit3 l =
    List.fold_left begin fun (a_acc, b_acc, c_acc) (a, b, c) ->
      (a::a_acc, b::b_acc, c::c_acc)
    end ([], [], []) l

  type acc = {
    jumps: Jumps.t;
    will_fallthru: bool;
    terminal: bool;
  }

  let init = { jumps = Jumps.empty;
               terminal = false;
               will_fallthru = true; }

  let folder ~cu = object (v)
    inherit [acc] Visitor.folder

    method! fold_goback' _ acc =
      skip @@ { acc with terminal = true; will_fallthru = false }

    method! fold_stop' _ acc =
      skip @@ { acc with terminal = true; will_fallthru = false }

    method! fold_exit' { payload = exit_stmt; _ } acc =
      skip @@
      match exit_stmt with
      | ExitSimple | ExitPerform _
      | ExitMethod _ | ExitProgram _ | ExitFunction _ -> acc
      | ExitParagraph -> { acc with will_fallthru = true } (* TODO change this to a goto next para *)
      | ExitSection -> { acc with will_fallthru = false } (* TODO: go to next section ? *)

    method! fold_evaluate' { payload; _ } acc =
      let { eval_branches; eval_otherwise; _ }: Cobol_ptree.evaluate_stmt =
        payload in
      let jumps, terminals, unreachables = List.map begin fun branch ->
          let { jumps; terminal; will_fallthru } =
            Cobol_ptree.Visitor.fold_evaluate_branch v branch init in
          (jumps, terminal, will_fallthru)
        end eval_branches |> listsplit3 in
      let other =
        Cobol_ptree.Visitor.fold_statements v eval_otherwise init in
      skip {
        jumps = List.fold_left Jumps.union acc.jumps (other.jumps::jumps);
        will_fallthru = List.fold_left (||) other.will_fallthru unreachables;
        terminal = List.fold_left (||) other.terminal terminals;
      }

    method! fold_statement' _ ({ will_fallthru; _ } as acc) =
      if will_fallthru then do_children acc else skip acc

    method! fold_if' { payload = { then_branch; else_branch; _ }; _ } acc =
      let {  jumps; terminal; will_fallthru } =
        Cobol_ptree.Visitor.fold_statements v then_branch acc in
      let { jumps = else_jumps;
            terminal = else_terminal;
            will_fallthru = else_fallthru } =
        Cobol_ptree.Visitor.fold_statements v else_branch init in
      skip {
        jumps = Jumps.union jumps else_jumps;
        will_fallthru = will_fallthru || else_fallthru;
        terminal = terminal || else_terminal;
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
  end
end
