(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

open Preproc_diagnostics

module ENV = Preproc_env

module TYPES = struct
  type context = frame list
  and frame =
    | If_condition of condition_frame
  and condition_frame =
    {
      condition: Compdir_tree.boolexpr with_loc;
      emitting: bool;
      if_loc: srcloc;
      else_loc: srcloc option;
    }
end
include TYPES

let empty_context: context = []

let top_context: context -> frame option = function
  | c :: _ -> Some c
  | [] -> None

let emitting: context -> bool = function
  | [] -> true
  | If_condition { emitting; _ } :: _ -> emitting

(* >>DEFINE / >>SET *)

let on_define_off ~loc var ~env =
  let var' = ENV.var ~&var in
  if ENV.mem var' env
  then Ok (ENV.undefine var' env)
  else Error (Undefine_of_unknown_env_variable { loc; var })

let on_define ~loc Compdir_tree.{ var; expr; override } ~env =
  let open struct exception KEEP_UNDEFINED end in
  try
    let def_loc = ENV.Source_location loc in
    let value = match ~&expr with
      | Alphanum_literal l ->
          ENV.Alphanum { pp_payload = ~&l;
                         pp_loc = Source_location ~@l }
      | Parameter ->                                           (* [Sys.getenv] *)
          let v = String.uppercase_ascii ~&var in
          match Sys.getenv_opt v with
          | Some value -> ENV.Alphanum { pp_payload = value;
                                         pp_loc = Process_environment }
          | None -> raise KEEP_UNDEFINED
    in
    Ok (ENV.define ~def_loc (ENV.var ~&var) value ~override env)
  with
  | KEEP_UNDEFINED ->
      Ok env
  | ENV.REDEFINITION { prev_def_loc } ->
      Error (Redefinition_of_env_variable { loc; var; prev_def_loc })

(* Conditionals *)

let eval_boolexpr env: Compdir_tree.boolexpr with_loc -> bool = fun e ->
  match ~&e with
  | Boolean_literal b ->
      if Array.length (~&b).bool_value > 0
      then (~&b).bool_value.(0)
      else false                     (* CHECKME: zero-length Boolean literal? *)
  | Defined_condition { var; polarity } ->
      ENV.mem (ENV.var ~& var) env = polarity

let on_if ~loc:if_loc ~condition ~env context =
  let emitting = eval_boolexpr env condition && emitting context in
  Ok (If_condition { condition; emitting; if_loc; else_loc = None } :: context)

let on_else ~loc = function
  | If_condition ({ else_loc = None; _ } as frame) :: parent_ctxt ->
      let emitting = not frame.emitting && emitting parent_ctxt in
      Ok (If_condition { frame with else_loc = Some loc;
                                    emitting } :: parent_ctxt)
  | If_condition { else_loc = Some _; if_loc = initial_if_loc; _ } :: _ ->
      let suggestion = EndIf_compiler_directive_missing { initial_if_loc } in
      let stuff = Else_compiler_directive { suggestion = Some suggestion } in
      Error (Unexpected { loc; stuff })
  | _ ->
      let stuff = Else_compiler_directive { suggestion = None } in
      Error (Unexpected { loc; stuff })

let on_elif ~loc ~condition ~env = function
  | If_condition ({ else_loc = None; _ } as frame) :: parent_ctxt ->
      let emitting =
        not frame.emitting
        && eval_boolexpr env condition
        && emitting parent_ctxt
      in
      Ok (If_condition { frame with emitting } :: parent_ctxt)
  | If_condition { else_loc = Some _; if_loc = initial_if_loc; _ } :: _ ->
      let suggestion = EndIf_compiler_directive_missing { initial_if_loc } in
      let stuff = Elif_compiler_directive { suggestion = Some suggestion } in
      Error (Unexpected { loc; stuff })
  | _ ->
      let stuff = Elif_compiler_directive { suggestion = None } in
      Error (Unexpected { loc; stuff })

let on_endif ~loc = function
  | If_condition _ :: context ->                                         (* pop *)
      Ok context
  | _ ->
      Error (Unexpected { loc; stuff = EndIf_compiler_directive })

(* Misc. *)

let flush_contexts ~loc : context -> context * diagnostics =
  let rec flush_context diags = function
    | [] ->
        [], diags
    | If_condition { if_loc; _ } :: tl ->
        let stuff = If_compiler_directive { suggested_endif_loc = loc } in
        let error = Unterminated { loc = if_loc; stuff } in
        flush_context (add_error error diags) tl
  in
  flush_context Preproc_diagnostics.none
