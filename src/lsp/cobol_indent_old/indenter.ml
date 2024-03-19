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

open Indent_type

(*indent a range of file, with the default indent_config*)
let indent_range ~dialect ~source_format ~indent_config ~range ~filename ~contents =
  let indent_config = Option.value ~default:Indent_config.default indent_config in
  let src_format =
    (* Note: this value doesn't actually matter, it will be overriden
       immediately by [fold_source_lines] calling [on_initial_source_format]
       below. *)
    Cobol_preproc.Src_format.from_config SFFixed
  in
  let state =
    Cobol_preproc.fold_source_lines ~dialect ~source_format
      ~on_initial_source_format:(fun src_format st -> { st with src_format })
      ~on_compiler_directive:(fun _ { payload = cd;  _} st ->
          match cd with
          | CDir_source { payload = src_format; _ } -> { st with src_format }
          | _ -> st
        )
      ~skip_compiler_directives_text:true
      ~f:(fun _lnum line acc -> Indent_check.check_indentation line acc)
      (String { filename; contents })
      { src_format
      ; indent_config
      ; scope = BEGIN
      ; context  = []
      ; acc = []
      ; range }
  in
  (* NB: note here we ignore diagnostics *)
  state.result.acc
