(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2
open EZCMD.TYPES

open Superbol_free_lib.Common_args

let parse_n_typeck { preproc_options; parser_options; platform; _ } filename =
  let filename = Option.value ~default:"" filename in
  Cobol_preproc.Input.from ~filename ~platform ~f:begin fun input ->
    Cobol_preproc.preprocessor ~options:preproc_options input |>
    Cobol_parser.parse_simple ~options:parser_options |>
    Cobol_parser.Outputs.translate_diags |>
    Cobol_common.Diagnostics.more_result ~f:begin fun ptree ->
      Cobol_typeck.compilation_group ~config:parser_options.config ptree
        ~fold_exec_block':Superbol_preprocs.Esql.fold_exec_block' |>
      Cobol_typeck.Results.translate_diags
    end |>
    Cobol_common.Diagnostics.show_diags ~platform
  end

let main ?style_renderer ?utf_8 () =
  Cobol_common.init_default_exn_printers ();
  Pretty.init_formatters ?style_renderer ?utf_8 ();
  let file = ref None in
  let common, common_args = Superbol_free_lib.Common_args.get () in
  let args =
    common_args @ [
      [],
      Arg.Anon (0, fun f -> file := Some f),
      EZCMD.info ~docv:"FILE" "COBOL file to interpret";
    ]
  in
  Printexc.record_backtrace true;
  EZCMD.main ~version:Version.version @@
  EZCMD.sub "tramabol" ~doc:"interpreter for COBOL programs" ~args @@
  begin fun () ->
    let type_checked_prog = parse_n_typeck (common ()) !file in
    ignore type_checked_prog;
  end
