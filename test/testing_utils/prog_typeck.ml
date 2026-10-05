(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

let typeck ?parser_options ?source_format ?copybooks ?filename contents =
  let module Config =
    (val match parser_options with
       | None -> Cobol_config.default
       | Some o -> o.Cobol_parser.Options.config)
  in
  let fold_exec_block' ~data_definitions:_ _exec_block acc =
    (* We could use Superbol_preprocs.Esql.fold_exec_block' to test
       SQL statements*)
    acc
  in
  Prog_parser.parse ?parser_options ?source_format ?copybooks ?filename contents |>
  Cobol_parser.Outputs.translate_diags |>
  Cobol_common.Diagnostics.map_result
    ~f:(Cobol_typeck.compilation_group
          ~options:{ binary_size = Config.binary_size#value }
          ~fold_exec_block' ) |>
  Cobol_common.Diagnostics.more_result
    ~f:Cobol_typeck.Results.translate_diags
