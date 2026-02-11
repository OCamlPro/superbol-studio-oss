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

let typeck ?parser_options ?source_format ?filename contents =
  let fold_exec_block' ~register_name:_ _exec_block acc =
    (* We could use Superbol_preprocs.Esql.fold_exec_block' to test
       SQL statements*)
    acc
  in
  Prog_parser.parse ?parser_options ?source_format ?filename contents |>
  Cobol_parser.Outputs.translate_diags |>
  Cobol_common.Diagnostics.map_result
    ~f:(Cobol_typeck.compilation_group ~fold_exec_block') |>
  Cobol_common.Diagnostics.more_result
    ~f:Cobol_typeck.Results.translate_diags
