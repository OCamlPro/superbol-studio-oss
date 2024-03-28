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

open Parser_testing

module DIAGS = Cobol_common.Diagnostics

let show_diagnostics ?(show_data = false) ?(verbose = false)
    ?source_format ?filename contents =
  preproc ?source_format ?filename contents |>
  Cobol_parser.parse_simple
    ~options: {
      default_parser_options with
      verbose;
      recovery = EnableRecovery { silence_benign_recoveries = true };
    } |>
  Cobol_parser.Outputs.translate_diags |>
  DIAGS.map_result ~f:Cobol_typeck.compilation_group |>
  DIAGS.more_result ~f:Cobol_typeck.Results.translate_diags |>
  DIAGS.show_n_forget ~set_status:false ~ppf:Fmt.stdout |>
  begin fun Cobol_typeck.Outputs.{ group; _ } ->
    if show_data then
      Cobol_unit.Visitor.fold_unit_group object
        inherit [unit] Cobol_unit.Visitor.folder
        method! fold_item_definition' { loc; payload = def } () =
          Cobol_common.Visitor.skip_children @@
          Pretty.out "%a@[<v>Item definition: %a@]@."
            Cobol_common.Srcloc.pp_srcloc loc
            Cobol_data.Printer.pp_item_definition def
        method! fold_record_renaming' { loc; payload = ren } () =
          Cobol_common.Visitor.skip_children @@
          Pretty.out "%a@[<v>Record renaming: %a@]@."
            Cobol_common.Srcloc.pp_srcloc loc
            Cobol_data.Printer.pp_record_renaming ren
      end group ()
  end

let show_data = show_diagnostics ~show_data:true
