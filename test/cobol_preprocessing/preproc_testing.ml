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

include Prog_preproc

let preprocess_n_then_cut_n_paste_right_of_indicator
    ?(verbose = false)
    ?(filename = "prog.cob")
    ?(source_format = Gnucobol_config.Auto)
    fixed_format_contents =
  let fixed_lines = EzString.split fixed_format_contents '\n' in
  let free_lines =
    List.map (fun l -> try EzString.after l 6 with Invalid_argument _ -> l)
      fixed_lines
  in
  let free_format_contents = String.concat "\n" free_lines in
  Pretty.out "fixed: %a@." show_lines fixed_lines;
  Pretty.out " free: %a@." show_lines free_lines;
  Cobol_preproc.Input.string ~filename fixed_format_contents |>
  Cobol_preproc.preprocessor
    ~options:Cobol_preproc.Options.{ default with verbose; source_format } |>
  show_all_text |>
  Cobol_preproc.reset_preprocessor_for_string free_format_contents |>
  show_all_text |>
  Cobol_preproc.reset_preprocessor_for_string fixed_format_contents |>
  show_all_text |>
  ignore
