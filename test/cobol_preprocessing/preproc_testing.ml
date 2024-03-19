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

open Cobol_common.Srcloc.INFIX

(** Note: won't show detailed source locations as the openned file is not
    actually on disk (that may be fixed later with a custom internal file
    store). *)

let preprocess
    ?(verbose = false)
    ?(filename = "prog.cob")
    ?(source_format = Cobol_config.(SF SFFixed))
    contents =
  Cobol_preproc.Outputs.show_n_forget ~ppf:Fmt.stdout @@
  Cobol_preproc.preprocess_input
    ~options:Cobol_preproc.Options.{ default with verbose; libpath = [];
                                                  source_format } @@
  Cobol_preproc.String { filename; contents }

let show_text
    ?(verbose = false)
    ?(filename = "prog.cob")
    ?(source_format = Cobol_config.(SF SFFixed))
    contents =
  let text =
    Cobol_preproc.Outputs.show_n_forget ~ppf:Fmt.stdout @@
    Cobol_preproc.text_of_input
      ~options:Cobol_preproc.Options.{ default with verbose; libpath = [];
                                                    source_format } @@
    Cobol_preproc.String { filename; contents }
  in
  Pretty.out "%a@\n" (Cobol_preproc.Text.pp_text' ~fsep:"@\n") text

let show_source_lines
    ?(with_line_numbers = false)
    ?(with_source_cdir_markers = false)
    ?(with_compiler_directives_text = true)
    ?(filename = "prog.cob")
    ?(dialect = Cobol_config.DIALECT.Default)
    ?(source_format = Cobol_config.(SF SFFixed))
    contents
  =
  Cobol_preproc.Outputs.show_n_forget ~ppf:Fmt.stdout @@
  Cobol_preproc.fold_source_lines ~dialect ~source_format
    ~f:begin fun lnum line () ->
      if with_line_numbers then Pretty.out "@\n%u: " lnum else Pretty.out "@\n";
      Pretty.out "%a" Cobol_preproc.Text.pp_text line;
    end (String { filename; contents }) ()
    ~skip_compiler_directives_text:(not with_compiler_directives_text)
    ?on_compiler_directive:begin
      if not with_source_cdir_markers then None
      else Option.some @@ fun lnum cdir () ->
        if with_line_numbers then Pretty.out "@\n%u: " lnum;
        match ~&cdir with
        | Cobol_preproc.Directives.CDir_source _ ->
            Pretty.out "|new source format|"
        | _ ->                        (* ignore every other kind of directives *)
            ()
    end

let rec show_all_text pp =
  match Cobol_preproc.next_chunk pp with
  | { payload = Cobol_preproc.Text.Eof; _ } :: _, _ ->
      Cobol_common.Diagnostics.Set.pp Fmt.stdout @@
      Cobol_preproc.Diagnostics.translate @@
      Cobol_preproc.diags pp;
      pp
  | text, pp ->
      Pretty.out "%a@\n" Cobol_preproc.Text.pp_text text;
      show_all_text pp

let show_lines ppf lines =
  Pretty.list ~fopen:"@[<v>" ~fsep:"@\n" ~fclose:"@]" (Fmt.fmt "%S")
    ppf lines

let preprocess_n_then_cut_n_paste_right_of_indicator
    ?(verbose = false)
    ?(filename = "prog.cob")
    ?(source_format = Cobol_config.Auto)
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
    ~options:Cobol_preproc.Options.{ default with verbose; libpath = [];
                                                  source_format } |>
  show_all_text |>
  Cobol_preproc.reset_preprocessor_for_string free_format_contents |>
  show_all_text |>
  Cobol_preproc.reset_preprocessor_for_string fixed_format_contents |>
  show_all_text |>
  ignore
