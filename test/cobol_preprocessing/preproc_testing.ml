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
module DIAGS = Cobol_common.Diagnostics

(** Note: won't show detailed source locations as the openned file is not
    actually on disk (that may be fixed later with a custom internal file
    store). *)
let show_diagnostics
    ?(verbose = false)
    ?(filename = "prog.cob")
    ?(source_format = Cobol_config.(SF SFFixed))
    contents =
  DIAGS.show_n_forget ~ppf:Fmt.stdout @@
  Cobol_preproc.preprocess_input
    ~options:Cobol_preproc.Options.{ default with verbose; libpath = [];
                                                  source_format } @@
  Cobol_preproc.String { filename; contents }

let show_source_lines
    ?(with_line_numbers = false)
    ?(with_source_cdir_markers = false)
    ?(with_compiler_directives_text = true)
    ?(filename = "prog.cob")
    ?(dialect = Cobol_config.DIALECT.Default)
    ?(source_format = Cobol_config.(SF SFFixed))
    contents
  =
  DIAGS.show_n_forget ~ppf:Fmt.stdout @@
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
        | Cobol_preproc.Directives.CDirSource _ ->
            Pretty.out "|new source format|"
    end
