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

let preproc
    ?(filename = "prog.cob")
    ?(source_format = Cobol_config.(SF SFFixed))
    contents
  =
  Cobol_common.Srcloc.TESTING.register_file_contents ~filename contents;
  String { filename; contents } |>
  Cobol_preproc.preprocessor
    ~options:Cobol_preproc.Options.{
        default with
        source_format
      }

let options
    ?(verbose = false)
    ?(exec_scanners = Superbol_preprocs.exec_scanners) () =
  {
    (Cobol_parser.Options.default ~exec_scanners) with
    verbose;
    recovery = EnableRecovery { silence_benign_recoveries = true }
  }


let show_parsed_tokens ?(parser_options = options ())
    ?(with_locations = false) ?source_format ?filename contents =
  let { result = WithArtifacts (_, { tokens; _ }); _ } =
    preproc ?source_format ?filename contents |>
    Cobol_parser.parse_with_artifacts ~options:parser_options
  in
  (if with_locations
   then Cobol_parser.Tokens.pp'_list_with_loc_info ~fsep:"@\n"
   else Cobol_parser.Tokens.pp'_list) Fmt.stdout (Lazy.force tokens)

let show_diagnostics ?(parser_options = options ())
    ?source_format ?filename contents =
  preproc ?source_format ?filename contents |>
  Cobol_parser.parse_simple ~options:parser_options |>
  Cobol_parser.Outputs.sink_result ~set_status:false ~ppf:Fmt.stdout

let parse ?(parser_options = options ())
    ?source_format ?filename contents =
  preproc ?source_format ?filename contents |>
  Cobol_parser.parse_simple ~options:parser_options

let just_parse ?parser_options ?source_format ?filename contents =
  ignore @@ parse ?parser_options ?source_format ?filename contents
