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

open Format
open Ez_file
open FileString.OP
open Testsuite_utils

let reparse_file ~source_format ~config filename =
  let parse ~source_format input =
    Cobol_parser.parse_simple
      ~options:Cobol_parser.Options.{
          default with
          recovery = DisableRecovery
        } @@
    Cobol_preproc.preprocessor
      ~options:Cobol_preproc.Options.{
          default with
          libpath = [];
          config;
          source_format
        } @@
    input
  in
  let print =
    Format.asprintf "@[%a@]@." Cobol_ptree.pp_compilation_group
  in
  match Cobol_preproc.Input.from ~filename ~f:(parse ~source_format) with
  | { result = Only Some cg; _ } -> (
      Format.printf "Parse: OK. ";
      let contents = print cg in
      match parse ~source_format:(SF SFFree) (String { contents; filename }) with
      | { result = Only Some cg'; _ } ->
        if Cobol_ptree.compare_compilation_group cg cg' = 0 then
          Format.printf "Reparse: OK."
        else
          Format.printf "Reparse: Different."
      | _ | exception _ -> Format.printf "Reparse: Failure."
  )
  | _ | exception _ -> Format.printf "Parse: Failure."

let () =
  (* Print one token per line so we can diff outputs more easily. *)
  Pretty.pp_set_margin std_formatter 3;
  let config = from_dialect Cobol_config.Dialect.mf_strict in
  deep_iter mf_root ~glob:"*.[cC][bB][lL]"
    ~f:begin fun path ->
      printf "@[<v 1>Re-parsing `%s':@ " @@ mf_testsuite // path;
      reparse_file ~source_format:(Cobol_config.Types.SF SFFixed) ~config
        (mf_root // path);
      printf "@]@."
    end;
  let config = Cobol_config.Config.default in
  deep_iter ibm_root ~glob:"*.cbl"
    ~f:begin fun path ->
      printf "@[<v 1>Re-parsing `%s':@ " @@ ibm_testsuite // path;
      reparse_file ~config ~source_format:(Cobol_config.Types.SF SFFree)
        (ibm_root // path);
      printf "@]@."
    end;
;;
