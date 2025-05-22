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

let default_parser_options =
  let exec_scanners =
    Superbol_preprocs.more [
      "SQL", Superbol_preprocs.Esql.scanner;
    ]
  in
  Cobol_parser.Options.{
    (default ~exec_scanners) with
    recovery = DisableRecovery
  }

let reparse_file ~source_format ~config filename =
  let parse ~source_format input =
    Cobol_parser.parse_simple
      ~options:default_parser_options @@
    Cobol_preproc.preprocessor
      ~options:Cobol_preproc.Options.{
          default with
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
  let config = from_dialect Cobol_config.DIALECT.mf_strict in
  deep_iter mf_root ~glob:"*.[cC][bB][lL]"
    ~f:begin fun path ->
      printf "@[<v 1>Re-parsing `%s':@ " @@ mf_testsuite // path;
      reparse_file ~source_format:(Cobol_config.SF SFFixed) ~config
        (mf_root // path);
      printf "@]@."
    end;
  let config = Cobol_config.default in
  deep_iter ibm_root ~glob:"*.cbl"
    ~f:begin fun path ->
      printf "@[<v 1>Re-parsing `%s':@ " @@ ibm_testsuite // path;
      (* Note: we (and GnuCOBOL) don't appear to provide IBM's "EXTENDED" format
         that has lines of 252 bytes. Closest we have is xCard (with lines of
         255 bytes). *)
      reparse_file ~config ~source_format:(Cobol_config.SF SFxCard)
        (ibm_root // path);
      printf "@]@."
    end;
  let config = Cobol_config.default in
  deep_iter sql_exec_root ~glob:"*.cbl"
    ~f:begin fun path ->
      printf "@[<v 1>Re-parsing `%s':@ " @@ sql_exec_testsuite // path;
      reparse_file ~config ~source_format:(Cobol_config.Auto)
        (sql_exec_root // path);
      printf "@]@."
    end;
  let config = Cobol_config.default in
  deep_iter gixsql_root ~glob:"*.cbl"
    ~f:begin fun path ->
      printf "@[<v 1>Re-parsing `%s':@ " @@ gixsql_testsuite // path;
      reparse_file ~config ~source_format:(Cobol_config.Auto)
        (gixsql_root // path);
      printf "@]@."
    end;
;;
