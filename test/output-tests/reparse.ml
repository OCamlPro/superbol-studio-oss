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
open Cobol_preproc

let deep_iter = FileString.(make_select iter_dir) ~deep:true
let srcdir = try Unix.getenv "DUNE_SOURCEROOT" with Not_found -> "."
let testsuites = "test/testsuite"
let ibm_testsuite = testsuites // "ibm/ibmmainframes.com"
let ibm_root = srcdir // ibm_testsuite
let mf_testsuite = testsuites // "microfocus/www.csis.ul.ie"
let mf_root = srcdir // mf_testsuite
;;

module Diags = Cobol_common.Diagnostics.InitStateful ()

let preprocess_file ~source_format ?config =
  preprocess_file ~source_format ?config ~verbose:false ~libpath:[]
    ~ppf:std_formatter ~epf:std_formatter

let reparse_file ~source_format ?config filename =
  let parse =
    Cobol_parser.parse_simple ~recovery:DisableRecovery ?config ~libpath:[]
  in
  let print =
    Format.asprintf "@[%a@]@." Cobol_parser.PTree.pp_compilation_group
  in
  match parse ~source_format (Filename filename) with
  | { parsed_output = Only Some cg; _ } -> (
      Format.printf "Parse: OK. ";
      let contents = print cg in
      match parse ~source_format:(SF SFFree) (String { contents; filename }) with
      | { parsed_output = Only Some cg'; _ } ->
        if Cobol_parser.PTree.compare_compilation_group cg cg' = 0 then
          Format.printf "Reparse: OK."
        else
          Format.printf "Reparse: Different."
      | _ | exception _ -> Format.printf "Reparse: Failure."
  )
  | _ | exception _ -> Format.printf "Parse: Failure."

let from_dialect = Cobol_config.from_dialect (module Diags)

let () =
  (* Print one token per line so we can diff outputs more easily. *)
  Pretty.pp_set_margin std_formatter 3;
  let config = from_dialect ~strict:true Cobol_config.DIALECT.MicroFocus in
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
      reparse_file ~config ~source_format:(Cobol_config.SF SFFree)
        (ibm_root // path);
      printf "@]@."
    end;
;;
