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

let find_dir anchor =
  let curdir = Sys.getcwd () in
  let rec iter path =
    if Sys.file_exists (path // anchor) then
      path
    else
      let path' = Filename.dirname path in
      if path = path' then
        Printf.kprintf failwith "Anchor %S not found from %s" anchor curdir;
      iter path'
  in
  iter curdir

let deep_iter = FileString.(make_select iter_dir) ~deep:true
let srcdir = try Unix.getenv "DUNE_SOURCEROOT" with Not_found ->
  find_dir "test"
let testsuites = "test/testsuite"
let ibm_testsuite = testsuites // "ibm/ibmmainframes.com"
let ibm_root = srcdir // ibm_testsuite
let mf_testsuite = testsuites // "microfocus/www.csis.ul.ie"
let mf_root = srcdir // mf_testsuite
;;

module Diags = Cobol_common.Diagnostics.InitStateful ()

let preprocess_file ~source_format ~config =
  preprocess_file ~ppf:std_formatter ~epf:std_formatter
    ~options:Cobol_preproc.Options.{ source_format; config;
                                     verbose = false; libpath = [] }

let from_dialect = Cobol_config.from_dialect (module Diags)

let () =
  (* Print one token per line so we can diff outputs more easily. *)
  Pretty.pp_set_margin std_formatter 3;
  let config = from_dialect ~strict:true Cobol_config.DIALECT.MicroFocus in
  deep_iter mf_root ~glob:"*.[cC][bB][lL]"
    ~f:begin fun path ->
      printf "@[<1>Pre-processing `%s':@\n" @@ mf_testsuite // path;
      preprocess_file ~source_format:(Cobol_config.SF SFFixed) ~config
        (mf_root // path);
      printf "@]@\nDone.@."
    end;
  let config = Cobol_config.default in
  deep_iter ibm_root ~glob:"*.cbl"
    ~f:begin fun path ->
      printf "@[<1>Pre-processing `%s':@\n" @@ ibm_testsuite // path;
      preprocess_file ~config ~source_format:(Cobol_config.SF SFFree)
        (ibm_root // path);
      printf "@]@\nDone.@."
    end;
;;
