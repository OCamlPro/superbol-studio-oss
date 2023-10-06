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

open Ez_file
open FileString.OP

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
let srcdir =
  try Unix.getenv "DUNE_SOURCEROOT" with Not_found -> find_dir "test"
let () =            (* TODO: avoid relying on this var only in `Cobol_config` *)
  Unix.putenv "COB_CONFIG_DIR" (srcdir // "import" // "gnucobol" // "config")

let srcdir_marker = "__srcdir__"
let srcdir_regexp = Str.(regexp @@ quote srcdir)
let end_with_postproc s =
  print_endline @@ Str.global_replace srcdir_regexp srcdir_marker s

let testsuites = "test" // "testsuite"
let ibm_testsuite = testsuites // "ibm" // "ibmmainframes.com"
let ibm_root = srcdir // ibm_testsuite
let mf_testsuite = testsuites // "microfocus" // "www.csis.ul.ie"
let mf_root = srcdir // mf_testsuite

module Diags = Cobol_common.Diagnostics.InitStateful ()

let from_dialect = Cobol_config.from_dialect (module Diags)
