(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2026 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

let () =
  let idl_file = Sys.argv.(1) in
  let ml_file = Sys.argv.(2) in
  let c_file = Sys.argv.(3) in
  match Parser.parse_c_file idl_file with
  | Some (ci_file) ->
      let contents = Synthetize.synthetize ci_file in
      Generate.generate contents ml_file c_file
  | None ->
      Printf.eprintf "Error preprocessing %s\n%!" idl_file
