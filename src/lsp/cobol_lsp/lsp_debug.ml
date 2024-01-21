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

let debug = ref None

let () =
  match Sys.getenv "SUPERBOL_LSP_DEBUG" with
  | exception Not_found -> ()
  | file ->
    let oc = open_out file in
    debug := Some oc

module LSP_IO = struct

  let read_line line =
    match !debug with
    | None -> ()
    | Some oc ->
      Printf.fprintf oc "==>LINE: %s\n%!" line

  let read_bytes buf i n =
    match !debug with
    | None -> ()
    | Some oc ->
      Printf.fprintf oc "==>[%d]: %s\n%!" n (Bytes.sub_string buf i n)

  let write s =
    match !debug with
    | None -> ()
    | Some oc ->
      let lines = String.split_on_char '\n' s in
      let s = String.concat "\n<== " lines in
      Printf.fprintf oc "<== %s\n%!" s
end

let message fmt =
  Printf.ksprintf (fun s ->
    match !debug with
    | None -> ()
    | Some oc ->
      let lines = String.split_on_char '\n' s in
      let s = String.concat "\nMSGCONT: " lines in
      Printf.fprintf oc       "MESSAGE: %s\n%!" s
    ) fmt
