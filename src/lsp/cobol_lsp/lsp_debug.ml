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

let debug_oc = ref None

let () =
  match Sys.getenv "SUPERBOL_LSP_DEBUG" with
  | exception Not_found -> ()
  | file ->

    let rec iter counter =
      let use_counter = ref false in
      let b = Buffer.create 1000 in
      Buffer.add_substitute b (function
          (* NB: not available in JS backend *)
          (* | "pid" -> *)
          (*     string_of_int ( Unix.getpid () ) *)
          | "count" ->
            use_counter := true ;
            string_of_int counter
          | s -> s
        ) file ;
      let file = Buffer.contents b in
      if !use_counter && Sys.file_exists file then
        iter (counter+1)
      else
        file
    in
    let file = iter 1 in
    let oc = open_out file in
    debug_oc := Some oc

module LSP_IO = struct

  let read_line line =
    match !debug_oc with
    | None -> ()
    | Some oc ->
      Printf.fprintf oc "==>LINE: %s\n%!" line

  let read_bytes buf i n =
    match !debug_oc with
    | None -> ()
    | Some oc ->
      Printf.fprintf oc "==>[%d]: %s\n%!" n (Bytes.sub_string buf i n)

  let write s =
    match !debug_oc with
    | None -> ()
    | Some oc ->
      let lines = String.split_on_char '\n' s in
      let s = String.concat "\n<== " lines in
      Printf.fprintf oc "<== %s\n%!" s
end

let message fmt =
  Printf.ksprintf (fun s ->
    match !debug_oc with
    | None -> ()
    | Some oc ->
      let lines = String.split_on_char '\n' s in
      let s = String.concat "\nMSGCONT: " lines in
      Printf.fprintf oc       "MESSAGE: %s\n%!" s
    ) fmt
