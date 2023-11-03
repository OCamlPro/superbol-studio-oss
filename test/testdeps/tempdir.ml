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

(* https://discuss.ocaml.org/t/how-to-create-a-temporary-directory-in-ocaml/1815/4 *)
let rand_digits () =
  Printf.sprintf "%06x" Random.State.(bits (make_self_init ()) land 0xFFFFFF)

let mk_temp_dir ?(mode=0o700) ?dir pat =
  let dir = match dir with
    | Some d -> d
    | None   -> Filename.get_temp_dir_name ()
  in
  let raise_err msg = raise (Sys_error msg) in
  let rec loop count =
    if count < 0
    then
      raise_err "mk_temp_dir: too many failing attemps"
    else
      let dir = Printf.sprintf "%s/%s%s" dir pat (rand_digits ()) in
      try (Unix.mkdir dir mode; dir) with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
      | Unix.Unix_error (Unix.EINTR, _, _)  -> loop count
      | Unix.Unix_error (e, _, _)           ->
          raise_err ("mk_temp_dir: " ^ (Unix.error_message e))
  in
  loop 1000

let make_n_enter dirname_pattern =
  let rundir = mk_temp_dir dirname_pattern in
  Unix.chdir rundir;
  rundir
