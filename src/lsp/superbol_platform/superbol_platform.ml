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

open Cobol_common.Platform.TYPES

let () =                     (* force use of `/` instead of `\` in file names *)
  Ez_file.Slashifier.enable ()

let peek_channel_prefix ic ~len =
  let buff = Buffer.create len in
  (try Buffer.add_channel buff ic len with End_of_file -> ());
  Stdlib.seek_in ic 0;                           (* FIXME: may break on pipes *)
  Buffer.contents buff

let stat_file ?(follow = true) path =
  match (if follow then Unix.stat else Unix.lstat) path with
  | { st_kind; st_mtime; st_size; _ } ->
      Some { file_kind = (match st_kind with
          | S_REG -> Regular_file
          | S_DIR -> Directory
          | S_LNK -> Symbolic_link
          | _ -> Other_file);
             file_mtime = st_mtime;
             file_size = st_size }
  | exception Unix.Unix_error _ ->
      None

let record =
  {
    verbosity = 0;
    eprintf = Printf.eprintf;
    error = Pretty.error;
    getcwd = (fun () -> Ez_file.V1.Slashifier.slashify @@ Sys.getcwd ());
    now = Unix.gettimeofday;
    read_text_file = (fun file -> Ez_file.V1.EzFile.read_text_file file);
    list_dir = begin fun dir ->
      try Array.to_list (Sys.readdir dir) with Sys_error _ -> []
    end;
    stat_file;
    with_stdin = (fun ~f -> f Stdlib.stdin);
    peek_channel_prefix;
    mk_temp_dir = Tempdir.create;
    remove_dir = (fun ?all dir -> Ez_file.V1.EzFile.remove_dir ?all dir);
    autodetect_format = Heuristics.autodetect_format;
    is_source_file = Heuristics.is_source_file;
    is_hidden_file = Heuristics.is_hidden_file;
    find_lib = Copybook_finder.find_lib;
    getenv_opt = (fun variable -> Sys.getenv_opt variable);
  }

module Heuristics = Heuristics
module Tempdir = Tempdir
module Cache_store = Cache_store
