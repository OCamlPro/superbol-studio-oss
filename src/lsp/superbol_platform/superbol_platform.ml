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

let peek_channel_prefix ic ~len =
  let buff = Buffer.create len in
  (try Buffer.add_channel buff ic len with End_of_file -> ());
  Stdlib.seek_in ic 0;                           (* FIXME: may break on pipes *)
  Buffer.contents buff

let record =
  {
    verbosity = 0;
    eprintf = Printf.eprintf;
    error = Pretty.error;
    read_file = (fun file -> Ez_file.V1.EzFile.read_file file);
    peek_channel_prefix;
    mk_temp_dir = Tempdir.create;
    remove_dir = (fun ?all dir -> Ez_file.V1.EzFile.remove_dir ?all dir);
    autodetect_format = Heuristics.autodetect_format;
    find_lib = Copybook_finder.find_lib;
    getenv_opt = (fun variable -> Sys.getenv_opt variable);
  }

module Tempdir = Tempdir
module Caching = Caching
