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

let record =
  {
    verbosity = 0;
    tab_stop = 8;
    eprintf = Printf.eprintf;
    error = Pretty.error;
    read_file = (fun file -> Ez_file.V1.EzFile.read_file file);
    mk_temp_dir = Tempdir.create;
    remove_dir = (fun ?all dir -> Ez_file.V1.EzFile.remove_dir ?all dir);
    autodetect_format = Heuristics.autodetect_format;
    find_lib = Copybook_finder.find_lib;
    getenv_opt = (fun variable -> Sys.getenv_opt variable);
  }

module Tempdir = Tempdir
module Caching = Caching
