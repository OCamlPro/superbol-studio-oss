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

open Cobol_common.Platform.TYPES

let record =
  {
    verbosity = 0;
    eprintf = Printf.eprintf;
    error = Pretty.error;
    read_file = (fun file -> Ez_file.V1.EzFile.read_file file);
    autodetect_format = Heuristics.autodetect_format;
    find_lib = Copybook_finder.find_lib;
    getenv_opt = (fun variable -> Sys.getenv_opt variable);
  }
