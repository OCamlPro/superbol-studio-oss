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

val copybook_extensions: string list

type lib_not_found_info =
  {
    libname: string;
    libpath: string list;
  }

val lib_not_found_error: (Pretty.delayed -> 'a) -> lib_not_found_info -> 'a

val find_lib
  : libpath:string list
  -> [< `Alphanum | `Word ] * string
  -> (string, lib_not_found_info) result
