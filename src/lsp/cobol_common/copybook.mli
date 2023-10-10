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

type lookup_info =
  {
    libname: string;
    libpath: string list;
  }

val pp_lookup_error: lookup_info Pretty.printer

val find_lib
  : libpath:string list
  -> [< `Alphanum | `Word ] * string
  -> (string, lookup_info) result
