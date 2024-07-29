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

type fileloc = [ `Word of string | `Alphanum of string ]

type lookup_info =
  {
    libname: string;
    libpath: string list;
    libexts: string list;
  }

(* --- *)

val copybook_extensions: string list

val pp_lookup_error: lookup_info Pretty.printer

(** [find_lib ~libpath ?exts ?fromfile ?libname txtname] attempts to locate a file
    containing the copybook [txtname], which is a file named [txtname], possibly
    appended with an extension from {[".CPY"; ".CBL"; ".COB"; ".CBX"; ".cpy";
    ".cbl"; ".cob"; ".cbx"]} (considered in order), {e unless} [txtname] is
    given as an alphanumeric literal ({i e.g, [txtname = `Alphanum filname] ---
    in which case no extension is assumed).

    If [libname] is not provided, then the file is searched within the
    directories listed in [libpath] (considered in order).  Otherwise, a single
    directory [libname] is considered; if [libname] is a relative path, it is
    interpreted relative to the directory that contains [fromfile].

    Lookup is performed in a case-insensitive way. Every directory in the path
    is considered in order, and all extensions are tried for a given directory
    of the path in the provided order.
 *)
val find_lib
  : libpath:string list
  -> ?exts:string list
  -> ?fromfile:string
  -> ?libname:fileloc
  -> fileloc
  -> (string, lookup_info) result
