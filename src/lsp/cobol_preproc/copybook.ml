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

open Ez_file.V1

type lib_not_found_info =
  {
    libname: string;
    libpath: string list;
  }

(** Filename extensions that we should treat as copybooks and not main
    programs. *)
let copybook_extensions =  (* this must be a subset of {!libfile_extensions}. *)
  [".CPY"; ".cpy"; ".cbx"]

let libfile_extensions =
  [".CPY"; ".CBL"; ".COB"; ".cpy"; ".cbl"; ".cob"; ""; ".cbx"]

let find_lib ~libpath libname : _ result =
  let rec try_file base = function
    | [] ->
        Error { libname = base; libpath }
    | suff :: tl ->
        try Ok (EzFile.find_in_path libpath (base ^ suff)) with
        | Not_found -> try_file base tl
  in
  match libname with
  | `Word, w ->
      begin match try_file w libfile_extensions with
        | Ok lib -> Ok lib
        | Error _ -> match try_file (String.lowercase_ascii w) libfile_extensions with
          | Ok lib -> Ok lib
          | Error err -> Error {err with libname = w; }
      end
  | `Alphanum, w -> try_file w [""]

let lib_not_found_error k { libname; libpath } =
  (* TODO: `note addendum about search path *)
  Pretty.delayed_to k
    "@[Library@ `%s'@ not@ found@ in@ search@ path@ (search@ path:@ @[%a@])@]"
    libname Pretty.path libpath
