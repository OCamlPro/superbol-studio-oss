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

open EzCompat
open Ez_file.V1

type fileloc = [ `Word of string | `Alphanum of string ]

type lookup_info =
  {
    libname: string;
    libpath: string list;
  }

(** Filename extensions that we should treat as copybooks and not main
    programs. *)
let copybook_extensions =  (* this must be a subset of {!libfile_extensions}. *)
  [".CPY"; ".cpy";
   ".CBX"; ".cbx"]

let libfile_extensions =
  [".CPY"; ".CBL"; ".COB"; ".CBX";
   ".cpy"; ".cbl"; ".cob"; ".cbx"; ""]

type directory = {
  dir : string ;
  files : string StringMap.t ; (* key is basename in lowercase *)
}

let find_lib ~libpath ?(exts=libfile_extensions) ?fromfile ?libname textname : _ result =
  let libpath = match libname, fromfile with
    | None, _ ->
        libpath
    | Some (`Word d | `Alphanum d), Some file
      when EzFile.is_relative d ->
        EzFile.[concat (dirname file) d]
    | Some (`Word d | `Alphanum d), _ ->
        [d]
  in
  let libpath_files = List.map (fun dir ->
      { dir ;
        files =
          let files = try Sys.readdir dir with _ -> [||] in
          let map = ref StringMap.empty in
          Array.iter (fun file ->
              let base = String.lowercase_ascii file in
              map := StringMap.add base file !map
            ) files ;
          !map
      }
    ) libpath
  in
  let exts = List.map String.lowercase_ascii exts in
  let try_file libname exts =
    let base = String.lowercase_ascii libname in
    let rec iter_path path =
      match path with
      | [] -> Error { libname; libpath }
      | d :: path ->
        iter_exts d path exts

    and iter_exts d path exts =
      match exts with
      | [] -> iter_path path
      | ext :: exts ->
        let file = base ^ ext in
        match StringMap.find file d.files with
        | exception Not_found -> iter_exts d path exts
        | file -> Ok ( Filename.concat d.dir file )
    in
    iter_path libpath_files
  in
  match textname with
  | `Alphanum w ->                       (* assume no more filename extension  *)
      try_file w [""]
  | `Word w ->
      match try_file w exts with
      | Ok lib -> Ok lib
      | Error err -> Error { err with libname = w }

let pp_lookup_error ppf { libname; libpath } =
  (* TODO: `note addendum about search path *)
  Pretty.print ppf
    "@[Library@ `%s'@ not@ found@ in@ search@ path@ (search@ path:@ @[%a@])@]"
    libname Pretty.path libpath
