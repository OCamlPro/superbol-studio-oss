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

open Cobol_common.Copybook.TYPES

open EzCompat
open Ez_file.V1

(* --- *)

type directory =
  {
    dir: string;
    files: string StringMap.t;                (* key is basename in lowercase *)
  }

let find_lib ~lookup_config:({ lookup_path = libpath;
                               lookup_exts = libexts } as lookup_config)
    ?fromfile ?libname textname : (string, lookup_error) result =
  let libpath = match libname, fromfile with
    | None, _ ->
        libpath
    | Some (`Word d | `Alphanum d), Some file
      when EzFile.is_relative d ->
        EzFile.[concat (dirname file) d]
    | Some (`Word d | `Alphanum d), _ ->
        [d]
  in
  let libpath_files =
    List.map begin fun dir ->
      let files =
        let files = try Sys.readdir dir with Sys_error _ -> [||] in
        let map = ref StringMap.empty in
        Array.iter begin fun file ->
          map := StringMap.add (String.lowercase_ascii file) file !map
        end files;
        !map
      in
      { dir; files }
    end libpath
  in
  let try_file libname exts =
    let base = String.lowercase_ascii libname in
    let without_ext d =
      Ok (Filename.concat d.dir @@ StringMap.find base d.files)
    and with_ext d ext =
      Ok (Filename.concat d.dir @@ StringMap.find (base ^ "." ^ ext) d.files)
    in
    let rec iter_path path =
      match path with
      | [] -> Error { lookup_libname = libname;
                      lookup_config = { lookup_config with
                                        lookup_path = libpath } }
      | d :: path -> iter_exts d path exts
    and iter_exts d path exts =
      match exts with
      | [] -> (try without_ext d with Not_found -> iter_path path)
      | ext :: exts -> (try with_ext d ext with Not_found -> iter_exts d path exts)
    in
    iter_path libpath_files
  in
  (* Note: GnuCOBOL handles copybook names given as string literals or plain
     text-words in the same way (except for distinct case-folding, that we don't
     handle yet).

     TODO: to handle case folding, a copybook name that is given as a text-word
     should be put in uppercase unless [lookup_fold=Lower]. *)
  match textname with
  | `Alphanum w
  | `Word w ->
      match try_file w libexts with
      | Ok lib -> Ok lib
      | Error err -> Error { err with lookup_libname = w }
