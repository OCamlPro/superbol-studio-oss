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

open Ez_file.V1

let with_in_bin file_name ~f =
  let ic = EzFile.open_in_bin file_name in
  match f ic with
  | v -> close_in ic; v
  | exception exn -> close_in ic; raise exn

(* --- *)

let version_tag_length = 40            (* use full commit hash when available *)

(** Internal version tag *)
let version_tag =
  let str = Option.value Version.commit_hash ~default:Version.version in
  if String.length str >= version_tag_length
  then String.sub str 0 version_tag_length
  else str ^ String.make (version_tag_length - String.length str) '_'

let write_cache_item ?(version_tag = version_tag) oc item =
  output_string oc version_tag;
  Marshal.to_channel oc item []

let read_cache_item ?(version_tag = version_tag) ic =
  let version_tag' = really_input_string ic version_tag_length in
  if version_tag' <> version_tag
  then Fmt.failwith "Bad version tag: got %s, expected %s\
                    " version_tag' version_tag;
  Marshal.from_channel ic

let cache_file_for ~item_name ~cache_dir =
  EzFile.concat cache_dir item_name

let save_named_item_cache ~cache_dir ~item_name ~write_item item =
  let cache_file = cache_file_for ~item_name ~cache_dir in
  EzFile.make_dir ~p:true @@ EzFile.dirname cache_file;
  EzFile.with_out_bin cache_file (write_item item)

let load_named_item_cache ~cache_dir ~item_name ~read_item =
  with_in_bin (cache_file_for ~item_name ~cache_dir) ~f:read_item
