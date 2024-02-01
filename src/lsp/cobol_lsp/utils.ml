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

(** [relative_path uri absolute_path] returns the relative path of [uri] {i
    w.r.t} [absolute_path] if [absolute_path] is a prefix of [uri], and raises
    {!Invalid_argument} otherwise. *)
let relative_path ~uri absolute_path =
  let path = Lsp.Uri.to_path uri in
  match EzString.chop_prefix ~prefix:absolute_path path with
  | Some "" -> "."
  | Some path when path.[0] = FileOS.dir_separator -> EzString.after path 0
  | Some path -> path
  | None -> Fmt.invalid_arg "%s is not contained within %s" path absolute_path

let is_file path = EzFile.exists path && not (EzFile.is_directory path)

let read_from path f =
  let ic = open_in_bin path in
  match f ic with
  | v -> close_in ic; v
  | exception exn -> close_in ic; raise exn

let write_to path f =
  let oc = open_out_bin path in
  try
    f oc;
    close_out oc;
    Pretty.error "Wrote %s@." path
  with e ->
    Pretty.error "Error when writing %s (removing): %a@." path Fmt.exn e;
    EzFile.remove path;
    raise e
