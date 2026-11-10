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

(** [path_of_uri uri] returns the file path of [uri], slashified: on Windows,
    {!Lsp.Uri.to_path} gives paths with backslashes, whereas every path
    handled by the server (and the CLI) uses slashes as directory separators,
    so that paths can be compared and used as cache keys. *)
let path_of_uri uri =
  Slashifier.slashify @@ Lsp.Uri.to_path uri

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
