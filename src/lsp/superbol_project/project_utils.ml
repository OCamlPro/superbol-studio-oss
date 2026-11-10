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

(** [relative_path ~filename absolute_path] returns the relative path of
    [filename] {i w.r.t} [absolute_path] if [absolute_path] is a prefix of
    [filename], and raises {!Invalid_argument} otherwise.

    [absolute_path] is expected to be slashified already; [filename] is
    slashified here, as on Windows it may be given with backslashes (e.g. when
    obtained from a URI) whereas the expected directory separator is a
    slash. *)
let relative_path ~filename:path absolute_path =
  let path = Slashifier.slashify path in
  match EzString.chop_prefix ~prefix:absolute_path path with
  | Some "" -> "."
  | Some path ->
      if path.[0] = Slashifier.get_dir_separator ()
      then EzString.after path 0
      else path
  | None -> Fmt.invalid_arg "%s is not contained within %s" path absolute_path
