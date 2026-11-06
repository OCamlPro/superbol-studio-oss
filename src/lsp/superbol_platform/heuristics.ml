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

open Cobol_common.Platform.TYPES

let autodetect_format ~source_contents ~filename:_ : source_format_id =
  (* Note: for now we ignore the filename extension. *)
  Cobol_preproc.Src_format.guess_from ~contents_prefix:source_contents

(** Filename extensions of files that may hold COBOL programs, compared
    regardless of case.  Some of them may also be those of copybooks (see
    {!Cobol_common.Copybook.copybook_extensions}), but none is a copybook-only
    one (see {!Cobol_common.Copybook.copybookonly_extensions}). *)
let source_extensions = ["cbl"; "cob"; "cobol"]

let is_source_file ~filename =
  match Filename.extension filename with
  | "" -> false
  | ext ->
      let ext = String.(lowercase_ascii @@ sub ext 1 (length ext - 1)) in
      List.mem ext source_extensions

(** Files and directories whose name starts with a dot are hidden, as is
    customary on Unix-like systems (and for many tools on Windows as well, e.g.
    [.git] or [.vscode]).  The hidden attribute of Windows file systems is not
    taken into account. *)
let is_hidden_file ~filename =
  let basename = Filename.basename filename in
  basename <> "" && basename.[0] = '.' &&
  basename <> "." && basename <> ".."
