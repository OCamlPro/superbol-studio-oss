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


open Types

(* Windows never distinguishes case; elsewhere the default assumes it does, and
   a concrete store corrects it once it has probed the volume it sits on. *)
let insensitive = ref (match Sys.os_type with
  | "Win32" | "Cygwin" -> true
  | _ -> false)

let set_case_insensitive b = insensitive := b
let case_insensitive () = !insensitive

let canonical_path name = Canon.path ~case_insensitive:!insensitive name

let file ~name ~contents =
  File { name = canonical_path name; digest = Digest.string contents }

(* Copybook lookups always match names regardless of case, whatever the host
   does, so extensions are compared in lowercase. *)
let ext_of entry = String.lowercase_ascii (Filename.extension entry)

let dir ?exts ~name ~entries () =
  let exts = Option.map (List.sort_uniq String.compare) exts in
  let entries = match exts with
    | None -> entries
    | Some exts -> List.filter (fun e -> List.mem (ext_of e) exts) entries
  in
  Dir { name = canonical_path name; exts;
        digest = Digest.string
            (Canon.listing ~case_insensitive:!insensitive entries) }

let env ~var ~value = Env { var; value }

let sub key = Sub { key }
