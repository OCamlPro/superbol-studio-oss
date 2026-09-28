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

let dir ~name ~entries =
  Dir { name = canonical_path name;
        digest = Digest.string
            (Canon.listing ~case_insensitive:!insensitive entries) }

let env ~var ~value = Env { var; value }

let sub key = Sub { key }
