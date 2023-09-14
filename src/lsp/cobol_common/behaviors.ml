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

(** Type tags, useful to name and determine component behaviors *)
(* This may not really belong to `Cobol_common` as it's not specific to COBOL.
   Still, only `Srcloc` really satisfies that property; we should probably end
   up with an `Cobol_srcloc` library, and rename `Cobol_common` into something
   like `Superbol_utils`. *)

type amnesic = ForgetAll
type eidetic = RememberAll
