#2 "src/tramabol/ezlibcob/v1_header.ml"
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

external resolve_cobol_and_call: string -> int =
  "ml_cob_resolve_cobol_and_call"

external cob_get_exception_code: unit -> int =
  "ml_cob_get_exception_code" [@@noalloc]
external cob_reset_exception_code: unit -> unit =
  "ml_cob_reset_exception_code" [@@noalloc]

(* The following line should be specified with an offset of +2 *)
#26 "_build/default/src/tramabol/ezlibcob/v1.ml"
