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

(* The following line should be specified with an offset of +2 *)
#21 "_build/default/src/tramabol/ezlibcob/v1.ml"
