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

let platform = Superbol_platform.record

(** Note: won't show detailed source locations as the openned file is not
    actually on disk (that may be fixed later with a custom internal file
    store). *)

let pp_srcloc = Cobol_common.Srcloc.pp_srcloc_without_caret
