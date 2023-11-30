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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module DIAGS = Cobol_common.Diagnostics

type error =
  | Invalid_picture_symbol of string with_loc

let error_loc = function
  | Invalid_picture_symbol { loc; _ } ->
      Some loc

let pp_error ppf = function
  | Invalid_picture_symbol s ->
      Pretty.print ppf "%s@ is@ not@ a@ valid@ PICTURE@ symbol." ~&s
