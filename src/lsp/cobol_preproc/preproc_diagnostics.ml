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

module DIAGS =  Cobol_common.Diagnostics

type error =
  | Malformed_or_unknown_compiler_directive of srcloc
  | Unknown_source_format of string * srcloc
  | Forbidden_change_of_source_format of srcloc

let error = function
  | Malformed_or_unknown_compiler_directive loc ->
      DIAGS.One.error ~loc "Malformed@ or@ unknown@ compiler@ directive"
  | Unknown_source_format (f, loc) ->
      DIAGS.One.error ~loc "Unknown@ source@ format@ `%s'" f
  | Forbidden_change_of_source_format loc ->
      DIAGS.One.error ~loc "Forbidden@ change@ of@ source@ format"
