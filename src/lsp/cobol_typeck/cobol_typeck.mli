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

module DIAGS = Cobol_common.Diagnostics
module StrMap = Cobol_common.Basics.StrMap
module Visitor = Cobol_common.Visitor
module PTree_visitor = Cobol_parser.PTree_visitor
module CUs = Cobol_data.Compilation_unit.SET

val analyze_compilation_group
  : ?config:(module Cobol_config.T)
  -> _ Cobol_parser.parsed_compilation_group
  -> (CUs.t * Cobol_parser.PTree.compilation_group * DIAGS.diagnostics,
      DIAGS.diagnostics)
    result

module Make
    (Config: Cobol_config.T)                      (* for dialect-based checks *)
    (Diags: DIAGS.STATEFUL) : sig
  val try_making_env_of_compilation_unit:
    Cobol_parser.PTree.compilation_unit Cobol_common.Srcloc.TYPES.with_loc ->
      Cobol_data.PROG_ENV.t option
end
