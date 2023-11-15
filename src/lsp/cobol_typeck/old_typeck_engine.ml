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

(** Type-checking and validation of COBOL compilation groups *)

module Cobol_data = Cobol_data.OLD
module Prog_builder = Old_prog_builder

module DIAGS = Cobol_common.Diagnostics
module CU = Cobol_data.Compilation_unit
module CUs = CU.SET

let analyze_compilation_group
    (type m) : ?config: _ -> m Cobol_parser.Outputs.parsed_compilation_group -> _ =
  fun ?(config = Cobol_config.default) ->
  function
  | Only None | WithArtifacts (None, _) ->
      DIAGS.result (Cobol_data.Compilation_unit.SET.empty, None)
  | Only Some cg | WithArtifacts (Some cg, _) ->
      match Prog_builder.compilation_group config cg with
      | { diags; _ } when DIAGS.Set.has_errors diags ->
          DIAGS.result ~diags (CUs.empty, Some cg)
      | { diags; result } ->
          DIAGS.result ~diags (result, Some cg)
