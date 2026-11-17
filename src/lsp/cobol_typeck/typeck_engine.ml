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

let compilation_group (type m)
  : options:Typeck_config.options
    -> m Cobol_parser.Outputs.parsed_compilation_group
    -> Typeck_outputs.t Typeck_results.with_diags =
  fun ~options ->
  function
  | Only None | WithArtifacts (None, _) ->
      Typeck_results.simple_result Typeck_outputs.none
  | Only Some cg | WithArtifacts (Some cg, _) ->
      Typeck_units.of_compilation_group ~options cg
