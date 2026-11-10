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

module ANALYSIS = Project_analysis
module CACHE = Superbol_cache.Cache
module CACHING = Superbol_cache.Caching
module WITNESS = Superbol_cache.Witness

type t = int array

(* --- Caching --- *)

(* Semantic tokens depend on the analysis for the LSP they are computed from,
   so that any miss on the latter entails a miss on the former. *)
let key keys =
  CACHE.shallow_key ~label:"semantic-tokens" ~inputs:(ANALYSIS.inputs keys)

let slot keys =
  ANALYSIS.slot ~label:"semantic-tokens" keys

let store cache ~keys ~label (s: t) =
  let key = key keys in
  match
    CACHE.store cache ~key ~label
      ~witnesses:[WITNESS.sub (ANALYSIS.key_for_lsp keys)]
      ~write_item:CACHING.write_item s
  with
  | Ok () ->
      (* Failing to supersede only leaves the previous version behind. *)
      ignore @@ CACHE.supersede cache ~slot:(slot keys) [key]
  | Error _ ->
      ()

let lookup cache ~keys ~resolve : t option =
  match
    CACHE.lookup cache ~key:(key keys) ~resolve ~read_item:CACHING.read_item
  with
  | Ok result -> result
  | Error _ -> None

let cached_analysis_for_lsp cache ~platform ?overrides ~semantic_tokens ~keys
    ~filename ~contents project =
  let analysis, hit, semtoks =
    ANALYSIS.cached_analysis_for_lsp cache ~platform ?overrides ~keys
      ~with_artifacts:semantic_tokens ~filename ~contents project
  in
  Option.iter begin
    store cache ~keys ~label:(Project.relative_path_for ~filename project)
  end semtoks;
  analysis, hit
