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

module ANALYSIS = Superbol_project.Analysis
module CACHE = Superbol_cache.Cache
module CACHING = Superbol_cache.Caching
module WITNESS = Superbol_cache.Witness

type t =
  {
    copybook: bool;
    parsing_diags: Cobol_parser.Diagnostics.ALL.t;
    typeck_diags: Cobol_typeck.Diagnostics.t;
  }

let of_analysis ({ lsp_copybook = copybook;
                   lsp_parsing_diags = parsing_diags;
                   lsp_typeck_diags = typeck_diags; _ }: ANALYSIS.for_lsp) =
  { copybook; parsing_diags; typeck_diags }

let diagnostics { parsing_diags; typeck_diags; _ } =
  Cobol_common.Diagnostics.Set.union
    (Cobol_parser.Diagnostics.ALL.translate parsing_diags)
    (Cobol_typeck.Diagnostics.translate typeck_diags)

(* --- Caching --- *)

(* Diagnostics depend on the analysis for the LSP they are taken from, so that
   any miss on the latter entails a miss on the former. *)
let key keys =
  CACHE.shallow_key ~label:"diagnostics" ~inputs:(ANALYSIS.inputs keys)

let slot keys =
  ANALYSIS.slot ~label:"diagnostics" keys

let store cache ~keys ~label (diags: t) =
  let key = key keys in
  match
    CACHE.store cache ~key ~label
      ~witnesses:[WITNESS.sub (ANALYSIS.key_for_lsp keys)]
      ~write_item:CACHING.write_item diags
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

let mem cache ~keys =
  Option.is_some (lookup cache ~keys ~resolve:(fun _ -> true))

let store_for_analysis cache ~keys ~label ?(fresh = false) analysis =
  if fresh || not (mem cache ~keys) then
    store cache ~keys ~label (of_analysis analysis)
