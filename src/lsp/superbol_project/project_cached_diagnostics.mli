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


(** Diagnostics of documents, cached on their own so that they can be reported
    for documents that are not open, without restoring their (much larger)
    analysis for the LSP.

    Diagnostics are held as the parser and the typechecker give them, and only
    translated when reported. *)

type t = private
  {
    copybook: bool;            (** copybooks are not analyzed, and hold none *)
    parsing_diags: Cobol_parser.Diagnostics.ALL.t;
    typeck_diags: Cobol_typeck.Diagnostics.t;
  }

(** [of_analysis a] gathers the diagnostics of [a]. *)
val of_analysis: Project_analysis.for_lsp -> t

(** [diagnostics t] gathers all diagnostics held in [t]. *)
val diagnostics: t -> Cobol_common.Diagnostics.Set.t

(** {1 Caching} *)

(** [slot keys] is the slot in which the diagnostics of the document [keys] are
    recorded; it is theirs only, as they are stored at times of their own. *)
val slot: Project_analysis.keys -> Superbol_cache.Types.key

(** [store cache ~keys ~label diags] records [diags] in [cache], for as long as
    the analysis for the LSP under [Project_analysis.key_for_lsp keys]
    holds, and supersedes the diagnostics of previous versions of the
    document. *)
val store
  : Superbol_cache.Cache.t
  -> keys: Project_analysis.keys
  -> label: string
  -> t
  -> unit

(** [lookup cache ~keys ~resolve] retrieves the diagnostics of the document
    [keys] from [cache]. *)
val lookup
  : Superbol_cache.Cache.t
  -> keys: Project_analysis.keys
  -> resolve: (Superbol_cache.Types.witness -> bool)
  -> t option

(** [mem cache ~keys] indicates whether [cache] holds diagnostics for the
    document [keys], without checking their witnesses. *)
val mem: Superbol_cache.Cache.t -> keys: Project_analysis.keys -> bool

(** [store_for_analysis cache ~keys ~label ?fresh analysis] stores the
    diagnostics of [analysis] in [cache], unless it already holds some for the
    document.  [fresh] (defaults to [false]) indicates [analysis] was just
    computed, so that [cache] need not be checked. *)
val store_for_analysis
  : Superbol_cache.Cache.t
  -> keys: Project_analysis.keys
  -> label: string
  -> ?fresh: bool
  -> Project_analysis.for_lsp
  -> unit
