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

    Diagnostics are held as the parser and the typechecker give them, and are
    only translated into LSP diagnostics when they are reported: their location
    then depends on the server's view of the project. *)

type t = private
  {
    copybook: bool;            (** copybooks are not analyzed, and hold none *)
    parsing_diags: Cobol_parser.Diagnostics.ALL.t;
    typeck_diags: Cobol_typeck.Diagnostics.t;
  }

(** [of_analysis a] gathers the diagnostics of [a]. *)
val of_analysis: Superbol_project.Analysis.for_lsp -> t

(** [diagnostics t] gathers all diagnostics held in [t]. *)
val diagnostics: t -> Cobol_common.Diagnostics.Set.t

(** {1 Caching} *)

(** [slot keys] is the slot in which the diagnostics of the document [keys] are
    recorded.  It is distinct from every other slot of the document, as
    diagnostics are stored at times of their own. *)
val slot: Superbol_project.Analysis.keys -> Superbol_cache.Types.key

(** [store cache ~keys ~label diags] records [diags] in [cache], for as long as
    the analysis for the LSP under [Analysis.key_for_lsp keys] holds, and
    supersedes the diagnostics of previous versions of the document. *)
val store
  : Superbol_cache.Cache.t
  -> keys: Superbol_project.Analysis.keys
  -> label: string
  -> t
  -> unit

(** [lookup cache ~keys ~resolve] retrieves the diagnostics of the document
    [keys] are for from [cache]. *)
val lookup
  : Superbol_cache.Cache.t
  -> keys: Superbol_project.Analysis.keys
  -> resolve: (Superbol_cache.Types.witness -> bool)
  -> t option

(** [mem cache ~keys] indicates whether [cache] holds diagnostics for the
    document [keys], without checking any of their witnesses. *)
val mem: Superbol_cache.Cache.t -> keys: Superbol_project.Analysis.keys -> bool

(** [store_for_analysis cache ~keys ~label ?fresh analysis] stores the
    diagnostics of [analysis] in [cache], unless the cache already holds
    diagnostics for the document.  [fresh] (defaults to [false]) indicates
    [analysis] was just computed, in which case the cache is not checked
    first. *)
val store_for_analysis
  : Superbol_cache.Cache.t
  -> keys: Superbol_project.Analysis.keys
  -> label: string
  -> ?fresh: bool
  -> Superbol_project.Analysis.for_lsp
  -> unit
