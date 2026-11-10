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

(** Semantic tokens of documents, cached apart from the analysis for the LSP
    they are computed from.

    Unlike other data derived from an analysis, they are computed along with
    it, as they need the tokens of the document, which analyses for the LSP do
    not hold (see {!Project_analysis.for_lsp}). *)

(** Semantic tokens of a document, in the encoding of the LSP (relative
    positions, five integers per token).  They only cover the document itself,
    and not its copybooks. *)
type t = int array

(** {1 Caching} *)

(** [key keys] is the key under which the semantic tokens of the document
    [keys] are cached. *)
val key: Project_analysis.keys -> Superbol_cache.Types.key

(** [slot keys] is the slot in which the semantic tokens of the document [keys]
    are recorded; it is theirs only, as they are stored at times of their
    own. *)
val slot: Project_analysis.keys -> Superbol_cache.Types.key

(** [store cache ~keys ~label s] records the semantic tokens [s] in [cache],
    for as long as the analysis for the LSP they were computed from (under
    [Project_analysis.key_for_lsp keys]) holds, and supersedes the semantic
    tokens of previous versions of the document.

    Analyses for the LSP may be stored without semantic tokens: front-ends that
    need them should consider such an analysis as missing. *)
val store
  : Superbol_cache.Cache.t
  -> keys: Project_analysis.keys
  -> label: string
  -> t
  -> unit

(** [lookup cache ~keys ~resolve] retrieves the semantic tokens of the document
    [keys] from [cache]. *)
val lookup
  : Superbol_cache.Cache.t
  -> keys: Project_analysis.keys
  -> resolve: (Superbol_cache.Types.witness -> bool)
  -> t option

(** [cached_analysis_for_lsp cache ~platform ?overrides ~semantic_tokens ~keys
    ~filename ~contents project] is like
    {!Project_analysis.cached_analysis_for_lsp}, except that, on a miss, it also
    stores the semantic tokens computed with [semantic_tokens] (unless the
    document is a copybook).  The Boolean indicates a hit; semantic tokens are
    not looked up, so a hit does not entail they are held.

    [semantic_tokens] computes the semantic tokens of the document [filename]
    from its parsing artifacts and typechecking outputs.  The LSP provides it;
    front-ends that fill the cache for the LSP should pass the very same
    function, as it takes no part in cache keys. *)
val cached_analysis_for_lsp
  : Superbol_cache.Cache.t
  -> platform: Cobol_common.Platform.TYPES.platform
  -> ?overrides: Project_analysis.overrides
  -> semantic_tokens: (filename: string ->
                       Cobol_parser.Outputs.artifacts ->
                       Cobol_typeck.Outputs.t ->
                       t)
  -> keys: Project_analysis.keys
  -> filename: string
  -> contents: string
  -> Project.t
  -> Project_analysis.for_lsp * bool
