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

(** Symbols of documents, for answering [workspace/symbol] requests.

    Symbols of a document are a flat table that holds no closure, cached apart
    from the (much larger) analysis they are computed from, so that those of
    documents that are not open can be retrieved without restoring their
    analysis. *)

(** {1 Symbol tables} *)

type symbol =
  {
    name: string;        (** as written in the source *)
    key: string;         (** [name] in upper case, with ['_'] mapped to ['-'] *)
    kind: Lsp.Types.SymbolKind.t;
    container: string option;     (** name of the enclosing unit or section *)
    range: Lsp.Types.Range.t;
  }

(** Symbols of one document. *)
type t = symbol array

(** [of_ptree ~filename ptree] collects the symbols of the document [filename]:
    programs, functions, classes, interfaces, methods, sections, paragraphs, and
    named data items (including constants, condition names and renames).
    Symbols that stem from copybooks are located at the [COPY] directive that
    brings them in. *)
val of_ptree: filename: string -> Cobol_ptree.compilation_group -> t

(** {1 Caching} *)

(** [slot keys] is the slot in which the symbols of the document [keys] are
    recorded.  It is distinct from every other slot of the document, as symbols
    are stored at times of their own. *)
val slot: Superbol_project.Analysis.keys -> Superbol_cache.Types.key

(** [store cache ~keys ~label symbols] records [symbols] in [cache], for as long
    as the analysis for the LSP under [Analysis.key_for_lsp keys] holds, and
    supersedes the symbols of previous versions of the document. *)
val store
  : Superbol_cache.Cache.t
  -> keys: Superbol_project.Analysis.keys
  -> label: string
  -> t
  -> unit

(** [lookup cache ~keys ~resolve] retrieves the symbols of the document [keys]
    are for from [cache]. *)
val lookup
  : Superbol_cache.Cache.t
  -> keys: Superbol_project.Analysis.keys
  -> resolve: (Superbol_cache.Types.witness -> bool)
  -> t option

(** [mem cache ~keys] indicates whether [cache] holds symbols for the document
    [keys], without checking any of their witnesses. *)
val mem: Superbol_cache.Cache.t -> keys: Superbol_project.Analysis.keys -> bool

(** [store_for_analysis cache ~keys ~filename ~label ?fresh analysis] stores the
    symbols of [analysis] (for the document [filename]) in [cache] unless
    [analysis] is that of a copybook, holds no typechecking outputs, or the
    cache already holds symbols for the document.  [fresh] (defaults to [false])
    indicates [analysis] was just computed, in which case the cache is not
    checked first. *)
val store_for_analysis
  : Superbol_cache.Cache.t
  -> keys: Superbol_project.Analysis.keys
  -> filename: string
  -> label: string
  -> ?fresh: bool
  -> Superbol_project.Analysis.for_lsp
  -> unit

(** {1 Searching} *)

(** [score ~query symbol] is [Some s] whenever [query] matches [symbol.key],
    where [s] is higher for better matches, and [None] otherwise.  [query] must
    be normalized with {!normalize_query}.

    Every character of the query must appear in the symbol's name, in order
    (case being ignored, and ['_'] matching ['-']); matches at the beginning of
    the name or of one of its hyphen-separated words, and consecutive matches,
    weigh more.  The empty query matches everything. *)
val score: query: string -> symbol -> int option

(** [normalize_query q] maps [q] to the form {!score} expects. *)
val normalize_query: string -> string

(** [search ~query ~limit docs] gives the (at most [limit]) symbols of [docs]
    that best match [query], from the best to the worst match. *)
val search
  : query: string
  -> limit: int
  -> (Lsp.Uri.t * t) Seq.t
  -> Lsp.Types.SymbolInformation.t list
