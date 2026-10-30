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

(** Analysis of individual documents, independently of any front-end.

    This gathers one canonical set of preprocessing, parsing and typechecking
    options, so that the LSP and the CLI produce comparable results, and the
    means to cache what such an analysis yields.

    File names given to functions of this module are expected to be absolute:
    they take part in cache keys, and two spellings of one file would otherwise
    yield distinct entries. *)

open EzCompat                                                    (* StringMap *)

(** {1 Results} *)

(** What an analysis of a document yields, in a form that holds no closure,
    and can therefore be cached.  This is enough for front-ends that report
    diagnostics or work on typechecking outputs.  On a copybook, nothing is
    analyzed: every field but [copybook] is empty. *)
type t =
  {
    copybook: bool;
    checked: Cobol_typeck.Outputs.t option;
    parsing_diags: Cobol_parser.Diagnostics.ALL.t;
    typeck_diags: Cobol_typeck.Diagnostics.t;
  }

(** Result for a copybook. *)
val copybook: t

(** [diagnostics t] gathers all diagnostics held in [t]. *)
val diagnostics: t -> Cobol_common.Diagnostics.Set.t

(** {2 Results for the LSP} *)

(** What an analysis yields, along with the parsing artifacts that are needed
    to answer LSP requests (preprocessing log, tokens, comments, ignored
    regions).  These are more costly to compute and to cache. *)
type for_lsp =
  {
    lsp_copybook: bool;
    lsp_pplog: Cobol_preproc.Trace.log;
    lsp_tokens: Cobol_parser.Outputs.tokens_with_locs;
    lsp_comments: Cobol_preproc.Text.comments StringMap.t;
    lsp_ignored: Cobol_common.Srcloc.lexloc list;
    lsp_checked: Cobol_typeck.Outputs.t option;
    lsp_parsing_diags: Cobol_parser.Diagnostics.ALL.t;
    lsp_typeck_diags: Cobol_typeck.Diagnostics.t;
  }

(** [of_artifacts_for_lsp ~checked ~parsing_diags ~typeck_diags
    ~parsing_artifacts] gathers the outputs of an analysis of a document that
    is not a copybook.  Forces the tokens (which are [lazy] in
    {!Cobol_parser.Outputs.artifacts}). *)
val of_artifacts_for_lsp
  : checked: Cobol_typeck.Outputs.t option
  -> parsing_diags: Cobol_parser.Diagnostics.ALL.t
  -> typeck_diags: Cobol_typeck.Diagnostics.t
  -> parsing_artifacts:Cobol_parser.Outputs.artifacts
  -> for_lsp

(** [artifacts a] rebuilds the parsing artifacts held in [a]. *)
val artifacts: for_lsp -> Cobol_parser.Outputs.artifacts

(** Result for the LSP for a copybook. *)
val copybook_for_lsp: for_lsp

(** [of_lsp_artifacts a] drops the artifacts of [a]. *)
val of_lsp_artifacts: for_lsp -> t

(** {1 Options} *)

(** Departures from the configuration of a project, and from the canonical
    options of this module.  Every function below that accepts some overrides
    defaults to {!no_overrides}. *)
type overrides =
  {
    override_cobol_config: (Cobol_config.t * string) option;
    (** COBOL configuration to use instead of the project's, along with a
        description of where it comes from.  The configuration itself cannot be
        digested, so its description is what takes part in cache keys: it
        should identify the configuration uniquely. *)

    override_source_format: Cobol_config.source_format_spec option;
    (** Source format to use regardless of the project configuration. *)

    override_libpath: string list;
    (** Copybook directories to search after the project's.  Should be
        absolute. *)

    override_libexts: string list;
    (** Copybook filename extensions to consider in addition to the
        project's. *)

    override_env: Cobol_preproc.Env.t;
    (** Preprocessor definitions. *)

    override_recovery: Cobol_parser.Options.recovery option;
    override_show: [`Pending] list option;
  }

val no_overrides: overrides

(** [detect_copybook ~platform ?overrides ~filename ~contents project]
    indicates whether the document [filename], holding [contents], is a
    copybook in [project]; such documents are not analyzed.  See
    {!Project.detect_copybook}. *)
val detect_copybook
  : platform: Cobol_common.Platform.TYPES.platform
  -> ?overrides: overrides
  -> filename: string
  -> contents: string
  -> Project.t
  -> bool

(** [preproc_options ~platform ?overrides ~filename project] are the
    preprocessing options for the document [filename] in [project]. *)
val preproc_options
  : platform: Cobol_common.Platform.TYPES.platform
  -> ?overrides: overrides
  -> filename: string
  -> Project.t
  -> Cobol_preproc.Options.preproc_options

(** [parser_options ?overrides project] are the parsing options for any document
    of [project]. *)
val parser_options
  : ?overrides: overrides
  -> Project.t
  -> Cobol_parser.Options.parser_options

(** [typecheck ?overrides ~parsing_diags project parsed] typechecks [parsed];
    typechecking diagnostics are dropped whenever [parsing_diags] holds
    errors. *)
val typecheck
  : ?overrides: overrides
  -> parsing_diags: Cobol_parser.Diagnostics.ALL.t
  -> Project.t
  -> _ Cobol_parser.Outputs.parsed_compilation_group
  -> Cobol_typeck.Outputs.t * Cobol_typeck.Diagnostics.t

(** [analyze ~platform ?overrides ~filename ~contents project] analyzes the
    document [filename], holding [contents], in [project].  Copybooks are
    detected and left unanalyzed. *)
val analyze
  : platform: Cobol_common.Platform.TYPES.platform
  -> ?overrides: overrides
  -> filename: string
  -> contents: string
  -> Project.t
  -> t

(** [analyze_for_lsp ~platform ?overrides ~filename ~contents project] is like
    {!analyze}, except that parsing artifacts are collected as well. *)
val analyze_for_lsp
  : platform: Cobol_common.Platform.TYPES.platform
  -> ?overrides: overrides
  -> filename: string
  -> contents: string
  -> Project.t
  -> for_lsp

(** {1 Caching} *)

(** Digest of everything about a project and some overrides that bears on how
    its documents are analyzed.  It may be computed once, and shared among
    every document of the project that is analyzed with the same overrides. *)
type config_inputs

(** [config_inputs ?overrides project] are the inputs that all keys of
    analyses of documents of [project] with [overrides] share.  [None] if they
    cannot be computed (for instance, if one input contains a closure). *)
val config_inputs
  : ?overrides: overrides
  -> Project.t
  -> config_inputs option

(** Keys under which the analyses of a document are cached, along with the
    inputs they are built from: these cover everything the analysis depends
    upon, apart from what it observes of the outside world (copybooks,
    environment variables...), which is recorded as witnesses instead. *)
type keys

(** [document_keys config ~filename ~contents] are the keys of the document
    [filename], holding [contents], where [config] are the {!config_inputs} of
    its project and overrides. *)
val document_keys
  : config_inputs
  -> filename: string
  -> contents: string
  -> keys

(** [keys ?overrides ~filename ~contents project] combines {!config_inputs}
    and {!document_keys}. *)
val keys
  : ?overrides: overrides
  -> filename: string
  -> contents: string
  -> Project.t
  -> keys option

(** [inputs keys] are the digests [keys] are built from.  Front-ends may build
    keys of their own from these, to cache data that depends on the
    analysis. *)
val inputs: keys -> Superbol_cache.Types.digest list

(** [key keys] is the key under which an analysis is cached. *)
val key: keys -> Superbol_cache.Types.key

(** [key_for_lsp keys] is the key under which an analysis for the LSP is
    cached; both kinds of results are stored on their own: a hit on one does
    not require the other to be held. *)
val key_for_lsp: keys -> Superbol_cache.Types.key

(** [store_for_lsp cache ~key ~label ~witnesses a] records [a] in [cache], for
    as long as [witnesses] hold. *)
val store_for_lsp
  : Superbol_cache.Cache.t
  -> key: Superbol_cache.Types.key
  -> label: string
  -> witnesses: Superbol_cache.Types.witness list
  -> for_lsp
  -> (unit, Superbol_cache.Types.error) result

(** [lookup_for_lsp cache ~key ~resolve] retrieves an analysis for the LSP from
    [cache]; see {!Superbol_cache.Cache.lookup}. *)
val lookup_for_lsp
  : Superbol_cache.Cache.t
  -> key: Superbol_cache.Types.key
  -> resolve: (Superbol_cache.Types.witness -> bool)
  -> for_lsp option

(** In the functions below, [keys] must be the {!keys} of [filename] and
    [contents] in [project], with [overrides]. *)

(** [cached_analysis cache ~platform ?overrides ~keys ~filename ~contents
    project] is the analysis of [filename]: it is obtained from the analysis
    for the LSP held in [cache] if any, or else from a (normal) analysis,
    answering witnesses from [platform].  On a miss on both, analyzes the
    document with {!analyze}, recording what [platform] is asked, and stores the
    result (which the LSP does not reuse).  The Boolean indicates a hit. *)
val cached_analysis
  : Superbol_cache.Cache.t
  -> platform: Cobol_common.Platform.TYPES.platform
  -> ?overrides: overrides
  -> keys: keys
  -> filename: string
  -> contents: string
  -> Project.t
  -> t * bool

(** [cached_analysis_n_key cache ~platform ?overrides ~keys ~filename
    ~contents project] is like {!cached_analysis}, except that it also returns
    the key of the entry that holds the analysis: [key_for_lsp keys] if it
    was obtained from an analysis for the LSP, or else [key keys].  Entries
    derived from the analysis may witness it. *)
val cached_analysis_n_key
  : Superbol_cache.Cache.t
  -> platform: Cobol_common.Platform.TYPES.platform
  -> ?overrides: overrides
  -> keys: keys
  -> filename: string
  -> contents: string
  -> Project.t
  -> t * bool * Superbol_cache.Types.key

(** [cached_analysis_for_lsp cache ~platform ?overrides ~keys ~filename
    ~contents project] looks the analysis for the LSP of [filename] up in
    [cache], answering witnesses from [platform]; on a miss, analyzes the
    document, recording what [platform] is asked, and stores the result.  The
    Boolean indicates a hit. *)
val cached_analysis_for_lsp
  : Superbol_cache.Cache.t
  -> platform: Cobol_common.Platform.TYPES.platform
  -> ?overrides: overrides
  -> keys: keys
  -> filename: string
  -> contents: string
  -> Project.t
  -> for_lsp * bool
