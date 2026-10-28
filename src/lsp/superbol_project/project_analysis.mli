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

(** Everything an analysis of a document yields, in a form that holds no
    closure, and can therefore be cached.  On a copybook, nothing is analyzed:
    every field but [copybook] is empty. *)
type t =
  {
    copybook: bool;
    pplog: Cobol_preproc.Trace.log;
    tokens: Cobol_parser.Outputs.tokens_with_locs;
    comments: Cobol_preproc.Text.comments StringMap.t;
    ignored: Cobol_common.Srcloc.lexloc list;
    checked: Cobol_typeck.Outputs.t option;
    parsing_diags: Cobol_parser.Diagnostics.ALL.t;
    typeck_diags: Cobol_typeck.Diagnostics.t;
  }

(** [of_artifacts ~checked ~parsing_diags ~typeck_diags ~parsing_artifacts]
    gathers the outputs of an analysis of a document that is not a copybook.
    Forces the tokens (which are [lazy] in
    {!Cobol_parser.Outputs.artifacts}). *)
val of_artifacts
  : checked: Cobol_typeck.Outputs.t option
  -> parsing_diags: Cobol_parser.Diagnostics.ALL.t
  -> typeck_diags: Cobol_typeck.Diagnostics.t
  -> parsing_artifacts:Cobol_parser.Outputs.artifacts
  -> t

(** [artifacts t] rebuilds the parsing artifacts held in [t]. *)
val artifacts: t -> Cobol_parser.Outputs.artifacts

(** Result for a copybook. *)
val copybook: t

(** [diagnostics t] gathers all diagnostics held in [t]. *)
val diagnostics: t -> Cobol_common.Diagnostics.Set.t

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

(** {1 Caching} *)

(** [inputs ?overrides ~filename ~contents project] are the inputs from which
    the key of an analysis of [filename] is built: they cover everything the
    analysis depends upon, apart from what it observes of the outside world
    (copybooks, environment variables...), which is recorded as witnesses
    instead.  [None] if they cannot be computed (for instance, if one input
    contains a closure).

    Front-ends may build keys of their own from these inputs, to cache data that
    depends on the analysis. *)
val inputs
  : ?overrides: overrides
  -> filename: string
  -> contents: string
  -> Project.t
  -> Superbol_cache.Types.digest list option

(** [key ?overrides ~filename ~contents project] is the key under which an
    analysis of [filename] is cached; see {!inputs}. *)
val key
  : ?overrides: overrides
  -> filename: string
  -> contents: string
  -> Project.t
  -> Superbol_cache.Types.key option

(** [store cache ~key ~label ~witnesses t] records [t] in [cache], for as long
    as [witnesses] hold. *)
val store
  : Superbol_cache.Cache.t
  -> key: Superbol_cache.Types.key
  -> label: string
  -> witnesses: Superbol_cache.Types.witness list
  -> t
  -> (unit, Superbol_cache.Types.error) result

(** [lookup cache ~key ~resolve] retrieves an analysis from [cache]; see
    {!Superbol_cache.Cache.lookup}. *)
val lookup
  : Superbol_cache.Cache.t
  -> key: Superbol_cache.Types.key
  -> resolve: (Superbol_cache.Types.witness -> bool)
  -> t option

(** [cached_analysis cache ~platform ?overrides ~filename ~contents project]
    looks the analysis of [filename] up in [cache], answering witnesses from
    [platform]; on a miss, analyzes the document, recording what [platform] is
    asked, and stores the result.  The Boolean indicates a hit. *)
val cached_analysis
  : Superbol_cache.Cache.t
  -> platform: Cobol_common.Platform.TYPES.platform
  -> ?overrides: overrides
  -> filename: string
  -> contents: string
  -> Project.t
  -> t * bool
