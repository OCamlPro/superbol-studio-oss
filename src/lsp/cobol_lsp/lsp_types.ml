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

(** Gathers some types shared between modules of the LSP server. *)

open Lsp.Types

type lsp_config =
  {
    lsp_project_layout: Superbol_project.layout;
    (** Describes the layout of projects to be managed by the LSP *)

    lsp_cache_for: rootdir:string -> Superbol_cache.Cache.t;
    (** [cache_for] looks up a cache based on a given project directory.  An
        application with no file-system at hand passes
        {!Superbol_cache.Cache.disabled}, which disables caching: every lookup
        misses and nothing is stored. *)

    lsp_cache_pruning_policy: Superbol_cache.Types.prune_policy;
    (** What a cache is allowed to keep, and/or for how long. *)

    lsp_cache_verbose: bool;
    (** Make the LSP server report some information on its operations on project
        caches. *)

    enable_client_configs: bool;
    (** Enable requests for configuration settings to the client. *)

    force_syntax_diagnostics: bool;
    (** Force reporting of syntax and pre-processor-related diagnostics (for
        dialects other that COBOL85, for which they are always enabled). *)
  }

type params =
  {
    config: lsp_config;
    root_uri: DocumentUri.t option;
    workspace_folders: DocumentUri.t list;                 (* includes root_uri *)
    with_semantic_tokens: bool;
    with_client_config_watcher: bool;
    with_client_file_watcher: [`no | `yes of [`absolute | `any]];
    position_encoding: [`UTF8 | `UTF16];
  }
