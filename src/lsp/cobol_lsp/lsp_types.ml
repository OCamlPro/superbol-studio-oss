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

type config =
  {
    project_layout: Superbol_project.layout;
    cache_config: cache_config;
    enable_client_configs: bool;
    force_syntax_diagnostics: bool;
  }

and cache_config =
  {
    cache_storage: cache_storage;
    cache_verbose: bool;
  }

and cache_storage =
  | No_storage
  | Store_in_file of
      {
        (** Name of cache file, relative to project root directory. *)
        relative_filename: string;
      }
  | Store_in_shared_dir of
      {
        dirname: string;
      }

type params =
  {
    config: config;
    root_uri: DocumentUri.t option;
    workspace_folders: DocumentUri.t list;                 (* includes root_uri *)
    with_semantic_tokens: bool;
    with_client_config_watcher: bool;
    with_client_file_watcher: [`no | `yes of [`absolute | `any]];
    position_encoding: [`UTF8 | `UTF16];
  }
