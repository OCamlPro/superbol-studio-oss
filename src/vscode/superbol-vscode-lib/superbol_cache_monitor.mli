(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(** Monitoring the caches of the language server, through the CLI commands of
    its executable ([superbol cache status/trim --json]). *)

(** Totals of the cache of a project. *)
type status =
  {
    project_root: string;
    cache: string option;  (** where the cache lives; [None] if there is none *)
    entries: int;
    values: int;
    disk_usage: float;     (** in bytes *)
  }

(** [run cli args] runs [cli] with [args], and gives its standard output, or a
    description of its failure. *)
val run
  : Superbol_languageclient.cli_invocation
  -> string list
  -> (string, string) result Promise.t

(** [status cli ~project_root] gives the totals of the cache of the project
    whose root directory is [project_root]. *)
val status
  : Superbol_languageclient.cli_invocation
  -> project_root: string
  -> (status, string) result Promise.t

(** [workspace_folders ()] are the workspace folders whose caches are
    monitored: those on the file-system.  The language server puts every file
    of a workspace folder in the project of that folder. *)
val workspace_folders: unit -> Vscode.WorkspaceFolder.t list

(** [pp_size] prints a number of bytes in binary units (e.g. [1.2 GiB]). *)
val pp_size: Format.formatter -> float -> unit

val string_of_size: float -> string
