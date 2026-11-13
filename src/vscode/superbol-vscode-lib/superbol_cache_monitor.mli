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

(** One entry of a cache. *)
type entry =
  {
    key: string;
    label: string;         (** usually the file the entry is about *)
    dependencies: int;
    disk_usage: float;     (** in bytes, of the entry and its value *)
    last_used: float;      (** in seconds since the epoch *)
    value: string;         (** digest of the value it holds... *)
    value_size: float;     (** ... which may be shared between entries *)
  }

(** Every entry of the cache of a project. *)
type listing =
  {
    totals: status;
    now: float;            (** when the listing was made *)
    items: entry list;
  }

(** Outcome of pruning a cache. *)
type trimmed =
  {
    reclaimed: int;        (** number of entries removed *)
    remaining: status;
  }

(** [now ()] is the current date, in seconds since the epoch. *)
val now: unit -> float

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

(** [listing cli ~project_root] lists every entry of the cache of the project
    whose root directory is [project_root]. *)
val listing
  : Superbol_languageclient.cli_invocation
  -> project_root: string
  -> (listing, string) result Promise.t

(** [trim cli ~project_root ?older_than ?max_size ()] prunes the cache of the
    project whose root directory is [project_root], by removing entries unused
    for more than [older_than] days, and then the least recently used ones
    until the cache fits within [max_size] (a size as [superbol cache trim]
    accepts it, e.g. [500M]). *)
val trim
  : Superbol_languageclient.cli_invocation
  -> project_root: string
  -> ?older_than: float
  -> ?max_size: string
  -> unit
  -> (trimmed, string) result Promise.t

(** [removed_by_age_bound ~now ~older_than items] is the number of entries
    among [items] that pruning entries unused for more than [older_than] days
    removes, and the number of bytes this frees: values held by entries that
    remain are not freed. *)
val removed_by_age_bound
  : now: float
  -> older_than: float
  -> entry list
  -> int * float

(** [workspace_folders ()] are the workspace folders whose caches are
    monitored: those on the file-system.  The language server puts every file
    of a workspace folder in the project of that folder. *)
val workspace_folders: unit -> Vscode.WorkspaceFolder.t list

(** [pp_size] prints a number of bytes in binary units (e.g. [1.2 GiB]). *)
val pp_size: Format.formatter -> float -> unit

val string_of_size: float -> string
