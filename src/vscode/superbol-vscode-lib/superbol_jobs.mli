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

(** Batches of COBOL files analyzed by [superbol cache analyze] processes, that
    fill the caches of the language server. *)

type job = private
  {
    files: Vscode.Uri.t list;
    project_root: string;          (** the workspace folder the files are in *)
    client_settings: string;        (** [superbol.cobol] section, as JSON *)
  }

(** [chunks n l] splits [l] into consecutive lists of at most [n] elements. *)
val chunks: int -> 'a list -> 'a list list

(** [make uris] splits [uris] into jobs of at most
    [superbol.workspaceAnalysis.filesPerProcess] files, that each belong to a
    single workspace folder (whose settings they carry).  Files that belong to
    no workspace folder are left out. *)
val make: Vscode.Uri.t list -> job list

(** [run cli ~token ~on_start ~on_completion jobs] runs [jobs] with [cli], with
    at most [superbol.workspaceAnalysis.maxProcesses] processes at once.
    [on_start ~processes] is called with the number of processes used, and
    [on_completion job ~ok] as soon as [job] is over ([ok] tells whether it
    succeeded).  Cancelling [token] kills running processes,
    and no job is started afterwards.  Failures are logged. *)
val run
  : Superbol_languageclient.cli_invocation
  -> token: Vscode.CancellationToken.t
  -> ?on_start: (processes: int -> unit)
  -> on_completion: (job -> ok: bool -> unit)
  -> job list
  -> unit Promise.t
