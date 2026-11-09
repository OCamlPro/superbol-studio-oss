(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

(** Running a sub-command of this program on batches of files, in parallel
    processes. *)

(** Whether this program can start processes (it cannot when compiled into
    JavaScript). *)
val supported: bool

(** [run ~jobs ?batch_size ~command ~args ~sequential files] splits [files]
    into batches of [batch_size] files, and runs this program as [<command>
    <args> -- <batch>] in a process of its own for each (preceded by [-v] as
    many times as needed for the same verbosity), with at most [jobs]
    processes at once: a process is started on the next batch left as soon as
    another one ends.  Progress is reported as [progress] tells, each time a
    process ends.  The output of each process is held back until it ends,
    and then copied onto the standard output and error of this program;
    processes output colors on either whenever this program does, and with the
    same margins.

    The exit status of each process is merged into that of this program (see
    {!Cobol_common.exit}); a process that cannot be started, or that does not
    end normally, counts as a failure to process its files.

    [batch_size] defaults to the size that splits [files] into [jobs] batches,
    up to 25 files.

    When [jobs] is less than 2 or there is a single batch, or if processes
    cannot be started (see {!supported}), [sequential files] is called
    instead. *)
val run
  : jobs: int
  -> ?batch_size: int
  -> progress: Common_args.progress_display
  -> command: string list
  -> args: string list
  -> sequential: (string list -> unit)
  -> string list
  -> unit

(** [args ()] is a function that gives the number of jobs and the size of
    batches for {!run}, along with the arguments ([--jobs] and [--batch-size])
    that set them. *)
val args: unit -> (unit -> int * int option) * Ezcmd.V2.EZCMD.TYPES.arg_list

(** Name of an environment variable that hides the banner of this program
    when set; it is set for every process {!run} starts. *)
val no_banner_var: string
