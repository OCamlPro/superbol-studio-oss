(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2022-2023 OCamlPro SAS                                  *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

type progress_display =
  | Hide
  | Show                       (** on a single line of stderr, overwritten *)
  | Log
  | Log_stderr

type t = {
  platform : Cobol_common.Platform.TYPES.platform;
  preproc_options: Cobol_preproc.Options.preproc_options;
  parser_options: Cobol_parser.Options.parser_options;
  progress_display: progress_display;
  pretty_progress: 'a. 'a Pretty.proc;               (** as [progress_display] *)
  analysis_overrides: Superbol_project.Analysis.overrides;
  (** Only what is explicitly given on the command line, for commands that
      analyze documents according to the configuration of their project. *)
}

val get : unit -> (unit -> t) * Ezcmd.V2.EZCMD.TYPES.arg_list

(** {2 Progress} *)

(** [progress_arg ()] is the [--progress] option (alone in its list), along with a function that
    tells how to display progress accordingly (by default, it is shown when
    stderr is a TTY). *)
val progress_arg
  : unit -> (unit -> progress_display) * Ezcmd.V2.EZCMD.TYPES.arg_list

(** [pretty_progress display] reports progress as [display] tells. *)
val pretty_progress : progress_display -> 'a Pretty.proc

(** [clear_progress ()] erases the progress message shown on stderr, if any, so
    that what is printed next does not mix with it. *)
val clear_progress : unit -> unit
