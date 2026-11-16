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

(** Arguments of commands that take many source files, on top of
    {!Project_args}: files are given as anonymous arguments, listed in a file or
    on standard input ([--files-from]), or found by walking the project
    ([--walk-project]). *)

(** [get ~docv ~doc ~walk ()] is the arguments (anonymous ones documented with
    [docv] and [doc]), along with a function that lists the files they
    designate: anonymous arguments first, then listed files, and then walked
    ones, without duplicates (see {!dedup}).  It warns when [--files-from] or
    [--walk-project] is given but no file is listed.

    [walk] (true by default) tells whether to provide [--walk-project], that
    only makes sense along with {!Project_args}. *)
val get
  : ?docv: string
  -> ?doc: string
  -> ?walk: bool
  -> unit
  -> (common: Common_args.t -> setup: Project_commands.setup -> string list)
     * Ezcmd.V2.EZCMD.TYPES.arg_list

(** [list_args ()] is the [--files-from] and [--null] arguments alone, along
    with a function that reads the files listed accordingly ([None] if
    [--files-from] is not given). *)
val list_args
  : unit
  -> (platform: Cobol_common.Platform.TYPES.platform -> string list option)
     * Ezcmd.V2.EZCMD.TYPES.arg_list

(** [designates_files words] tells whether the command-line arguments [words]
    hold [--files-from], [--null] or [--walk-project]. *)
val designates_files: string list -> bool

(** [dedup files] is [files] without those given several times (as told by
    their absolute names), in the order they were first given. *)
val dedup: string list -> string list

(** [walk_project ~common ~setup] lists the source files of the project rooted
    at [setup.project_root], or of the project the current directory belongs
    to, as the LSP server does (see {!Superbol_project.Walk.source_files}). *)
val walk_project
  : common: Common_args.t
  -> setup: Project_commands.setup
  -> string list

(** [read_list ~platform ~sep source] reads a list of file names separated by
    [sep] from file [source], or from standard input when [source] is ["-"].
    Empty entries are skipped, as are carriage returns that end entries when
    [sep] is ['\n']. *)
val read_list
  : platform: Cobol_common.Platform.TYPES.platform
  -> sep: char
  -> string
  -> string list
