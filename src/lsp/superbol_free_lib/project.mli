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

val layout: Superbol_project.layout
(** Layout of projects managed via the CLI. *)

(** {1 Loading}

    Functions for loading projects.  They report diagnostics and exit the
    program upon error (invalid configuration file or system error).

    Note: No file is ever written by these procedures, even in the case of
    project creation. *)

(** {2 Direct (using the intended root directory)} *)

(** [in_ ~dirname] loads or creates a project with root directory [dirname]. *)
val in_: dirname: string -> Superbol_project.t

(** [load ~dirname ()] is an alias for [in_ ~dirname], and [load ()] is
    equivalent to [in_ ~dirname:"."]. *)
val load: ?dirname: string -> unit -> Superbol_project.t

(** {2 Indirect (using a regular file)} *)

(** [for_ ~filename] searches the parent directories of [filename] until a
    project directory, that contains a ["superbol.toml"] configuration file, is
    found.  If none is found, returns a project with a default configuration and
    [Filename.dirname filename] as root directory. *)
val for_: filename: string -> Superbol_project.t

(** [for_filename filename] is an alias for [for_ ~filename]. *)
val for_filename: string -> Superbol_project.t
