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

(** [source_files ~platform ~layout project] lists the files under the root
    directory of [project] that [platform] deems source files, or that have an
    extension the project configuration assigns a source format to.

    Skipped are: hidden files and directories, the work directory of the
    project according to [layout] (see {!Project.work_dirname}), directories
    the project configuration excludes (see {!Project_config.excludes_dir}),
    and symbolic links to directories. *)
val source_files
  : platform: Cobol_common.Platform.TYPES.platform
  -> layout: Project.layout
  -> Project.t
  -> string list
