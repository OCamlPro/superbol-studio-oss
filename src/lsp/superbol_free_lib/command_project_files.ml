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

(** Listing the source files of a project. *)

open Ezcmd.V2
open EZCMD.TYPES

let name = "project files"

let cmd =
  let null = ref false in
  let common, common_args = Common_args.get () in
  let project_setup, project_args = Project_args.get () in
  EZCMD.sub name
    begin fun () ->
      let sep = if !null then '\000' else '\n' in
      List.iter (fun f -> print_string f; print_char sep) @@
      Input_files.walk_project ~common:(common ()) ~setup:(project_setup ())
    end
    ~args:(common_args @ project_args @ [
        ["null"; "0"], Arg.Set null,
        EZCMD.info "Separate file names with NUL characters instead of \
                    newlines (for `--null' options of other commands)";
      ])
    ~doc:"List the source files of a project"
    ~man:[ `S "DESCRIPTION";
           `Blocks [ `P "List the files that the LSP server deems source files \
                         of the project (the one given with \
                         $(b,--project-root), or else the one of the current \
                         directory), one per line: those with extensions that \
                         the platform and project configuration deem source \
                         files, skipping hidden files, directories excluded by \
                         the configuration, and the working directory of the \
                         project.";
                     `P "This is the list of files other commands consider \
                         with $(b,--walk-project)." ];
           `S "EXAMPLES";
           `Pre "superbol project files -0 | \
                 superbol parallel -j 8 -0 --files-from - -- sql preproc" ]
