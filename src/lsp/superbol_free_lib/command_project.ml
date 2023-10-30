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

open Ezcmd.V2
open EZCMD.TYPES
open Ez_file.V1

module DIAGS = Cobol_common.Diagnostics


let load_project ?(dirname = EzFile.current_dir_name) () =
  Pretty.error "Loading project in `%s'@." dirname;
  try
    let layout = Project_config.layout in
    let rootdir = Superbol_project.rootdir_at ~dirname in
    let project = Superbol_project.for_ ~rootdir ~layout in
    DIAGS.show_n_forget project
  with
  | Superbol_project.Config.ERROR e ->
      DIAGS.(pp Fmt.stderr @@ One.error "%a"
               Superbol_project.Diagnostics.pp_error e);
      exit 2
  | Sys_error msg | Invalid_argument msg ->
      DIAGS.(pp Fmt.stderr @@ One.error "%s" msg);
      exit 2


let config_cmd =
  let dirname = ref None in
  let action () =
    let project = load_project ?dirname:!dirname () in
    Pretty.out "%s@."
      (Ez_toml.V1.TOML.string_of_node
         (Ezr_toml.toml project.config.toml_handle))
  in
  EZCMD.sub
    "project config"
    action
    ~args:[
      [], Arg.Anon (0, fun s -> dirname := Some s),
      EZCMD.info ~docv:"DIR" "Project directory";
    ]
    ~doc: "Print current project configuration in TOML format"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command prints the current project configuration."
      ];
    ]


let init_cmd =
  let dirname = ref None in
  EZCMD.sub
    "project init"
    (fun () -> Superbol_project.save_config @@ load_project ?dirname:!dirname ())
    ~args:[
      [], Arg.Anon (0, fun s -> dirname := Some s),
      EZCMD.info ~docv:"DIR" "Project directory";
    ]
    ~doc: "Project initialization"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command initializes a default project in a given directory (or \
            the current directory if not provided)."
      ];
    ]
