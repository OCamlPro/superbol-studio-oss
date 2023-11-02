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

let config_cmd =
  let dirname = ref None in
  let action () =
    let project = Project.load ?dirname:!dirname () in
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
    (fun () ->
      Superbol_project.save_config @@
      Project.load ?dirname:!dirname ())
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
