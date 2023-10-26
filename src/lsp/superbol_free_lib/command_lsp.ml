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

let lsp_config =
  Cobol_lsp.config ~project_layout:Project_config.layout

let run_lsp () =
  match Cobol_lsp.run ~config:lsp_config with
  | Ok () -> ()
  | Error exit_msg -> Pretty.error "%s@." exit_msg; exit 1

let cmd =
  EZCMD.sub "lsp"
    run_lsp
    ~doc:"run LSP server"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Start a COBOL LSP server"
      ];
    ]
