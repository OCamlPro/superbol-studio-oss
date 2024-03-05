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


let run_lsp ~enable_caching ~force_syntax_diagnostics ~storage =
  Cobol_lsp.INTERNAL.Debug.message "LSP Started with pid %d\n%!"
    (Unix.getpid ());
  Cobol_preproc.Src_overlay.debug_oc := !Cobol_lsp.INTERNAL.Debug.debug_oc;

  let project_layout, fallback_storage_directory =
    match storage with
    | None ->
        Project.layout, None
    | Some dir ->
        Project.{ layout with relative_work_dirname = None }, Some dir
  in
  let lsp_config =
    Cobol_lsp.config ()
      ~enable_caching
      ~enable_client_configs:true
      ~force_syntax_diagnostics
      ~project_layout
      ?fallback_storage_directory
  in
  match Cobol_lsp.run ~config:lsp_config with
  | Ok () -> ()
  | Error exit_msg -> Pretty.error "%s@." exit_msg; exit 1


let cmd =
  let caching, caching_args =
    Arg_utils.switch `enable_disable ~name:"caching" ~default:true
  and syntax_diagnostics, syntax_diagnostics_args =
    Arg_utils.switch `force ~name:"syntax-diagnostics" ~default:false
      ~descr:"reporting of syntax error and hint diagnostics for dialects other \
              than COBOL85 (for which they are always enabled)"
  in
  let storage = ref None in
  EZCMD.sub "lsp"
    (fun () ->
       run_lsp ~enable_caching:!caching
         ~force_syntax_diagnostics:!syntax_diagnostics
         ~storage:!storage)
    ~doc:"run LSP server"
    ~args: (caching_args @ syntax_diagnostics_args @ [
        ["storage-directory"], Arg.String (fun s -> storage := Some s),
        EZCMD.info ~docv:"DIR"
          "Directory under which to store cache data --- prevents the creation \
           of a \"_superbol\" storage directory at the root of project trees.";
      ])
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "Start a COBOL LSP server"
      ];
    ]
