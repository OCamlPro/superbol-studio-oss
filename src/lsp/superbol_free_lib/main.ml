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

(*
open Ezcmd.V2
open EZCMD.TYPES
*)

open Ez_file.V1
open EzFile.OP

let public_subcommands = [
  Command_pp.cmd ;
  Command_lsp.cmd;
  Command_texi2rst.cmd ;
  Command_indent_range.cmd;
  Command_indent_file.cmd;
  Command_json_vscode.cmd;
  Command_snapshot.cmd;

  Command_switch.cmd;
  Command_switch.env_cmd; (* env *)
  Command_switch.switch_env_cmd;
  Command_switch.import_cmd;
  Command_switch.list_cmd;
  Command_switch.set_cmd;
  Command_switch.add_cmd;
  Command_switch.build_cmd;

  Command_project.init_cmd;
  Command_project.config_cmd;
]

let () =
  Cobol_common.init_default_exn_printers ()

let main ?style_renderer ?utf_8 () =

  (* Print a banner only if ~/.config/superbol/NO-BANNER is absent *)
  if not @@ Sys.file_exists ( Misc.config_dir // "NO-BANNER" ) then
    Printf.eprintf
      "SuperBOL, by OCamlPro. https://get-superbol.com. Affero GPL version.\n%!";
  Pretty.init_formatters ?style_renderer ?utf_8 ();
  Globals.MAIN.main
    (* ~on_error:Cobol_common.keep_temporary_files *)
    ~on_exit:Cobol_common.exit
    ~print_config:Config.print
    ~common_args:[]
    public_subcommands
