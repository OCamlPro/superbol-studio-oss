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

(* Commands that run other programs, or change the working directory or the
   environment of the process, which the JavaScript build (as run by node)
   cannot do: they are hidden from that build.

   TODO: these have nothing to do with the language server, and should
   eventually be removed from this executable altogether. *)
let process_subcommands = [
  Command_snapshot.cmd;

  Command_switch.env_cmd; (* env *)
  Command_switch.switch_cmd;
  Command_switch.switch_env_cmd;
  Command_switch.switch_import_cmd;
  Command_switch.switch_list_cmd;
  Command_switch.switch_set_cmd;
  Command_switch.switch_add_cmd;
  Command_switch.switch_build_cmd;
  Command_switch.switch_config_cmd;
]

let public_subcommands = [
  Command_sql.preproc_cmd ;
  Command_pp.cmd ;
  Command_lsp.cmd;
  Command_texi2rst.cmd ;
  Command_indent_file.cmd;
  Command_check_syntax.cmd;
  Command_json_vscode.cmd;

  Command_ebcdic.ebcdic_translate_cmd ;

  Command_project.init_cmd;
  Command_project.config_cmd;

  Command_cache.status_cmd;
  Command_cache.analyze_cmd;
  Command_cache.trim_cmd;

  Command_util.util_detect_cycle_cmd;
  Command_util.util_wc_cmd;
] @ (match Sys.backend_type with
    | Other "js_of_ocaml" -> []
    | Native | Bytecode | Other _ -> process_subcommands)

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
