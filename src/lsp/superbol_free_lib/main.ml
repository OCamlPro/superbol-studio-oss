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

let public_subcommands = [
  Command_pp.cmd ;
  Command_lsp.cmd;
  Command_texi2rst.cmd ;
  Command_indent_range.cmd;
  Command_indent_file.cmd;
  Command_json_package.cmd;
]

let main ?style_renderer ?utf_8 () =
  Printf.eprintf
    "SuperBOL, Copyright OCamlPro. https://get-superbol.com. Affero GPL version.\n";
  Pretty.init_formatters ?style_renderer ?utf_8 ();
  Globals.MAIN.main
    (* ~on_error:Cobol_common.keep_temporary_files *)
    ~on_exit:Cobol_common.exit
    ~print_config:Config.print
    ~common_args:[]
    public_subcommands
