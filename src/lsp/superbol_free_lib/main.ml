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

open Ez_file.V1
open EzFile.OP

let public_subcommands = [
  Command_cache.analyze_cmd;
  Command_cache.status_cmd;
  Command_cache.trim_cmd;
  Command_check_syntax.cmd;
  Command_indent_file.cmd;
  Command_json_vscode.cmd;
  Command_lsp.cmd;
  Command_sql.preproc_cmd;
]

let () =
  Cobol_common.init_default_exn_printers ()

(** [main ?enable_banner ?style_renderer ?utf_8 ?subcommands ()] runs this
    program with [subcommands] (that default to {!public_subcommands}), along
    with [parallel], that may run any of them.
    @raise Invalid_argument if [subcommands] holds a [parallel] command.

    May print a banner unless [enable_banner = false]. *)
let main ?(enable_banner = true) ?style_renderer ?utf_8
    ?(subcommands = public_subcommands) () =
  let is_parallel Ezcmd.V2.EZCMD.TYPES.{ sub_name; _ } =
    sub_name = Command_parallel.name in
  if List.exists is_parallel subcommands then
    invalid_arg @@
    Printf.sprintf "Superbol_free_lib.Main.main: a `%s' sub-command is given"
      Command_parallel.name;

  (* Print a banner only if ~/.config/superbol/NO-BANNER is absent, and this
     process does not handle a batch of files for another one *)
  if enable_banner &&
     not (Sys.file_exists ( Misc.config_dir // "NO-BANNER" ) ||
          Option.is_some (Sys.getenv_opt Parallel_batches.no_banner_var)) then
    Printf.eprintf
      "SuperBOL, by OCamlPro. https://get-superbol.com. Affero GPL version.\n%!";
  Pretty.init_formatters ?style_renderer ?utf_8 ();
  (* Set outputs up as the environment says; [--color] takes precedence, as it
     is parsed afterwards. *)
  List.iter begin fun (var, set) ->
    Option.iter begin fun spec ->
      match set spec with
      | Ok () -> ()
      | Error msg ->
          Cobol_common.Diagnostics.Now.warn Fmt.stderr
            "Ignoring@ the@ value@ of@ %s:@ %s" var msg
    end (Sys.getenv_opt var)
  end [Globals.colors_var, Pretty.set_colors;
       Globals.margins_var, Pretty.set_margins];
  let run ?argv subcommands =
    Globals.MAIN.main
      (* ~on_error:Cobol_common.keep_temporary_files *)
      ~on_exit:Cobol_common.exit
      ~print_config:Config.print
      ~common_args:Globals.common_args
      ?argv subcommands
  in
  (* `superbol parallel' runs other sub-commands in-process when it does not
     start processes; it cannot run itself, as it is not among them. *)
  let run_in_process args =
    run ~argv:(Array.of_list (Sys.argv.(0) :: args)) subcommands
  in
  run (subcommands @ [Command_parallel.make ~subcommands ~run_in_process])
