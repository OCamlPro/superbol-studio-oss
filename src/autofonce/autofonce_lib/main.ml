(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.0, as described in the LICENSE.md file in the root  *)
(*  directory of this source tree.                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2
open Ez_call.V1
open Ez_file.V1

module Misc = Autofonce_misc.Misc

let set_verbosity n =
  Globals.verbose := n;
  EzCall.debug := !Globals.verbose > 1

let get_verbosity () = !Globals.verbose

module PROGRAM = struct
  let command = "autofonce"
  let about = "autofonce COMMAND COMMAND-OPTIONS"
  let set_verbosity = set_verbosity
  let get_verbosity = get_verbosity

  (* Use `AUTOFONCE_BACKTRACE=y autofonce run` or
     `autofonce run -v` to get a full backtrace *)
  let backtrace_var = Some "AUTOFONCE_BACKTRACE"
  let usage = "Modern runner for GNU Autoconf testsuites"
  let version = Version.version
  exception Error = Misc.Error
end
module MAIN = EZCMD.MAKE( PROGRAM )
include PROGRAM

let commands = [
  Command_init.cmd ;
  Command_list.cmd ;
  Command_config.cmd ;
  Command_run.cmd ;
  Command_new.cmd ;
  Command_regen.cmd ;
  Command_diff.cmd ;
  Command_promote.cmd ;
]

let main () =

  Slashifier.enable ();
  begin
    try ignore ( Misc.getcwd () )
    with _ ->
      Printf.eprintf "Current directory does not exist anymore. Move back up.\n%!";
      exit 2
  end ;
  Printexc.record_backtrace true;

  let common_args = [
  ] in

  (* Check to avoid errors in 'autofonce rst' *)
  List.iter EZCMD.TYPES.(fun cmd ->
      let args = cmd.sub_args in
      let args = List.map (fun (args, _order, _info) ->
          "--" ^ String.concat " | --" args
        ) args in
      let args = List.sort compare args in
      let rec iter args =
        match args with
        | arg1 :: arg2 :: _ when arg1 = arg2 ->
            Printf.eprintf
              "ERROR: sub-command %S contains twice option '--%s'\n%!"
              cmd.sub_name arg1;
            exit 2
        | [] -> ()
        | _ :: args -> iter args
      in
      iter args
    ) commands ;

  MAIN.main
    ~on_error: (fun () -> () )
    ~on_exit: (fun () -> () )
    ~print_config: (fun () -> () )
    (* ~argv *)
    commands
    ~common_args;
