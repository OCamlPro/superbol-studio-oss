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
open EzFile.OP

module TYPES = struct

  type encoded_string = string
  let encoded_string_enc = EzEncoding.encoded_string

  type snapshot = {
    cmd : encoded_string array ;
    env : encoded_string list ;
    pwd : encoded_string ;
  }
  [@@deriving json_encoding]

end

open TYPES

let set_env env =
  List.iter (fun v ->
      let v,s = EzString.cut_at v '=' in
      Unix.putenv v s
    ) env

let snapshots_dir = Misc.config_dir // "snapshots"

let load_and_exec ~snapshot_id ~args ~no_cd ~env () =
  let snapshot_filename = snapshots_dir // snapshot_id ^ ".json" in
  if not @@ Sys.file_exists snapshot_filename then
    Misc.error "Snapshot %S does not exist (%s)" snapshot_id snapshot_filename;
  let s = EzFile.read_file snapshot_filename in
  let snap = EzEncoding.destruct TYPES.snapshot_enc s in
  if not no_cd then Unix.chdir snap.pwd;
  set_env snap.env;
  set_env env;
  let args =
    match args with
    | [||] -> snap.cmd
    | _ ->
      Array.of_list (List.flatten (List.map (function
          | "__" -> Array.to_list snap.cmd
          | "_0" -> [ snap.cmd.(0) ]
          | "_1" -> List.tl @@ Array.to_list snap.cmd
          | arg -> [arg]) (Array.to_list args)))
  in
  Unix.execvp args.(0) args

let save_and_run ~snapshot_id ~args ~quit ~env () =
  let snapshot_filename = snapshots_dir // snapshot_id ^ ".json" in
  let command_env = Unix.environment () in
  let command_env = Array.to_list command_env in
  let command_env = command_env @ env in
  let snap = {
    cmd = args ;
    pwd = Sys.getcwd () ;
    env = command_env ;
  } in
  set_env env;
  Misc.write_file ~mkdir:true snapshot_filename
    ( EzEncoding.construct ~compact:false TYPES.snapshot_enc snap);
  if quit then exit 2;
  Unix.execvp args.(0) args


type kind =
  | Load of string
  | Save of string

let cmd =
  let kind = ref None in
  let no_cd = ref false in
  let quit = ref false in
  let args = ref [||] in
  let env = ref [] in
  EZCMD.sub
    "snapshot"
    (fun () ->
       match !kind with
       | None ->
         Misc.error "you must either use --save ID or --load ID"
       | Some ( Load snapshot_id ) ->
         if !quit then
           Misc.error "--load ID and --quit are incompatible options";
         load_and_exec ~snapshot_id ~args:!args ~no_cd:!no_cd ~env:!env ()
       | Some ( Save snapshot_id ) ->
         if !no_cd then
           Misc.error "--save ID and --no-cd are incompatible options";
         if !args = [||] then
           Misc.error "--save ID requires a command to call";
         save_and_run ~snapshot_id ~args:!args ~quit:!quit ~env:!env ()
    )
    ~args:[

      [ "save" ], Arg.String (fun s -> kind := Some (Save s)),
      EZCMD.info ~docv:"ID" "Create snapshot ID from state";

      [ "env" ], Arg.String (fun s -> env := s :: !env),
      EZCMD.info ~docv:"VAR=VALUE" "Set a variable in the environment";

      [ "no-cd" ], Arg.Set no_cd,
      EZCMD.info "Do not change directory to run the command";

      [ "quit" ], Arg.Set quit,
      EZCMD.info "Do not run the command, just exit";

      [ "load" ], Arg.String (fun s -> kind := Some (Load s)),
      EZCMD.info ~docv:"ID" "Load snapshot ID";

      [], Arg.Anons (fun list -> args := Array.of_list list),
      EZCMD.info ~docv:"ARGS" "Command line arguments";

    ]
    ~doc: "Manage environment snapshots"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P "This command can be used to snapshot the environment when a command is called, typically in a test script, to be able to run this command in the same environment from outside the script." ;
        `P "A snapshot typically contains:";
        `I ("* $(b,cmd)", "The command to run with its arguments");
        `I ("* $(b,pwd)", "The directory where the command should be run");
        `I ("* $(b,env)", "The environment variables for the command");
        `P "Snapshots are stored in $(b,\\$HOME/.config/superbol/snapshots).";
      ];
      `S "SNAPSHOT CREATION";
      `Blocks [
        `P "To snapshot a command, use:";
        `Pre {|# $(mname) $(tname) --save SNAP_ID -- cmd args|};
        `P "The previous command will create a snapshot $(b,SNAP_ID), and then run the command $(b,cmd args).";
        `P "You can use $(b,--quit) if you don't want to run the command at all (the command will exit with status 2)";
      ];
      `S "SNAPSHOT USAGE";
      `Blocks [
        `P "To run a command in a previously created snapshot, use:";
        `Pre {|# $(mname) $(tname) --load SNAP_ID -- cmd args|};
        `P "The previous command will load the snapshot $(b,SNAP_ID), go to its directory, set the environment and then run the command $(b,cmd args).";
        `P "If $(b,cmd args) is empty, then the snapshot command is run.";
        `P "The argument $(b,--no-cd) can be used to run the command in the current directory.";
        `P "The argument $(b,--env VAR=VALUE) can be used to add an extra variable to the environment, after the one set by the snapshot.";
        `P "If a $(b,cmd args) is provided, the following special items are substituted:";
        `I ("* $(b,__)", "is replaced by all the arguments (including the command) from the snapshot");
        `I ("* $(b,_0)", "is replaced by the command from the snapshot");
        `I ("* $(b,_1)", "is replaced by all the arguments (excluding the commnad) from the snapshot");
        `P "For example:";
        `Pre {|# $(mname) $(tname) --load ID -- gdb _0 --args __|};
        `P "will run the command inside $(b,gdb) with its arguments as provided by the snapshot.";
      ];
    ]
