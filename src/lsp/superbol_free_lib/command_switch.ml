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

open EzCompat (* for StringMap *)
open Ezcmd.V2
open EZCMD.TYPES
open Ez_file.V1
open EzFile.OP

open Ez_toml.V1
open EZTOML.TYPES

module TYPES = struct

  type config = {
    (* Where all switches are installed. "/opt/gnucobol" for example *)
    mutable switch_dir : string ;
    (* The list of existing switches names *)
    mutable switch_list : string StringMap.t ;
    (* The number for the next switch. Every switch is called
       "S<number>-*" *)
    mutable switch_num : int ;
    (* The current activated switch *)
    mutable switch_current : string option ;
    (* Whether compiler coverage should be activated *)
    mutable with_compiler_coverage : bool ;
    user_config : toml_file ;
  }

end

open TYPES

let section_name = "switches"

let about : block list = [
  `S "ABOUT SWITCHES" ;
  `Blocks [
    `P "Switches are used to easily handle multiple GnuCOBOL installations. The section $(b,[switches]) in the user configuration file $(b,\\$HOME/.config/superbol/config.toml) contains several variables:";
    `I ("* 'dir'", "The directory where installations should be performed, and switches imported from.");
    `I ("* 'current'", "The current switch name to use by default");
    `I ("* 'list'", "The list of known switches, with the corresponding installation directory");
    `I ("* 'num'", "A counter used to name switches");

    `P "Switches can be created, imported and used using the following commands:";
    `I ("$(b,superbol env [SWITCH] [--last] [--global])",
        "Output a list of shell commands to set the environment variables to use a specific switch");
    `I ("$(b,superbol import [DIRS] [--clear] [--no-set])",
        "Scan directories to detect GnuCOBOL installations, and created associated switches");
    `I ("$(b,superbol add DIR [--switch SWITCH] [--no-set])",
        "Add a specific GnuCOBOL installation directory as a switch");
    `I ("$(b,superbol build [DIR] [--sudo] [--switch SWITCH] [--no-set])",
        "From inside GnuCOBOL sources, configure, build and install and add the corresponding switch");
    `I ("$(b,superbol list)",
        "List known switches");
    `I ("$(b,superbol set [SWITCH] [--last])",
        "Set the current default switch");
  ]
]

let about man =
  man @ about


let user_config_file = Misc.config_dir // "config.toml"

let save_user_config config =
  EZTOML.save ~verbose:true user_config_file config.user_config

let get_config () =
  let user_config = EZTOML.load user_config_file in
  let config = {
    switch_dir = Misc.config_dir // "switches" ;
    switch_list = StringMap.empty ;
    switch_num = 1 ;
    switch_current = None ;
    with_compiler_coverage = false ;
    user_config ;
  } in


  let section =
    EZTOML.section
      ~name: section_name
      ~comments:[ "Management of GnuCOBOL installations" ]
      EZTOML.[
        option_string
          "dir"
          ~comments: [ "The directory where GnuCOBOL versions should be \
                              installed by default." ]
          ~getter:(fun config -> config.switch_dir )
          ~setter:(fun config value -> config.switch_dir <- value)
        ;

        option_string_map
          "list"
          ~comments: [ "The table of known switches" ]
          ~getter: (fun config -> config.switch_list )
          ~setter: (fun config value -> config.switch_list <- value)
        ;

        option_int
          "num"
          ~comments: [ "Next ID to be used for switch prefix" ]
          ~getter:(fun config -> config.switch_num )
          ~setter:(fun config v -> config.switch_num <- v )
        ;

        option_string_option
          "current"
          ~comments: [ "Current switch" ]
          ~getter:(fun config -> config.switch_current)
          ~setter:(fun config v -> config.switch_current <- v)
        ;

        option_bool
          "compiler-coverage"
          ~comments: [ "Whether compiler coverage should be activated" ]
          ~getter:(fun config -> config.with_compiler_coverage)
          ~setter:(fun config v -> config.with_compiler_coverage <- v)

      ]
  in

  EZTOML.add_section user_config config section ;
  config

let get_config () =
  try
    get_config ()
  with
  | TOML.Types.Error (loc, _code, error) ->
    Printf.eprintf "%s: %s\n%!"
      ( TOML.string_of_location loc )
      ( TOML.string_of_error error );
    Printf.eprintf "Try to edit your configuration to fix this error.\n%!";
    exit 2

let find_switch_name ?switch_name ?(add=false) ~dir ~file config =
  let exception Found of string in
  let dirname = dir // file in
  match
    StringMap.iter (fun s x ->
        if x = dirname then raise (Found s)) config.switch_list with
  | exception Found s -> s (* already exists *)
  | () ->
    let switch_name =
      let switch_name =
        match switch_name with
        | Some s -> s
        | None ->
          match EzString.chop_prefix file ~prefix:"gnucobol-" with
          | Some s -> s
          | None ->
            match EzString.chop_prefix file ~prefix:"gnucobol" with
            | Some s -> s
            | None -> file
      in
      let name = Printf.sprintf "S%02d-%s" config.switch_num switch_name in
      if add then
        config.switch_num <- config.switch_num + 1;
      name
    in
    if add then begin
      Printf.eprintf "Adding %S at\n   %s\n%!" switch_name dirname;
      config.switch_list <- StringMap.add switch_name dirname config.switch_list;
    end;
    switch_name

let add_switch ~dir ?switch_name ~file ~set config =
  let switch_name =
    find_switch_name ?switch_name ~add:true ~dir ~file config in
  if set then
    config.switch_current <- Some switch_name;
  ()



let find_switch ?switch ~last ~current config =
  match switch, last with
  | None, false ->
    begin
      if current then
        match config.switch_current with
        | None ->
          Misc.error
            "No current switch, you must specify the switch to use"
        | Some switch ->
          if StringMap.mem switch config.switch_list then
            switch
          else
            Misc.error "Current switch %S does not exist anymore" switch
      else
        Misc.error "No switch selected, select one"
    end
  | None, true ->
    begin match StringMap.max_binding config.switch_list with
      | exception _ ->
        Misc.error "Current list of switches is empty"
      | (s, _) -> s
    end
  | Some switch, _ ->
    if StringMap.mem switch config.switch_list then
      switch
    else
      let found = ref [] in
      let regexp = Str.regexp switch in
      StringMap.iter (fun s dir ->
          match Str.search_forward regexp s 0 with
          | _ ->
            found := s :: !found
          | exception _ ->
            match Str.search_forward regexp dir 0 with
            | _ -> found := s :: !found
            | exception _ -> ()
        ) config.switch_list ;
      match !found with
      | [] ->
        Misc.error "Can not find switch %S in current list" switch
      | [switch] ->
        Printf.eprintf "Selecting switch %S\n%!" switch;
        switch
      | (found_switch :: _ ) as switches ->
        if last then
          found_switch
        else
          Misc.error "Multiple switches matching %S in current list ( %s )"
            switch ( String.concat ", " ( List.rev switches ))

let set_switch_link config =
  match config.switch_current with
  | None -> assert false
  | Some switch ->
    Printf.eprintf "Current switch modified\n%!";
    let dir = StringMap.find switch config.switch_list in
    Misc.mkdir_rec Misc.config_dir;
    let switch_link = Misc.config_dir // "switch" in
    if Sys.file_exists switch_link then
      Sys.remove switch_link ;
    Unix.symlink ~to_dir:true dir switch_link

(*** switch import ***)

let switch_import ~dirs ~clear ~set () =

  let config = get_config () in
  let dirs = match dirs with
      [] -> [ config.switch_dir ]
    | dirs -> dirs
  in
  if clear then begin
    Printf.eprintf "Clearing all switches (--clear)\n%!";
    config.switch_list <- StringMap.empty;
    config.switch_num <- 1;
    config.switch_current <- None;
  end else begin
    StringMap.iter (fun s dir ->
        if not ( Sys.file_exists dir ) then begin
          Printf.eprintf "Clearing removed switch %S\n%!" s;
          config.switch_list <- StringMap.remove s config.switch_list;
          if config.switch_current = Some s then
            config.switch_current <- None
        end;
      ) config.switch_list;
  end;
  let current = config.switch_current in
  List.iter (fun dir ->
      let dir = if Filename.is_relative dir then
          Misc.current_dir // dir
        else
          dir
      in
      Printf.eprintf "Scanning directory %s\n%!" dir;
      let subdirs = try
          Sys.readdir dir
        with _ ->
          Printf.eprintf "Warning: could not read dir %s.\n" dir;
          Printf.eprintf "  Check that it exists and is readable\n%!";
          [||]
      in
      Array.sort compare subdirs;
      Array.iter (fun file ->
          if Sys.file_exists ( dir // file // "bin" // "cobc" ) then
            add_switch ~dir ~file ~set config
        ) subdirs
    ) dirs;
  if config.switch_current <> current then
    set_switch_link config ;
  save_user_config config

let import_cmd =
  let dirs = ref [] in
  let clear = ref false in
  let set = ref true in
  EZCMD.sub
    "switch import"
    (fun () ->
       switch_import ~dirs:!dirs ~clear:!clear ~set:!set ()
    )
    ~args:[

      [ "clear" ], Arg.Set clear,
      EZCMD.info "Clear the list before importing";

      [], Arg.Anons (fun list -> dirs := list),
      EZCMD.info ~docv:"DIRS" "Directories to scan";

      [ "no-set" ], Arg.Clear set,
      EZCMD.info "Do not set the last imported directory as the current one";

    ]
    ~doc: "Import existing GnuCOBOL installations as switches"
    ~man: ( about @@ [
      `S "DESCRIPTION";
      `Blocks [
        `P "This command will scan the directories, looking for \
            $(b,gnucobol-*) folders with GnuCOBOL installed, and add \
            them as switches. With no argument, it scans the default \
            installation directory." ;
      ];
    ] )


(*** switch list ***)

let switch_list () =

  let config = get_config () in
  StringMap.iter (fun name dir ->
      Printf.printf "* %S%s\n     %s\n%!" name
        (if Some name = config.switch_current then
           " [CURRENT]" else "")
       dir;
    ) config.switch_list

let list_cmd =
  EZCMD.sub
    "switch list"
    (fun () ->
       switch_list ()
    )
    ~args:[

      (*
      [], Arg.Anons (fun list -> dirs := list),
      EZCMD.info ~docv:"DIRS" "Directories to scan";
*)
    ]
    ~doc: "List known switches"
    ~man:( about [
      `S "DESCRIPTION";
      `Blocks [
        `P "This command list existing switches.";
      ];
    ])




(*** switch env / env ***)

let switch_env ?switch ~last ~global () =

  if global then
    let env_file = Misc.config_dir // "env" in
    EzFile.write_file env_file
      {|#!/bin/sh
case ":${PATH}:" in
    *"$HOME/.config/superbol/switch/bin":*)
        ;;
    *)
        PATH="$HOME/.config/superbol/switch/bin:$PATH";
        export PATH;
        MANPATH="$HOME/.config/superbol/switch/share/man:$MANPATH";
        export MANPATH;
        LOCPATH="$HOME/.config/superbol/switch/share/locale:$LOCPATH";
        export LOCPATH;
        LD_LIBRARY_PATH="$HOME/.config/superbol/switch/lib:$LD_LIBRARY_PATH";
        export LD_LIBRARY_PATH;
        ;;
esac
|};
    Unix.chmod  env_file 0o755;
    Printf.eprintf {|Add:

. "$HOME/.config/superbol/env"

to your $HOME/.profile file.|};
    Printf.eprintf "\n%!"
  else

    let config = get_config () in
    let switch = find_switch config ?switch ~last ~current:true in

    let switch_dir = StringMap.find switch config.switch_list in

    let set_path name subdir =
      let switch_dir = switch_dir // subdir in
      let path =
        match Sys.getenv name with
        | exception Not_found -> switch_dir
        | path ->
          let set = ref StringSet.empty in
          StringMap.iter (fun _ dir ->
              set := StringSet.add ( dir // subdir ) !set
            ) config.switch_list ;
          let path = EzString.split path ':' in
          let path = List.filter (fun s ->
              not ( StringSet.mem s !set )) path
          in
          String.concat ":" ( switch_dir :: path )
      in
      ( name, path )
    in

    List.iter (fun (var, v) ->
        Printf.printf "%s='%s'; export %s;\n" var v var)
      [ "GNUCOBOL_DIR", switch_dir;
        set_path "PATH" "bin";
        set_path "LD_LIBRARY_PATH" "lib" ;
        set_path "MANPATH" "share/man" ;
        set_path "LOCPATH" "share/locale" ;
      ];
    ()

let switch_env_cmd, env_cmd =
  let switch = ref None in
  let last = ref false in
  let global = ref false in
  let add_env_command name =
  EZCMD.sub
    name
    (fun () ->
       switch_env ?switch:!switch ~last:!last ~global:!global ()
    )
    ~args:[

      [], Arg.Anon (0, fun s -> switch := Some s),
      EZCMD.info ~docv:"SWITCH" "Switch to use, instead of current";

      [ "last" ], Arg.Set last,
      EZCMD.info "Use the latest imported switch";

      [ "global" ], Arg.Set global,
      EZCMD.info "Use global configuration for default switch";

    ]
    ~doc: "Generate commands to set environment variables to use the \
           specified switch"
    ~man:(about [
      `S "DESCRIPTION";
      `Blocks [
        `P "This command generates a list of command to set \
            environment variables for an existing switch.";
        `P "It is usually used as:";
        `Pre {|# eval \$(superbol env)
# cobc --version|};
        `P "The $(b,--switch SWITCH) option can be used to choose a different switch from the default one. SWITCH should match a sub-string of a switch name. $(b,--last) can be used to select the most recent switch.";
        `P "The $(b,--global) option can be used to generate a file $(b,\\$HOME/.config/superbol/env) that can be loaded from shell init scripts, using a symlink to point PATH and LD_LIBRARY_PATH to the current switch at any time. Changing the current switch will then automatically redirect cobc to that new switch.";
      ];
    ])
  in
  add_env_command "switch env",
  add_env_command "env"



(*** switch set ***)

let switch_set ?switch ~last () =

  let config = get_config () in
  let switch = find_switch config ?switch ~last ~current:true in
  config.switch_current <- Some switch ;
  set_switch_link config ;
  Printf.eprintf "Current switch set to %S\n%!" switch;
  save_user_config config

let set_cmd =
  let switch = ref None in
  let last = ref false in
  EZCMD.sub
    "switch set"
    (fun () ->
       switch_set ?switch:!switch ~last:!last ()
    )
    ~args:[
      [], Arg.Anon (0, fun s -> switch := Some s),
      EZCMD.info ~docv:"SWITCH" "Switch to use";

      [ "last" ], Arg.Set last,
      EZCMD.info "Use the latest imported switch";
    ]
    ~doc: "Set the current default switch"
    ~man:(about [
      `S "DESCRIPTION";
      `Blocks [
        `P "This command sets the current default switch.";
      ];
    ])



(*** switch add ***)

let switch_add ~dirname ?switch_name ~set () =

  let config = get_config () in
  let file = Filename.basename dirname in
  let dir = Filename.dirname dirname in
  let current = config.switch_current in
  add_switch config ?switch_name ~file ~dir ~set ;
  if config.switch_current <> current then
    set_switch_link config ;
  EZTOML.save user_config_file config.user_config

let add_cmd =
  let dirname = ref None in
  let switch_name = ref None in
  let set = ref true in
  EZCMD.sub
    "switch add"
    (fun () ->
       match !dirname with
       | None -> Misc.error "a directory to add must be specified"
       | Some dirname ->
         if Sys.file_exists ( dirname // "bin" // "cobc" ) then
           switch_add ~dirname ?switch_name:!switch_name ~set:!set ()
         else
           Misc.error "Directory %s does not contain bin/cobc" dirname
    )
    ~args:[

      [], Arg.Anon (0, fun s -> dirname := Some s),
      EZCMD.info ~docv:"DIR" "Directory to add";

      [ "switch" ], Arg.String (fun s -> switch_name := Some s),
      EZCMD.info ~docv:"SWITCH" "Name of switch to add";

      [ "no-set" ], Arg.Clear set,
      EZCMD.info "Do not set this directory as the current one";

    ]
    ~doc: "Add a directory as a switch"
    ~man:(about [
      `S "DESCRIPTION";
      `Blocks [
        `P "This command adds a new known switch.";
      ];
    ])




(*** switch build ***)

let switch_build ?dir ?switch_name ?branch ~set ~sudo () =

  let config = get_config () in

  let branch = match branch with
    | None ->
      let branch = Call.call_stdout_string [ "git" ; "branch" ; "--show-current" ] in
      let branch = String.trim branch in
      Printf.eprintf "Current branch: %S\n%!" branch;
      if branch = "gnucobol-3.x" then
        let commit = Call.call_stdout_string [ "git" ;  "rev-parse" ; "--short" ; "HEAD" ] in
        let date = Call.call_stdout_string [ "date" ; "+%Y-%m-%d" ] in
        Printf.sprintf "3.x-%s-%s" date commit
      else
      if branch = "master" then
        let commit = Call.call_stdout_string [ "git" ;  "rev-parse" ; "--short" ; "HEAD" ] in
        let date = Call.call_stdout_string [ "date" ; "+%Y-%m-%d" ] in
        Printf.sprintf "4.x-%s-%s" date commit
      else
        branch
    | Some branch -> branch
  in
  let file = "gnucobol-" ^ branch in
  let dir = match dir with
    | None -> config.switch_dir
    | Some dir -> dir
  in

  let computed_switch_name = find_switch_name config ?switch_name ~file ~dir in
  Printf.eprintf "Switch name with be: %S\n and installed in: %s/%s\n%!"
    computed_switch_name dir file;

  if not ( Sys.file_exists dir ) then
    Misc.error "Directory %s should exist (%s will be created inside)" dir file;
  let destdir = dir // file in
  let rec iter dirname =
    Unix.chdir dirname ;
    if Sys.file_exists "COPYING.LESSER" &&
       Sys.file_exists "libcob" then begin
      if not ( Sys.file_exists "_build" ) then
        Unix.mkdir "_build" 0o755;
      Unix.chdir "_build"
    end else
      let newdir = Filename.dirname dirname in
      if newdir = dirname then
        Misc.error "Could not find GnuCOBOL source rootdir from current directory";
      iter newdir
  in
  iter ( Sys.getcwd () );

  if not ( Sys.file_exists "../configure" ) then
    Call.call ~echo:true [ "../build_aux/bootstrap" ];

  let cmd =
    [ "../configure" ;
      "--enable-cobc-internal-checks";
      "--enable-debug";
      "--prefix" ; destdir ;
      "--exec-prefix" ; destdir ;
    ] in
  let cmd = if config.with_compiler_coverage then
      cmd @ [
        "--enable-code-coverage";
      ]
    else
      cmd
  in
  Call.call ~echo:true cmd ;
  Call.call ~echo:true [ "make" ];

  Call.call ~echo:true (let cmd = [ "make" ; "install" ] in
                        if sudo then  "sudo" :: cmd else cmd );

  let current = config.switch_current in
  add_switch config ?switch_name ~file ~dir ~set ;
  if config.switch_current <> current then
    set_switch_link config;
  save_user_config config

let build_cmd =
  let dir = ref None in
  let switch_name = ref None in
  let set = ref true in
  let sudo = ref false in
  let branch = ref None in
  EZCMD.sub
    "switch build"
    (fun () ->
       switch_build ?dir:!dir ?switch_name:!switch_name ?branch:!branch
         ~set:!set ~sudo:!sudo ()
    )
    ~args:[

      [], Arg.Anon (0, fun s -> dir := Some s),
      EZCMD.info ~docv:"DIR" "Directory where GnuCOBOL should be installed";

      [ "switch" ], Arg.String (fun s -> switch_name := Some s),
      EZCMD.info ~docv:"SWITCH" "Name of switch to add";

      [ "branch" ], Arg.String (fun s -> branch := Some s),
      EZCMD.info ~docv:"BRANCH" "Branch name to use instead of git branch (the auto-detected name from git will be 3.x-$DATE-$COMMIT)";

      [ "no-set" ], Arg.Clear set,
      EZCMD.info "Do not set this directory as the current one";

      [ "sudo" ], Arg.Set sudo,
      EZCMD.info "Use sudo for 'make install'";

    ]
    ~doc: "Configure, build, install GnuCOBOL and add a switch"
    ~man:(about [
      `S "DESCRIPTION";
      `Blocks [
        `P "This command will build and install GnuCOBOL and add the corresponding switch. If DIR is specified, the installation directory will be created inside, otherwise the $(b,dir) user option will be used. The name of the directory and the switch names are generated automatically from the GIT configuration. If $(b,--switch SWITCH) is provided, it will be used for the switch name. The $(b,--sudo) option will decide if installation should be performed with sudo. If the installation is successful, the switch is created and automatically set as the default switch, unless $(b,--no-set) is specified.";
      ];
    ])


let cmd =
  EZCMD.sub
    "switch"
    (fun () ->
       let config = get_config () in
       match config.switch_current with
       | None -> Printf.printf "No current switch\n%!"
       | Some switch ->
         match StringMap.find switch config.switch_list with
         | dir ->
           Printf.eprintf "%S %s\n%!" switch dir
         | exception _ ->
           Printf.eprintf "Current switch %S does not exist\n%!" switch
    )
    ~args:[
    ]
    ~doc: "Print current switch"
    ~man:(about [
      `S "DESCRIPTION";
      `Blocks [
        `P "This command prints the current default switch."
      ];
    ])
