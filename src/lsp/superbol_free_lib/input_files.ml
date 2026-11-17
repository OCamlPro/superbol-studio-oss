(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2026 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2
open EZCMD.TYPES

let walk_project ~(common: Common_args.t) ~(setup: Project_commands.setup) =
  let platform = common.platform and layout = setup.layout in
  let project =
    Project_commands.with_client_settings ~setup ~platform @@
    match setup.project_root with
    | Some dirname ->
        Project.in_ ~dirname ~layout ()
    | None ->
        (* Projects are looked up from the directory of a file *)
        Project.for_filename ~layout @@
        Project.absolute_dirname (platform.getcwd ()) ^ "/_"
  in
  List.sort String.compare @@
  Superbol_project.Walk.source_files ~platform ~layout project

let read_list ~(platform: Cobol_common.Platform.TYPES.platform) ~sep source =
  let contents =
    try
      if source = "-"
      then platform.with_stdin ~f:In_channel.input_all
      else In_channel.with_open_bin source In_channel.input_all
    with Sys_error msg ->
      Cobol_common.fatal "Unable to read list of files: %s" msg
  in
  let strip_cr s =
    let n = String.length s in
    if sep = '\n' && n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1)
    else s
  in
  List.filter ((<>) "") @@
  List.map strip_cr @@
  String.split_on_char sep contents

let dedup files =
  let seen = Hashtbl.create (List.length files) in
  List.filter begin fun file ->
    let key = Project.absolute_dirname file in
    not (Hashtbl.mem seen key) && (Hashtbl.add seen key (); true)
  end files

let list_args () =
  let lists = ref [] and sep = ref '\n' in
  let args = [
    ["files-from"], Arg.String (fun s -> lists := s :: !lists),
    EZCMD.info ~docv:"LIST"
      "Also consider the files listed in file LIST, one per line, or on \
       standard input if LIST is `-'; relative names are relative to the \
       current directory.  May be given several times";

    ["null"; "0"], Arg.Unit (fun () -> sep := '\000'),
    EZCMD.info "Names in lists given with `--files-from' are separated by NUL \
                characters instead of newlines (as output by `find -print0')";
  ] in
  let get ~platform =
    match List.rev !lists with
    | [] -> None
    | lists -> Some (List.concat_map (read_list ~platform ~sep:!sep) lists)
  in
  get, args

let file_options = ["files-from"; "null"; "walk-project"]

(* Options may be abbreviated, and given their value after `=' *)
let designates_files words =
  List.exists begin fun word ->
    word = "-0" ||
    String.starts_with ~prefix:"--" word &&
    let name = List.hd @@ String.split_on_char '=' @@
      String.sub word 2 (String.length word - 2) in
    name <> "" &&
    List.exists (String.starts_with ~prefix:name) file_options
  end words

let get ?(docv = "FILES") ?(doc = "Cobol files to consider")
    ?(walk = true) () =
  let files = ref [] and walking = ref false in
  let get_listed, list_args = list_args () in
  let walk_args = [
    ["walk-project"], Arg.Set walking,
    EZCMD.info "Also consider every source file of the project (the one given \
                with `--project-root', or else the one of the current \
                directory), as the LSP server does: according to the extensions \
                that the platform and project configuration deem source files, \
                and skipping hidden files, directories excluded by the \
                configuration, and the working directory of the project";
  ] in
  let args = [
    [], Arg.Anons (fun list -> files := list),
    EZCMD.info ~docv doc;
  ] @ list_args @ (if walk then walk_args else []) in
  let get ~(common: Common_args.t) ~setup =
    let listed = get_listed ~platform:common.platform in
    let walked = if !walking then walk_project ~common ~setup else [] in
    let all =
      dedup (!files @ Option.value listed ~default:[] @ walked) in
    if all = [] && (!walking || Option.is_some listed) then
      Cobol_common.Diagnostics.Now.warn Fmt.stderr "No@ file@ to@ consider";
    all
  in
  get, args
