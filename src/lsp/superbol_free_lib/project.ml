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

module DIAGS = Cobol_common.Diagnostics

let try_load ~f arg =
  try f arg with
  | Superbol_project.Config.ERROR e ->
      DIAGS.(pp Fmt.stderr @@ One.error "%a"
               Superbol_project.Diagnostics.pp_error e);
      exit 2
  | Sys_error msg | Invalid_argument msg ->
      DIAGS.(pp Fmt.stderr @@ One.error "%s" msg);
      exit 2

(* Direct loading (based on directory name) *)

let absolute_dirname dirname =
  let dirname = Slashifier.slashify dirname in
  let dirname =
    if EzFile.is_absolute dirname then dirname
    else Slashifier.slashify (Sys.getcwd ()) ^ "/" ^ dirname
  in
  (* Spelled without `.' components nor trailing separators, as the LSP server
     spells the directories it gets from its client. *)
  let prefix =                        (* `//' starts UNC paths: `//host/a' *)
    if String.starts_with ~prefix:"//" dirname then "//"
    else if String.starts_with ~prefix:"/" dirname then "/"
    else ""                         (* drive letter first, as in `C:/foo' *)
  in
  let parts =
    List.filter (fun p -> p <> "" && p <> ".") @@
    String.split_on_char '/' dirname
  in
  prefix ^ String.concat "/" parts

let in_ ~dirname ?(layout = Superbol_project.Layout.default) () =
  try_load () ~f:begin fun () ->
    let dirname = absolute_dirname dirname in
    let rootdir = Superbol_project.rootdir_at ~dirname in
    let project =
      Superbol_project.for_ ~platform:Superbol_platform.record ~rootdir ~layout
    in
    DIAGS.show_n_forget project ~platform:Superbol_platform.record
  end

let load ?(dirname = EzFile.current_dir_name) = in_ ~dirname

(* Indirect loading (based on file name, looking up the directory hierarchy) *)

let for_ ~filename ?(layout = Superbol_project.Layout.default) () =
  try_load () ~f:begin fun () ->
    let rootdir = Superbol_project.rootdir_for ~filename ~layout in
    let project =
      Superbol_project.for_ ~platform:Superbol_platform.record ~rootdir ~layout
    in
    DIAGS.show_n_forget project ~platform:Superbol_platform.record
  end

let for_filename ?layout filename = for_ ~filename ?layout ()
