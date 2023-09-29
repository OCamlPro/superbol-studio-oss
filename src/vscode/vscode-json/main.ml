(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This source code is licensed under the MIT license found in the       *)
(*  LICENSE.md file in the root directory of this source tree.            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1
open EzFile.OP

type 'a result =
  | Ok of 'a
  | Error of string

type errors = (* warnings *) string list * (* errors *) string list

module Json_encoding = struct

  include Json_encoding

  let destruct encoding buf =
  try
    let json = Ezjsonm.from_string buf in
    Ok ( destruct encoding json )
  with
  | Cannot_destruct (path, exn)  ->
    let error =
      Printf.sprintf "Error during destruction path %s : %s"
        ( Json_query.json_pointer_of_path ~wildcards:true path )
        (Printexc.to_string exn)
    in
    Error error
  | Unexpected_field field ->
    let s = Printf.sprintf
        "Error during destruction path, unexpected field %S"
        field
    in
    Error s

end

let read_file file encoding =
  let s = EzFile.read_file file in
  Json_encoding.destruct encoding s

let write_file filename encoding p =
  let p = Json_encoding.construct encoding p in
  let s = Ezjsonm.value_to_string ~minify:false p in
  if filename = "-" then
    Printf.printf "%s\n%!" s
  else
    let oc = open_out filename in
    output_string oc s;
    close_out oc

type state = {
  dir : string ;
  verbose : bool ;
  mutable warnings : string list ;
  mutable errors : string list ;
}

let add_error ?(error=true) state fmt =
  Printf.kprintf (fun s ->
      if error then
        state.errors <- s :: state.errors
      else
        state.warnings <- s :: state.warnings
    ) fmt

let check_exists state ?field ?error file =
  if file.[0] = '$' then () (* a builtin file *)
  else
    let filename = state.dir // file in
    if not (Sys.file_exists filename ) then
      match field with
      | None ->
        add_error ?error state "File %S does not exist" filename
      | Some field ->
        add_error ?error state "File %S in field %S does not exist" filename field

let check_option_exists state ~field ?error file =
  match file with
  | None -> ()
  | Some file -> check_exists state ~field ?error file

let check_encoding state encoding ~field ?error file =
  check_exists state ~field
    ?error file ;
  let filename = state.dir // file in
  if Sys.file_exists filename then
    let s = EzFile.read_file filename in
    if state.verbose then
      Printf.eprintf "  Check encoding of %s\n%!" filename;
    match Json_encoding.destruct encoding s with
    | Ok _ -> ()
    | Error s ->
      add_error ?error state
        "Could not destruct %S in field %s:\n      %s" filename field s
    | exception exn ->
      add_error ?error state
        "File %S raised exception %s" filename (Printexc.to_string exn)

open Manifest
let check_project ?(verbose=false) file =
  if verbose then
    Printf.eprintf "Checking project file %S\n%!" file;
  match read_file file Manifest.vscode_enc with
  | Error s -> [], [s]
  | Ok p ->
    if verbose then
      Printf.eprintf "  File OK\n%!";
    let dir = Filename.dirname file in
    let dir = match dir with
      | "." | "./" -> ""
      | _ -> dir
    in
    let state = {
      verbose ;
      warnings = [] ;
      errors = [];
      dir ;
    } in
    check_exists state "README.md" ;
    check_exists state ~error:false "CHANGELOG.md" ;
    check_option_exists state ~field:"icon" p.marketplace.icon ;
    check_option_exists state ~field:"main" p.package.main ;
    check_option_exists state ~field:"browser" p.package.browser ;
    begin
      match p.contributes with
      | None -> ()
      | Some contributes ->

        List.iter (fun g ->
            check_encoding state Grammar.grammar_enc
              ~field:"p.contributes.grammars.path"
              g.grammar_path
          ) contributes.grammars ;

        List.iter (fun l ->
            match l.lang_configuration with
            | None -> ()
            | Some path ->
              check_encoding state Language.language_enc
                ~field:"p.contributes.languages.configuration"
                path
          ) contributes.languages ;

        List.iter (fun g ->
            check_exists state ~field:"p.contributes.iconThemes.path"
              g.iconTheme_path
          ) contributes.iconThemes ;

        List.iter (fun g ->
            check_exists state ~field:"p.contributes.productionIconThemes.path"
              g.pit_path
          ) contributes.productIconThemes ;

        List.iter (fun g ->
            check_encoding state Snippets.snippets_enc
              ~field:"p.contributes.snippets.path"
              g.snippet_path ;
          ) contributes.snippets ;

        List.iter (fun c ->
            match c.command_icon with
            | None -> ()
            | Some icon ->
              check_exists state
                ~field:"p.contributes.commands.icon.icon_light"
                icon.icon_light ;
              check_exists state
                ~field:"p.contributes.commands.icon.icon_dark"
                icon.icon_dark ;
          ) contributes.commands ;

        List.iter (fun d ->
            check_option_exists state
              ~field:"p.contributes.debuggers.program"
              d.debugger_program ;
          ) contributes.debuggers ;


    end ;
    state.warnings, state.errors

let check_file encoding pp file =
  match read_file file encoding with
  | Error s -> [], [s]
  | Ok p ->
    pp Format.std_formatter p;
    [], []
