(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1
open Types
open EzCompat

module TOML = struct

  module Types = Types

  include Internal_printer
  include Internal_accessors

  let default_config = Internal_misc.default_config
  let override_config = {
    default_config with
    silent_errors = EzCompat.IntSet.add 4 default_config.silent_errors }

  let of_string ?file ?(config = default_config) string =
    Internal_lexing.init ();
    let lexbuf = Lexing.from_string string in
    begin
      match file with
      | None -> ()
      | Some file ->
          (* Lexing.set_filename lexbuf file available at 4.11 *)
          let open Lexing in
          lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file}
    end;
    try
      let eof = ref false in
      let tomlex lexbuf =
        if !eof then Internal_parser.EOF else
          let token = Internal_lexer.tomlex lexbuf in
          if token = EOF then begin
            eof := true ;
            EOL
          end else
            token
      in
      let lines = Internal_parser.toml tomlex lexbuf in
      if config.debug then Internal_parsing.eprint_lines lines;
      let loc = Internal_lexing.loc_of_lexbuf lexbuf in
      let node = Internal_parsing.table_of_lines ~loc config lines in
      let loc2 = Internal_lexing.loc_of_lexbuf lexbuf in
      Internal_lexing.expand_loc loc loc2;
      node
    with
    | Internal_parser.Error ->
        let loc = Internal_lexing.loc_of_lexbuf lexbuf in
        Internal_misc.error ~loc 0 Parse_error
    | Failure msg ->
        let loc = Internal_lexing.loc_of_lexbuf lexbuf in
        Internal_misc.error ~loc 1 ( Syntax_error msg )

  let of_file ?config file =
    let s = EzFile.read_file file in
    of_string ?config ~file s

  let to_string node = Internal_printer.string_of_node node
  let to_file node file =
    let s = to_string node in
    EzFile.write_file file s

  let noloc = Internal_misc.noloc
  let node = Internal_misc.node

end

module EZTOML = struct

  module TYPES = struct

    type toml_file = {
      toml: node;                     (* warning: hides even more mutable stuff *)
      mutable checksum: Digest.t;
      mutable update_hooks: (string * (toml_file -> bool)) list;
    }

    type 'value section_option = {
      option_name: string;
      option_comments: string list;
      option_getter: ('value -> TOML.Types.value);
      option_setter: ('value -> TOML.Types.node -> unit);
    }

    type 'value section = {
      section_name: string;
      section_comments: string list;
      section_options: 'value section_option list;
    }

  end

  include TYPES

  let checksum th = th.checksum
  let toml th = th.toml

  let add_update_hook toml name hook =
    toml.update_hooks <- toml.update_hooks @ [ name, hook ]

  let option ?(comments = []) ~getter ~setter name =
    { option_name = name;
      option_comments = comments;
      option_getter = getter;
      option_setter = setter;
    }

  let section ~name ?(comments = []) options =
    { section_name = name;
      section_comments = comments;
      section_options = options }

  (* --- *)

  let make_empty () =
    { toml = TOML.node (Table StringMap.empty);
      checksum = Digest.string "";
      update_hooks = [] }

  let load ?(verbose = false) filename =
    if verbose then
      Printf.eprintf "Trying to load `%s'...%!" filename;
    try
      (* Use exception to check file existence while attempting to read. *)
      let string = EzFile.read_file filename in
      if verbose then Printf.eprintf " done\n%!";
      { toml = TOML.of_string ~file:filename string;
        checksum = Digest.string string;
        update_hooks = [] }
    with Sys_error _ when not (EzFile.exists filename) ->
      (* Only intercept and return empty if the file does not exist, and let every
         other case escape. *)
      if verbose then Printf.eprintf " not found\n%!";
      make_empty ()

  let save ?(verbose = false) filename toml =
    assert ( toml.update_hooks != [] );

    let modified =
      List.fold_left (fun modified (name, f) ->
          try f toml || modified
          with e ->
            Printf.eprintf "Exception raised in update hook %s:\n %s\n%!"
              name (Printexc.to_string e);
            raise e
        ) false toml.update_hooks
    in

    if modified then
      let s = TOML.to_string toml.toml in
      EzFile.safe_mkdir (Filename.dirname filename);
      EzFile.write_file filename s;
      if verbose then
        Printf.eprintf "Updated file `%s'\n%!" filename;
      toml.checksum <- Digest.string s

  (* --- *)

  let update_section
      { section_name; section_comments; section_options }
      { toml; _ } value =
    let modified =
      TOML.maybe_add_section section_name toml ~before:("" :: section_comments)
    in
    List.fold_left
      (fun modified { option_name; option_comments;
                      option_getter ; _ } ->

        let option_value = option_getter value in
        TOML.maybe_set_value toml [section_name; option_name] option_value
          ~before: ("" :: option_comments) || modified
      ) modified section_options

  let add_section toml_file value section =
    add_update_hook toml_file
      (Printf.sprintf "[%s]" section.section_name)
      (fun toml_file ->
         update_section section toml_file value);

    List.iter (fun option ->
        match TOML.get toml_file.toml [ section.section_name ;
                         option.option_name ] with
          | exception _ -> ()
          | option_value ->
            option.option_setter value option_value
      ) section.section_options


  let option_string ?comments name ~getter ~setter =
    option ?comments name
      ~getter:(fun c -> TOML.value_of_string (getter c))
      ~setter:(fun c v ->  setter c (TOML.extract_string v))

  let option_string_map ?comments name ~getter ~setter =
    option ?comments name
      ~getter:(fun c ->
          TOML.Types.Table ( StringMap.map TOML.string (getter c)))
      ~setter:(fun c v ->
          setter c (
            StringMap.map TOML.extract_string
              ( TOML.extract_table v )))

  let option_string_option ?comments name ~getter ~setter =
    option ?comments name
      ~getter:(fun c -> TOML.value_of_string (
          match getter c with
          | None -> ""
          | Some s -> s))
      ~setter:(fun c v ->  setter c (match TOML.extract_string v with
          | "" -> None
          | s -> Some s))

  let option_int ?comments name ~getter ~setter =
    option ?comments name
      ~getter:(fun c -> TOML.value_of_int (getter c))
      ~setter:(fun c v ->  setter c (TOML.extract_int v))

  let option_bool ?comments name ~getter ~setter =
    option ?comments name
      ~getter:(fun c -> TOML.value_of_bool (getter c))
      ~setter:(fun c v ->  setter c (TOML.extract_bool v))

end
