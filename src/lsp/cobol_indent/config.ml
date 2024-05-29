(**************************************************************************)
(*                                                                        *)
(*                        SuperBOL OSS Studio                             *)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro SAS                                       *)
(*                                                                        *)
(* All rights reserved.                                                   *)
(* This source code is licensed under the GNU Affero General Public       *)
(* License version 3 found in the LICENSE.md file in the root directory   *)
(* of this source tree.                                                   *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1
open EzFile.OP

open Types

let verbose = false

let fixed_format = {
  name = "fixed" ;
  free = false ;    (* whether there is an indicator or inline
                       comments *)
  skip_before = 6 ; (* skip 6 columns, 1..6 *)
  max_text_length = 65; (* skip columns 8..72 *)
}

let free_format = {
  name = "free" ;
  free = true ;
  skip_before = 0 ;
  max_text_length = 65536;
}

let variable_format = {
  fixed_format with
  name = "variable";
  max_text_length = fixed_format.max_text_length + (250-72) ;
}

let xcard_format = {
  fixed_format with
  name = "xcard";
  max_text_length = fixed_format.max_text_length + (255-72) ;
}

let cobolx_format = {
  fixed_format with
  name = "cobolx";
  skip_before = 0 ;
  max_text_length = 255 ;
}

let default = {
  scan_for_indent = true;
  verbosity = 1;

  source_format = fixed_format ;
  arg_offset = 3 ;
  inner_offset = 3 ;
  data_item_offset = None ;
  select_in_area_a = false ;

  whole_file_indent = 0 ;
}

let to_string config =
  let b = Buffer.create 1000 in
  Printf.bprintf b "{\n";
  Printf.bprintf b "  source_format = %s ;\n" config.source_format.name ;
  Printf.bprintf b "  arg_offset = %d ;\n" config.arg_offset ;
  Printf.bprintf b "  inner_offset = %d ;\n" config.inner_offset ;
  Printf.bprintf b "  data_item_offset = %s ;\n" (match config.data_item_offset with
      | None -> "gap"
      | Some n -> string_of_int n);
  Printf.bprintf b "  select_in_area_a = %b ;\n" config.select_in_area_a ;
  Printf.bprintf b "  whole_file_indent = %d ;\n" config.whole_file_indent ;
  Printf.bprintf b "}";
  Buffer.contents b


let load ~source_format ~filename =
  if verbose then
    Printf.eprintf "Config.load for %S\n%!" filename;
  let filename =
    if Filename.is_relative filename then
      ( Sys.getcwd ()) // filename
    else
      filename
  in
  let dirname = Filename.dirname filename in
  let default = {
    default with source_format
  }
  in

  let rec iter dir =
    let dirdir = Filename.dirname dir in
    if dir = dirdir then
      default
    else
      let config = iter dirdir in
      let file = Filename.concat dir ".superbol-indent" in
      if Sys.file_exists file then
        let config = ref config in
        if verbose then
          Printf.eprintf "Reading file %S\n%!" file;
        EzFile.iter_lines (fun line ->
            let len = String.length line in
            if len > 0 then
              if line.[0] != '#' then
                let option, value = EzString.cut_at line '=' in
                let option = String.trim option in
                let value = String.trim value in
                match String.lowercase_ascii option,
                      String.lowercase_ascii value
                with
                | "format", ( "fixed" | "cobol85" ) ->
                  config := { !config with source_format = fixed_format }
                | "format", "free" ->
                  config := { !config with source_format = free_format }
                | "format", "variable" ->
                  config := { !config with source_format = variable_format }
                | "format", "xcard" ->
                  config := { !config with source_format = xcard_format }
                | "format", "cobolx" ->
                  config := { !config with source_format = cobolx_format }
                (* xopen, crt and terminal are not yet supported.

                   https://get-superbol.com/gnucobol/manual/chapter2.html?source-format
                *)


                | "whole-file-indent", n ->
                  config := { !config with whole_file_indent = int_of_string n }


                | "arg-offset", n ->
                  config := { !config with arg_offset = int_of_string n }

                | "select-in-area-a", ( "true" | "yes" | "y" ) ->
                  config := { !config with select_in_area_a = true }

                | ( "data-item-offset" ), n ->
                  config := { !config with
                              data_item_offset = Some ( int_of_string n ) }

                | ( "inner-offset" ), n ->
                  config := { !config with inner_offset = int_of_string n }

                | "format", format ->
                  Printf.eprintf "Warning in %S: unknown format %S\n%!"
                    file format
                | _ ->
                  Printf.eprintf "Warning in %S: unknown option %S\n%!"
                    file option
          ) file;
        !config
      else
        config
  in
  iter dirname

let generate ?(config=default) ?(only_comment = true) filename =
  let action = if only_comment then "# " else "" in
  let oc = open_out filename in

  let string_of_int_option x =
    match x with
    | None -> ""
    | Some n -> string_of_int n
  in

  List.iter (fun (name, comment, value, default) ->
      Printf.fprintf oc
        "### %s: %s\n" name comment;
      Printf.fprintf oc "%s%s = %s\n\n"
        (if value <> default then
           action else "# ") name
        value ;
    )

    [
      "format",
      "either 'fixed', 'free', 'variable', 'xcard', 'cobolx'",
      config.source_format.name,
      default.source_format.name ;

      "whole-file-indent",
      "apply the given indent to the whole file",
      string_of_int config.whole_file_indent,
      string_of_int default.whole_file_indent ;

      "arg-offset",
      "offset of arguments",
      string_of_int config.arg_offset,
      string_of_int default.arg_offset ;

      "inner-offset",
      "offset of inner commands in blocks",
      string_of_int config.inner_offset,
      string_of_int default.inner_offset ;

      "data-item-offset",
      "offset of inner data items (comment for aligned)",
      string_of_int_option config.data_item_offset,
      string_of_int_option default.data_item_offset ;

      "select-in-area-a",
      "force select in area A instead of area B",
      string_of_bool config.select_in_area_a,
      string_of_bool default.select_in_area_a ;

    ] ;

  close_out oc;
  Printf.eprintf "File %S generated\n%!" filename



open Cobol_config.Types

let source_format format =
  let source_format = match format with
    | Auto -> SFFixed (* TODO : auto-detect *)
    | SF sf -> sf
  in
  match source_format with
  | SFFixed -> fixed_format
  | SFFree -> free_format
  | SFVariable -> variable_format
  | SFxCard -> xcard_format
  | SFCOBOLX -> cobolx_format
  | SFXOpen
  | SFCRT
  | SFTrm (* terminal *)
    -> failwith "format not supported"
