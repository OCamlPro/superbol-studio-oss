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

open EzCompat
open Types

(** Bare keys only allow [A-Za-z0-9_-]. *)
let key_is_bare key =
  let len = String.length key in
  let rec iter i key len =
    if i = len then
      true
    else
      match key.[i] with
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '_'
      | '-' -> iter (i+1) key len
      | _ -> false
  in
  iter 0 key len

let bprint_escape_char b char =
  match char with
  | '"' -> Buffer.add_string b "\\\""
  | '\\' -> Buffer.add_string b "\\\\"
  | '\b' -> Buffer.add_string b "\\b"
  | '\t' -> Buffer.add_string b "\\t"
  | '\n' -> Buffer.add_string b "\\n"
  | '\012' -> Buffer.add_string b "\\f"
  | '\r' -> Buffer.add_string b "\\r"
  | _ ->
    let code = Char.code char in
    if code <= 31 then
      Printf.bprintf b "\\u%04x" code
    else
      Buffer.add_char b char

(* Do we need some form of escaping for non-bare keys ? *)
let bprint_key_path b key_path =
  List.iteri (fun i key ->
      if i > 0 then Buffer.add_char b '.' ;
      if key = "" then
        Buffer.add_string b "''"
      else
      if key_is_bare key then
        Buffer.add_string b key
      else
      if String.contains key '"' then
        if String.contains key '\'' then
          Printf.kprintf failwith
            "Key %S cannot contain both simple and double quotes" key
        else
          Printf.bprintf b "'%s'" key
      else
        Printf.bprintf b "\"%s\"" key
    ) key_path

let string_of_key_path key_path =
  let b = Buffer.create 100 in
  bprint_key_path b key_path ;
  Buffer.contents b

let is_table_node node =
  match node.node_value, node.node_format with
  | Table _, Any -> true
  | _ -> false

let split_table table =
  let sections = ref [] in
  let simple_values = ref [] in
  StringMap.iter (fun key node ->
      let is_section =
        match node.node_value, node.node_format with
        | Table _, Any -> true
        | Array array, ( Any | Multiline )
          when
            Array.length array > 0
            && Array.for_all is_table_node array -> true
        | _ -> false
      in
      if is_section then
        sections := ( key, node ) :: !sections
      else
        simple_values := ( key, node ) :: !simple_values
    ) table ;

  let simple_values = List.rev !simple_values in
  let simple_values = List.stable_sort (fun (_,n1) (_,n2) ->
      compare n1.node_pos n2.node_pos) simple_values in
  let sections = List.rev !sections in
  let sections = List.stable_sort (fun (_,n1) (_,n2) ->
      compare n1.node_pos n2.node_pos) sections in
  (simple_values, sections)

(* Here, we assume that x.y.z = v will be parsed by putting all node
   information in the v node, and not intermediary table nodes *)
let rec extract_singleton key_path node =
  match node.node_value with
  | Table table ->
      if StringMap.cardinal table = 1 then
        let (key, node) = StringMap.min_binding table in
        extract_singleton ( key_path @ [ key ] ) node
      else
        ( key_path, node )
  | _ -> ( key_path, node )

let rec bprint_toplevel_table b config table =
  bprint_table b config [] ( split_table table )

and bprint_table b config prefix (simple_values, sections) =

  List.iter (fun (key,node) ->
      let key_path, node = extract_singleton [key] node in
      bprint_comment_before b config node ;
      bprint_key_path b key_path ;
      Printf.bprintf b " = ";
      bprint_value b config node.node_format InsideTable node.node_value ;
      bprint_comment_after b config node ;
    ) simple_values ;

  List.iter (fun (key,node) ->
      match node.node_value with
      | Table table ->
          bprint_comment_before b config node ;
          let (simple_values,sections) = split_table table in
          let key_path = prefix @ [key] in
          begin
            match simple_values, sections with
            | [], _ :: _ -> ()
            | _ ->
                Printf.bprintf b "[";
                bprint_key_path b key_path;
                Printf.bprintf b "]";
                bprint_comment_after b config node ;
          end;
          bprint_table b config key_path (simple_values, sections);
      | Array array ->
          let key_path = prefix @ [ key ] in
          Array.iter (fun node ->
              bprint_comment_before b config node ;
              Buffer.add_string b "[[";
              bprint_key_path b key_path ;
              Buffer.add_string b "]]";
              bprint_comment_after b config node ;
              let table = match node.node_value with
                | Table table -> table
                | _ -> assert false
              in
              bprint_table b config key_path ( split_table table )
            ) array
      | _ -> assert false
    ) sections ;
  ()

and bprint_comment_before b config node =
  List.iter (fun line ->
      if line = "" then
        Buffer.add_string b config.newline
      else
        Printf.bprintf b "#%s%s" line config.newline)
    node.node_comment_before

and bprint_comment_after b config node =
  begin match node.node_comment_after with
    | None -> ()
    | Some comment -> Printf.bprintf  b" #%s" comment
  end;
  Buffer.add_string b config.newline

and bprint_value b config format context value =
  match value with
  | Int int -> Buffer.add_string b int
  | Float float -> Buffer.add_string b float
  | Date date -> Buffer.add_string b date
  | Bool bool -> Printf.bprintf b "%b" bool
  | Table table ->
      Printf.bprintf b "{ ";
      let simple_values = ref [] in
      StringMap.iter (fun key node ->
          (* all values are simple ! *)
          simple_values := ( key, node ) :: !simple_values
        ) table ;
      let simple_values = List.rev !simple_values in
      let simple_values = List.stable_sort (fun (_,n1) (_,n2) ->
          compare n1.node_pos n2.node_pos) simple_values in

      List.iteri (fun i (key,node) ->
          let key_path, node = extract_singleton [key] node in
          if i > 0 then Buffer.add_string b ", ";
          bprint_key_path b key_path ;
          Printf.bprintf b " = ";
          bprint_value b config
            node.node_format InsideInlineTable node.node_value ;
        ) simple_values ;
      Printf.bprintf b " }"
  | Array array ->
      let allow_multiline = match format, context with
        | Any, InsideInlineTable
        | Inline, _
          -> false
        | _ -> true
      in
      Printf.bprintf b "[ ";
      Array.iteri (fun i node ->
          if i > 0 then Buffer.add_string b ", ";
          bprint_value b config node.node_format
            (if allow_multiline then
               InsideArray
             else
               InsideInlineTable) node.node_value ;
        ) array ;
      Printf.bprintf b " ]"
  | String string ->
      let has_newline = String.contains string '\n' in
      begin
        match format, context, has_newline with
        | Literal, _, _ (* for now, just use double-quotes. TODO *)

        | _, InsideInlineTable, _
        | _, InsideArray, _
        | _, _, false
        | Inline, _, _
          ->
            Buffer.add_char b '"';
            String.iter (bprint_escape_char b) string;
            Buffer.add_char b '"'
        | Any, InsideTable, true
        | Multiline, InsideTable, true
          ->
            Buffer.add_string b {|"""|};
            Buffer.add_string b config.newline;
            let rec iter nquotes value pos len =
              if pos < len then
                let c = value.[pos] in
                match c with
                | '"' ->
                    let nquotes =
                      if nquotes = 2 then begin
                        bprint_escape_char b c;
                        0
                      end else begin
                        bprint_escape_char b '"';
                        nquotes+1
                      end
                    in
                    iter nquotes value (pos+1) len
                | '\n' ->
                    Buffer.add_string b config.newline;
                    iter 0 value (pos+1) len
                | c ->
                    bprint_escape_char b c;
                    iter 0 value (pos+1) len
            in
            iter 0 string 0 (String.length string);
            Buffer.add_string b {|"""|}
      end

let string_of_node
    ?(config = Internal_misc.default_config)
    ?(format = Any)
    ?(context = InsideTable)
    node =
  let b = Buffer.create 10000 in
  begin
    match node.node_value, format, context with
    | Table table, Any, InsideTable ->
        bprint_toplevel_table b config table
    | value, _, _->
        bprint_value b config format context value
  end;
  Buffer.contents b

let string_of_value
    ?(config = Internal_misc.default_config)
    ?(format = Any)
    ?(context = InsideTable)
    value =
  let b = Buffer.create 10000 in
  bprint_value b config format context value;
  Buffer.contents b

let string_of_location loc =
  Printf.sprintf "File %S, line %s, characters %s"
    loc.file
    ( if loc.line_begin = loc.line_end then
        string_of_int loc.line_begin
      else
        Printf.sprintf "%d-%d" loc.line_begin loc.line_end )
    ( if loc.char_begin = loc.char_end then
        string_of_int loc.char_begin
      else
        Printf.sprintf "%d-%d" loc.char_begin loc.char_end )

let rec string_of_error error =
  match error with
  | Parse_error -> "Parse error"
  | Syntax_error msg -> Printf.sprintf "Syntax error: %s" msg
  | Invalid_lookup ->
      Printf.sprintf "Invalid lookup in something not a table"
  | Invalid_lookup_in_inline_array ->
      Printf.sprintf "Invalid lookup in inline array"
  | Key_already_exists key_path ->
      Printf.sprintf "Duplicate addition of key %S"
        (string_of_key_path key_path )
  | Invalid_key_set key ->
      Printf.sprintf "Invalid: setting key %S in something not a table" key
  | Invalid_table key_path ->
      Printf.sprintf "Invalid: %S is not a table"
        ( string_of_key_path key_path )
  | Append_item_to_non_array key_path ->
      Printf.sprintf
        "Appending a table array item %S to a non-array"
        ( string_of_key_path key_path )
  | Append_item_to_non_table_array key_path ->
      Printf.sprintf
        "Appending a table array item %S to a non-table array"
        ( string_of_key_path key_path )
  | Invalid_escaped_unicode u ->
      Printf.sprintf "Invalid escaped unicode \\u%s" u
  | Expected_error_before_end_of_file n ->
      Printf.sprintf "Expected error %d, but end of file found" n
  | Expected_error_did_not_happen n ->
      Printf.sprintf "Expected error %d, but no error happened on next line" n
  | Expected_error_but_another_error (n, loc, nn, error) ->
      Printf.sprintf "Expected error %d, but another error %d happened: %s at %s"
        n nn ( string_of_error error ) ( string_of_location loc )
  | Forbidden_escaped_character ->
      "Forbidden escaped character"
  | Unterminated_string ->
      "Unterminated string"
  | Control_characters_must_be_escaped _c ->
      "Control characters in (U+0000 to U+001F) must be escaped"
  | Duplicate_table_item key_path ->
      Printf.sprintf "Duplicate table declaration %S"
        ( string_of_key_path key_path )
  | Type_mismatch (value, expected) ->
      Printf.sprintf "Type mismatch, expecting %s, found %s"
        expected ( string_of_value value )
  | Bad_convertion (value, expected) ->
      Printf.sprintf "Bad convertion %s, found %s"
        expected ( string_of_value value )
  | Invalid_lookup_in_empty_array ->
      "Invalid lookup in empty array"
  | Key_already_exists_in_inline_table key_path ->
      Printf.sprintf "Duplicate key %S in inline table"
        (string_of_key_path key_path )
  | Invalid_use_of_extension extension ->
      Printf.sprintf "Invalid use of extension %S" extension

let edump toml =
  let rec dump_table indent toml =
    let indent2 = indent ^ "  " in
    Printf.eprintf "{\n";
    StringMap.iter (fun s node ->
        Printf.eprintf "%s%S -> " indent s;
        dump_node indent2 node;
        Printf.eprintf ";\n") toml;
    Printf.eprintf "%s}" indent;

  and dump_array indent toml =
    let indent2 = indent ^ "  " in
    Printf.eprintf "[\n";
    Array.iter (fun node ->
        dump_node indent2 node;
        Printf.eprintf ";\n") toml;
    Printf.eprintf "%s]" indent;

  and dump_node indent node =
    let indent2 = indent ^ "  " in
    Printf.eprintf "{\n";
    Printf.eprintf "%slocation = %s;\n" indent2
      ( string_of_location node.node_loc );
    Printf.eprintf "%svalue = " indent2;
    begin
      match node.node_value with
      | Table table ->
        Printf.eprintf "Table ";
        dump_table indent2 table
      | String b -> Printf.eprintf "String %S" b
      | Bool b -> Printf.eprintf "Bool %b" b
      | Int b -> Printf.eprintf "Int %S" b
      | Float b -> Printf.eprintf "Float %S" b
      | Date b -> Printf.eprintf "Date %S" b
      | Array t ->
        Printf.eprintf "Array ";
        dump_array indent t
    end;
    Printf.eprintf ";\n";
    Printf.eprintf "%s}" indent;
  in
  dump_node "" toml

let () =
  Printexc.register_printer (function
      | Types.Error (loc, _code, error) ->
        Some (
          Printf.sprintf "Error in TOML file %s: %s"
            ( string_of_location loc )
            ( string_of_error error )
        )
      | _ -> None )
