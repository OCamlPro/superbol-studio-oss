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

open Internal_types

open EzCompat
open Types

let is_table_node node = match node.node_value with
  | Table _ -> true
  | _ -> false

let rec get_node_table ~loc node =
  match node.node_value with
  | Array array ->
      let len = Array.length array in
      if len = 0 then
        Internal_misc.error ~loc 19 Invalid_lookup_in_empty_array ;
      if node.node_format = Inline then
        Internal_misc.error ~loc 3 Invalid_lookup_in_inline_array ;
      get_node_table ~loc array. ( len - 1 )
  | Table table -> node, table
  | _ -> Internal_misc.error ~loc 2 Invalid_lookup

let get_key ~loc node key =
  let _node, table = get_node_table ~loc node in
  StringMap.find key table

let set_key ~loc ~config node key v =
  let node, table = get_node_table ~loc node in
  if not ( IntSet.mem 4 config.silent_errors )
  && StringMap.mem key table then
    Internal_misc.error ~loc 4 ( Key_already_exists [ key ] ) ;
  node.node_value <- Table ( StringMap.add key v table )

let rec get_key_path ~loc node key_path =
  match key_path with
  | [] -> node
  | key :: key_path ->
      let node = get_key ~loc node key in
      get_key_path ~loc node key_path

let rec set_key_path ~loc ~config ?(op=OpEqual) table_node key_path ~value:v =
  match key_path with
  | [] -> assert false
  | [ key ] ->
      (* TODO: we should probably dealt with the case where the node
         is not a table. We could have a specific error for trying to
         set a value to a table, and not error if silent_errors *)
      let table_node, table = get_node_table ~loc table_node in
      let continue =
        match op with
        | OpEqual ->
            if not ( IntSet.mem 4 config.silent_errors ) &&
               StringMap.mem key table then
              Internal_misc.error ~loc 4 ( Key_already_exists key_path ) ;
            true
        | OpInit -> not ( StringMap.mem key table )
        | OpSet -> true
        | OpUnset -> assert false
      in
      if continue then begin
        table_node.node_value <- Table ( StringMap.add key v table );
        Internal_lexing.expand_loc table_node.node_loc v.node_loc
      end
  | key :: key_path ->
      let new_table_node = try
          let node = get_key ~loc table_node key in
          Internal_lexing.expand_loc node.node_loc v.node_loc ;
          node
        with Not_found ->
          (*          Printf.eprintf "New table for %s\n%!" key; *)
          let new_node = Internal_misc.node ~loc @@ Table StringMap.empty in
          set_key ~loc ~config table_node key new_node;
          new_node
      in
      set_key_path ~loc ~config ~op new_table_node key_path ~value:v


let rec unset_key_path ~loc ~config table_node key_path =
  match key_path with
  | [] -> assert false
  | [ key ] ->
      let table_node, table = get_node_table ~loc table_node in
      table_node.node_value <- Table ( StringMap.remove key table )
  | key :: key_path ->
      match get_key ~loc table_node key with
      | exception Not_found -> ()
      | new_table_node ->
          unset_key_path ~loc ~config new_table_node key_path

let rec update_key_path ~loc ~config table_node key_path (v_opt : node option) =
  match key_path with
  | [] -> assert false
  | [ key ] ->
      (* TODO: we should probably dealt with the case where the node
         is not a table. We could have a specific error for trying to
         set a value to a table, and not error if silent_errors *)
      begin match table_node.node_value with
        | Table table ->
            begin
              match v_opt with
              | Some new_node ->
                  { table_node with
                    node_value = Table (StringMap.add key
                                          new_node table)
                  }
              | None ->
                  match StringMap.find key table with
                  | exception Not_found -> table_node
                  | _old_v ->
                      { table_node with
                        node_value = Table (StringMap.remove key table)
                      }
            end
        | Array array ->
            let len = Array.length array in
            if len = 0 then
              Internal_misc.error ~loc 19 Invalid_lookup_in_empty_array ;
            if table_node.node_format = Inline then
              Internal_misc.error ~loc 3 Invalid_lookup_in_inline_array ;
            let old_node = array.(len-1) in
            let new_node = update_key_path
                ~loc ~config old_node key_path v_opt in
            if old_node == new_node then
              table_node
            else
              let new_array = Array.copy array in
              new_array.(len-1) <- new_node;
              { table_node with node_value = Array new_array }

        | _ -> Internal_misc.error ~loc 2 Invalid_lookup
      end

  | key :: internal_key_path ->

      begin match table_node.node_value with
        | Table table ->
            let old_node =
              match StringMap.find key table with
              | exception Not_found ->
                  Internal_misc.node ~loc @@ Table StringMap.empty
              | node -> node
            in
            let new_node = update_key_path
                ~loc ~config old_node internal_key_path v_opt in
            { table_node with
              node_value = Table (StringMap.add key new_node table)
            }
        | Array array ->
            let len = Array.length array in
            if len = 0 then
              Internal_misc.error ~loc 19 Invalid_lookup_in_empty_array ;
            if table_node.node_format = Inline then
              Internal_misc.error ~loc 3 Invalid_lookup_in_inline_array ;
            let old_node = array.(len-1) in
            let new_node = update_key_path
                ~loc ~config old_node key_path v_opt in
            if old_node == new_node then
              table_node
            else
              let new_array = Array.copy array in
              new_array.(len-1) <- new_node;
              { table_node with node_value = Array new_array }

        | _ -> Internal_misc.error ~loc 2 Invalid_lookup
      end

let rec table_mem_key_path ~loc table key_path =
  match key_path with
  | [] -> true
  | key :: key_path ->
      match StringMap.find key table with
      | exception Not_found -> false
      | node ->
          match node.node_value with
          | Table table -> table_mem_key_path ~loc table key_path
          | _ ->
              Internal_misc.error ~loc 5 (Invalid_key_set key)

let val_node v value =
  Internal_misc.node ~loc:v.loc value

let rec node_of_inline config v =
  match v.txt with
  | IBool b -> val_node v @@ Bool b
  | IString (_, s) -> val_node v @@ String s
  | IInt s -> val_node v @@ Int s
  | IFloat s -> val_node v @@ Float s
  | IDate s -> val_node v @@ Date s
  | IArray array ->
      val_node v @@ Array ( Array.of_list array |>
                Array.map (node_of_inline config ))
  | ITable table ->
      let table_node = val_node v @@ Table StringMap.empty in
      List.iter (fun bind ->
          let var = bind.bind_var in
          let loc = var.loc in
          let v = bind.bind_val in
          begin match get_key_path ~loc:var.loc table_node var.txt with
            | exception Not_found -> ()
            | _ ->
                if not ( IntSet.mem 20 config.silent_errors ) then
                  Internal_misc.error ~loc 20
                    ( Key_already_exists_in_inline_table var.txt );
          end;
          let v = node_of_inline config v in
          set_key_path ~loc:var.loc ~config table_node var.txt ~value:v
        ) table ;
      table_node

(*
let set_key_path ~loc table_node key_path ~value =
  Printf.eprintf "set_key_path %s\n%!"
    ( string_of_key_path key_path );
  set_key_path ~loc table_node key_path ~value
*)

let line_node line key_path v =
  Internal_misc.node
    ~loc:line.line_operation_loc
    ~before:line.line_comments_before
    ?after:line.line_comment_after
    ~format:Any
    ~name:key_path
    v

let eprint_comments comments =
  List.iter (fun { txt = comment; _ } ->
      if comment = "" then
        Printf.eprintf "DEBUG \n"
      else
        Printf.eprintf "DEBUG %s\n" comment
    ) comments

let eprint_lines (comments, lines) =
  eprint_comments comments;
  List.iter (fun (line, comments) ->
      begin
        match line.line_operation with
        | Array_item key_path ->
            Printf.eprintf "DEBUG [[ %s ]]\n%!"
              ( Internal_printer.string_of_key_path key_path )
        | Table_item key_path ->
            Printf.eprintf "DEBUG [ %s ]\n%!"
              ( Internal_printer.string_of_key_path key_path )
        | Set bind ->
            Printf.eprintf "DEBUG %s = ...\n%!"
              ( Internal_printer.string_of_key_path bind.bind_var.txt )
        | Error_item error ->
            Printf.eprintf "DEBUG [!%d]\n%!" error
      end;
      eprint_comments comments;
    ) lines

let txt { txt ; _ } = txt

let table_of_lines ~loc config (comments, lines) =
  let top_node = Internal_misc.node ~loc @@ Table StringMap.empty in

  let rec iter ( prefix : key_path ) before_comments lines =
    match lines with
    | [] -> ()
    | (line, comments) :: lines ->
        (*
        Printf.eprintf "New line:\n";
        eprint_lines ( [], [ line, comments ]);
*)
        match comments with
        | [] -> assert false
        | after_comment :: next_comments ->
            line.line_comments_before <- List.map txt before_comments ;
            if after_comment.txt <> "" then
              line.line_comment_after <- Some after_comment.txt;
            let loc = line.line_operation_loc in
            match line.line_operation with
            | Array_item key_path ->
                let array_node = try get_key_path ~loc top_node key_path with
                  | Not_found ->
                      let line_without_comments =
                        { line with
                          line_comments_before = [];
                          line_comment_after = None } in
                      let array_node = line_node line_without_comments
                          key_path @@ Array [||] in
                      set_key_path ~loc ~config top_node key_path ~value:array_node;
                      array_node
                in
                begin
                  match array_node.node_value with
                  | Array old_array ->
                      let table_node =
                        line_node line key_path @@ Table StringMap.empty in

                      if not ( IntSet.mem 8 config.silent_errors ) &&
                         not ( Array.for_all is_table_node old_array ) then
                        Internal_misc.error ~loc 8
                          ( Append_item_to_non_table_array key_path );

                      array_node.node_value <-
                        Array ( Array.concat [ old_array ;
                                               [| table_node |] ] )
                  | _ ->
                      Internal_misc.error ~loc 7
                        ( Append_item_to_non_array key_path )
                end;
                iter key_path next_comments lines
            | Table_item [] ->
                if not ( IntSet.mem 21 config.silent_errors ) then
                  Internal_misc.error ~loc 21 ( Invalid_use_of_extension "[]") ;
                iter [] ( before_comments @ comments ) lines
            | Table_item key_path ->
                begin
                  match get_key_path ~loc top_node key_path with
                  | exception Not_found ->
                      let table_node =
                        line_node line key_path @@ Table StringMap.empty in
                      set_key_path ~loc ~config top_node key_path ~value:table_node
                  | table_node ->
                      match table_node.node_value with
                      | Table _ ->
                          if not ( IntSet.mem 16 config.silent_errors ) then
                            Internal_misc.error ~loc 16
                              ( Duplicate_table_item key_path )
                      | _ ->
                          Internal_misc.error ~loc 6 ( Invalid_table key_path )
                end;
                iter key_path next_comments lines
            | Set bind ->
                begin

                  begin
                    match bind.bind_op with
                    | OpEqual -> ()
                    | op ->
                        if not ( IntSet.mem 21 config.silent_errors ) then
                          Internal_misc.error ~loc 21
                            ( Invalid_use_of_extension (match op with
                                  | OpInit -> "=="
                                  | OpSet -> ":="
                                  | OpUnset -> "-="
                                  | _ -> assert false)
                            ) ;
                  end;
                  let var = bind.bind_var in
                  let v = bind.bind_val in
                  let key_path = prefix @ var.txt in
                  begin
                    match bind.bind_op with
                    | OpUnset ->
                        let keys = match bind.bind_val.txt with
                          | IString (_, s) -> [s]
                          | IArray array ->
                              List.map (function
                                    { txt = IString (_,s); _ } -> s
                                  | _ ->
                                      failwith "-= expects an array of strings"
                                ) array
                          | _ -> failwith "-= expects an array of strings"
                        in
                        List.iter (fun key ->
                            unset_key_path ~loc ~config top_node ( key_path @ [key])
                          ) keys
                    | op ->
                        let node = line_node line key_path
                            ( node_of_inline config v ).node_value in
                        set_key_path ~loc ~config ~op top_node key_path ~value:node
                  end
                end;
                iter prefix next_comments lines
            | Error_item n ->
                match lines with
                | [] ->
                    Internal_misc.error ~loc 10
                      ( Expected_error_before_end_of_file n )
                | (line, comments) :: lines ->
                    (* eprint_lines [ line ]; *)
                    match iter prefix []
                            [ line,
                              [ { txt = "" ; loc =
                                               Internal_misc.noloc } ] ] with
                    | () ->
                        Internal_misc.error ~loc 11
                          ( Expected_error_did_not_happen n )
                    | exception Error (loc, nn, error ) ->
                        if n <> nn then
                          Internal_misc.error ~loc 12
                            ( Expected_error_but_another_error (n, loc, nn, error ));
                        iter prefix ( List.tl comments ) lines
  in
  iter [] comments lines;
  top_node
