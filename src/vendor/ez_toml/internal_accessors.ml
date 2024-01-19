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

let get node key_path =
  Internal_parsing.get_key_path ~loc:Internal_misc.noloc node key_path

let set ?(config = Internal_misc.default_config) node key_path
    ~value:internal_node =
  Internal_parsing.set_key_path
    ~config
    ~loc:Internal_misc.noloc
    node key_path ~value:internal_node

let remove ?(config = Internal_misc.default_config) node key_path =
  Internal_parsing.unset_key_path
    ~config
    ~loc:Internal_misc.noloc
    node key_path


let update ?(config = Internal_misc.default_config) node key_path
    v_opt =
  Internal_parsing.update_key_path
    ~config
    ~loc:Internal_misc.noloc
    node key_path v_opt

let value ?before ?pos v =
  Internal_misc.node ?before ?pos @@ v

let value_of_string s = String s
let value_of_bool b = Bool b
let value_of_int n = Int ( string_of_int n )
let value_of_float f = Float ( string_of_float f )
let value_of_date d =
  Date ( ISO8601.Permissive.string_of_datetimezone (d, 0.) )
let value_of_array t = Array t
let value_of_table t = Table t

let string ?before ?pos s =
  Internal_misc.node ?before ?pos @@ value_of_string s
let bool ?before ?pos b =
  Internal_misc.node ?before ?pos @@ value_of_bool b
let int ?before ?pos n =
  Internal_misc.node ?before ?pos @@ value_of_int n
let float ?before ?pos f =
  Internal_misc.node ?before ?pos @@ value_of_float f
let date ?before ?pos d =
  Internal_misc.node ?before ?pos @@ value_of_date d
let array ?before ?pos t =
  Internal_misc.node ?before ?pos @@ value_of_array t
let table ?before ?pos t =
  Internal_misc.node ?before ?pos @@ value_of_table t
let empty_table ?before ?pos () =
  Internal_misc.node ?before ?pos @@ Table StringMap.empty
let table_of_list ?before ?pos t =
  table ?before ?pos ( StringMap.of_list t )

let value_of_ints t = Array ( Array.map int t )
let value_of_strings t = Array ( Array.map string t )

let ints ?before ?pos t =
  Internal_misc.node ?before ?pos @@ value_of_ints t
let strings ?before ?pos t =
  Internal_misc.node ?before ?pos @@ value_of_strings t

let type_of_value value = match value with
  | Table _ -> "Table"
  | Array _ -> "Array"
  | String _ -> "String"
  | Bool _ -> "Bool"
  | Int _ -> "Int"
  | Float _ -> "Float"
  | Date _ -> "Date"

let type_of_node node = type_of_value node.node_value

let error_mismatch node expected =
  Internal_misc.error 17 ( Type_mismatch (node, expected) )

let error_convertion node expected =
  Internal_misc.error 18 ( Bad_convertion (node, expected) )

let extract_bool node =
  match node.node_value with
  | Bool s -> s
  | _ -> error_mismatch node "Bool"

let extract_int node =
  match node.node_value with
  | Int s ->
      ( try int_of_string s with _ -> error_convertion node "Int" )
  | _ -> error_mismatch node "Int"

let extract_float node =
  match node.node_value with
  | Float s ->
      ( try float_of_string s with _ -> error_convertion node "Float" )
  | _ -> error_mismatch node "Float"

let extract_date node =
  match node.node_value with
  | Date s ->
      ( try
          fst (ISO8601.Permissive.datetime_tz ~reqtime:false s)
        with _ -> error_convertion node "Date" )
  | _ -> error_mismatch node "Date"

let extract_table node =
  match node.node_value with
  | Table s -> s
  | _ -> error_mismatch node "Table"

let extract_array node =
  match node.node_value with
  | Array s -> s
  | _ -> error_mismatch node "Array"

let extract_string node =
  match node.node_value with
  | String s -> s
  | _ -> error_mismatch node "String"

let extract_strings node =
  Array.map extract_string @@ extract_array node

let extract_ints node =
  Array.map extract_int @@ extract_array node

let extract_value node = node.node_value

let extract_with_default extract ?default k node =
  match get k node with
  | v ->
    extract v
  | exception exn ->
    match default with
    | None -> raise exn
    | Some v -> v

let get_value = extract_with_default extract_value
let get_bool = extract_with_default extract_bool
let get_int = extract_with_default extract_int
let get_string = extract_with_default extract_string
let get_float = extract_with_default extract_float
let get_date = extract_with_default extract_date
let get_array = extract_with_default extract_array
let get_table = extract_with_default extract_table

let get_strings = extract_with_default extract_strings
let get_ints = extract_with_default extract_ints

let set_value ?before ?pos k node v = set k node ~value:(value ?before ?pos v)
let set_bool ?before ?pos k node v = set k node ~value:(bool ?before ?pos v)
let set_int ?before ?pos k node v = set k node ~value: (int ?before ?pos v)
let set_float ?before ?pos k node v = set k node ~value: (float ?before ?pos v)
let set_string ?before ?pos k node v = set k node ~value: (string ?before ?pos v)
let set_date ?before ?pos k node v = set k node ~value: (date ?before ?pos v)
let set_array ?before ?pos k node v = set k node ~value: (array ?before ?pos v)
let set_table ?before ?pos k node v = set k node ~value: (table ?before ?pos v)
let set_table_of_list ?before ?pos k node v =
  set k node ~value:(table_of_list ?before ?pos v)

let set_strings ?before ?pos k node v = set k node
    ~value:(strings ?before ?pos v)
let set_ints ?before ?pos k node v = set k node
    ~value:(ints ?before ?pos v)

let get_node_value node = node.node_value
let set_node_value node v = node.node_value <- v


let table_iter node f = StringMap.iter f ( extract_table node )
let array_iteri node f = Array.iteri f ( extract_array node )
let array_length node = Array.length ( extract_array node )

let add_comments node comments =
  node.node_comment_before <- node.node_comment_before @ comments

let add_eol_comment node comment =
  node.node_comment_after <- Some comment

let next_section_pos node =
  let table = extract_table node in
  let next_section_pos = ref 0 in
  StringMap.iter (fun _ node ->
      match node.node_value with
      | Table _ ->
        next_section_pos := max !next_section_pos (node.node_pos+1)
      | _ -> ()
    ) table;
  !next_section_pos

let maybe_add_section ?before section toml =
  match get toml [ section ] with
  | _node -> false
  | exception Not_found ->
    let node = empty_table ?before ~pos:(next_section_pos toml) () in
    set toml [ section ] ~value:node;
    true

let array_compare compare t1 t2 =
  let l1 = Array.length t1 in
  let l2 = Array.length t2 in
  let rec iter i l1 l2 t1 t2 compare =
    if i = l1 then
      if i = l2 then
        0
      else
        (-1)
    else
    if i = l2 then
      1
    else
      let v1 = t1.(i) in
      let v2 = t2.(i) in
      let c = compare v1 v2 in
      if c = 0 then
        iter (i+1) l1 l2 t1 t2 compare
      else
        c
  in
  iter 0 l1 l2 t1 t2 compare

let rec compare_value v1 v2 =
  match v1, v2 with
  | Table t1, Table t2 ->
    StringMap.compare compare_node_value t1 t2
  | Array t1, Array t2 ->
    array_compare compare_node_value t1 t2
  | _ -> compare v1 v2

and compare_node_value n1 n2 =
  compare_value ( extract_value n1 ) ( extract_value n2 )

let maybe_set_value ?before key_path toml value =
  match get key_path toml with
  | node ->
    if compare_value ( extract_value node ) value <> 0 then begin
      set_node_value node value ;
      true
    end else false
  | exception Not_found ->
    set_value ?before key_path toml value;
    true
