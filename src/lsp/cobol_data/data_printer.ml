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

open Data_types

open Cobol_common.Srcloc.INFIX

let pp_offset = Data_memory.pp_offset
let pp_size = Data_memory.pp_size

let pp_int' = Cobol_ptree.pp_with_loc Fmt.int

let vfield ?label field =
  Pretty.vfield ~sep:(Fmt.any ": ") ?label field

let pp_braced_record fields =
  Fmt.(any "{@;<1 2>" ++ record fields ++ any "@;}")

type 'a conditional_field =
  | T of 'a Fmt.t
  | C of bool * 'a Fmt.t
  | I of bool * 'a Fmt.t * 'a Fmt.t

let pp_braced_record_with_conditional_fields fields =
  pp_braced_record @@ List.filter_map begin function
    | C (true, pp) | I (true, pp, _) | I (false, _, pp) | T pp -> Some pp
    | C (false, _) -> None
  end fields

let rec pp_item_definition: item_definition Pretty.printer = fun ppf x ->
  let item_qualname x = x.item_qualname
  and item_layout x = ~&(x.item_layout)
  and item_offset x = x.item_offset
  and item_size x = x.item_size
  and item_redefinitions x = x.item_redefinitions in
  pp_braced_record_with_conditional_fields [
    I (x.item_qualname <> None,
       Fmt.field "qualname" item_qualname (Fmt.option Cobol_ptree.pp_qualname),
       Fmt.(styled `Yellow @@ any "filler"));
    T (Fmt.field "offset" item_offset pp_offset);
    T (Fmt.field "size" item_size pp_size);
    T (vfield "layout" item_layout pp_item_layout);
    C (x.item_redefinitions <> [],
       vfield "redefs" item_redefinitions pp_item_definition_list);
  ] ppf x

and pp_item_definition_list: item_definition list Pretty.printer = fun ppf ->
  Fmt.(list ~sep:nop) pp_item_definition ppf

and pp_item_definitions: item_definitions Pretty.printer = fun ppf defs ->
  NEL.pp ~fopen:"" ~fsep:"" ~fclose:"" pp_item_definition ppf defs

and pp_item_layout: item_layout Pretty.printer = fun ppf -> function
  | Elementary { picture; value } ->
      pp_braced_record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "elementary");
        T (Fmt.field "category" (fun () -> picture.category)
             Data_picture.pp_category);
        C (value <> None,
           Fmt.field "value" (fun () -> value)
             (Fmt.option Cobol_ptree.pp_literal'));
      ] ppf ()
  | Struct { fields } ->
      pp_braced_record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ const string "structure");
        T (vfield "fields" Fun.id pp_item_definitions);
      ] ppf fields
  | FixedTable { items; length; value } ->
      pp_braced_record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "fixed-length table");
        T Fmt.(field "length" (fun () -> length) pp_int');
        T (vfield "items" (fun () -> items) pp_item_definitions);
        C (value <> None,
           vfield "value" (fun () -> value) Fmt.(option Cobol_ptree.pp_literal'));
      ] ppf ()
  | DependingTable { items; min_occurs; max_occurs; depending; value } ->
      pp_braced_record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "variable-length table");
        T Fmt.(field "min_occurs" (fun () -> min_occurs) pp_int');
        T Fmt.(field "max_occurs" (fun () -> max_occurs) pp_int');
        T Fmt.(field "depending" (fun () -> depending) Cobol_ptree.pp_qualname');
        T (vfield "items" (fun () -> items) pp_item_definitions);
        C (value <> None,
           vfield "value" (fun () -> value) Fmt.(option Cobol_ptree.pp_literal'));
      ] ppf ()
  | DynamicTable { items; capacity; min_capacity; max_capacity; initialized;
                   value } ->
      pp_braced_record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "dynamic-length table");
        C (capacity <> None,
           Fmt.(field "capacity" (fun () -> capacity)
                  (option Cobol_ptree.pp_qualname')));
        C (min_capacity <> None,
           Fmt.(field "min_capacity" (fun () -> min_capacity) (option pp_int')));
        C (max_capacity <> None,
           Fmt.(field "max_capacity" (fun () -> max_capacity) (option pp_int')));
        C (~&initialized, Fmt.any "initialized");
        T (vfield "items" (fun () -> items) pp_item_definitions);
        C (value <> None,
           vfield "value" (fun () -> value) Fmt.(option Cobol_ptree.pp_literal'));
      ] ppf ()

let pp_renamed_item_layout: renamed_item_layout Pretty.printer = fun ppf -> function
  | RenamedElementary { picture } ->
      pp_braced_record [
        Fmt.(styled `Yellow @@ any "elementary");
        Fmt.field "category" (fun () -> picture.category) Data_picture.pp_category;
      ] ppf ()
  | RenamedStruct { fields } ->
      pp_braced_record [
        Fmt.(styled `Yellow @@ const string "structure");
        (vfield "fields" Fun.id pp_item_definitions);
      ] ppf fields

let pp_record_renaming: record_renaming Pretty.printer = fun ppf r ->
  pp_braced_record_with_conditional_fields [
    T (Fmt.field "qualname" (fun _ -> r.renaming_name) Cobol_ptree.pp_qualname);
    T (Fmt.field "from" (fun _ -> r.renaming_from) Cobol_ptree.pp_qualname');
    C (r.renaming_thru <> None,
       Fmt.(field "thru" (fun _ -> r.renaming_thru)
              (option Cobol_ptree.pp_qualname')));
    T (Fmt.field "offset" (fun _ -> r.renaming_offset) Data_memory.pp_offset);
    T (Fmt.field "size" (fun _ -> r.renaming_size) Data_memory.pp_size);
    T (vfield "layout" (fun _ -> r.renaming_layout) pp_renamed_item_layout);
  ] ppf ()

let pp_record_renaming_list: record_renaming list Pretty.printer = fun ppf ->
  Fmt.(list ~sep:nop) pp_record_renaming ppf

let pp_record: record Pretty.printer = fun ppf r ->
  pp_braced_record_with_conditional_fields [
    T (Fmt.field "record" (fun x -> x.record_name) Fmt.string);
    T (Fmt.field "storage" (fun x -> x.record_storage) pp_data_storage);
    T (vfield "item" (fun x -> x.record_item) pp_item_definition);
    C (r.record_renamings <> [],
       vfield "renamings" (fun x -> x.record_renamings) pp_record_renaming_list);
  ] ppf r

let pp_item ppf = function
  | Data_item { def; record = { record_name; _ } } ->
      pp_braced_record [
        Fmt.(styled `Yellow @@ any "data item");
        Fmt.field "record" (fun () -> record_name) Fmt.string;
        vfield "def" (fun () -> def) pp_item_definition;
      ] ppf ()
  | Data_renaming { def; record = { record_name; _ } } ->
      pp_braced_record [
        Fmt.(styled `Yellow @@ any "data item renaming");
        Fmt.field "record" (fun () -> record_name) Fmt.string;
        vfield "def" (fun () -> def) pp_record_renaming;
      ] ppf ()
