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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

let pp_offset = Data_memory.pp_offset
let pp_size = Data_memory.pp_size

let pp_int' = Cobol_ptree.pp_with_loc Fmt.int
let pp_int'_opt = Fmt.option pp_int'
let pp_qualname'_opt = Fmt.option Cobol_ptree.pp_qualname'
let pp_literal'_opt = Fmt.option Cobol_ptree.pp_literal'

let pp_usage: usage Pretty.printer = fun ppf -> function
  | Usage picture ->
      Pretty.record [
        Fmt.((styled `Yellow @@ any "display") ++ any " (dev: temporary)");
        Fmt.field "category" (fun () -> picture.category) Data_picture.pp_category;
      ] ppf ()

let rec pp_item_definition: item_definition Pretty.printer = fun ppf x ->
  let item_qualname x = x.item_qualname
  and item_redefines x = x.item_redefines
  and item_layout x = x.item_layout
  and item_offset x = x.item_offset
  and item_size x = x.item_size
  and item_redefinitions x = x.item_redefinitions in
  Pretty.record_with_conditional_fields [
    I ((fun x -> x.item_qualname <> None),
       Fmt.field "qualname" item_qualname pp_qualname'_opt,
       Fmt.(styled `Yellow @@ any "filler"));
    C ((fun x -> x.item_redefines <> None),
       Fmt.field "redefines" item_redefines pp_qualname'_opt);
    T (Fmt.field "offset" item_offset pp_offset);
    T (Fmt.field "size" item_size pp_size);
    T (Pretty.vfield "layout" item_layout pp_item_layout);
    C ((fun x -> x.item_redefinitions <> []),
       Pretty.vfield "redefs" item_redefinitions pp_item_redefinitions);
  ] ppf x

and pp_item_definition': item_definition with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.pp_with_loc pp_item_definition ppf

and pp_item_definitions: item_definitions Pretty.printer = fun ppf defs ->
  NEL.pp ~fopen:"" ~fsep:"" ~fclose:"" pp_item_definition' ppf defs

and pp_item_redefinitions: item_redefinitions Pretty.printer = fun ppf ->
  Fmt.(list ~sep:nop) pp_item_definition' ppf

and pp_item_layout: item_layout Pretty.printer = fun ppf -> function
  | Elementary_item { usage; value } ->
      Pretty.record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "elementary");
        T (Pretty.vfield "usage" (fun () -> usage) pp_usage);
        C'(value <> None,
           Fmt.field "value" (fun () -> value) pp_literal'_opt);
      ] ppf ()
  | Struct_item { fields } ->
      Pretty.record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ const string "structure");
        T (Pretty.vfield "fields" Fun.id pp_item_definitions);
      ] ppf fields
  | Fixed_table { items; length; value } ->
      Pretty.record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "fixed-length table");
        T (Fmt.field "length" (fun () -> length) pp_int');
        T (Pretty.vfield "items" (fun () -> items) pp_item_definitions);
        C'(value <> None,
           Pretty.vfield "value" (fun () -> value) pp_literal'_opt);
      ] ppf ()
  | Depending_table { items; min_occurs; max_occurs; depending; value } ->
      Pretty.record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "variable-length table");
        T (Fmt.field "min_occurs" (fun () -> min_occurs) pp_int');
        T (Fmt.field "max_occurs" (fun () -> max_occurs) pp_int');
        T (Fmt.field "depending" (fun () -> depending) Cobol_ptree.pp_qualname');
        T (Pretty.vfield "items" (fun () -> items) pp_item_definitions);
        C'(value <> None,
           Pretty.vfield "value" (fun () -> value) pp_literal'_opt);
      ] ppf ()
  | Dynamic_table { items; capacity; min_capacity; max_capacity; initialized;
                    value } ->
      Pretty.record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "dynamic-length table");
        C'(capacity <> None,
           Fmt.field "capacity" (fun () -> capacity) pp_qualname'_opt);
        C'(min_capacity <> None,
           Fmt.field "min_capacity" (fun () -> min_capacity) pp_int'_opt);
        C'(max_capacity <> None,
           Fmt.field "max_capacity" (fun () -> max_capacity) pp_int'_opt);
        C'(~&initialized, Fmt.any "initialized");
        T (Pretty.vfield "items" (fun () -> items) pp_item_definitions);
        C'(value <> None,
           Pretty.vfield "value" (fun () -> value) pp_literal'_opt);
      ] ppf ()

let pp_renamed_item_layout: renamed_item_layout Pretty.printer = fun ppf -> function
  | Renamed_elementary { usage } ->
      Pretty.record [
        Fmt.(styled `Yellow @@ any "elementary");
        Pretty.vfield "usage" (fun () -> usage) pp_usage;
      ] ppf ()
  | Renamed_struct { fields } ->
      Pretty.record [
        Fmt.(styled `Yellow @@ const string "structure");
        Pretty.vfield "fields" Fun.id pp_item_definitions;
      ] ppf fields

let pp_record_renaming: record_renaming Pretty.printer =
  Pretty.record_with_conditional_fields [
    T (Fmt.field "qualname" (fun r -> r.renaming_name) Cobol_ptree.pp_qualname');
    T (Fmt.field "from" (fun r -> r.renaming_from) Cobol_ptree.pp_qualname');
    C ((fun r -> r.renaming_thru <> None),
       Fmt.field "thru" (fun r -> r.renaming_thru) pp_qualname'_opt);
    T (Fmt.field "offset" (fun r -> r.renaming_offset) Data_memory.pp_offset);
    T (Fmt.field "size" (fun r -> r.renaming_size) Data_memory.pp_size);
    T (Pretty.vfield "layout" (fun r -> r.renaming_layout) pp_renamed_item_layout);
  ]

let pp_record_renaming': record_renaming with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.pp_with_loc pp_record_renaming ppf

let pp_record_renamings: record_renamings Pretty.printer = fun ppf ->
  Fmt.(list ~sep:nop) pp_record_renaming' ppf

let pp_record: record Pretty.printer =
  Pretty.record_with_conditional_fields [
    T (Fmt.field "record" (fun x -> x.record_name) Fmt.string);
    T (Fmt.field "storage" (fun x -> x.record_storage) pp_data_storage);
    T (Pretty.vfield "item" (fun x -> x.record_item) pp_item_definition');
    C ((fun x -> x.record_renamings <> []),
       Pretty.vfield "renamings" (fun x -> x.record_renamings) pp_record_renamings);
  ]

let pp_item ppf = function
  | Data_item { def; record = { record_name; _ } } ->
      Pretty.record [
        Fmt.(styled `Yellow @@ any "data item");
        Fmt.field "record" (fun () -> record_name) Fmt.string;
        Pretty.vfield "def" (fun () -> def) pp_item_definition';
      ] ppf ()
  | Data_renaming { def; record = { record_name; _ } } ->
      Pretty.record [
        Fmt.(styled `Yellow @@ any "data item renaming");
        Fmt.field "record" (fun () -> record_name) Fmt.string;
        Pretty.vfield "def" (fun () -> def) pp_record_renaming';
      ] ppf ()
