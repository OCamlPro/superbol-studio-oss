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

open Types

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

let pp_offset = Memory.pp_offset
let pp_size = Memory.pp_size

let pp_int' = Cobol_ptree.Types.pp_with_loc Fmt.int
let pp_int'_opt = Fmt.option pp_int'
let pp_qualname'_opt = Fmt.option Cobol_ptree.Types.pp_qualname'
let pp_qualname'_list = Fmt.(hbox (list ~sep:comma Cobol_ptree.Types.pp_qualname'))
  (* Pretty.list ~fopen:"@[<h>" ~fsep:",@;" ~fclose:"@]" Cobol_ptree.Types.pp_qualname' *)
let pp_literal'_opt = Fmt.option Cobol_ptree.Types.pp_literal'
let pp_literal'_list = Fmt.list Cobol_ptree.Types.pp_literal'

(* usage *)

let pp_usage: usage Pretty.printer =
  let pp_usage_with_picture ppf name (picture: Picture.t) =
    Pretty.record [
      Fmt.(styled `Yellow @@ any name);
      Fmt.field "category" (fun () -> picture.category) Picture.pp_category;
    ] ppf ()
  and pp_usage_with_sign ppf name signed =
    Fmt.(styled `Yellow @@ (if signed then any "signed-" else nop) ++ any name)
      ppf ()
  and pp_width_tag ppf tag =
    Fmt.int ppf @@
    match tag with `W16 -> 16 | `W32 -> 32 | `W34 -> 34 | `W64 -> 64 | `W128 -> 128
  in
  fun ppf -> function
    | Binary picture ->
        pp_usage_with_picture ppf "binary" picture
    | Binary_C_long { signed } ->
        pp_usage_with_sign ppf "binary-c-long" signed
    | Binary_char { signed } ->
        pp_usage_with_sign ppf "binary-char" signed
    | Binary_double { signed } ->
        pp_usage_with_sign ppf "binary-double" signed
    | Binary_long { signed } ->
        pp_usage_with_sign ppf "binary-long" signed
    | Binary_short { signed } ->
        pp_usage_with_sign ppf "binary-short" signed
    | Bit picture ->
        pp_usage_with_picture ppf "bit" picture
    | Display picture ->
        pp_usage_with_picture ppf "display" picture
    | Float_binary { width; endian = _ } ->
        Pretty.record [
          Fmt.(styled `Yellow @@ any "float-binary");
          Fmt.field "width" (fun () -> width) pp_width_tag;
        ] ppf ()
    | Float_decimal { width; endian = _; encoding = _ } ->
        Pretty.record [
          Fmt.(styled `Yellow @@ any "float-decimal");
          Fmt.field "width" (fun () -> width) pp_width_tag;
        ] ppf ()
    | Float_extended ->
        Pretty.print ppf "float-extended (long double)"
    | Float_long ->
        Pretty.print ppf "float-long (double)"
    | Float_short ->
        Pretty.print ppf "float-short (float)"
    | Function_pointer _ ->
        Pretty.print ppf "function pointer"
    | Index ->
        Pretty.print ppf "index"
    | National picture ->
        pp_usage_with_picture ppf "national" picture
    | Object_reference _ ->
        Pretty.print ppf "object reference"
    | Packed_decimal picture ->
        pp_usage_with_picture ppf "packed-decimal" picture
    | Pointer _ ->
        Pretty.print ppf "pointer"
    | Program_pointer _ ->
        Pretty.print ppf "program-pointer"

(* table range/span *)

let pp_fixed_span: fixed_span Pretty.printer =
  Fmt.field "fixed-length" (fun x -> x.occurs_times) pp_int'

and pp_depending_span: depending_span Pretty.printer =
  Pretty.record_with_conditional_fields [
    T Fmt.(styled `Yellow @@ any "depending-span");
    T (Fmt.field "min_occurs" (fun x -> x.occurs_depending_min) pp_int');
    T (Fmt.field "max_occurs" (fun x -> x.occurs_depending_max) pp_int');
    T (Fmt.field "depending" (fun x -> x.occurs_depending) Cobol_ptree.Types.pp_qualname');
  ]

and pp_dynamic_span: dynamic_span Pretty.printer =
  Pretty.record_with_conditional_fields [
    T Fmt.(styled `Yellow @@ any "dynamic-span");
    C ((fun x -> x.occurs_dynamic_capacity <> None),
       Fmt.field "capacity" (fun x -> x.occurs_dynamic_capacity) pp_qualname'_opt);
    C ((fun x -> x.occurs_dynamic_capacity_min <> None),
       Fmt.field "min_capacity" (fun x -> x.occurs_dynamic_capacity_min)
         pp_int'_opt);
    C ((fun x -> x.occurs_dynamic_capacity_max <> None),
       Fmt.field "max_capacity" (fun x -> x.occurs_dynamic_capacity_max)
         pp_int'_opt);
    C ((fun x -> x.occurs_dynamic_initialized.payload), Fmt.any "initialized");
  ]

let pp_span: span Pretty.printer = fun ppf -> function
  | Fixed_span d -> pp_fixed_span ppf d
  | Depending_span d -> pp_depending_span ppf d
  | Dynamic_span d -> pp_dynamic_span ppf d

let pp_table_range: table_range Pretty.printer = fun ppf x ->
  Pretty.record_with_conditional_fields [
    T (Pretty.vfield "span" (fun x -> x.range_span) pp_span);
    C ((fun x -> x.range_indexes <> []),
       Pretty.vfield "indexes" (fun x -> x.range_indexes) pp_qualname'_list);
  ] ppf x


(* items *)

let rec pp_item_definition: item_definition Pretty.printer = fun ppf -> function
  | Field def -> pp_field_definition ppf def
  | Table def -> pp_table_definition ppf def

and pp_item_definition': item_definition with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.Types.pp_with_loc pp_item_definition ppf

and pp_item_definitions: item_definitions Pretty.printer = fun ppf defs ->
  NEL.pp ~fopen:"" ~fsep:"" ~fclose:"" pp_item_definition' ppf defs

and pp_item_redefinitions: item_redefinitions Pretty.printer = fun ppf ->
  Fmt.(list ~sep:nop) pp_item_definition' ppf


(* fields *)

and pp_field_definition: field_definition Pretty.printer = fun ppf x ->
  Pretty.record_with_conditional_fields [
    I ((fun x -> x.field_qualname <> None),
       Fmt.field "qualname" (fun x -> x.field_qualname) pp_qualname'_opt,
       Fmt.(styled `Yellow @@ any "filler"));
    C ((fun x -> x.field_redefines <> None),
       Fmt.field "redefines" (fun x -> x.field_redefines) pp_qualname'_opt);
    C ((fun x -> x.field_leading_ranges <> []),
       Fmt.field "leading ranges"
         (fun x -> List.length x.field_leading_ranges) Fmt.int);
    T (Fmt.field "offset" (fun x -> x.field_offset) pp_offset);
    T (Fmt.field "size" (fun x -> x.field_size) pp_size);
    T (Pretty.vfield "layout" (fun x -> x.field_layout) pp_field_layout);
    C ((fun x -> x.field_conditions <> []),
       Pretty.vfield "conditions" (fun x -> x.field_conditions) pp_condition_names);
    C ((fun x -> x.field_redefinitions <> []),
       Pretty.vfield "redefs" (fun x -> x.field_redefinitions)
         pp_item_redefinitions);
  ] ppf x

and pp_field_definition': field_definition with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.Types.pp_with_loc pp_field_definition ppf

(* and pp_field_definitions: field_definitions Pretty.printer = fun ppf defs -> *)
(*   NEL.pp ~fopen:"" ~fsep:"" ~fclose:"" pp_field_definition' ppf defs *)

and pp_field_layout: field_layout Pretty.printer = fun ppf -> function
  | Elementary_field { usage; init_value } ->
      Pretty.record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "elementary");
        T (Pretty.vfield "usage" (fun () -> usage) pp_usage);
        C'(init_value <> None,
           Fmt.field "value" (fun () -> init_value) pp_literal'_opt);
      ] ppf ()
  | Struct_field { subfields } ->
      Pretty.record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ const string "structure");
        T (Pretty.vfield "fields" Fun.id pp_item_definitions);
      ] ppf subfields


(* fields *)

and pp_table_definition: table_definition Pretty.printer = fun ppf x ->
  Pretty.record_with_conditional_fields [
    T Fmt.(styled `Yellow @@ any "table");
    C ((fun x -> x.table_redefines <> None),
       Fmt.field "redefines" (fun x -> x.table_redefines) pp_qualname'_opt);
    T (Fmt.field "offset" (fun x -> x.table_offset) pp_offset);
    T (Fmt.field "size" (fun x -> x.table_size) pp_size);
    T (Pretty.vfield "range" (fun x -> x.table_range) pp_table_range);
    C ((fun x -> x.table_init_values <> []),
       Fmt.field "init-values" (fun _ -> "...") Fmt.string);
    T (Pretty.vfield "field" (fun x -> x.table_field) pp_field_definition');
    C ((fun x -> x.table_redefinitions <> []),
       Pretty.vfield "redefs" (fun x -> x.table_redefinitions)
         pp_item_redefinitions);
  ] ppf x

and pp_table_definition': table_definition with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.Types.pp_with_loc pp_table_definition ppf


(* condition-names *)

and pp_condition_name: condition_name Pretty.printer =
  Pretty.record_with_conditional_fields [
    T (Fmt.field "qualname" (fun r -> r.condition_name_qualname)
         Cobol_ptree.Types.pp_qualname');
    T (Fmt.field "values" (fun _ -> "...") Fmt.string);
  ]

and pp_condition_name': condition_name with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.Types.pp_with_loc pp_condition_name ppf

and pp_condition_names: condition_names Pretty.printer = fun ppf ->
  Fmt.(list ~sep:nop) pp_condition_name' ppf

let pp_renamed_item_layout: renamed_item_layout Pretty.printer = fun ppf -> function
  | Renamed_elementary { usage } ->
      Pretty.record [
        Fmt.(styled `Yellow @@ any "elementary");
        Pretty.vfield "usage" Fun.id pp_usage;
      ] ppf usage
  | Renamed_struct { subfields } ->
      Pretty.record [
        Fmt.(styled `Yellow @@ const string "structure");
        Pretty.vfield "fields" Fun.id pp_item_definitions;
      ] ppf subfields

let pp_record_renaming: record_renaming Pretty.printer =
  Pretty.record_with_conditional_fields [
    T (Fmt.field "qualname" (fun r -> r.renaming_name) Cobol_ptree.Types.pp_qualname');
    T (Fmt.field "from" (fun r -> r.renaming_from) Cobol_ptree.Types.pp_qualname');
    C ((fun r -> r.renaming_thru <> None),
       Fmt.field "thru" (fun r -> r.renaming_thru) pp_qualname'_opt);
    T (Fmt.field "offset" (fun r -> r.renaming_offset) Memory.pp_offset);
    T (Fmt.field "size" (fun r -> r.renaming_size) Memory.pp_size);
    T (Pretty.vfield "layout" (fun r -> r.renaming_layout) pp_renamed_item_layout);
  ]

let pp_record_renaming': record_renaming with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.Types.pp_with_loc pp_record_renaming ppf

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

let pp_data_definition ppf = function
  | Data_field { def; record = { record_name; _ } } ->
      Pretty.record [
        Fmt.(styled `Yellow @@ any "data field");
        Fmt.field "record" (fun () -> record_name) Fmt.string;
        Pretty.vfield "def" (fun () -> def) pp_field_definition';
      ] ppf ()
  | Data_renaming { def; record = { record_name; _ } } ->
      Pretty.record [
        Fmt.(styled `Yellow @@ any "data field renaming");
        Fmt.field "record" (fun () -> record_name) Fmt.string;
        Pretty.vfield "def" (fun () -> def) pp_record_renaming';
      ] ppf ()
  | Data_condition { def; record = { record_name; _ }; field } ->
      Pretty.record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "data condition");
        T (Fmt.field "record" (fun () -> record_name) Fmt.string);
        I'(~&field.field_qualname <> None,
           Fmt.field "field" (fun () -> ~&field.field_qualname) pp_qualname'_opt,
           Fmt.field "field-offset" (fun () -> ~&field.field_offset) pp_offset);
	T (Pretty.vfield "def" (fun () -> def) pp_condition_name');
      ] ppf ()
  | Table_index { table; record = { record_name; _ }; _ } ->
      Pretty.record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "table index");
        T (Fmt.field "record" (fun () -> record_name) Fmt.string);
	T (Pretty.vfield "table" (fun () -> table) pp_table_definition');
      ] ppf ()
