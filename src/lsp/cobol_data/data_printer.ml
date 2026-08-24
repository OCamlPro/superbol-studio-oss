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

let pp_alphanum_value ppf a =                      (* print as escaped string *)
  Pretty.print ppf "%S" a
let pp_boolean_value ppf b =
  Cobol_ptree.pp_boolean ppf (Data_value.ptree_of_boolean b)
let pp_integer_value =
  Z.pp_print
let pp_fixed_value ppf q =
  Cobol_ptree.pp_fixed ppf (Data_value.ptree_of_fixed q)
let pp_floating_value ppf f =
  Cobol_ptree.pp_floating ppf (Data_value.ptree_of_floating f)

let pp_value ppf = function
  | Alphanum_value x -> pp_alphanum_value ppf x
  | Boolean_value x -> pp_boolean_value ppf x
  | Integer_value x -> pp_integer_value ppf x
  | Fixed_value x -> pp_fixed_value ppf x
  | Floating_value x -> pp_floating_value ppf x
  | Zero_value -> Fmt.string ppf "ZERO"
  | Space_value -> Fmt.string ppf "SPACE"
  | Quote_value -> Fmt.string ppf "QUOTE"
  | Low_value -> Fmt.string ppf "LOW-VALUE"
  | High_value -> Fmt.string ppf "HIGH-VALUE"
  | All_alphanum_value a -> Fmt.pf ppf "ALL@ %a" pp_alphanum_value a
let pp_value'_opt = Fmt.option (pp_with_loc pp_value)

(** Pretty-prints the given alphanum as a literal; appends a slash and an
    escaped "real value" only if the alphanum value was given in hexadeciaml. *)
let pp_alphanum_literal ppf (a: alphanum_literal) =
  Cobol_ptree.pp_alphanum ppf a.alphanum_ptree;
  if a.alphanum_ptree.hexadecimal then Fmt.pf ppf "/%S" a.alphanum_value
let pp_boolean_literal  ppf b = pp_boolean_value  ppf b.bool_value
let pp_integer_literal  ppf i = pp_integer_value  ppf i.int_value
let pp_fixed_literal    ppf f = pp_fixed_value    ppf f.fixed_value
let pp_floating_literal ppf f = pp_floating_value ppf f.float_value

let pp_offset = Data_memory.pp_offset
let pp_size = Data_memory.pp_size

let pp_int' = Cobol_ptree.pp_with_loc Fmt.int
let pp_int'_opt = Fmt.option pp_int'
let pp_qualname'_opt = Fmt.option Cobol_ptree.pp_qualname'
let pp_qualname'_list = Fmt.(hbox (list ~sep:comma Cobol_ptree.pp_qualname'))
(* Pretty.list ~fopen:"@[<h>" ~fsep:",@;" ~fclose:"@]" Cobol_ptree.pp_qualname' *)

let pp_data_storage ppf = function
  | File n -> Fmt.pf ppf "FILE@ %a" Cobol_ptree.pp_name' n
  | Local_storage -> Fmt.string ppf "LOCAL-STORAGE"
  | Working_storage -> Fmt.string ppf "WORKING-STORAGE"
  | Linkage -> Fmt.string ppf "LINKAGE"

(* usage *)

let pp_usage: usage Pretty.printer =
  let pp_usage_with_picture ppf name (picture: Data_picture.t) =
    Pretty.record [
      Fmt.(styled `Yellow @@ any name);
      Fmt.field "category" (fun () -> picture.category) Data_picture.pp_detailed_category;
    ] ppf ()
  and pp_usage_with_sign ppf name signed =
    Fmt.(styled `Yellow @@ (if signed then any "signed-" else nop) ++ any name)
      ppf ()
  and pp_width_tag ppf tag =
    Fmt.int ppf @@
    match tag with `W16 -> 16 | `W32 -> 32 | `W34 -> 34 | `W64 -> 64 | `W128 -> 128
  and pp_range_extended_tag ppf digits =
    if digits = None then Fmt.string ppf "(range-extended)"
  in
  fun ppf -> function
    | Binary picture ->
        pp_usage_with_picture ppf "binary" picture
    | Binary_C_long { signed } ->
        pp_usage_with_sign ppf "binary-c-long" signed
    | Binary_char { signed } ->
        pp_usage_with_sign ppf "binary-char" signed
    | Binary_double { signed; digits; _ } ->
        pp_usage_with_sign ppf "binary-double" signed;
        pp_range_extended_tag ppf digits
    | Binary_long { signed; digits; _ } ->
        pp_usage_with_sign ppf "binary-long" signed;
        pp_range_extended_tag ppf digits
    | Binary_short { signed; digits; _ } ->
        pp_usage_with_sign ppf "binary-short" signed;
        pp_range_extended_tag ppf digits
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
    | Procedure_pointer ->
        Pretty.print ppf "procedure-pointer"
    | Index ->
        Pretty.print ppf "index"
    | National picture ->
        pp_usage_with_picture ppf "national" picture
    | Object_reference _ ->
        Pretty.print ppf "object reference"
    | Packed_decimal { picture; with_sign_nibble } ->
        pp_usage_with_picture ppf
          (if with_sign_nibble then "packed-decimal" else "packed-decimal-no-sign")
          picture
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
    T (Fmt.field "depending" (fun x -> x.occurs_depending) Cobol_ptree.pp_qualname');
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
  Cobol_ptree.pp_with_loc pp_item_definition ppf

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
    C ((fun x -> x.field_has_definition_issues),
       Fmt.any "/!\\ with_errors /!\\");
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
  Cobol_ptree.pp_with_loc pp_field_definition ppf

(* and pp_field_definitions: field_definitions Pretty.printer = fun ppf defs -> *)
(*   NEL.pp ~fopen:"" ~fsep:"" ~fclose:"" pp_field_definition' ppf defs *)

and pp_field_layout: field_layout Pretty.printer = fun ppf -> function
  | Elementary_field { usage; init_value } ->
      Pretty.record_with_conditional_fields [
        T Fmt.(styled `Yellow @@ any "elementary");
        T (Pretty.vfield "usage" (fun () -> usage) pp_usage);
        C'(init_value <> None,
           Fmt.field "value" (fun () -> init_value) pp_value'_opt);
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
    C ((fun x -> x.table_has_definition_issues),
       Fmt.any "/!\\ with_errors /!\\");
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
  Cobol_ptree.pp_with_loc pp_table_definition ppf


(* condition-names *)

and pp_condition_name: condition_name Pretty.printer =
  Pretty.record_with_conditional_fields [
    T (Fmt.field "qualname" (fun r -> r.condition_name_qualname)
         Cobol_ptree.pp_qualname');
    T (Fmt.field "values" (fun _ -> "...") Fmt.string);
  ]

and pp_condition_name': condition_name with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.pp_with_loc pp_condition_name ppf

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
    T (Fmt.field "qualname" (fun r -> r.renaming_name) Cobol_ptree.pp_qualname');
    C ((fun r -> r.renaming_has_definition_issues),
       Fmt.any "/!\\ with_errors /!\\");
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

(* --- *)

let pp_literal_class ppf = function
  | Boolean ->
      Pretty.string ppf "Boolean"
  | Fixed ->
      Pretty.print ppf "fixed-point@ numeric"
  | Floating ->
      Pretty.print ppf "floating-point@ numeric"
  | Hexadecimal ->
      Pretty.string ppf "hexadecimal"
  | Integer ->
      Pretty.print ppf "Integer"

let pp_invalid_stuff ppf = function
  | Character_in_literal { literal_class; char } ->
      Pretty.print ppf "character@ `%c'@ in@ %a@ literal"
        char pp_literal_class literal_class

let pp_unsupported_stuff ppf = function
  | Figurative_constant x ->
      Pretty.print ppf "figurative@ constant@ %a" Cobol_ptree.pp_figurative x
  | National_literal ->
      Pretty.print ppf "national@ literal"
  | Concatenation_of_literals ->
      Pretty.print ppf "concatenation@ of@ literals"

let pp_error ppf = function
  | Invalid { stuff; _ } ->
      Pretty.print ppf "Invalid@ %a" pp_invalid_stuff stuff
  | Unsupported { stuff; _ } ->
      Pretty.print ppf "Unsupported@ %a" pp_unsupported_stuff stuff
  | Overlong_literal { max_length; literal_string; _ } ->
      Pretty.print ppf "Literal@ of@ length@ %u@ exceeds@ maximum@ allowed@ \
                        length@ %u" (String.length literal_string) max_length
