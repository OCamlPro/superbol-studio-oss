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

open Cobol_data.Types

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

let pp_size = Fmt.(any "Size: " ++ Cobol_data.Memory.pp_size ++ any " bits")

let pp_int' = Cobol_ptree.pp_with_loc Fmt.int

let pp_cobol_block: _ Fmt.t -> _ Fmt.t = fun pp ->
  Fmt.(any "```cobol\n" ++ pp ++ any "\n```")

(* usage *)

let pp_usage: usage Pretty.printer =
  let pp_usage_with_picture ppf name (picture: Cobol_data.Picture.t) =
    Fmt.(
      pp_cobol_block (fun ppf _ ->
        pf ppf "PIC %a USAGE %s"
        Cobol_data.Picture.pp_picture_symbols picture.pic
        name)
      ++ const string "\n\n"
      ++ const Cobol_data.Picture.pp_category picture.category)
    ppf ()
  and pp_usage_with_sign ppf name signed =
    pp_cobol_block Fmt.(any "USAGE " ++ any name ++ any (if signed then " SIGNED" else " UNSIGNED"))
    ppf ()
  and pp_width_tag ppf tag =
    Fmt.int ppf @@
    match tag with `W16 -> 16 | `W32 -> 32 | `W34 -> 34 | `W64 -> 64 | `W128 -> 128
  in
  fun ppf -> function
    | Binary picture ->
        pp_usage_with_picture ppf "BINARY" picture
    | Binary_C_long { signed } ->
        pp_usage_with_sign ppf "BINARY-C-LONG" signed
    | Binary_char { signed } ->
        pp_usage_with_sign ppf "BINARY-CHAR" signed
    | Binary_double { signed } ->
        pp_usage_with_sign ppf "BINARY-DOUBLE" signed
    | Binary_long { signed } ->
        pp_usage_with_sign ppf "BINARY-LONG" signed
    | Binary_short { signed } ->
        pp_usage_with_sign ppf "BINARY-SHORT" signed
    | Bit picture ->
        pp_usage_with_picture ppf "BIT" picture
    | Display picture ->
        pp_usage_with_picture ppf "DISPLAY" picture
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
        Pretty.print ppf "Function pointer"
    | Procedure_pointer ->
        Pretty.print ppf "Procedure pointer"
    | Index ->
        Pretty.print ppf "Index"
    | National picture ->
        pp_usage_with_picture ppf "NATIONAL" picture
    | Object_reference _ ->
        Pretty.print ppf "Object reference"
    | Packed_decimal picture ->
        pp_usage_with_picture ppf "PACKED-DECIMAL" picture
    | Pointer _ ->
        Pretty.print ppf "Pointer"
    | Program_pointer _ ->
        Pretty.print ppf "Program pointer"

(* table range/span *)

let pp_fixed_span: fixed_span Pretty.printer = fun ppf x ->
  Fmt.pf ppf "OCCURS %a TIMES" pp_int' x.occurs_times

and pp_depending_span: depending_span Pretty.printer = fun ppf x ->
  Fmt.pf ppf "OCCURS %a TO %a TIMES DEPENDING ON %a" pp_int' x.occurs_depending_min pp_int' x.occurs_depending_max Cobol_ptree.pp_qualname' x.occurs_depending

and pp_dynamic_span: dynamic_span Pretty.printer =
  Fmt.any "OCCURS DYNAMIC"

let pp_span: span Pretty.printer = fun ppf -> function
  | Fixed_span d -> pp_fixed_span ppf d
  | Depending_span d -> pp_depending_span ppf d
  | Dynamic_span d -> pp_dynamic_span ppf d

let pp_struct: item_definitions Pretty.printer = fun ppf subfields ->
  let n = List.length (NEL.to_list subfields) in
    Fmt.pf ppf "Group of %d subfield%s" n (if n > 1 then "s" else "")

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

and pp_field_layout: field_layout Pretty.printer = fun ppf x ->
  match x with
  | Elementary_field { usage; init_value } ->
      Fmt.(
        const pp_usage usage
      ++ any "\n\n"
      ++ const (option ~none:nop (any "VALUE " ++ Cobol_ptree.pp_literal')) init_value)
      ppf x
  | Struct_field { subfields } ->
      Fmt.const pp_struct subfields ppf x

and pp_field_definition: field_definition Pretty.printer = fun ppf x ->
  let pp_qualname_opt_in_block' = pp_cobol_block Fmt.(option ~none:(any "FILLER") Cobol_ptree.pp_qualname')  in
  Fmt.(
    const pp_qualname_opt_in_block' x.field_qualname
    ++ any "\n\n"
    ++ const pp_field_layout x.field_layout
    ++ match x.field_layout with
    | Struct_field _ -> any "\n\n" ++ const pp_size x.field_size
    | _ -> nop
    ++ any "\n\n"
    ++ const (option (any "Redefines:\n" ++ pp_cobol_block Cobol_ptree.pp_qualname')) x.field_redefines)
  ppf x

and pp_field_definition': field_definition with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.pp_with_loc pp_field_definition ppf

(* fields *)

and pp_table_definition: table_definition Pretty.printer = fun ppf x ->
  Fmt.(
    any "Table\n\n"
    ++ pp_cobol_block (
      const pp_span x.table_range.range_span
      ++ any "\nINDEXED BY "
      ++ const (list ~sep:(any ", ") Cobol_ptree.pp_qualname') x.table_range.range_indexes)
    ++ any "\n\nFields:\n\n"
    ++ const pp_field_definition' x.table_field)
  ppf x

and pp_table_definition': table_definition with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.pp_with_loc pp_table_definition ppf


(* condition-names *)

and pp_condition_name: condition_name Pretty.printer = fun ppf x ->
  pp_cobol_block Fmt.(const Cobol_ptree.pp_condition_name_item x.condition_name_item) ppf x

and pp_condition_name': condition_name with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.pp_with_loc pp_condition_name ppf

(* and pp_condition_names: condition_names Pretty.printer = fun ppf -> *)
(*   Fmt.(list ~sep:nop) pp_condition_name' ppf *)

let pp_renamed_item_layout: renamed_item_layout Pretty.printer = fun ppf x ->
  match x with
  | Renamed_elementary { usage } ->
      Fmt.const pp_usage usage ppf x
  | Renamed_struct { subfields } ->
      Fmt.const pp_struct subfields ppf x

let pp_record_renaming: record_renaming Pretty.printer = fun ppf r ->
  Fmt.(
    pp_cobol_block (
      const Cobol_ptree.pp_qualname' r.renaming_name
      ++ any "\nRENAMES "
      ++ const Cobol_ptree.pp_qualname' r.renaming_from
      ++ const (option (any "\nTHRU " ++ Cobol_ptree.pp_qualname')) r.renaming_thru)
    ++ any "\n\n"
    ++ const pp_renamed_item_layout r.renaming_layout
    ++ match r.renaming_layout with
    | Renamed_struct _ -> any "\n\n" ++ const pp_size r.renaming_size
    | _ -> nop )
  ppf r

let pp_record_renaming': record_renaming with_loc Pretty.printer = fun ppf ->
  Cobol_ptree.pp_with_loc pp_record_renaming ppf

(* let pp_record_renamings: record_renamings Pretty.printer = fun ppf -> *)
(*   Fmt.(list ~sep:nop) pp_record_renaming' ppf *)

(* let pp_record: record Pretty.printer = *)
(*   Pretty.record_with_conditional_fields [ *)
(*     T (Fmt.field "record" (fun x -> x.record_name) Fmt.string); *)
(*     T (Fmt.field "storage" (fun x -> x.record_storage) pp_data_storage); *)
(*     T (Pretty.vfield "item" (fun x -> x.record_item) pp_item_definition'); *)
(*     C ((fun x -> x.record_renamings <> []), *)
(*        Pretty.vfield "renamings" (fun x -> x.record_renamings) pp_record_renamings); *)
(*   ] *)

let pp_data_definition ppf = function
  | Data_field { def; _ } ->
      Fmt.const pp_field_definition' def ppf ()
  | Data_renaming { def; _ } ->
      Fmt.const pp_record_renaming' def ppf ()
  | Data_condition { def; field; _ } ->
      Fmt.pf ppf "%a\n\n%a" pp_condition_name ~&def pp_field_definition ~&field
  | Table_index { table; _ } ->
      Fmt.const pp_table_definition' table ppf ()
