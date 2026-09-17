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
open Cobol_preproc.Env.TYPES
open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

let pp_readable_size ppf size =
  try
    let bits = Cobol_data.Memory.as_bits size in
    if Int.rem bits 8 = 0 then
      let bytes = bits / 8 in
      Fmt.pf ppf "%u byte%s" bytes (if bytes <> 1 then "s" else "")
    else
      Fmt.pf ppf "%u bit%s" bits (if bits <> 1 then "s" else "")
  with Cobol_data.Memory.NOT_SCALAR _ ->
    Fmt.pf ppf "*variable*"

let pp_size =
  Fmt.(any "Size: " ++ pp_readable_size)

let pp_total_size =
  Fmt.(any "Total size: " ++ pp_readable_size)

(* The record name is only worth showing when the item is not the record
   itself; on a root item it would just repeat the item name, or expose the
   placeholder name given to unnamed records. *)
let enclosing_record ~record_name
  : Cobol_ptree.qualname with_loc option -> string option = function
  | Some qualname ->
      (match ~&qualname with
       | Cobol_ptree.Qual _ -> record_name
       | Name _ -> None)
  | None ->                                         (* FILLER: keep the record *)
      record_name

let pp_offset_in record ppf offset =
  Fmt.pf ppf "Offset: %a%a" pp_readable_size offset
    Fmt.(option (any " in " ++ string)) record

let pp_int' = Cobol_ptree.pp_with_loc Fmt.int

let pp_cobol_block: _ Fmt.t -> _ Fmt.t = fun pp ->
  Fmt.(any "```cobol\n" ++ pp ++ any "\n```")

(* usage *)

let max_value digits scale =
  let s = "123456789123456789123456789123456789" in
  let whole = (digits - scale) in
  let scale = if scale < 0 then 0 else scale in
  let whole_part = Str.string_before s whole in
  let decimal_part = Str.string_before (Str.string_after s whole) scale in
  float_of_string (whole_part ^ "." ^ decimal_part)

let nbsp_repl = Str.global_replace (Str.regexp " ") " " (* <- utf8 nbsp *)

let pp_example_of ppf (picture: Cobol_data.Picture.t) =
  try
    match picture.category with
    | FixedNum { digits; scale; _ } ->
      let max = max_value digits scale in
      let max_str =
        if Float.is_integer max
        then string_of_int (int_of_float max)
        else string_of_float max in
      Fmt.pf ppf "\n\n*e.g,* [`%s`] (0), [`%s`] (%s)"
        (Lsp_picture_interp.example_of ~picture 0. |> nbsp_repl)
        (Lsp_picture_interp.example_of ~picture max |> nbsp_repl)
        max_str
    | _ -> ()

  with Invalid_argument _ -> ()

let pp_usage: usage Pretty.printer =
  let pp_usage_with_picture ppf name (picture: Cobol_data.Picture.t) =
    Fmt.pf ppf "%a\n\n%a%a"
      (pp_cobol_block (fun ppf _ ->
           Fmt.pf ppf ("PIC %a USAGE " ^^ name)
             Cobol_data.Picture.pp_picture_symbols picture.pic
         )) ()
      Cobol_data.Picture.pp_category picture.category
      pp_example_of picture
  and pp_width_tag ppf tag =
    Fmt.int ppf @@
    match tag with `W16 -> 16 | `W32 -> 32 | `W34 -> 34 | `W64 -> 64 | `W128 -> 128
  in
  let pp_usage_with_optional_picture ppf name signed picture =
    match picture with
    | Some picture ->
        pp_usage_with_picture ppf name picture
    | None ->
        pp_cobol_block Fmt.(any "USAGE " ++ any name ++
                            any (if signed then " SIGNED" else " UNSIGNED"))
          ppf ()
  in
  fun ppf -> function
    | Alphanumeric { picture; _ }
    | Display_numeric { picture; _ } ->
        pp_usage_with_picture ppf "DISPLAY" picture
    | Binary { picture; byte_size = Byte_size; signed;
               truncation = Truncate_to_native_size; _ } ->
        pp_usage_with_optional_picture ppf "BINARY-CHAR" signed picture
    | Binary { picture; byte_size = Short_size; signed;
               truncation = Truncate_to_native_size; _ } ->
        pp_usage_with_optional_picture ppf "BINARY-SHORT" signed picture
    | Binary { picture; byte_size = Long_size; signed;
               truncation = Truncate_to_native_size; _ } ->
        pp_usage_with_optional_picture ppf "BINARY-LONG" signed picture
    | Binary { picture; byte_size = Double_size; signed;
               truncation = Truncate_to_native_size; _ } ->
        pp_usage_with_optional_picture ppf "BINARY-DOUBLE" signed picture
    | Binary { picture; byte_size = C_long_size; signed;
               truncation = Truncate_to_native_size; _ } ->
        pp_usage_with_optional_picture ppf "BINARY-C-LONG" signed picture
    | Binary { picture; signed; _ } ->
        pp_usage_with_optional_picture ppf "BINARY" signed picture
    | Bit picture ->
        pp_usage_with_picture ppf "BIT" picture
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
    | Packed_decimal { picture; with_sign_nibble } ->
        pp_usage_with_picture ppf
          (if with_sign_nibble then "PACKED-DECIMAL" else "PACKED-DECIMAL-NO-SIGN")
          picture
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
      ++ const (option ~none:nop @@ any "VALUE " ++
                pp_with_loc Cobol_data.Printer.pp_value) init_value)
      ppf x
  | Struct_field { subfields } ->
      Fmt.const pp_struct subfields ppf x

and pp_field_definition
  : ?record_name:string -> field_definition Pretty.printer
  = fun ?record_name ppf x ->
  let definition_has_issues = x.field_has_definition_issues in
  let pp_qualname_opt_in_block' =
    pp_cobol_block Fmt.(option ~none:(any "FILLER") Cobol_ptree.pp_qualname')
  (* Size only displayed in case there's no errors on field *)
  and pp_layout_size ppf x =
    if x.field_has_definition_issues then ()
    else Fmt.fmt "  \n%a" ppf pp_size x.field_size
  (* Offset is relative to the record, so name it when we know it *)
  and pp_layout_offset ppf x =
    if x.field_has_definition_issues then ()
    else
      let record = enclosing_record ~record_name x.field_qualname in
      Fmt.fmt "  \n%a" ppf (pp_offset_in record) x.field_offset
  in
  match x.field_layout with
  | Elementary_field _ when definition_has_issues ->
      Fmt.(const pp_qualname_opt_in_block' x.field_qualname ++ any "\n\n" ++
           any "*(layout omitted due to issues in item definition)*  \n" ++
           const (option @@
                  any "Redefines:\n" ++ pp_cobol_block Cobol_ptree.pp_qualname')
             x.field_redefines)
        ppf x
  | _ ->
      Fmt.(const pp_qualname_opt_in_block' x.field_qualname ++ any "\n\n" ++
           const pp_field_layout x.field_layout ++
           const pp_layout_size x ++ const pp_layout_offset x ++ any "  \n" ++
           const (option @@
                  any "Redefines:\n" ++ pp_cobol_block Cobol_ptree.pp_qualname')
             x.field_redefines)
        ppf x

and pp_field_definition'
  : ?record_name:string -> field_definition with_loc Pretty.printer
  = fun ?record_name ppf ->
  Cobol_ptree.pp_with_loc (pp_field_definition ?record_name) ppf

(* fields *)

and pp_table_definition
  : ?record_name:string -> table_definition Pretty.printer
  = fun ?record_name ppf x ->
  (* Size and offset of the whole table; the nested field prints the size of a
     single occurrence. *)
  let pp_table_size_offset ppf x =
    if x.table_has_definition_issues then ()
    else
      let record = enclosing_record ~record_name ~&(x.table_field).field_qualname in
      Fmt.fmt "%a  \n%a  \n" ppf
        pp_total_size x.table_size
        (pp_offset_in record) x.table_offset
  in
  Fmt.(
    any "Table\n\n"
    ++ pp_cobol_block (
      const pp_span x.table_range.range_span
      ++ any "\nINDEXED BY "
      ++ const (list ~sep:(any ", ") Cobol_ptree.pp_qualname') x.table_range.range_indexes)
    ++ any "\n\n"
    ++ const pp_table_size_offset x
    ++ any "Fields:\n\n"
    ++ const (pp_field_definition' ?record_name) x.table_field)
  ppf x

and pp_table_definition'
  : ?record_name:string -> table_definition with_loc Pretty.printer
  = fun ?record_name ppf ->
  Cobol_ptree.pp_with_loc (pp_table_definition ?record_name) ppf


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

let pp_record_renaming
  : ?record_name:string -> record_renaming Pretty.printer
  = fun ?record_name ppf r ->
  let open Fmt in begin
    pp_cobol_block begin
      const Cobol_ptree.pp_qualname' r.renaming_name ++ any "\n" ++
      if r.renaming_has_definition_issues then nop else
        any "RENAMES " ++
        const Cobol_ptree.pp_qualname' r.renaming_from ++
        const (option (any "\nTHRU " ++ Cobol_ptree.pp_qualname'))
          r.renaming_thru
    end ++ any "\n\n" ++
    if r.renaming_has_definition_issues then nop else
      let record = enclosing_record ~record_name (Some r.renaming_name) in
      const pp_renamed_item_layout r.renaming_layout ++
      any "  \n" ++ const pp_size r.renaming_size ++
      any "  \n" ++ const (pp_offset_in record) r.renaming_offset
  end ppf r

let pp_record_renaming'
  : ?record_name:string -> record_renaming with_loc Pretty.printer
  = fun ?record_name ppf ->
  Cobol_ptree.pp_with_loc (pp_record_renaming ?record_name) ppf

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

let pp_compilation_var_definition ppf (Preproc_var def | Compilation_var def) =
  Fmt.pf ppf "Compilation@ variable@ with@ value@ %a%t"
    Cobol_preproc.Env.pp_value def.src_payload.compvar_value.src_payload
    (fun ppf -> match def.src_payload.compvar_value.src with
       | Source_location _ ->
           ()
       | Process_parameter ->
           Fmt.pf ppf "@ (given@ as@ process@ parameter)"
       | Process_environment ->
           Fmt.pf ppf "@ (defined@ in@ process@ environment)")

(* Records without a name of their own are given a placeholder ("FILLER 1") by
   the type-checker; showing it would not help. *)
let named_record { record_name; record_item; _ } =
  match Cobol_data.Item.qualname ~&record_item with
  | None -> None
  | Some _ -> Some record_name

let pp_data_definition ppf = function
  | Data_field { def; record } ->
      pp_field_definition' ?record_name:(named_record record) ppf def
  | Data_renaming { def; record } ->
      pp_record_renaming' ?record_name:(named_record record) ppf def
  | Data_condition { def; field; record } ->
      Fmt.pf ppf "%a\n\n%a" pp_condition_name ~&def
        (pp_field_definition ?record_name:(named_record record)) ~&field
  | Table_index { table; record; _ } ->
      pp_table_definition' ?record_name:(named_record record) ppf table
