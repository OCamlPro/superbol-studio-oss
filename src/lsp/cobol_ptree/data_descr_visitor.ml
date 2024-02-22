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

open Cobol_common.Visitor
open Cobol_common.Visitor.INFIX                         (* for `>>` (== `|>`) *)
open Terms_visitor
open Data_descr

(* --- *)

let fold_file_data_clause (v: _ #folder) : file_data_clause -> 'a -> 'a =
  fold_name'_list v

(* --- *)

class ['a] folder = object
  inherit ['a] Terms_visitor.folder

  method fold_class_clause              : (class_clause              , 'a) fold = default
  method fold_data_occurs_clause        : (data_occurs_clause        , 'a) fold = default
  method fold_table_data_value          : (table_data_value          , 'a) fold = default
  method fold_data_value_clause         : (data_value_clause         , 'a) fold = default
  method fold_data_varying              : (data_varying              , 'a) fold = default
  method fold_external_clause           : (external_clause           , 'a) fold = default
  method fold_file_block_contents       : (file_block_contents       , 'a) fold = default
  method fold_file_linage_clause        : (file_linage_clause        , 'a) fold = default
  method fold_format_clause             : (format_clause             , 'a) fold = default
  method fold_group_usage_clause        : (group_usage_clause        , 'a) fold = default
  method fold_label_clause              : (label_clause              , 'a) fold = default
  method fold_locale_phrase             : (locale_phrase             , 'a) fold = default
  method fold_object_reference_kind     : (object_reference_kind     , 'a) fold = default
  method fold_property_clause           : (property_clause           , 'a) fold = default
  method fold_property_kind             : (property_kind             , 'a) fold = default
  method fold_record_clause             : (record_clause             , 'a) fold = default
  method fold_report_data_name_or_final : (report_data_name_or_final , 'a) fold = default
  method fold_report_screen_usage_clause: (report_screen_usage_clause, 'a) fold = default
  method fold_report_type_clause        : (report_type_clause        , 'a) fold = default
  method fold_select_when_clause        : (select_when_clause        , 'a) fold = default
  method fold_sign_clause               : (sign_clause               , 'a) fold = default
  method fold_sort_direction            : (sort_direction            , 'a) fold = default
  method fold_sort_spec                 : (sort_spec                 , 'a) fold = default
  method fold_synchronized_clause       : (synchronized_clause       , 'a) fold = default
  method fold_usage_clause              : (usage_clause              , 'a) fold = default
  method fold_validation_clause         : (validation_clause         , 'a) fold = default
  method fold_valueof_clause            : (valueof_clause            , 'a) fold = default

end

(* --- *)

let fold_table_data_value (v: _ #folder) =
  handle v#fold_table_data_value
    ~continue:begin fun { table_data_values;
                          table_data_from;
                          table_data_to } x -> x
      >> fold_with_loc_list ~fold:fold_literal v table_data_values
      >> fold_list ~fold:fold_subscript v table_data_from
      >> fold_list ~fold:fold_subscript v table_data_to
    end

let fold_data_value_clause (v: _ #folder) =
  handle v#fold_data_value_clause
    ~continue:begin function
      | ValueData l -> fold_literal' v l
      | ValueTable t -> fold_list ~fold:fold_table_data_value v t
    end

let fold_data_varying (v: _ #folder) =
  handle v#fold_data_varying
    ~continue:begin fun { data_varying; data_varying_from; data_varying_by } x -> x
      >> fold_name' v data_varying
      >> fold_option ~fold:fold_expr v data_varying_from
      >> fold_option ~fold:fold_expr v data_varying_by
    end

let fold_external_clause (v: _ #folder) =
  handle v#fold_external_clause
    ~continue:(fold_option ~fold:fold_strlit v)

let fold_file_block_contents (v: _ #folder) =
  leaf v#fold_file_block_contents

let fold_file_linage_clause (v: _ #folder) =
  handle v#fold_file_linage_clause
    ~continue:begin fun { file_linage_lines;
                          file_linage_with_footing_at;
                          file_linage_lines_at_top;
                          file_linage_lines_at_bottom } x -> x
      >> fold_qualname_or_intlit v file_linage_lines
      >> fold_option ~fold:fold_qualname_or_intlit v file_linage_with_footing_at
      >> fold_option ~fold:fold_qualname_or_intlit v file_linage_lines_at_top
      >> fold_option ~fold:fold_qualname_or_intlit v file_linage_lines_at_bottom
    end

let fold_format_clause (v: _ #folder) =
  leaf v#fold_format_clause

let fold_group_usage_clause (v: _ #folder) =
  leaf v#fold_group_usage_clause

let fold_label_clause (v: _ #folder) =
  leaf v#fold_label_clause

let fold_locale_phrase (v: _ #folder) =
  handle v#fold_locale_phrase
    ~continue:begin fun { locale_name; locale_size } x -> x
      >> fold_name'_opt v locale_name
      >> fold_integer v locale_size
    end

let fold_object_reference_kind (v: _ #folder) =
  handle v#fold_object_reference_kind
    ~continue:begin function
      | ActiveClass _ ->
          Fun.id                                             (* consider leaf *)
      | Name { class_or_interface_name; _ } ->
          fold_name' v class_or_interface_name
    end

let fold_property_kind (v: _ #folder) =
  leaf v#fold_property_kind

let fold_property_clause (v: _ #folder) =
  handle v#fold_property_clause
    ~continue:begin fun { property_with_no; property_is_final } x -> x
      >> fold_option ~fold:fold_property_kind v property_with_no
      >> fold_bool v property_is_final
    end

let fold_record_clause (v: _ #folder) =
  handle v#fold_record_clause
    ~continue:begin fun c x -> match c with
      | FixedLength i -> x
          >> fold_integer v i
      | VariableLength { min_length; max_length; depending } -> x
          >> fold_integer_opt v min_length
          >> fold_integer_opt v max_length
          >> fold_qualname'_opt v depending
      | FixedOrVariableLength { min_length; max_length } -> x
          >> fold_integer v min_length
          >> fold_integer v max_length
    end

let fold_report_data_name_or_final (v: _ #folder) =
  handle v#fold_report_data_name_or_final
    ~continue:begin function
      | ReportDataName q -> fold_qualident v q
      | ReportFinal -> Fun.id
    end

let fold_report_screen_usage_clause (v: _ #folder) =
  leaf v#fold_report_screen_usage_clause

let fold_report_type_clause (v: _ #folder) =
  handle v#fold_report_type_clause
    ~continue:begin function
      | Detail
      | ReportHeading
      | ReportFooting
      | PageHeading
      | PageFooting
      | ControlHeading None
      | ControlFooting None -> Fun.id
      | ControlHeading Some (f, _)
      | ControlFooting Some f -> fold_report_data_name_or_final v f
    end

let fold_select_when_clause (v: _ #folder) =
  handle v#fold_select_when_clause
    ~continue:begin function
      | SelectWhen n -> fold_name' v n
      | SelectWhenOther -> Fun.id
    end

let fold_sign_clause (v: _ #folder) =
  leaf v#fold_sign_clause (* leaf should be enough, even though it's a record *)

let fold_sort_direction (v: _ #folder) =
  leaf v#fold_sort_direction

let fold_sort_spec (v: _ #folder) =
  handle v#fold_sort_spec
    ~continue:begin fun { sort_key_direction; sort_key_names } x -> x
      >> fold_sort_direction v sort_key_direction
      >> fold_list ~fold:fold_qualname v sort_key_names
    end

let fold_synchronized_clause (v: _ #folder) =
  leaf v#fold_synchronized_clause

let fold_usage_clause (v: _ #folder) =
  handle v#fold_usage_clause
    ~continue:begin function
      | ObjectReference o ->
          fold_option ~fold:fold_object_reference_kind v o
      | FunctionPointer n ->
          fold_name' v n
      | Pointer n
      | ProgramPointer n ->
          fold_name'_opt v n
      | _ ->
          Fun.id
          (* | Binary *)
          (* | BinaryChar of signedness option *)
          (* | BinaryShort of signedness option *)
          (* | BinaryLong of signedness option *)
          (* | BinaryDouble of signedness option *)
          (* | Bit *)
          (* | Display *)
          (* | FloatBinary32 of endianness_mode option *)
          (* | FloatBinary64 of endianness_mode option *)
          (* | FloatBinary128 of endianness_mode option *)
          (* | FloatDecimal16 of encoding_endianness *)
          (* | FloatDecimal34 of encoding_endianness *)
          (* | FloatExtended *)
          (* | FloatLong *)
          (* | FloatShort *)
          (* | Index *)
          (* | National *)
          (* | PackedDecimal *)
          (* | UsagePending of [`Comp0 | `Comp1 | `Comp2 | `Comp3 | `Comp5 | `Comp6 | *)
          (*                    `CompX | `CompN | `Comp9 | `Comp10 | `Comp15 | *)
          (*                    `BinaryCLong of signedness option] *)
    end

let fold_class_clause (v: _ #folder) =
  handle v#fold_class_clause
    ~continue:begin function
      | Alphabetic
      | AlphabeticLower
      | AlphabeticUpper
      | Boolean
      | Numeric -> Fun.id
      | ClassOrAlphabet n -> fold_name' v n
    end

let fold_validation_clause (v: _ #folder) =
  handle v#fold_validation_clause
    ~continue:begin function
      | Class c -> fold_class_clause v c
      | Default i -> fold_option ~fold:fold_ident_or_literal v i
      | Destination i -> fold_list ~fold:fold_ident v i
      | InvalidWhen c -> fold_list ~fold:fold_cond v c
      | PresentWhen c -> fold_cond v c
      | Varying l -> fold_list ~fold:fold_data_varying v l
      | ValidateStatus { is_; when_; on; for_ } -> ignore (when_, on); fun x -> x
        >> fold_ident_or_literal v is_
        >> fold_list ~fold:fold_ident v for_
    end

let fold_valueof_clause (v: _ #folder) =
  handle v#fold_valueof_clause
    ~continue:begin fun { valueof_valued; valueof_value } x -> x
      >> fold_name' v valueof_valued
      >> fold_qualname_or_literal v valueof_value
    end

let fold_data_occurs_clause (v: _ #folder) =
  handle v#fold_data_occurs_clause
    ~continue:begin fun c x -> match c with
      | OccursFixed { times; key_is; indexed_by } -> x
          >> fold_integer' v times
          >> fold_list ~fold:fold_sort_spec v key_is
          >> fold_name'_list v indexed_by
      | OccursDepending { from; to_; depending;
                          key_is; indexed_by } -> x
          >> fold_integer' v from
          >> fold_integer' v to_
          >> fold_qualname' v depending
          >> fold_list ~fold:fold_sort_spec v key_is
          >> fold_name'_list v indexed_by
      | OccursDynamic { capacity_in; from; to_;
                        initialized; key_is; indexed_by } -> x
          >> fold_name'_opt v capacity_in
          >> fold_integer'_opt v from
          >> fold_integer'_opt v to_
          >> fold' v ~fold:fold_bool initialized
          >> fold_list ~fold:fold_sort_spec v key_is
          >> fold_name'_list v indexed_by
    end
