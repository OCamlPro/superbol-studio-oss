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

(** Representation of COBOL data items *)

(* Note: location of qualnames often correspond to the *unqualified* name, with
   implicit qualification based on item groups. *)

open Cobol_common.Srcloc.TYPES

module NEL = Cobol_common.Basics.NEL
type 'a nel = 'a NEL.t

(** {2 Literals} *)

type alphanum_literal =
  {
    alphanum_ptree: Cobol_ptree.alphanum [@compare fun _ _ -> 0];
    alphanum_value: alphanum_value;
    (** differs from [alphanum_ptree.given_str] iff [alphanum_ptree.hexadecimal]
        holds *)
  }

and alphanum_value = string

and boolean_literal =
  {
    bool_ptree: Cobol_ptree.boolean [@compare fun _ _ -> 0];
    bool_value: boolean_value;
  }

and boolean_value =
  {
    bool_width: int;                                (** may be 0 *)
    bool_bits: Z.t; [@printer Z.pp_print]           (** irrelevant if 0-width *)
  }

and integer_literal =
  {
    int_ptree: Cobol_ptree.integer [@compare fun _ _ -> 0];
    int_value: integer_value;
  }

and integer_value = Z.t [@printer Z.pp_print]

and fixed_literal =
  {
    fixed_ptree: Cobol_ptree.fixed [@compare fun _ _ -> 0];
    fixed_value: fixed_value;
  }

and fixed_value = Q.t [@printer Q.pp_print]

and floating_literal =
  {
    float_ptree: Cobol_ptree.floating [@compare fun _ _ -> 0];
    float_value: floating_value;
  }

and floating_value =
  {
    float_significand: fixed_value;
    float_exponent: int;                    (* 0 <= . <= 9999 in ISO/IEC 2014 *)
  }

(** Values that may come from literals only. *)
and literal_value =
  | Alphanum_value of alphanum_value
  | Boolean_value of boolean_value
  | Integer_value of integer_value
  | Fixed_value of fixed_value
  | Floating_value of floating_value
  | Zero_value
  | Space_value
  | Quote_value
  | Low_value
  | High_value
  | All_alphanum_value of alphanum_value

[@@deriving ord]

(** {2 Storage} *)

type picture_config = Data_picture.TYPES.config
type picture = Data_picture.t

type usage =
  | Alphanumeric of                                   (* ALPHANUMERIC DISPLAY *)
      {
        picture: picture;
        size: int;
      }
  | Binary of
      {
        picture: (* [`numeric] *) picture option;
        signed: bool;
        scaling: int;
        byte_size: byte_size;
        truncation: binary_truncation;
        (* always_bigendian: bool;             (\** always use big-endian representation *)
        (*                                         (COMP-5/X) *\) *)
      }
  | Bit of (* [`boolean] *) picture
  | Display_numeric of
      {
        picture: picture;
        sign: display_sign;
      }
  | Float_binary of
      {
        width: [`W32|`W64|`W128];                                 (* +COB2002 *)
        endian: Cobol_ptree.endianness_mode;
      }
  | Float_decimal of
      {
        width: [`W16 | `W34];                                     (* +COB2002 *)
        endian: Cobol_ptree.endianness_mode;
        encoding: Cobol_ptree.encoding_mode;
      }
  | Float_extended                                                (* +COB2002 *)
  | Float_long                                                    (* +COB2002 *)
  | Float_short                                                   (* +COB2002 *)
  | Procedure_pointer                                             (* MF *)
  | Function_pointer of Cobol_ptree.name with_loc                 (* tmp *)
  | Index
  | National of (* [any] *) picture
  | Object_reference of Cobol_ptree.object_reference_kind option       (* tmp *)
  | Packed_decimal of
      {
        picture: (* [`numeric] *) picture;
        with_sign_nibble: bool; (* distinguishes COMP-6 from COMP-3/PACKED-DECIMAL *)
      }
  | Pointer of Cobol_ptree.name with_loc option                        (* tmp *)
  | Program_pointer of Cobol_ptree.name with_loc option                (* tmp *)
and signedness = { signed: bool }
and binary_truncation =
  | Truncate_to_digits of { digits: int }
  | Truncate_to_native_size
and byte_size =
  | Byte_size
  | Short_size
  | Long_size
  | Double_size
  | Long_double_size
  | C_long_size
  | Custom_size of int                                    (* > 2, != 4, 8, 16 *)
and display_sign =
  | Display_unsigned
  | Display_signed of display_sign_config
and display_sign_config =
  {
    sign_position: display_sign_position;
    sign_separate: bool;         (** [true] = separate character (extra byte) *)
  }
and display_sign_position = Leading | Trailing

type data_storage =
  | Generic_file of
      {
        file_name: Cobol_ptree.name with_loc;
        file_record_size_info: file_record_size_info option;
      }
  | Sort_merge_file of
      {
        file_name: Cobol_ptree.name with_loc;
      }
  | Local_storage
  | Working_storage
  | Linkage

and file_record_size_info =
  | Fixed_record_size of
      {
        size: int with_loc;
      }
  | Varying_record_size of
      {
        min: int with_loc option;
        max: int with_loc option;
        depending: Cobol_ptree.qualname with_loc option;
      }
  | Bound_record_size of    (* either fixed or variable (implementor-defined) *)
      {
        min: int with_loc;
        max: int with_loc;
      }

type length_variability =
  | Fixed_length
  | Variable_length
  (* Note: OCCURS DYNAMIC is considered fixed-length in ISO/IEC *)

(** {2 Records} *)

type record =
  {
    record_name: string;
    record_storage: data_storage;
    record_item: item_definition with_loc;
    record_renamings: record_renamings;
  }
and item_definitions = item_definition with_loc nel
and item_redefinitions = item_definition with_loc list

and item_definition =
  | Field of field_definition        (** for data items without OCCURS clause *)
  | Table of table_definition        (** for data items with an OCCURS clause *)

and field_definition =
  {
    field_qualname: Cobol_ptree.qualname with_loc option;
    field_leading_ranges: table_range list;
    field_offset: Data_memory.offset;         (** offset w.r.t record address *)
    field_size: Data_memory.size;
    field_layout: field_layout;
    field_length_variability: length_variability;
    field_conditions: condition_names; (** Named conditions on the value of this
                                           field. *)
    field_redefines: Cobol_ptree.qualname with_loc option;
    (** Set iff this field is a redefinition.  In that case this field appears
        inside item_redefinitions of the item it redefines.  Later, we may
        create instead a item_redefinition type. *)
    field_redefinitions: item_redefinitions; (** List of alternative definitions
                                                 for this field *)
    field_has_definition_issues: bool;
  }

and field_layout =
  | Elementary_field of
      {
        usage: usage;
        init_value: literal_value with_loc option;
      }
  | Struct_field of
      {
        subfields: item_definitions;
      }

and table_definition =
  {
    table_field: field_definition with_loc;
    table_offset: Data_memory.offset;
    table_size: Data_memory.size;
    table_range: table_range;
    table_init_values: Cobol_ptree.literal with_loc list;     (* list for now *)
    table_redefines: Cobol_ptree.qualname with_loc option; (* same as [field_redefines] but for tables *)
    table_redefinitions: item_redefinitions;
    (** List of alternative definitions for the full table.  Note that by
        default the typechecker generates a warning on table redefinition. *)
    table_has_definition_issues: bool;
  }
and table_range =
  {
    range_span: span;
    range_indexes: Cobol_ptree.qualname with_loc list;
  }
and span =
  | Fixed_span of fixed_span         (* OCCURS _ TIMES *)
  | Depending_span of depending_span (* OCCURS _ TO _ TIMES DEPENDING ON _ *)
  | Dynamic_span of dynamic_span     (* OCCURS DYNAMIC CAPACITY _ FROM _ TO _ *)

and fixed_span =
  {
    occurs_times: int with_loc;                                (* int for now *)
  }
and depending_span =
  {
    occurs_depending_min: int with_loc;                        (* int for now *)
    occurs_depending_max: int with_loc;                        (* ditto *)
    occurs_depending: Cobol_ptree.qualname with_loc;
  }
and dynamic_span =
  {
    occurs_dynamic_capacity: Cobol_ptree.qualname with_loc option;
    occurs_dynamic_capacity_min: int with_loc option;
    occurs_dynamic_capacity_max: int with_loc option;
    occurs_dynamic_initialized: bool with_loc;
  }

(** Named conditions a.k.a. level 88 items *)
and condition_names = condition_name with_loc list
and condition_name =
  {
    condition_name_qualname: Cobol_ptree.qualname with_loc;
    condition_name_item: Cobol_ptree.condition_name_item;          (* for now *)
  }

(** Note: RENAMES could be represented by simply adding an (optional,
    non-constant) offset to redefinitions (and use group layouts with FILLERs
    throughout to forbid using the new name as a qualifier).

    Such a representation would be much more general than what typical COBOL
    data definitions allow; in particular, one could have "shifted" redefintions
    of any non-01 group item.

    However, we keep the distinction between RENAMES and REDEFINES to better
    match said typical COBOL, and possibly allow more detailed error
    reporting. *)
and record_renamings = record_renaming with_loc list
and record_renaming =
  {
    renaming_name: Cobol_ptree.qualname with_loc;
    renaming_layout: renamed_item_layout;
    renaming_offset: Data_memory.offset;
    renaming_size: Data_memory.size;
    renaming_from: Cobol_ptree.qualname with_loc;
    renaming_thru: Cobol_ptree.qualname with_loc option;
    renaming_has_definition_issues: bool;
  }
and renamed_item_layout =
  | Renamed_elementary of
      {
        usage: usage;
      }
  | Renamed_struct of
      {
        subfields: item_definitions;    (* CHECKME: items rather than fields? *)
      }

(* type data_const_record = *)
(*   { *)
(*     const_name: Cobol_ptree.name with_loc; *)
(*     const_descr: Cobol_ptree.constant_item_descr; *)
(*     const_layout: const_layout; *)
(*   } *)

(** [data_definition] provides a direct access to the pair of each item and
    associated record items.  It is used for instance to retrieve data item
    information from its name. *)
type data_definition =
  | Data_field of
      {
        record: record;
        def: field_definition with_loc;
        main_def: item_definition with_loc option;
        (** Main item definition, if distinct from [Field def]. This gives the
            full definition (including [def] itself) of any item with a
            REDEFINES clause. *)
        table_def: table_definition with_loc option;
        (** When [def] is a field with an OCCURS clause (i.e, its
            [field_leading_ranges] field is not empty), this gives the
            definition for the whole table.  Otherwise it is [None]. *)
      }
  | Data_renaming of                                              (* not sure *)
      {
        record: record;
        def: record_renaming with_loc;
      }
  | Data_condition of
      {
        record: record;
        field: field_definition with_loc;
        def: condition_name with_loc;
      }
  | Table_index of
      {
        record: record;                    (* record where [table] is defined *)
        table: table_definition with_loc;          (* table whose index it is *)
        qualname: Cobol_ptree.qualname with_loc;   (* fully qualified name *)
      }

(* screen: "_ OCCURS n TIMES" only. Max 2 dimensions. *)

(** {2 Diagnostics} *)

type error =
  | Invalid of { loc: srcloc; stuff: invalid_stuff }
  | Unsupported of { loc: srcloc; stuff: unsupported_stuff }
  | Overlong_literal of { loc: srcloc;
                          literal_string: string;
                          max_length: int }       (* TODO: +kind *)

and invalid_stuff =
  | Character_in_literal of { literal_class: literal_class; char: char }

and literal_class =
  | Boolean
  | Fixed
  | Floating
  | Hexadecimal
  | Integer

and unsupported_stuff =
  | Figurative_constant: 'x. 'x Cobol_ptree.figurative -> unsupported_stuff
  | National_literal
  | Concatenation_of_literals              (* FIXME: may just be a user error *)

type errors = error NEL.t
