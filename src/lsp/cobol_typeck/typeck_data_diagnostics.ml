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

open Cobol_common.Srcloc.TYPES
open Cobol_common.Srcloc.INFIX

module DIAGS = Cobol_common.Diagnostics
module NEL = Cobol_data.Types.NEL

type entry =
  | Redefines_entry
  | Renames_entry
  | Condition_name_entry

let pp_entry ppf e =
  Pretty.string ppf @@ match e with
  | Redefines_entry -> "REDEFINES"
  | Renames_entry -> "RENAMES"
  | Condition_name_entry -> "condition-name entry"

type error =
  | Item_not_allowed_in_section of
      {
        level: int with_loc;
        section: Cobol_data.Types.data_storage;
      }
  | Invalid_level_number of
      {
        level: int with_loc;
      }
  | Unexpected_level_number of
      {
        level: int with_loc;
        expected: int list;
      }
  | Unexpected_redefinition_level of
      {
        redef_loc: srcloc;
        redef_name: Cobol_ptree.data_name with_loc option;
        redef_level: int with_loc;
        expected_level: int;
        expected_redefined_loc: srcloc;
      }
  | Unexpected_redefinition_name of
      {
        redef_loc: srcloc;
        redef_name: Cobol_ptree.data_name with_loc option;
        redef_redefines: Cobol_ptree.name with_loc;
        expected_name: string;
      }
  | Redefinition_of_ODO_item of
      {
        redef_loc: srcloc;
        redef_name: Cobol_ptree.data_name with_loc option;
        redef_redefines: Cobol_ptree.name with_loc;
        odo_item: (* (_, [>`variable_length])  *)Cobol_data.Types.item_definition with_loc;
        (* odo_item_name: Cobol_ptree.qualname with_loc option; *)
      }
  | Missing_picture_clause_for_elementary_item of
      {
        item_name: Cobol_ptree.data_name with_loc option;
        item_loc: srcloc;
      }
  | Unexpected_picture_clause of
      {
        picture: Cobol_ptree.picture_clause with_loc;
        reason: [`Group_item |
                 `Item_with_usage of Cobol_ptree.usage_clause ];
        item_name: Cobol_ptree.data_name with_loc option;
        item_loc: srcloc;
      }
  | Unexpected_table_value_clause of
      {
        item_name: Cobol_ptree.data_name with_loc option;
        value_loc: srcloc;
      }
  | Occurs_in_rename_operand of
      {
        operand: Cobol_ptree.qualname with_loc;
        field: Cobol_data.Types.field_definition with_loc;
      }
  | Invalid_renaming_range of
      {
        loc: srcloc;
        from_field: Cobol_data.Types.field_definition with_loc;
        thru_field: Cobol_data.Types.field_definition with_loc;
      }
  | Invalid_renaming_of_variable_length_range of
      {
        loc: srcloc;
        depending_vars: Cobol_ptree.qualname NEL.t;
      }
  | Picture_error of
      {
        picture_loc: srcloc;
        error: Cobol_data.Picture.TYPES.error with_loc;
      }
  | Incompatible_picture of
      {
        picture: Cobol_data.Picture.t with_loc;
        usage_clause: Cobol_ptree.usage_clause;
        expected: [`Numeric_category | `Boolean_class | `Nonalpha_class];
      }
  | Item_not_found of
      {
        qualname: Cobol_ptree.qualname with_loc;
      }
  | Misplaced of
      {
        entry: entry;
        loc: srcloc;
        expl: misplacement_explanation;
      }
  | Pending_feature of
      {
        name: string;
        loc: srcloc;
      }

and warning =
  | Redefinition_of_table_item of     (* in GnuCOBOL that's a warning as well *)
      {
        redef_loc: srcloc;
        redef_name: Cobol_ptree.data_name with_loc option;
        redef_redefines: Cobol_ptree.name with_loc;
        table_item_name: Cobol_ptree.qualname with_loc option;
        (* table_item: (\* ([>`table], _)  *\)Cobol_data.Types.item_definition with_loc; *)
      }
  | Mismatching_usage_in_group of                 (* not enforced by GnuCOBOL *)
      {
        item_name: Cobol_ptree.data_name with_loc option;
        item_usage: Cobol_ptree.usage_clause with_loc;
        group_usage: Cobol_ptree.usage_clause with_loc;
      }
  | Duplicate_clause of
      {
        first_loc: srcloc;
        second_loc: srcloc;
        clause_name: string;
      }
  (* | Extraneous_clause of *)
  (*     { *)
  (*       clause_name: string; *)
  (*       clause_loc: srcloc; *)
  (*     } *)

and misplacement_explanation =
  | Following of string
  | Following_level of int

let pp_misplacement_explanation ppf = function
  | Following s ->
      Pretty.print ppf "following@ %s" s
  | Following_level l ->
      Pretty.print ppf "following@ item@ at@ level@ %02d" l

let error_loc = function
  | Incompatible_picture { picture = { loc; _ }; _ }
  | Invalid_level_number { level = { loc; _ }; _ }
  | Invalid_renaming_of_variable_length_range { loc; _ }
  | Invalid_renaming_range { loc; _ }
  | Item_not_allowed_in_section { level = { loc; _ }; _ }
  | Item_not_found { qualname = { loc; _ } }
  | Misplaced { loc; _ }
  | Missing_picture_clause_for_elementary_item { item_loc = loc; _ }
  | Occurs_in_rename_operand { operand = { loc; _ }; _ }
  | Pending_feature { loc; _ }
  | Picture_error { error = { loc; _ }; _ }
  | Redefinition_of_ODO_item { redef_loc = loc; _ }
  | Unexpected_level_number { level = { loc; _ }; _ }
  | Unexpected_picture_clause { picture = { loc; _ }; _ }
  | Unexpected_redefinition_level { redef_level = { loc; _ }; _ }
  | Unexpected_redefinition_name { redef_redefines = { loc; _ }; _ }
  | Unexpected_table_value_clause { value_loc = loc; _ } ->
      Some loc

let warning_loc = function
  | Duplicate_clause { second_loc = loc; _ }
  | Mismatching_usage_in_group { item_usage = { loc; _ }; _ }
  | Redefinition_of_table_item { redef_loc = loc; _ } ->
      Some loc

let pp_data_name'_opt
  : Cobol_ptree.data_name with_loc option Pretty.printer
  = fun ppf -> function
    | Some { payload = DataName n; _ } ->
        Pretty.print ppf "item '%s'" ~&n
    | Some { payload = DataFiller; _ } | None ->
        Pretty.print ppf "FILLER item"

let pp_one_of pp_e ppf = function                   (* assumes non-empty list *)
  | [s] -> pp_e ppf s
  | lst -> Fmt.(hbox @@ any "one of: " ++ list ~sep:comma pp_e) ppf lst

let pp_error ppf = function
  | Incompatible_picture { picture; usage_clause; expected } ->
      Pretty.print ppf "PICTURE@ of@ category@ %a@ is@ incompatible@ with@ \
                        USAGE@ %a; expected@ a@ PICTURE@ for@ %(%)@ data@ item"
        Cobol_data.Picture.pp_category_name ~&picture.category
        Cobol_ptree.pp_usage_clause usage_clause
        (match expected with
         | `Numeric_category -> "numeric"
         | `Boolean_class -> "boolean"
         | `Nonalpha_class -> "boolean,@ national,@ national-edited,@ numeric,@ \
                              or@ numeric-edited")
  | Item_not_allowed_in_section { level; section } ->
      Pretty.print ppf "%d-level@ item@ not@ allowed@ in@ %a@ section" ~&level
        Cobol_data.Types.pp_data_storage section
  | Invalid_level_number { level } ->
      Pretty.print ppf "Invalid@ level@ number:@ %02d" ~&level
  | Unexpected_level_number { level; expected } ->
      Pretty.print ppf "Unexpected@ level@ number@ %02d:@ expected@ %a" ~&level
        (pp_one_of Fmt.(fmt "%02d")) expected
  | Unexpected_redefinition_level { expected_level; redef_level;
                                    redef_name; _ } ->
      Pretty.print ppf "Invalid@ level@ %02d@ for@ %a@ with@ REDEFINES@ clause;@ \
                        expected@ level@ %02d"
        ~&redef_level pp_data_name'_opt redef_name expected_level
  | Unexpected_redefinition_name { expected_name; redef_redefines;
                                   redef_name; _ } ->
      Pretty.print ppf "Unexpected@ target@ name@ '%s'@ in@ REDEFINES@ clause@ \
                        for@ %a;@ expected@ '%s'"
        ~&redef_redefines pp_data_name'_opt redef_name expected_name
  | Redefinition_of_ODO_item { odo_item; _ } ->
      Pretty.print ppf "Invalid@ redefinition@ of@ record@ with@ subordinate@ \
                        OCCURS@ DEPENDING%a"
        (Cobol_data.Item.pp_item_qualname ~leading:Fmt.sp) ~&odo_item
  | Missing_picture_clause_for_elementary_item { item_name; _ } ->
      Pretty.print ppf "Missing@ PICTURE@ clause@ for@ %a"
        pp_data_name'_opt item_name
  | Unexpected_picture_clause { item_name; reason = `Group_item; _ } ->
      Pretty.print ppf "Unexpected@ PICTURE@ clause@ for@ group@ %a"
        pp_data_name'_opt item_name
  | Unexpected_picture_clause { item_name; reason = `Item_with_usage u; _ } ->
      Pretty.print ppf "Unexpected@ PICTURE@ clause@ for@ %a@ with@ USAGE@ %a"
        pp_data_name'_opt item_name Cobol_ptree.pp_usage_clause u
  | Unexpected_table_value_clause { item_name; _ } ->
      Pretty.print ppf "Unexpected@ table@ VALUE@ clause@ for@ %a"
        pp_data_name'_opt item_name
  | Occurs_in_rename_operand { operand; _ } ->
      Pretty.print ppf "RENAMES@ operand@ '%a'@ has@ or@ is@ subordinate@ to@ \
                        an@ OCCURS@ clause"
        Cobol_ptree.pp_qualname' operand
  | Invalid_renaming_range { from_field; thru_field; _ } ->
      Pretty.print ppf "Invalid@ pair@ of@ RENAMES@ operands: first@ field@ %a@ \
                        should@ precede@ second@ field@ %a"
        (Fmt.option Cobol_ptree.pp_qualname') ~&from_field.field_qualname
        (Fmt.option Cobol_ptree.pp_qualname') ~&thru_field.field_qualname
  | Invalid_renaming_of_variable_length_range { depending_vars = vars; _ } ->
      Pretty.print ppf "Renaming@ of@ variable-length@ range@ (length@ depends@ \
                        on:@ %a)"
        Fmt.(list ~sep:comma Cobol_ptree.pp_qualname) (NEL.to_list vars)
  | Picture_error { error; _ } ->
      Cobol_data.Picture.pp_error ppf ~&error
  | Item_not_found { qualname; _ } ->
      Pretty.print ppf "Item@ '%a'@ not@ found" Cobol_ptree.pp_qualname' qualname
  | Misplaced { entry; expl; _ } ->
      Pretty.print ppf "Misplaced@ %a@ %a\
                       " pp_entry entry pp_misplacement_explanation expl
  | Pending_feature { name; _ } ->
      Pretty.print ppf "%s@ is@ not@ supported@ yet" name

let pp_warning ppf = function
  | Redefinition_of_table_item { table_item_name; _ } ->
      Pretty.print ppf "Redefinition@ of@ item@ with@ OCCURS@ clause%a"
        Fmt.(option (sp ++ Cobol_ptree.pp_qualname')) table_item_name
  | Mismatching_usage_in_group { item_usage; item_name; group_usage } ->
      Pretty.print ppf "Mismatching@ USAGE@ %a@ for@ %a,@ subordinate@ to@ a@ \
                        group@ with@ USAGE@ %a"
        Cobol_ptree.pp_usage_clause ~&item_usage
        pp_data_name'_opt item_name
        Cobol_ptree.pp_usage_clause ~&group_usage
  | Duplicate_clause { clause_name; _ } ->   (* TODO: addendum with second loc *)
      Pretty.print ppf "Duplicate %s clause" clause_name
  (* | Extraneous_clause { clause_name; _ } -> *)
  (*     Pretty.print ppf "Extraneous %s clause" clause_name *)
