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

open Lsp.Types
open Cobol_data.Types
open Cobol_common.Srcloc.INFIX

type comp_type =
  | Alphanum
  | Numeric
  | Any

let category_of_pic ({ category; _ }: picture) =
  match category with
  | National _ | Alphabetic _ | Alphanumeric _ -> Alphanum
  | FloatNum _ | FixedNum _ -> Numeric
  | Boolean _ -> Any

let category_of_usage : usage -> comp_type = function
  | Binary _
  | Binary_C_long _
  | Binary_char _
  | Binary_double _
  | Binary_long _
  | Binary_short _
  | Float_long
  | Float_short
  | Float_binary _
  | Float_decimal _
  | Float_extended
  | Packed_decimal _ -> Numeric
  | Procedure_pointer
  | Function_pointer _
  | Pointer _
  | Index
  | Program_pointer _ -> Numeric
  | National _ -> Alphanum
  | Object_reference _
  | Bit _ -> Any
  | Display pic -> category_of_pic pic

let category_of_datadef : data_definition -> comp_type = fun d ->
  match d with
  | Data_field { def; _ } -> begin
      match ~&def.field_layout with
      | Elementary_field { usage; _ } -> category_of_usage usage
      | Struct_field _ -> Alphanum
    end
  | Data_renaming { def; _} -> begin
    match ~&def.renaming_layout with
    | Renamed_elementary { usage } -> category_of_usage usage
    | Renamed_struct _ -> Alphanum
  end
  | Data_condition _
  | Table_index _ ->
    Any

let is_valid ~(comp_type: comp_type) data =
  let t = category_of_datadef data in
  match comp_type with
  | Any -> true
  | _ when t = comp_type -> true
  | _ -> false

let type_at_position ~filename (pos: Position.t) group : comp_type =
  let open Cobol_common.Visitor in
  let open Cobol_ptree.Visitor in
  let open struct

    type acc = {
      context: comp_type;
      value: comp_type;
      after_pos: bool;
    }
    let init = { context = Any; value = Any; after_pos = false }
    let result acc = acc.value
  end in
  let set context f acc =
    if acc.after_pos
    then acc
    else { (f { acc with context }) with context = acc.context }
  in
  let any = set Any in
  Cobol_unit.Visitor.fold_unit_group
    object (v)
      inherit [acc] Cobol_unit.Visitor.folder

      method! fold' _ acc =
        if acc.after_pos
        then skip_children acc
        else do_children acc

      method! fold_qualname' { loc; _ } ({ context; _ } as acc) =
        try
          let (start, _) = Cobol_common.Srcloc.lexloc_in ~filename loc in
          let start = start.pos_lnum - 1, start.pos_cnum - start.pos_bol in
          let pos = (pos.line, pos.character) in
          if start <= pos
          then skip_children { acc with value = context }
          else skip_children { acc with after_pos = true }
        with Invalid_argument _ ->
          skip_children acc

      method! fold_unstring' { payload = u; _ } acc =
        acc
        |> set Alphanum @@ fold_ident v u.unstring_source
        |> set Alphanum @@ fold_list ~fold:fold_unstring_delimiter v u.unstring_delimiters
        |> any          @@ fold_list ~fold:fold_unstring_target v u.unstring_targets
        |> set Numeric  @@ fold_option ~fold:fold_ident v u.unstring_pointer
        |> set Numeric  @@ fold_option ~fold:fold_ident v u.unstring_tallying
        |> any          @@ fold_dual_handler v u.unstring_on_overflow
        |> skip

      method! fold_unstring_target t acc =
        acc
        |> any          @@ fold_ident v t.unstring_target
        |> set Alphanum @@ fold_option ~fold:fold_ident v t.unstring_target_delimiter
        |> set Numeric  @@ fold_option ~fold:fold_ident v t.unstring_target_count
        |> skip

    end group init |> result
