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

type comp_category =
  | Alphanum
  | Numeric
  | NumericEdited
  | Group
  | Any
  (* numedited, boolean, procedure_pointer, group *)

let pp_comp_category ppf = Fmt.(function
  | Alphanum -> string ppf "Alphanum"
  | Numeric -> string ppf "Numeric"
  | NumericEdited -> string ppf "NumericEdited"
  | Group -> string ppf "Group"
  | Any -> string ppf "Any")

let category_of_pic ({ category; _ }: picture) =
  match category with
  | National _ | Alphabetic _ | Alphanumeric _ -> Alphanum
  | FixedNum { editions; _ } when (editions.basics <> [] ||
                                   editions.floating <> None ||
                                   editions.zerorepl <> None)
    -> NumericEdited
  | FloatNum { editions; _ } when editions <> []
    -> NumericEdited
  | FloatNum _ | FixedNum _ -> Numeric
  | Boolean _ -> Any

let category_of_usage : usage -> comp_category = function
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

let category_of_datadef : data_definition -> (comp_category * bool) = fun d ->
  match d with
  | Data_field { def; _ } -> begin
      match ~&def.field_layout with
      | Elementary_field { usage; _ } -> (category_of_usage usage, false)
      | Struct_field _ -> (Alphanum, true)
    end
  | Data_renaming { def; _} -> begin
    match ~&def.renaming_layout with
    | Renamed_elementary { usage } -> (category_of_usage usage, false)
    | Renamed_struct _ -> (Alphanum, true)
  end
  | Data_condition _
  | Table_index _ ->
    (Any, false)

let is_valid ~comp_categories data =
  let (data_cat, is_group) = category_of_datadef data in
  List.exists
    begin fun cat ->
      cat == data_cat ||
      cat == Any ||
      (is_group && cat == Group)
    end
    comp_categories

let type_at_position ~filename (pos: Position.t) group : comp_category list =
  let open Cobol_common.Visitor in
  let open Cobol_ptree.Visitor in
  let open struct
    type acc = {
      context: comp_category list;
      value: comp_category list;
      after_pos: bool;
    }
    let init = { context = [Any]; value = [Any]; after_pos = false }
    let result acc = acc.value

    let set context f acc =
      if acc.after_pos
      then acc
      else { (f { acc with context }) with context = acc.context }

    let (@>>@) ctx fold =
      set ctx fold

    let (@>@) ctx fold =
      [ctx] @>>@ fold
  end in
  Cobol_unit.Visitor.fold_unit_group
    object (v)
      inherit [acc] Cobol_unit.Visitor.folder

      method! fold_cobol_unit cu acc =
        acc
        |> Cobol_unit.Visitor.fold_procedure v cu.unit_procedure
        |> skip

      method! fold' { loc; _ } acc =
        if acc.after_pos
        then skip acc
        else
          try
            let start = Cobol_common.Srcloc.start_pos_in ~filename loc in
            let start = start.pos_lnum - 1, start.pos_cnum - start.pos_bol in
            let pos = (pos.line, pos.character) in
            if start <= pos
            then do_children { acc with value = acc.context }
            else skip { acc with after_pos = true }
          with Invalid_argument _ ->
            skip acc

      (* add / subtract *)
      method! fold_basic_arithmetic_operands o acc =
        begin match o with
          | ArithSimple { sources; targets } ->
            acc
            |> Numeric @>@ fold_list ~fold:fold_ident_or_numlit v sources
            |> Numeric @>@ fold_list ~fold:fold_ident_or_numlit v sources
            |> Numeric @>@ fold_rounded_idents v targets
          | ArithGiving { sources; to_or_from_item; targets } ->
            acc
            |> Numeric @>@ fold_list ~fold:fold_ident_or_numlit v sources
            |> Numeric @>@ fold_ident_or_numlit v to_or_from_item
            |> [Numeric; NumericEdited] @>>@ fold_rounded_idents v targets
          | ArithCorresponding { source; target } ->
            acc
            |> Group   @>@ fold_qualname v source
            |> Group   @>@ fold_rounded_ident v target
        end
        |> skip

      method! fold_allocate_kind k acc =
        begin match k with
          | AllocateDataItem n ->
            acc
            |> Any (* linkage level 01 or 77 *) @>@ fold_name' v n
          | _ -> acc
        end
        |> skip

      method! fold_allocate' { payload = al; _ } acc =
        acc
        |> fold_allocate_kind v al.allocate_kind
        |> (* pointer || pointer32 *) fold_ident'_opt v al.allocate_returning
        |> skip

      method! fold_call_prefix p acc =
        begin match p with
          | CallGeneral i ->
            acc
            |> Alphanum (* +procedure_pointer *) @>@ fold_ident_or_strlit v i
          | _ -> acc
        end
        |> skip

      method! fold_cancel' { payload = c; _ } acc =
        acc
        |> Alphanum @>@ fold_list ~fold:fold_ident_or_strlit v c
        |> skip

      method! fold_compute' { payload = c; _ } acc =
        acc
        |> [Numeric; NumericEdited] (* + boolean *)
        @>>@ fold_rounded_idents v c.compute_targets
        |> fold_dual_handler v c.compute_on_size_error
        |> skip

      method! fold_divide_operands dop acc =
        begin match dop with
          | DivideInto i ->
            acc
            |> Numeric @>@ fold_scalar v i.divisor
            |> Numeric @>@ fold_rounded_idents v i.dividends
          | DivideGiving g ->
            acc
            |> Numeric @>@ fold_scalar v g.divisor
            |> Numeric @>@ fold_scalar v g.dividend
            |> [Numeric; NumericEdited] @>>@ fold_rounded_idents v g.giving
            |> [Numeric; NumericEdited] @>>@ fold_option ~fold:fold_ident v g.remainder
        end |> skip

      method! fold_entry_by_clause clause acc =
        begin match clause with
          | EntryByReference l ->
            acc
            |> Any (* linkage lvl 01 77*) @>@ fold_name'_list v l
          | _ -> acc
        end
        |> skip

      method! fold_free' { payload = f; _ } acc =
        acc
        |> (* pointer pointer32 *) fold_list ~fold:fold_name' v f
        |> skip

      method! fold_goto' { payload = g; _ } acc =
        begin match g with
          | GoToSimple { depending_on; _ }
          | GoToEntry { depending_on; _ } ->
            acc
            |> Numeric (* int *) @>@ fold_option ~fold:fold_ident v depending_on
        end
        |> skip

      method! fold_inspect' { payload = i; _ } acc =
        acc
        |> (* usage display *) fold_ident v i.inspect_item
        |> skip

      method! fold_invoke' { payload = i; _ } acc =
        acc
          |> (* object ref + 4byte*) fold_ident v i.invoke_target
          |> Alphanum @>@ fold_ident_or_strlit v i.invoke_method
          |> skip

      method! fold_move' { payload = m; _ } acc =
        begin match m with
          | MoveCorresponding { from; to_ } ->
            acc
            |> Group @>@ fold_ident v from
            |> Group @>@ fold_list ~fold:fold_ident v to_
          | _ -> acc
        end
        |> skip

      method! fold_multiply_operands mo acc =
          begin match mo with
          | MultiplyBy b ->
            acc
            |> Numeric @>@ fold_scalar v b.multiplier
            |> Numeric @>@ fold_rounded_idents v b.multiplicand
          | MultiplyGiving g ->
            acc
            |> Numeric @>@ fold_scalar v g.multiplier
            |> Numeric @>@ fold_scalar v g.multiplicand
            |> [Numeric; NumericEdited] @>>@ fold_rounded_idents v g.targets
        end
        |> skip

      method! fold_perform_mode pm acc =
        begin match pm with
          | PerformNTimes i ->
            acc |> Numeric @>@ fold_ident_or_intlit v i
          | PerformVarying pv ->
            acc
            |> fold_varying_phrase' v pv.varying
            |> fold_list ~fold:fold_varying_phrase' v pv.after
          | _ -> acc
        end
        |> skip

      method! fold_varying_phrase vp acc =
        acc
        |> Numeric @>@ fold_ident v vp.varying_ident
        |> Numeric @>@ fold_scalar v vp.varying_from
        |> Numeric @>@ fold_option ~fold:fold_scalar v vp.varying_by
        |> skip

      method! fold_raise' { payload = r; _ } acc =
        begin match r with
          | RaiseIdent id -> acc |> (* object ref *) fold_ident v id
          | _ -> acc
        end
        |> skip

      method! fold_search' { payload = s; _ } acc =
        acc
        |> Numeric (* +index *) @>@ fold_option ~fold:fold_ident v s.search_varying
        |> skip

      method! fold_string_stmt' { payload = s; _ } acc =
        acc
        |> Alphanum @>@ fold_ident v s.string_target
        |> fold_dual_handler v s.string_on_overflow
        |> skip

      method! fold_transform' { payload = t; _ } acc =
        acc
        |> (* technically should not be (numeric without display) *)
        fold_ident' v t.transform_ident
        |> Alphanum @>@fold' ~fold:fold_ident_or_nonnum v t.transform_from
        |> Alphanum @>@ fold' ~fold:fold_ident_or_nonnum v t.transform_to
        |> skip

      method! fold_unstring' { payload = u; _ } acc =
        acc
        |> Alphanum @>@ fold_ident v u.unstring_source
        |> Alphanum @>@ fold_list ~fold:fold_unstring_delimiter v u.unstring_delimiters
        |> (* usage display *) fold_list ~fold:fold_unstring_target v u.unstring_targets
        |> Numeric  @>@ fold_option ~fold:fold_ident v u.unstring_pointer
        |> Numeric  @>@ fold_option ~fold:fold_ident v u.unstring_tallying
        |> fold_dual_handler v u.unstring_on_overflow
        |> skip

      method! fold_unstring_target t acc =
        acc
        |> Any      @>@ fold_ident v t.unstring_target
        |> Alphanum @>@ fold_option ~fold:fold_ident v t.unstring_target_delimiter
        |> Numeric  @>@ fold_option ~fold:fold_ident v t.unstring_target_count
        |> skip

    end group init |> result
